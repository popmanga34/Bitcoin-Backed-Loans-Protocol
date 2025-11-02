(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_LOAN_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_COLLATERAL (err u102))
(define-constant ERR_LOAN_ALREADY_EXISTS (err u103))
(define-constant ERR_LOAN_NOT_ACTIVE (err u104))
(define-constant ERR_PAYMENT_FAILED (err u105))
(define-constant ERR_LIQUIDATION_NOT_ALLOWED (err u106))
(define-constant ERR_INVALID_AMOUNT (err u107))
(define-constant ERR_LOAN_EXPIRED (err u108))

(define-constant COLLATERAL_RATIO u150)
(define-constant LIQUIDATION_THRESHOLD u120)
(define-constant INTEREST_RATE u5)
(define-constant LOAN_DURATION u144)

(define-constant ERR_EXTENSION_NOT_ALLOWED (err u109))
(define-constant ERR_MAX_EXTENSIONS_REACHED (err u110))
(define-constant EXTENSION_FEE_RATE u10)
(define-constant MAX_EXTENSIONS u3)
(define-constant EXTENSION_DURATION u72)

(define-constant AUCTION_DURATION u144)
(define-constant MIN_BID_INCREMENT u1)
(define-constant ERR_AUCTION_ENDED (err u301))
(define-constant ERR_AUCTION_NOT_ACTIVE (err u302))
(define-constant ERR_BID_TOO_LOW (err u303))
(define-constant ERR_SELF_BID (err u304))

(define-constant ERR_TRANSFER_NOT_ALLOWED (err u401))
(define-constant ERR_INVALID_RECIPIENT (err u402))
(define-constant ERR_TRANSFER_TO_SELF (err u403))

(define-data-var loan-id-nonce uint u0)
(define-data-var total-loans-issued uint u0)
(define-data-var total-active-loans uint u0)
(define-data-var protocol-fee-rate uint u1)

(define-map loans
  uint
  {
    borrower: principal,
    lender: (optional principal),
    btc-collateral: uint,
    loan-amount: uint,
    interest-rate: uint,
    start-block: uint,
    duration: uint,
    status: (string-ascii 20),
    repaid-amount: uint
  }
)

(define-map user-loan-count principal uint)
(define-map lender-funds principal uint)
(define-map borrower-collateral principal uint)

(define-public (create-loan-request (btc-collateral uint) (loan-amount uint))
  (let (
    (current-loan-id (+ (var-get loan-id-nonce) u1))
    (current-user-loans (default-to u0 (map-get? user-loan-count tx-sender)))
  )
    (asserts! (> btc-collateral u0) ERR_INVALID_AMOUNT)
    (asserts! (> loan-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (* btc-collateral u100) (* loan-amount COLLATERAL_RATIO)) ERR_INSUFFICIENT_COLLATERAL)
    
    (map-set loans current-loan-id {
      borrower: tx-sender,
      lender: none,
      btc-collateral: btc-collateral,
      loan-amount: loan-amount,
      interest-rate: INTEREST_RATE,
      start-block: u0,
      duration: LOAN_DURATION,
      status: "pending",
      repaid-amount: u0
    })
    
    (map-set user-loan-count tx-sender (+ current-user-loans u1))
    (map-set borrower-collateral tx-sender (+ (default-to u0 (map-get? borrower-collateral tx-sender)) btc-collateral))
    (var-set loan-id-nonce current-loan-id)
    
    (ok current-loan-id)
  )
)

(define-public (fund-loan (loan-id uint))
  (let (
    (loan-data (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
    (loan-amount (get loan-amount loan-data))
  )
    (asserts! (is-eq (get status loan-data) "pending") ERR_LOAN_NOT_ACTIVE)
    (asserts! (is-none (get lender loan-data)) ERR_LOAN_ALREADY_EXISTS)
    
    (try! (stx-transfer? loan-amount tx-sender (get borrower loan-data)))
    
    (map-set loans loan-id (merge loan-data {
      lender: (some tx-sender),
      start-block: stacks-block-height,
      status: "active"
    }))
    
    (map-set lender-funds tx-sender (+ (default-to u0 (map-get? lender-funds tx-sender)) loan-amount))
    (var-set total-loans-issued (+ (var-get total-loans-issued) u1))
    (var-set total-active-loans (+ (var-get total-active-loans) u1))
    
    (ok true)
  )
)

(define-public (repay-loan (loan-id uint) (payment-amount uint))
  (let (
    (loan-data (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
    (lender (unwrap! (get lender loan-data) ERR_LOAN_NOT_FOUND))
    (total-owed (calculate-total-owed loan-id))
    (current-repaid (get repaid-amount loan-data))
    (new-repaid-amount (+ current-repaid payment-amount))
  )
    (asserts! (is-eq (get borrower loan-data) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status loan-data) "active") ERR_LOAN_NOT_ACTIVE)
    (asserts! (> payment-amount u0) ERR_INVALID_AMOUNT)
    
    (try! (stx-transfer? payment-amount tx-sender lender))
    
    (if (>= new-repaid-amount total-owed)
      (begin
        (map-set loans loan-id (merge loan-data {
          status: "repaid",
          repaid-amount: total-owed
        }))
        (map-set borrower-collateral tx-sender 
          (- (default-to u0 (map-get? borrower-collateral tx-sender)) (get btc-collateral loan-data)))
        (var-set total-active-loans (- (var-get total-active-loans) u1))
      )
      (map-set loans loan-id (merge loan-data {
        repaid-amount: new-repaid-amount
      }))
    )
    
    (ok true)
  )
)

(define-public (liquidate-loan (loan-id uint))
  (let (
    (loan-data (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
    (lender (unwrap! (get lender loan-data) ERR_LOAN_NOT_FOUND))
    (borrower (get borrower loan-data))
    (collateral-value (get btc-collateral loan-data))
    (loan-amount (get loan-amount loan-data))
    (total-owed (calculate-total-owed loan-id))
    (current-ratio (/ (* collateral-value u100) loan-amount))
  )
    (asserts! (is-eq tx-sender lender) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status loan-data) "active") ERR_LOAN_NOT_ACTIVE)
    (asserts! (or 
      (< current-ratio LIQUIDATION_THRESHOLD)
      (> (+ (get start-block loan-data) (get duration loan-data)) stacks-block-height)
    ) ERR_LIQUIDATION_NOT_ALLOWED)
    
    (map-set loans loan-id (merge loan-data {
      status: "liquidated"
    }))
    
    (map-set borrower-collateral borrower 
      (- (default-to u0 (map-get? borrower-collateral borrower)) collateral-value))
    (var-set total-active-loans (- (var-get total-active-loans) u1))
    
    (ok collateral-value)
  )
)

(define-public (cancel-loan-request (loan-id uint))
  (let (
    (loan-data (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
  )
    (asserts! (is-eq (get borrower loan-data) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status loan-data) "pending") ERR_LOAN_NOT_ACTIVE)
    (asserts! (is-none (get lender loan-data)) ERR_LOAN_ALREADY_EXISTS)
    
    (map-set loans loan-id (merge loan-data {
      status: "cancelled"
    }))
    
    (map-set borrower-collateral tx-sender 
      (- (default-to u0 (map-get? borrower-collateral tx-sender)) (get btc-collateral loan-data)))
    
    (ok true)
  )
)

(define-read-only (get-loan (loan-id uint))
  (map-get? loans loan-id)
)

(define-read-only (get-user-loan-count (user principal))
  (default-to u0 (map-get? user-loan-count user))
)

(define-read-only (get-lender-funds (lender principal))
  (default-to u0 (map-get? lender-funds lender))
)

(define-read-only (get-borrower-collateral (borrower principal))
  (default-to u0 (map-get? borrower-collateral borrower))
)

(define-read-only (calculate-total-owed (loan-id uint))
  (match (map-get? loans loan-id)
    loan-data 
    (let (
      (principal-amount (get loan-amount loan-data))
      (blocks-elapsed (- stacks-block-height (get start-block loan-data)))
      (interest-amount (/ (* principal-amount (get interest-rate loan-data) blocks-elapsed) (* u100 u144)))
    )
      (+ principal-amount interest-amount)
    )
    u0
  )
)

(define-read-only (get-loan-health (loan-id uint))
  (match (map-get? loans loan-id)
    loan-data
    (let (
      (collateral-value (get btc-collateral loan-data))
      (loan-amount (get loan-amount loan-data))
      (health-ratio (/ (* collateral-value u100) loan-amount))
    )
      (ok health-ratio)
    )
    ERR_LOAN_NOT_FOUND
  )
)

(define-read-only (is-loan-liquidatable (loan-id uint))
  (match (map-get? loans loan-id)
    loan-data
    (let (
      (collateral-value (get btc-collateral loan-data))
      (loan-amount (get loan-amount loan-data))
      (current-ratio (/ (* collateral-value u100) loan-amount))
      (is-expired (> stacks-block-height (+ (get start-block loan-data) (get duration loan-data))))
    )
      (ok (or (< current-ratio LIQUIDATION_THRESHOLD) is-expired))
    )
    ERR_LOAN_NOT_FOUND
  )
)

(define-read-only (get-protocol-stats)
  {
    total-loans-issued: (var-get total-loans-issued),
    total-active-loans: (var-get total-active-loans),
    current-loan-id: (var-get loan-id-nonce),
    collateral-ratio: COLLATERAL_RATIO,
    liquidation-threshold: LIQUIDATION_THRESHOLD,
    interest-rate: INTEREST_RATE,
    loan-duration: LOAN_DURATION
  }
)

(define-read-only (get-pending-loans (start uint) (limit uint))
  (let (
    (max-id (var-get loan-id-nonce))
    (end-id (if (> (+ start limit) max-id) max-id (+ start limit)))
  )
    (map get-loan-if-pending (list start (+ start u1) (+ start u2) (+ start u3) (+ start u4)))
  )
)

(define-private (get-loan-if-pending (loan-id uint))
  (match (map-get? loans loan-id)
    loan-data
    (if (is-eq (get status loan-data) "pending")
      (some {loan-id: loan-id, loan-data: loan-data})
      none
    )
    none
  )
)

(define-map loan-extensions
  uint
  {
    extensions-used: uint,
    total-extension-fees: uint,
    last-extension-block: uint
  }
)

(define-public (extend-loan (loan-id uint))
  (let (
    (loan-data (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
    (lender (unwrap! (get lender loan-data) ERR_LOAN_NOT_FOUND))
    (extension-data (default-to {extensions-used: u0, total-extension-fees: u0, last-extension-block: u0} 
                                (map-get? loan-extensions loan-id)))
    (extension-fee (/ (* (get loan-amount loan-data) EXTENSION_FEE_RATE) u100))
    (current-extensions (get extensions-used extension-data))
    (blocks-until-expiry (- (+ (get start-block loan-data) (get duration loan-data)) stacks-block-height))
  )
    (asserts! (is-eq (get borrower loan-data) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status loan-data) "active") ERR_LOAN_NOT_ACTIVE)
    (asserts! (< current-extensions MAX_EXTENSIONS) ERR_MAX_EXTENSIONS_REACHED)
    (asserts! (<= blocks-until-expiry u24) ERR_EXTENSION_NOT_ALLOWED)
    
    (try! (stx-transfer? extension-fee tx-sender lender))
    
    (map-set loans loan-id (merge loan-data {
      duration: (+ (get duration loan-data) EXTENSION_DURATION)
    }))
    
    (map-set loan-extensions loan-id {
      extensions-used: (+ current-extensions u1),
      total-extension-fees: (+ (get total-extension-fees extension-data) extension-fee),
      last-extension-block: stacks-block-height
    })
    
    (ok true)
  )
)

(define-read-only (get-extension-info (loan-id uint))
  (map-get? loan-extensions loan-id)
)

(define-read-only (can-extend-loan (loan-id uint))
  (match (map-get? loans loan-id)
    loan-data
    (let (
      (extension-data (default-to {extensions-used: u0, total-extension-fees: u0, last-extension-block: u0} 
                                  (map-get? loan-extensions loan-id)))
      (blocks-until-expiry (- (+ (get start-block loan-data) (get duration loan-data)) stacks-block-height))
    )
      (ok (and 
        (is-eq (get status loan-data) "active")
        (< (get extensions-used extension-data) MAX_EXTENSIONS)
        (<= blocks-until-expiry u24)
      ))
    )
    ERR_LOAN_NOT_FOUND
  )
)

(define-read-only (calculate-extension-fee (loan-id uint))
  (match (map-get? loans loan-id)
    loan-data
    (ok (/ (* (get loan-amount loan-data) EXTENSION_FEE_RATE) u100))
    ERR_LOAN_NOT_FOUND
  )
)

(define-constant ALERT_LIQUIDATION_RISK "liquidation-risk")
(define-constant ALERT_PAYMENT_DUE "payment-due")
(define-constant ALERT_LOAN_FUNDED "loan-funded")
(define-constant ALERT_REPAYMENT_RECEIVED "repayment-received")

(define-constant ERR_INVALID_ALERT_TYPE (err u201))
(define-constant ERR_ALREADY_SUBSCRIBED (err u202))
(define-constant ERR_NOT_SUBSCRIBED (err u203))

(define-map user-alert-subscriptions
  {user: principal, alert-type: (string-ascii 20)}
  {subscribed: bool, subscription-block: uint}
)

(define-map loan-alerts
  uint
  {alert-count: uint, last-alert-block: uint, alert-types: (list 10 (string-ascii 20))}
)

(define-public (subscribe-to-alert (alert-type (string-ascii 20)))
  (let (
    (subscription-key {user: tx-sender, alert-type: alert-type})
    (existing-subscription (map-get? user-alert-subscriptions subscription-key))
  )
    (asserts! (or 
      (is-eq alert-type ALERT_LIQUIDATION_RISK)
      (is-eq alert-type ALERT_PAYMENT_DUE)
      (is-eq alert-type ALERT_LOAN_FUNDED)
      (is-eq alert-type ALERT_REPAYMENT_RECEIVED)
    ) ERR_INVALID_ALERT_TYPE)
    
    (asserts! (is-none existing-subscription) ERR_ALREADY_SUBSCRIBED)
    
    (map-set user-alert-subscriptions subscription-key {
      subscribed: true,
      subscription-block: stacks-block-height
    })
    
    (print {event: "alert-subscription", user: tx-sender, alert-type: alert-type})
    (ok true)
  )
)

(define-public (unsubscribe-from-alert (alert-type (string-ascii 20)))
  (let (
    (subscription-key {user: tx-sender, alert-type: alert-type})
    (existing-subscription (unwrap! (map-get? user-alert-subscriptions subscription-key) ERR_NOT_SUBSCRIBED))
  )
    (map-delete user-alert-subscriptions subscription-key)
    (print {event: "alert-unsubscription", user: tx-sender, alert-type: alert-type})
    (ok true)
  )
)

(define-public (trigger-loan-alert (loan-id uint) (alert-type (string-ascii 20)))
  (let (
    (loan-data (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
    (borrower (get borrower loan-data))
    (lender (get lender loan-data))
    (alert-data (default-to {alert-count: u0, last-alert-block: u0, alert-types: (list)} (map-get? loan-alerts loan-id)))
    (new-alert-types (unwrap-panic (as-max-len? (append (get alert-types alert-data) alert-type) u10)))
  )
    (map-set loan-alerts loan-id {
      alert-count: (+ (get alert-count alert-data) u1),
      last-alert-block: stacks-block-height,
      alert-types: new-alert-types
    })
    
    (print {
      event: "loan-alert",
      loan-id: loan-id,
      alert-type: alert-type,
      borrower: borrower,
      lender: lender,
      block-height: stacks-block-height
    })
    
    (ok true)
  )
)

(define-read-only (is-subscribed-to-alert (user principal) (alert-type (string-ascii 20)))
  (is-some (map-get? user-alert-subscriptions {user: user, alert-type: alert-type}))
)

(define-read-only (get-loan-alert-history (loan-id uint))
  (map-get? loan-alerts loan-id)
)

(define-read-only (get-user-subscriptions (user principal))
  {
    liquidation-risk: (is-subscribed-to-alert user ALERT_LIQUIDATION_RISK),
    payment-due: (is-subscribed-to-alert user ALERT_PAYMENT_DUE),
    loan-funded: (is-subscribed-to-alert user ALERT_LOAN_FUNDED),
    repayment-received: (is-subscribed-to-alert user ALERT_REPAYMENT_RECEIVED)
  }
)


(define-map loan-auctions
  uint
  {
    auction-end-block: uint,
    highest-bidder: (optional principal),
    lowest-interest-rate: uint,
    bid-count: uint,
    auction-active: bool
  }
)

(define-map auction-bids
  {loan-id: uint, bidder: principal}
  {
    interest-rate: uint,
    bid-block: uint,
    funds-locked: uint
  }
)

(define-public (start-auction (loan-id uint))
  (let (
    (loan-data (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
  )
    (asserts! (is-eq (get borrower loan-data) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status loan-data) "pending") ERR_LOAN_NOT_ACTIVE)
    
    (map-set loan-auctions loan-id {
      auction-end-block: (+ stacks-block-height AUCTION_DURATION),
      highest-bidder: none,
      lowest-interest-rate: INTEREST_RATE,
      bid-count: u0,
      auction-active: true
    })
    
    (ok true)
  )
)

(define-public (place-bid (loan-id uint) (interest-rate uint))
  (let (
    (loan-data (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
    (auction-data (unwrap! (map-get? loan-auctions loan-id) ERR_AUCTION_NOT_ACTIVE))
    (loan-amount (get loan-amount loan-data))
    (current-lowest (get lowest-interest-rate auction-data))
  )
    (asserts! (not (is-eq tx-sender (get borrower loan-data))) ERR_SELF_BID)
    (asserts! (get auction-active auction-data) ERR_AUCTION_NOT_ACTIVE)
    (asserts! (< stacks-block-height (get auction-end-block auction-data)) ERR_AUCTION_ENDED)
    (asserts! (< interest-rate current-lowest) ERR_BID_TOO_LOW)
    
    (try! (stx-transfer? loan-amount tx-sender (as-contract tx-sender)))
    
    (map-set auction-bids {loan-id: loan-id, bidder: tx-sender} {
      interest-rate: interest-rate,
      bid-block: stacks-block-height,
      funds-locked: loan-amount
    })
    
    (map-set loan-auctions loan-id (merge auction-data {
      highest-bidder: (some tx-sender),
      lowest-interest-rate: interest-rate,
      bid-count: (+ (get bid-count auction-data) u1)
    }))
    
    (ok true)
  )
)

(define-public (finalize-auction (loan-id uint))
  (let (
    (loan-data (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
    (auction-data (unwrap! (map-get? loan-auctions loan-id) ERR_AUCTION_NOT_ACTIVE))
    (winning-bidder (get highest-bidder auction-data))
    (winning-rate (get lowest-interest-rate auction-data))
    (loan-amount (get loan-amount loan-data))
  )
    (asserts! (>= stacks-block-height (get auction-end-block auction-data)) ERR_AUCTION_NOT_ACTIVE)
    (asserts! (get auction-active auction-data) ERR_AUCTION_ENDED)
    
    (if (is-some winning-bidder)
      (begin
        (try! (as-contract (stx-transfer? loan-amount tx-sender (get borrower loan-data))))
        (map-set loans loan-id (merge loan-data {
          lender: winning-bidder,
          interest-rate: winning-rate,
          start-block: stacks-block-height,
          status: "active"
        }))
      )
      (map-set loans loan-id (merge loan-data {status: "expired"}))
    )
    
    (map-set loan-auctions loan-id (merge auction-data {auction-active: false}))
    (ok true)
  )
)

(define-read-only (get-auction-info (loan-id uint))
  (map-get? loan-auctions loan-id)
)


(define-map loan-transfers
  uint
  {transfer-count: uint, last-transfer-block: uint, original-lender: principal}
)

(define-public (transfer-loan (loan-id uint) (recipient principal))
  (let (
    (loan-data (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
    (current-lender (unwrap! (get lender loan-data) ERR_LOAN_NOT_FOUND))
    (transfer-data (map-get? loan-transfers loan-id))
  )
    (asserts! (is-eq tx-sender current-lender) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status loan-data) "active") ERR_TRANSFER_NOT_ALLOWED)
    (asserts! (not (is-eq recipient tx-sender)) ERR_TRANSFER_TO_SELF)
    (asserts! (not (is-eq recipient (get borrower loan-data))) ERR_INVALID_RECIPIENT)
    
    (map-set loans loan-id (merge loan-data {lender: (some recipient)}))
    
    (map-set lender-funds tx-sender 
      (- (default-to u0 (map-get? lender-funds tx-sender)) (get loan-amount loan-data)))
    (map-set lender-funds recipient 
      (+ (default-to u0 (map-get? lender-funds recipient)) (get loan-amount loan-data)))
    
    (map-set loan-transfers loan-id 
      (if (is-some transfer-data)
        (let ((data (unwrap-panic transfer-data)))
          {
            transfer-count: (+ (get transfer-count data) u1),
            last-transfer-block: stacks-block-height,
            original-lender: (get original-lender data)
          }
        )
        {
          transfer-count: u1,
          last-transfer-block: stacks-block-height,
          original-lender: tx-sender
        }
      )
    )
    
    (print {
      event: "loan-transferred",
      loan-id: loan-id,
      from: tx-sender,
      to: recipient,
      block: stacks-block-height
    })
    
    (ok true)
  )
)

(define-read-only (get-transfer-history (loan-id uint))
  (map-get? loan-transfers loan-id)
)

(define-read-only (can-transfer-loan (loan-id uint) (sender principal))
  (match (map-get? loans loan-id)
    loan-data
    (ok (and 
      (is-eq (unwrap-panic (get lender loan-data)) sender)
      (is-eq (get status loan-data) "active")
    ))
    ERR_LOAN_NOT_FOUND
  )
)


(define-constant ERR_LOAN_NOT_PENDING (err u501))
(define-constant ERR_OVERFUNDING (err u502))
(define-constant ERR_LOAN_ALREADY_ACTIVE (err u503))
(define-constant ERR_NO_CONTRIBUTION (err u504))
(define-constant ERR_ALREADY_ACTIVATED (err u505))

(define-map partial-fundings
  uint
  {
    total-funded: uint,
    target-amount: uint,
    contributor-count: uint,
    is-activated: bool,
    activation-block: uint
  }
)

(define-map lender-contributions
  {loan-id: uint, lender: principal}
  {
    amount-contributed: uint,
    contribution-block: uint,
    share-percentage: uint
  }
)

(define-map loan-contributors
  uint
  (list 50 principal)
)

(define-public (contribute-to-loan (loan-id uint) (contribution-amount uint))
  (let (
    (funding-data (default-to 
      {total-funded: u0, target-amount: u0, contributor-count: u0, is-activated: false, activation-block: u0}
      (map-get? partial-fundings loan-id)))
    (new-total (+ (get total-funded funding-data) contribution-amount))
    (contribution-key {loan-id: loan-id, lender: tx-sender})
    (existing-contribution (map-get? lender-contributions contribution-key))
    (contributors (default-to (list) (map-get? loan-contributors loan-id)))
  )
    (asserts! (> contribution-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (not (get is-activated funding-data)) ERR_LOAN_ALREADY_ACTIVE)
    
    (if (is-some existing-contribution)
      (let ((contrib (unwrap-panic existing-contribution)))
        (map-set lender-contributions contribution-key {
          amount-contributed: (+ (get amount-contributed contrib) contribution-amount),
          contribution-block: stacks-block-height,
          share-percentage: u0
        })
      )
      (begin
        (map-set lender-contributions contribution-key {
          amount-contributed: contribution-amount,
          contribution-block: stacks-block-height,
          share-percentage: u0
        })
        (map-set loan-contributors loan-id 
          (unwrap-panic (as-max-len? (append contributors tx-sender) u50)))
      )
    )
    
    (map-set partial-fundings loan-id {
      total-funded: new-total,
      target-amount: (get target-amount funding-data),
      contributor-count: (if (is-some existing-contribution) 
                           (get contributor-count funding-data) 
                           (+ (get contributor-count funding-data) u1)),
      is-activated: false,
      activation-block: u0
    })
    
    (ok new-total)
  )
)

(define-public (activate-partial-loan (loan-id uint) (target-amount uint))
  (let (
    (funding-data (unwrap! (map-get? partial-fundings loan-id) ERR_LOAN_NOT_FOUND))
    (contributors (default-to (list) (map-get? loan-contributors loan-id)))
  )
    (asserts! (>= (get total-funded funding-data) target-amount) ERR_INVALID_AMOUNT)
    (asserts! (not (get is-activated funding-data)) ERR_ALREADY_ACTIVATED)
    
    (update-contributor-shares loan-id (get total-funded funding-data) contributors)
    
    (map-set partial-fundings loan-id (merge funding-data {
      is-activated: true,
      activation-block: stacks-block-height,
      target-amount: target-amount
    }))
    
    (ok true)
  )
)

(define-private (update-contributor-shares (loan-id uint) (total-amount uint) (contributors (list 50 principal)))
  (fold update-share-for-contributor contributors {loan-id: loan-id, total: total-amount})
)

(define-private (update-share-for-contributor (contributor principal) (context {loan-id: uint, total: uint}))
  (let (
    (loan-id (get loan-id context))
    (total-amount (get total context))
    (contribution-key {loan-id: loan-id, lender: contributor})
    (contrib-data (unwrap-panic (map-get? lender-contributions contribution-key)))
    (share-pct (/ (* (get amount-contributed contrib-data) u10000) total-amount))
  )
    (map-set lender-contributions contribution-key (merge contrib-data {
      share-percentage: share-pct
    }))
    context
  )
)

(define-read-only (get-funding-status (loan-id uint))
  (map-get? partial-fundings loan-id)
)

(define-read-only (get-lender-contribution (loan-id uint) (lender principal))
  (map-get? lender-contributions {loan-id: loan-id, lender: lender})
)

(define-read-only (get-all-contributors (loan-id uint))
  (map-get? loan-contributors loan-id)
)

(define-read-only (calculate-funding-progress (loan-id uint) (target uint))
  (match (map-get? partial-fundings loan-id)
    funding-data
    (ok (/ (* (get total-funded funding-data) u100) target))
    (ok u0)
  )
)