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
