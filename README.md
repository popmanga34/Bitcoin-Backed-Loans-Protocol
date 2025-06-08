# 🏦 Bitcoin-Backed Loans Protocol

A non-custodial lending platform built on Stacks that enables users to get loans using Bitcoin as collateral. Borrowers can request loans backed by BTC, while lenders can fund these requests and earn interest.

## 🚀 Features

- 💰 **BTC-Collateralized Loans**: Secure loans backed by Bitcoin collateral
- 🔒 **Non-Custodial**: Users maintain control of their assets
- ⚡ **Automated Liquidation**: Protect lenders with automatic liquidation when collateral ratio drops
- 📊 **Health Monitoring**: Real-time loan health tracking
- 🎯 **Interest Earning**: Lenders earn 5% interest on funded loans
- ⏰ **Time-Based Loans**: Fixed duration loans with clear repayment terms

## 📋 Contract Parameters

- **Collateral Ratio**: 150% (borrowers must provide 1.5x BTC value)
- **Liquidation Threshold**: 120% (loans liquidated below this ratio)
- **Interest Rate**: 5% per loan duration
- **Loan Duration**: 144 blocks (~24 hours)

## 🛠 Usage Instructions

### For Borrowers

#### 1. Create Loan Request
```clarity
(contract-call? .btc-loans create-loan-request u1000000 u666666)
```
- `btc-collateral`: Amount of BTC collateral (in satoshis)
- `loan-amount`: Desired loan amount in STX (in microSTX)

#### 2. Repay Loan
```clarity
(contract-call? .btc-loans repay-loan u1 u100000)
```
- `loan-id`: ID of your loan
- `payment-amount`: Amount to repay (in microSTX)

#### 3. Cancel Pending Request
```clarity
(contract-call? .btc-loans cancel-loan-request u1)
```

### For Lenders

#### 1. Fund a Loan
```clarity
(contract-call? .btc-loans fund-loan u1)
```
- `loan-id`: ID of the loan to fund

#### 2. Liquidate Unhealthy Loan
```clarity
(contract-call? .btc-loans liquidate-loan u1)
```

### Read-Only Functions

#### Check Loan Details
```clarity
(contract-call? .btc-loans get-loan u1)
```

#### Check Loan Health
```clarity
(contract-call? .btc-loans get-loan-health u1)
```

#### Calculate Total Owed
```clarity
(contract-call? .btc-loans calculate-total-owed u1)
```

#### Check if Liquidatable
```clarity
(contract-call? .btc-loans is-loan-liquidatable u1)
```

#### Get Protocol Statistics
```clarity
(contract-call? .btc-loans get-protocol-stats)
```

#### View Pending Loans
```clarity
(contract-call? .btc-loans get-pending-loans u0 u10)
```

## 🔍 Loan Lifecycle

1. **📝 Request**: Borrower creates loan request with BTC collateral
2. **💸 Funding**: Lender funds the loan request
3. **⏳ Active**: Loan becomes active, interest starts accruing
4. **💰 Repayment**:
