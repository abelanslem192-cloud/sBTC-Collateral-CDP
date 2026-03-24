;; cdp-vault.clar
;; Core CDP logic: Lock sBTC, mint Stablecoin

(use-trait sip010-token .sip-010-trait.sip-010-trait)
(use-trait stable-token-trait .sip-010-trait.sip-010-trait)
(use-trait flash-loan-trait .flash-loan-trait.flash-loan-trait)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-collateral (err u101))
(define-constant err-vault-exists (err u102))
(define-constant err-vault-not-found (err u103))
(define-constant err-under-collateralized (err u104))
(define-constant err-repayment-too-high (err u105))
(define-constant err-liquidation-not-allowed (err u106))
(define-constant err-transfer-failed (err u107))
(define-constant err-paused (err u110))
(define-constant err-not-guardian (err u111))
(define-constant err-no-debt-required (err u112))
(define-constant err-emergency-withdraw-disabled (err u113))

(define-constant liquidation-ratio u150)
(define-constant liquidation-penalty u10)
(define-constant oracle-decimals u8)
(define-constant flash-mint-fee u10)

(define-data-var sbtc-price uint u50000000000)
(define-data-var pause-deposits bool false)
(define-data-var pause-borrows bool false)
(define-data-var pause-repayments bool false)
(define-data-var pause-withdrawals bool false)
(define-data-var pause-liquidations bool false)
(define-data-var pause-flash-mints bool false)
(define-data-var global-pause bool false)
(define-data-var emergency-withdraw-enabled bool false)
(define-data-var pause-guardian (optional principal) none)

(define-map vaults
    principal
    {
        collateral: uint,
        debt: uint,
    }
)

(define-read-only (get-vault (user principal))
    (default-to {
        collateral: u0,
        debt: u0,
    }
        (map-get? vaults user)
    )
)

(define-read-only (get-btc-price)
    (var-get sbtc-price)
)

(define-read-only (calculate-collateral-value (collateral-amount uint))
    (/ (* collateral-amount (var-get sbtc-price)) u100000000)
)

(define-read-only (calculate-current-ratio (user principal))
    (let (
            (vault (get-vault user))
            (collateral-val (calculate-collateral-value (get collateral vault)))
            (debt (get debt vault))
        )
        (if (is-eq debt u0)
            u99999999
            (/ (* collateral-val u100) debt)
        )
    )
)

(define-read-only (is-deposit-paused)
    (or (var-get global-pause) (var-get pause-deposits))
)

(define-read-only (is-borrow-paused)
    (or (var-get global-pause) (var-get pause-borrows))
)

(define-read-only (is-repayment-paused)
    (or (var-get global-pause) (var-get pause-repayments))
)

(define-read-only (is-withdrawal-paused)
    (or (var-get global-pause) (var-get pause-withdrawals))
)

(define-read-only (is-liquidation-paused)
    (or (var-get global-pause) (var-get pause-liquidations))
)

(define-read-only (is-flash-mint-paused)
    (or (var-get global-pause) (var-get pause-flash-mints))
)

(define-read-only (get-pause-status)
    {
        global: (var-get global-pause),
        deposits: (is-deposit-paused),
        borrows: (is-borrow-paused),
        repayments: (is-repayment-paused),
        withdrawals: (is-withdrawal-paused),
        liquidations: (is-liquidation-paused),
        flash-mints: (is-flash-mint-paused),
        emergency-withdraw: (var-get emergency-withdraw-enabled),
        guardian: (var-get pause-guardian),
    }
)

(define-read-only (get-pause-guardian)
    (var-get pause-guardian)
)

(define-read-only (is-guardian (caller principal))
    (match (var-get pause-guardian)
        guardian (is-eq caller guardian)
        false
    )
)

(define-private (is-owner-or-guardian (caller principal))
    (or (is-eq caller contract-owner) (is-guardian caller))
)

(define-public (set-pause-guardian (new-guardian (optional principal)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set pause-guardian new-guardian)
        (print {
            event: "pause-guardian-updated",
            guardian: new-guardian,
            caller: tx-sender,
        })
        (ok true)
    )
)

(define-public (set-global-pause (paused bool))
    (begin
        (asserts! (is-owner-or-guardian tx-sender) err-not-guardian)
        (if paused
            (begin
                (var-set global-pause true)
                (print {
                    event: "global-pause-activated",
                    caller: tx-sender,
                })
            )
            (begin
                (asserts! (is-eq tx-sender contract-owner) err-owner-only)
                (var-set global-pause false)
                (print {
                    event: "global-pause-deactivated",
                    caller: tx-sender,
                })
            )
        )
        (ok true)
    )
)

(define-public (set-operation-pause
        (deposits bool)
        (borrows bool)
        (repayments bool)
        (withdrawals bool)
        (liquidations bool)
        (flash-mints bool)
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set pause-deposits deposits)
        (var-set pause-borrows borrows)
        (var-set pause-repayments repayments)
        (var-set pause-withdrawals withdrawals)
        (var-set pause-liquidations liquidations)
        (var-set pause-flash-mints flash-mints)
        (print {
            event: "operation-pause-updated",
            deposits: deposits,
            borrows: borrows,
            repayments: repayments,
            withdrawals: withdrawals,
            liquidations: liquidations,
            flash-mints: flash-mints,
            caller: tx-sender,
        })
        (ok true)
    )
)

(define-public (guardian-pause-deposits)
    (begin
        (asserts! (is-owner-or-guardian tx-sender) err-not-guardian)
        (var-set pause-deposits true)
        (print { event: "guardian-paused-deposits", caller: tx-sender })
        (ok true)
    )
)

(define-public (guardian-pause-borrows)
    (begin
        (asserts! (is-owner-or-guardian tx-sender) err-not-guardian)
        (var-set pause-borrows true)
        (print { event: "guardian-paused-borrows", caller: tx-sender })
        (ok true)
    )
)

(define-public (guardian-pause-liquidations)
    (begin
        (asserts! (is-owner-or-guardian tx-sender) err-not-guardian)
        (var-set pause-liquidations true)
        (print { event: "guardian-paused-liquidations", caller: tx-sender })
        (ok true)
    )
)

(define-public (guardian-pause-flash-mints)
    (begin
        (asserts! (is-owner-or-guardian tx-sender) err-not-guardian)
        (var-set pause-flash-mints true)
        (print { event: "guardian-paused-flash-mints", caller: tx-sender })
        (ok true)
    )
)

(define-public (set-emergency-withdraw (enabled bool))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set emergency-withdraw-enabled enabled)
        (print {
            event: "emergency-withdraw-toggled",
            enabled: enabled,
            caller: tx-sender,
        })
        (ok true)
    )
)

(define-public (set-price (new-price uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set sbtc-price new-price)
        (ok true)
    )
)

(define-public (deposit-collateral (amount uint))
    (let (
            (vault (get-vault tx-sender))
            (current-collateral (get collateral vault))
            (new-collateral (+ current-collateral amount))
        )
        (asserts! (not (is-deposit-paused)) err-paused)
        (try! (contract-call? .sbtc-token transfer amount tx-sender
            (as-contract tx-sender) none
        ))
        (map-set vaults tx-sender {
            collateral: new-collateral,
            debt: (get debt vault),
        })
        (ok new-collateral)
    )
)

(define-public (borrow (amount uint))
    (let (
            (vault (get-vault tx-sender))
            (current-debt (get debt vault))
            (new-debt (+ current-debt amount))
            (collateral-val (calculate-collateral-value (get collateral vault)))
        )
        (asserts! (not (is-borrow-paused)) err-paused)
        (asserts! (>= (/ (* collateral-val u100) new-debt) liquidation-ratio)
            err-under-collateralized
        )
        (try! (contract-call? .stable-token mint-for-vault amount tx-sender))
        (map-set vaults tx-sender {
            collateral: (get collateral vault),
            debt: new-debt,
        })
        (ok new-debt)
    )
)

(define-public (repay (amount uint))
    (let (
            (vault (get-vault tx-sender))
            (current-debt (get debt vault))
        )
        (asserts! (not (is-repayment-paused)) err-paused)
        (asserts! (<= amount current-debt) err-repayment-too-high)
        (try! (contract-call? .stable-token burn-for-vault amount tx-sender))
        (map-set vaults tx-sender {
            collateral: (get collateral vault),
            debt: (- current-debt amount),
        })
        (ok (- current-debt amount))
    )
)

(define-public (withdraw-collateral (amount uint))
    (let (
            (vault (get-vault tx-sender))
            (current-collateral (get collateral vault))
            (new-collateral (- current-collateral amount))
            (debt (get debt vault))
            (collateral-val (calculate-collateral-value new-collateral))
        )
        (asserts! (not (is-withdrawal-paused)) err-paused)
        (asserts! (>= current-collateral amount) err-insufficient-collateral)
        (asserts!
            (or (is-eq debt u0) (>= (/ (* collateral-val u100) debt) liquidation-ratio))
            err-under-collateralized
        )
        (try! (as-contract (contract-call? .sbtc-token transfer amount tx-sender tx-sender none)))
        (map-set vaults tx-sender {
            collateral: new-collateral,
            debt: debt,
        })
        (ok new-collateral)
    )
)

(define-public (emergency-withdraw (amount uint))
    (let (
            (vault (get-vault tx-sender))
            (current-collateral (get collateral vault))
            (debt (get debt vault))
            (new-collateral (- current-collateral amount))
        )
        (asserts! (var-get emergency-withdraw-enabled) err-emergency-withdraw-disabled)
        (asserts! (is-eq debt u0) err-no-debt-required)
        (asserts! (>= current-collateral amount) err-insufficient-collateral)
        (try! (as-contract (contract-call? .sbtc-token transfer amount tx-sender tx-sender none)))
        (map-set vaults tx-sender {
            collateral: new-collateral,
            debt: u0,
        })
        (print {
            event: "emergency-withdrawal",
            user: tx-sender,
            amount: amount,
            remaining: new-collateral,
        })
        (ok new-collateral)
    )
)

(define-public (liquidate (target principal))
    (let (
            (vault (get-vault target))
            (collateral (get collateral vault))
            (debt (get debt vault))
            (ratio (calculate-current-ratio target))
        )
        (asserts! (not (is-liquidation-paused)) err-paused)
        (asserts! (< ratio liquidation-ratio) err-liquidation-not-allowed)
        (let (
                (debt-value-in-collateral (/ (* debt u100000000) (var-get sbtc-price)))
                (reward-collateral (/ (* debt-value-in-collateral (+ u100 liquidation-penalty)) u100))
                (actual-reward (if (> reward-collateral collateral)
                    collateral
                    reward-collateral
                ))
            )
            (try! (contract-call? .stable-token burn-for-vault debt tx-sender))
            (try! (as-contract (contract-call? .sbtc-token transfer actual-reward tx-sender
                tx-sender none
            )))
            (map-delete vaults target)
            (ok actual-reward)
        )
    )
)

(define-public (flash-mint
        (amount uint)
        (flash-loan-contract <flash-loan-trait>)
    )
    (let (
            (fee (/ (* amount flash-mint-fee) u10000))
            (total-repay (+ amount fee))
        )
        (asserts! (not (is-flash-mint-paused)) err-paused)
        (try! (contract-call? .stable-token mint-for-vault amount tx-sender))
        (try! (contract-call? flash-loan-contract execute amount))
        (try! (contract-call? .stable-token burn-for-vault total-repay tx-sender))
        (ok total-repay)
    )
)
