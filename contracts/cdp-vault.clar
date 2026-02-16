(use-trait sip010-token .sip-010-trait.sip-010-trait)
(use-trait stable-token-trait .sip-010-trait.sip-010-trait)
(use-trait flash-loan-trait .flash-loan-trait.flash-loan-trait)
(use-trait oracle-trait .oracle-trait.oracle-trait)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-collateral (err u101))
(define-constant err-vault-exists (err u102))
(define-constant err-vault-not-found (err u103))
(define-constant err-under-collateralized (err u104))
(define-constant err-repayment-too-high (err u105))
(define-constant err-liquidation-not-allowed (err u106))
(define-constant err-transfer-failed (err u107))
(define-constant err-collateral-not-supported (err u108))
(define-constant err-debt-ceiling-reached (err u109))
(define-constant err-oracle-error (err u110))
(define-constant err-unauthorized (err u111))

(define-constant liquidation-penalty u10)
(define-constant oracle-decimals u8)
(define-constant flash-mint-fee u10)
(define-constant scale-factor u100000000)
(define-constant target-ratio-after-liquidation u175)
(define-constant critical-ratio u100)

(define-map supported-collateral
    principal
    {
        name: (string-ascii 32),
        liquidation-ratio: uint,
        debt-ceiling: uint,
        interest-rate: uint,
        total-debt: uint,
        active: bool,
    }
)

;; Single Oracle Registry for all assets
(define-data-var oracle-registry-contract principal tx-sender)

(define-map vaults
    {
        owner: principal,
        collateral-token: principal,
    }
    {
        collateral: uint,
        debt: uint,
        last-accrued-block: uint,
    }
)

(define-map vault-delegates
    {
        owner: principal,
        delegate: principal,
    }
    bool
)

(define-private (calculate-accrued-interest
        (debt uint)
        (last-accrued-block uint)
        (interest-rate uint)
    )
    (if (or (is-eq debt u0) (is-eq last-accrued-block u0))
        u0
        (let (
                (delta-blocks (- burn-block-height last-accrued-block))
                (interest (/ (* debt interest-rate delta-blocks) scale-factor))
            )
            interest
        )
    )
)

(define-private (is-authorized
        (vault-owner principal)
        (caller principal)
    )
    (or
        (is-eq vault-owner caller)
        (default-to false
            (map-get? vault-delegates {
                owner: vault-owner,
                delegate: caller,
            })
        )
    )
)

(define-read-only (get-collateral-type (token principal))
    (map-get? supported-collateral token)
)

(define-read-only (get-vault
        (user principal)
        (token principal)
    )
    (default-to {
        collateral: u0,
        debt: u0,
        last-accrued-block: burn-block-height,
    }
        (map-get? vaults {
            owner: user,
            collateral-token: token,
        })
    )
)

;; Helper to fetch latest price from registry
(define-public (get-price-from-oracle
        (token principal)
        (oracle <oracle-trait>)
    )
    (begin
        ;; Ensure passed oracle trait matches registry
        (asserts! (is-eq (contract-of oracle) (var-get oracle-registry-contract))
            err-oracle-error
        )
        (contract-call? oracle get-price token)
    )
)

(define-read-only (calculate-collateral-value
        (collateral-amount uint)
        (price uint)
    )
    ;; Price from registry is 8 decimals (compatible with sBTC 8 decimals)
    ;; Value should be 6 decimals (STBL)
    ;; (Collateral * Price) / 10^8
    (/ (* collateral-amount price) u100000000)
)

(define-public (calculate-current-ratio
        (user principal)
        (token principal)
        (oracle <oracle-trait>)
    )
    (let (
            (vault (get-vault user token))
            (type-info (unwrap! (get-collateral-type token) err-collateral-not-supported))
            (price (unwrap! (get-price-from-oracle token oracle) err-oracle-error))
            (collateral-val (calculate-collateral-value (get collateral vault) price))
            (pending-interest (calculate-accrued-interest (get debt vault)
                (get last-accrued-block vault) (get interest-rate type-info)
            ))
            (total-debt (+ (get debt vault) pending-interest))
        )
        (if (is-eq total-debt u0)
            (ok u99999999)
            (ok (/ (* collateral-val u100) total-debt))
        )
    )
)

;; Admin Functions

(define-public (set-oracle-registry (registry principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set oracle-registry-contract registry))
    )
)

(define-public (add-collateral-type
        (token principal)
        (name (string-ascii 32))
        (liquidation-ratio uint)
        (debt-ceiling uint)
        (interest-rate uint)
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set supported-collateral token {
            name: name,
            liquidation-ratio: liquidation-ratio,
            debt-ceiling: debt-ceiling,
            interest-rate: interest-rate,
            total-debt: u0,
            active: true,
        }))
    )
)

;; Delegation Management

(define-public (add-delegate (delegate principal))
    (ok (map-set vault-delegates {
        owner: tx-sender,
        delegate: delegate,
    }
        true
    ))
)

(define-public (remove-delegate (delegate principal))
    (ok (map-delete vault-delegates {
        owner: tx-sender,
        delegate: delegate,
    }))
)

;; User Functions

(define-public (deposit-collateral
        (amount uint)
        (collateral-token <sip010-token>)
        (vault-owner principal)
    )
    (let (
            (token-principal (contract-of collateral-token))
            (type-info (unwrap! (map-get? supported-collateral token-principal)
                err-collateral-not-supported
            ))
            (vault (get-vault vault-owner token-principal))
            (current-collateral (get collateral vault))
            (new-collateral (+ current-collateral amount))
            (pending-interest (calculate-accrued-interest (get debt vault)
                (get last-accrued-block vault) (get interest-rate type-info)
            ))
            (new-debt (+ (get debt vault) pending-interest))
        )
        (asserts! (is-authorized vault-owner tx-sender) err-unauthorized)
        (asserts! (get active type-info) err-collateral-not-supported)

        ;; Transfer from caller (delegate or owner) to contract
        (try! (contract-call? collateral-token transfer amount tx-sender
            (as-contract tx-sender) none
        ))

        (map-set vaults {
            owner: vault-owner,
            collateral-token: token-principal,
        } {
            collateral: new-collateral,
            debt: new-debt,
            last-accrued-block: burn-block-height,
        })
        (ok new-collateral)
    )
)

(define-public (borrow
        (amount uint)
        (collateral-token <sip010-token>)
        (oracle <oracle-trait>)
        (vault-owner principal)
    )
    (let (
            (token-principal (contract-of collateral-token))
            (type-info (unwrap! (map-get? supported-collateral token-principal)
                err-collateral-not-supported
            ))
            (vault (get-vault vault-owner token-principal))
            (price (unwrap! (get-price-from-oracle token-principal oracle)
                err-oracle-error
            ))
            (pending-interest (calculate-accrued-interest (get debt vault)
                (get last-accrued-block vault) (get interest-rate type-info)
            ))
            (current-debt (+ (get debt vault) pending-interest))
            (new-debt (+ current-debt amount))
            (collateral-val (calculate-collateral-value (get collateral vault) price))
        )
        (asserts! (is-authorized vault-owner tx-sender) err-unauthorized)
        (asserts! (get active type-info) err-collateral-not-supported)
        (asserts!
            (>= (/ (* collateral-val u100) new-debt)
                (get liquidation-ratio type-info)
            )
            err-under-collateralized
        )
        (asserts!
            (<= (+ (get total-debt type-info) amount)
                (get debt-ceiling type-info)
            )
            err-debt-ceiling-reached
        )

        ;; Mint stablecoin to VAULT OWNER, never delegate
        (try! (contract-call? .stable-token mint-for-vault amount vault-owner))

        (map-set vaults {
            owner: vault-owner,
            collateral-token: token-principal,
        } {
            collateral: (get collateral vault),
            debt: new-debt,
            last-accrued-block: burn-block-height,
        })

        (map-set supported-collateral token-principal
            (merge type-info { total-debt: (+ (get total-debt type-info) amount) })
        )

        (ok new-debt)
    )
)

(define-public (repay
        (amount uint)
        (collateral-token <sip010-token>)
        (vault-owner principal)
    )
    (let (
            (token-principal (contract-of collateral-token))
            (type-info (unwrap! (map-get? supported-collateral token-principal)
                err-collateral-not-supported
            ))
            (vault (get-vault vault-owner token-principal))
            (pending-interest (calculate-accrued-interest (get debt vault)
                (get last-accrued-block vault) (get interest-rate type-info)
            ))
            (current-debt (+ (get debt vault) pending-interest))
        )
        ;; Anyone can repay? Or only authorized? Sticking to authorized for now as per plan logic
        (asserts! (is-authorized vault-owner tx-sender) err-unauthorized)
        (asserts! (<= amount current-debt) err-repayment-too-high)

        ;; Burn stablecoin from caller (delegate or owner)
        (try! (contract-call? .stable-token burn-for-vault amount tx-sender))

        (map-set vaults {
            owner: vault-owner,
            collateral-token: token-principal,
        } {
            collateral: (get collateral vault),
            debt: (- current-debt amount),
            last-accrued-block: burn-block-height,
        })

        (map-set supported-collateral token-principal
            (merge type-info { total-debt: (- (get total-debt type-info) amount) })
        )

        (ok (- current-debt amount))
    )
)

(define-public (withdraw-collateral
        (amount uint)
        (collateral-token <sip010-token>)
        (oracle <oracle-trait>)
        (vault-owner principal)
    )
    (let (
            (token-principal (contract-of collateral-token))
            (type-info (unwrap! (map-get? supported-collateral token-principal)
                err-collateral-not-supported
            ))
            (vault (get-vault vault-owner token-principal))
            (price (unwrap! (get-price-from-oracle token-principal oracle)
                err-oracle-error
            ))
            (current-collateral (get collateral vault))
            (new-collateral (- current-collateral amount))
            (pending-interest (calculate-accrued-interest (get debt vault)
                (get last-accrued-block vault) (get interest-rate type-info)
            ))
            (current-debt (+ (get debt vault) pending-interest))
            (collateral-val (calculate-collateral-value new-collateral price))
        )
        (asserts! (is-authorized vault-owner tx-sender) err-unauthorized)
        (asserts! (>= current-collateral amount) err-insufficient-collateral)
        (asserts!
            (or (is-eq current-debt u0) (>= (/ (* collateral-val u100) current-debt)
                (get liquidation-ratio type-info)
            ))
            err-under-collateralized
        )

        ;; Transfer collateral to VAULT OWNER, never delegate
        (try! (as-contract (contract-call? collateral-token transfer amount tx-sender vault-owner
            none
        )))

        (map-set vaults {
            owner: vault-owner,
            collateral-token: token-principal,
        } {
            collateral: new-collateral,
            debt: current-debt,
            last-accrued-block: burn-block-height,
        })
        (ok new-collateral)
    )
)

(define-private (liquidate-full
        (target principal)
        (collateral-token <sip010-token>)
        (oracle <oracle-trait>)
    )
    (let (
            (liquidator tx-sender)
            (token-principal (contract-of collateral-token))
            (type-info (unwrap! (map-get? supported-collateral token-principal)
                err-collateral-not-supported
            ))
            (vault (get-vault target token-principal))
            (price (unwrap! (get-price-from-oracle token-principal oracle)
                err-oracle-error
            ))
            (collateral (get collateral vault))
            (pending-interest (calculate-accrued-interest (get debt vault)
                (get last-accrued-block vault) (get interest-rate type-info)
            ))
            (debt (+ (get debt vault) pending-interest))
            (collateral-val (calculate-collateral-value collateral price))
            (ratio (if (is-eq debt u0)
                u99999999
                (/ (* collateral-val u100) debt)
            ))
        )
        (asserts! (< ratio (get liquidation-ratio type-info))
            err-liquidation-not-allowed
        )

        (let (
                (debt-value-in-collateral (/ (* debt u100000000) price))
                (reward-collateral (/ (* debt-value-in-collateral (+ u100 liquidation-penalty)) u100))
                (actual-reward (if (> reward-collateral collateral)
                    collateral
                    reward-collateral
                ))
            )
            (try! (contract-call? .stable-token burn-for-vault debt tx-sender))

            (try! (as-contract (contract-call? collateral-token transfer actual-reward tx-sender
                liquidator none
            )))

            (map-delete vaults {
                owner: target,
                collateral-token: token-principal,
            })

            (map-set supported-collateral token-principal
                (merge type-info { total-debt: (- (get total-debt type-info) debt) })
            )

            (ok actual-reward)
        )
    )
)

(define-public (partial-liquidate
        (target principal)
        (collateral-token <sip010-token>)
        (oracle <oracle-trait>)
    )
    (let (
            (token-principal (contract-of collateral-token))
            (type-info (unwrap! (map-get? supported-collateral token-principal)
                err-collateral-not-supported
            ))
            (vault (get-vault target token-principal))
            (price (unwrap! (get-price-from-oracle token-principal oracle)
                err-oracle-error
            ))
            (collateral (get collateral vault))
            (pending-interest (calculate-accrued-interest (get debt vault)
                (get last-accrued-block vault) (get interest-rate type-info)
            ))
            (current-debt (+ (get debt vault) pending-interest))
            (collateral-val (calculate-collateral-value collateral price))
            (current-ratio (if (is-eq current-debt u0)
                u99999999
                (/ (* collateral-val u100) current-debt)
            ))
        )
        (asserts! (< current-ratio (get liquidation-ratio type-info))
            err-liquidation-not-allowed
        )

        (if (< current-ratio critical-ratio)
            (liquidate-full target collateral-token oracle)
            (let (
                    (target-debt (/ (* collateral-val u100) target-ratio-after-liquidation))
                    (debt-to-repay (- current-debt target-debt))
                    (debt-value-in-collateral (/ (* debt-to-repay u100000000) price))
                    (reward-collateral (/ (* debt-value-in-collateral (+ u100 liquidation-penalty))
                        u100
                    ))
                    (new-debt target-debt)
                    (new-collateral (- collateral reward-collateral))
                )
                (try! (contract-call? .stable-token burn-for-vault debt-to-repay
                    tx-sender
                ))

                (try! (as-contract (contract-call? collateral-token transfer reward-collateral
                    tx-sender tx-sender none
                )))

                (map-set vaults {
                    owner: target,
                    collateral-token: token-principal,
                } {
                    collateral: new-collateral,
                    debt: new-debt,
                    last-accrued-block: burn-block-height,
                })

                (map-set supported-collateral token-principal
                    (merge type-info { total-debt: (- (get total-debt type-info) debt-to-repay) })
                )

                (ok reward-collateral)
            )
        )
    )
)

(define-public (liquidate
        (target principal)
        (collateral-token <sip010-token>)
        (oracle <oracle-trait>)
    )
    (liquidate-full target collateral-token oracle)
)

(define-public (flash-mint
        (amount uint)
        (flash-loan-contract <flash-loan-trait>)
    )
    (let (
            (fee (/ (* amount flash-mint-fee) u10000))
            (total-repay (+ amount fee))
        )
        (try! (contract-call? .stable-token mint-for-vault amount tx-sender))
        (try! (contract-call? flash-loan-contract execute amount))
        (try! (contract-call? .stable-token burn-for-vault total-repay tx-sender))
        (ok total-repay)
    )
)
