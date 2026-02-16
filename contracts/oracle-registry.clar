(impl-trait .oracle-trait.oracle-trait)
(use-trait oracle-trait .oracle-trait.oracle-trait)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-unknown-oracle (err u101))
(define-constant err-stale-price (err u102))
(define-constant err-not-enough-sources (err u103))

(define-constant stale-block-threshold u144) ;; ~24 hours

(define-map trusted-oracles
    { asset: principal, oracle: principal }
    bool
)

(define-map asset-prices
    principal
    {
        price: uint,
        last-updated: uint,
        decimals: uint
    }
)

(define-private (is-trusted (asset principal) (oracle principal))
    (default-to false (map-get? trusted-oracles { asset: asset, oracle: oracle }))
)

;; Admin: Add trusted oracle for asset
(define-public (add-oracle (asset principal) (oracle principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set trusted-oracles { asset: asset, oracle: oracle } true))
    )
)

;; Trusted Oracle: Update price
(define-public (update-price (asset principal) (price uint) (decimals uint))
    (begin
        (asserts! (is-trusted asset tx-sender) err-unknown-oracle)
        (ok (map-set asset-prices asset {
            price: price,
            last-updated: burn-block-height,
            decimals: decimals
        }))
    )
)

;; Read: Get trusted, non-stale price
(define-public (get-price (asset principal))
    (let (
            (price-info (unwrap! (map-get? asset-prices asset) err-unknown-oracle))
        )
        (asserts! (<= (- burn-block-height (get last-updated price-info)) stale-block-threshold) err-stale-price)
        (ok (get price price-info))
    )
)

(define-public (get-decimals)
    (ok u8)
)
