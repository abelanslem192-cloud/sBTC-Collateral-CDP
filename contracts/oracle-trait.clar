(define-trait oracle-trait
    (
        (get-price (principal) (response uint uint))
        (get-decimals () (response uint uint))
    )
)
