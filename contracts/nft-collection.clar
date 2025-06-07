(impl-trait 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip009-trait)

;; Constants
(define-constant MAX_SUPPLY u10000)
(define-constant ERR_NOT_OWNER u100)
(define-constant ERR_MAX_SUPPLY u101)
(define-constant ERR_NOT_FOUND u404)
(define-constant ERR_UNAUTHORIZED u401)

;; NFT definition
(define-non-fungible-token ART-TOKEN uint)

;; Data storage
(define-data-var total-supply uint u0)
(define-data-var contract-owner principal tx-sender)
(define-map token-owner {token-id: uint} principal)

;; Minting function
(define-public (mint (recipient principal))
    (let ((new-token-id (+ (var-get total-supply) u1)))
        (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR_NOT_OWNER))
        (asserts! (< (var-get total-supply) MAX_SUPPLY) (err ERR_MAX_SUPPLY))
        (nft-mint? ART-TOKEN new-token-id recipient)
        (map-set token-owner {token-id: new-token-id} recipient)
        (var-set total-supply new-token-id)
        (ok new-token-id)
    )
)

;; SIP-009 required functions
(define-public (transfer (token-id uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) (err ERR_UNAUTHORIZED))
        (asserts! (is-some (map-get? token-owner {token-id: token-id})) (err ERR_NOT_FOUND))
        (nft-transfer? ART-TOKEN token-id sender recipient)
        (map-set token-owner {token-id: token-id} recipient)
        (ok true)
    )
)

(define-read-only (get-owner (token-id uint))
    (match (map-get? token-owner {token-id: token-id})
        owner (some owner)
        none
    )
)

(define-read-only (get-token-uri (token-id uint))
    (ok (some "https://metadata.artforearth.io/{id}"))
)

(define-read-only (get-last-token-id)
    (ok (var-get total-supply))
)
