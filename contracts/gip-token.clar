(impl-trait 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip010-trait)

;; ========================
;; CONFIGURATION & CONSTANTS
;; ========================
(define-constant TOKEN_NAME "Green Impact Points")
(define-constant TOKEN_SYMBOL "GIP")
(define-constant TOKEN_DECIMALS u6)
(define-constant MAX_SUPPLY u1000000000000000) ;; 1 billion tokens (6 decimals)

;; Error codes
(define-constant ERR_NOT_MINTER u100)
(define-constant ERR_NOT_OWNER u101)
(define-constant ERR_MINT_LIMIT u102)
(define-constant ERR_BURN_LIMIT u103)
(define-constant ERR_TRANSFER_FAIL u104)
(define-constant ERR_CONTRACT_DISABLED u105)

;; ========================
;; DATA STORAGE
;; ========================
(define-fungible-token gip-token)
(define-data-var total-supply uint u0)
(define-data-var contract-owner principal tx-sender)
(define-data-var contract-enabled bool true)
(define-map minters principal bool)
(define-map burners principal bool)

;; Token locking
(define-map locked-tokens principal (tuple (amount uint) (release-height uint)))

;; ========================
;; EVENTS
;; ========================
(define-event Minted (minter principal) (recipient principal) (amount uint))
(define-event Burned (burner principal) (amount uint))
(define-event Transfer (amount uint) (sender principal) (recipient principal))
(define-event TransferMemo (amount uint) (sender principal) (recipient principal) (memo (buff 34)))
(define-event Distributed (minter principal) (recipient principal) (amount uint))
(define-event Locked (user principal) (amount uint) (release-height uint))
(define-event Unlocked (user principal) (amount uint))

;; ========================
;; SIP-010 METADATA
;; ========================
(define-read-only (get-name) (ok TOKEN_NAME))
(define-read-only (get-symbol) (ok TOKEN_SYMBOL))
(define-read-only (get-decimals) (ok TOKEN_DECIMALS))
(define-read-only (get-balance (owner principal)) (ok (ft-get-balance gip-token owner)))
(define-read-only (get-total-supply) (ok (var-get total-supply)))

;; ========================
;; ADMIN FUNCTIONS
;; ========================
(define-public (set-minter (minter principal) (enabled bool))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR_NOT_OWNER))
    (map-set minters minter enabled)
    (ok true)
  )
)

(define-public (set-burner (burner principal) (enabled bool))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR_NOT_OWNER))
    (map-set burners burner enabled)
    (ok true)
  )
)

(define-public (toggle-contract (enabled bool))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR_NOT_OWNER))
    (var-set contract-enabled enabled)
    (ok true)
  )
)

;; ========================
;; CORE TOKEN OPERATIONS
;; ========================
(define-public (mint (recipient principal) (amount uint))
  (begin
    (asserts! (var-get contract-enabled) (err ERR_CONTRACT_DISABLED))
    (asserts! (is-eq (unwrap-panic (map-get? minters tx-sender)) true) (err ERR_NOT_MINTER))
    (asserts! (> amount u0) (err ERR_MINT_LIMIT))
    (let ((new-supply (unwrap-panic (add (var-get total-supply) amount))))
      (asserts! (<= new-supply MAX_SUPPLY) (err ERR_MINT_LIMIT))
      (ft-mint? gip-token amount recipient)
      (var-set total-supply new-supply)
      (emit-event Minted tx-sender recipient amount)
      (ok amount)
    )
  )
)

(define-public (burn (amount uint))
  (begin
    (asserts! (var-get contract-enabled) (err ERR_CONTRACT_DISABLED))
    (asserts! (is-eq (unwrap-panic (map-get? burners tx-sender)) true) (err ERR_NOT_MINTER))
    (asserts! (> amount u0) (err ERR_BURN_LIMIT))
    (let ((balance (ft-get-balance gip-token tx-sender)))
      (asserts! (>= balance amount) (err ERR_BURN_LIMIT))
      (ft-burn? gip-token amount tx-sender)
      (var-set total-supply (unwrap-panic (sub (var-get total-supply) amount)))
      (emit-event Burned tx-sender amount)
      (ok amount)
    )
  )
)

(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (var-get contract-enabled) (err ERR_CONTRACT_DISABLED))
    (asserts! (is-eq tx-sender sender) (err ERR_NOT_OWNER))
    (asserts! (> amount u0) (err ERR_TRANSFER_FAIL))
    (ft-transfer? gip-token amount sender recipient)
    (match memo
      some-memo (emit-event TransferMemo amount sender recipient some-memo)
      none (emit-event Transfer amount sender recipient)
    )
    (ok true)
  )
)

;; ========================
;; DISTRIBUTION
;; ========================
(define-public (distribute (recipients (list 100 principal)) (amounts (list 100 uint)))
  (begin
    (asserts! (is-eq (unwrap-panic (map-get? minters tx-sender)) true) (err ERR_NOT_MINTER))
    (asserts! (is-eq (len recipients) (len amounts)) (err u502))
    (let (
      (total (fold amounts u0 (lambda (acc amt) (unwrap-panic (add acc amt)))))
    )
      (asserts! (> total u0) (err u504))
      (map (lambda (recipient amount)
        (begin
          (ft-mint? gip-token amount recipient)
          (emit-event Distributed tx-sender recipient amount)
        )
      ) recipients amounts)
      (var-set total-supply (unwrap-panic (add (var-get total-supply) total)))
      (ok total)
    )
  )
)

;; ========================
;; TOKEN LOCKING (VESTING)
;; ========================
(define-public (lock-tokens (amount uint) (release-height uint))
  (begin
    (asserts! (>= (ft-get-balance gip-token tx-sender) amount) (err ERR_TRANSFER_FAIL))
    (ft-transfer? gip-token amount tx-sender (as-contract tx-sender))
    (map-set locked-tokens tx-sender {amount: amount, release-height: release-height})
    (emit-event Locked tx-sender amount release-height)
    (ok true)
  )
)

(define-public (unlock-tokens)
  (let (
    (lock-info (unwrap! (map-get? locked-tokens tx-sender) (err u510)))
    (amount (get amount lock-info))
    (release (get release-height lock-info))
  )
    (asserts! (>= block-height release) (err u511))
    (ft-transfer? gip-token amount (as-contract tx-sender) tx-sender)
    (map-delete locked-tokens tx-sender)
    (emit-event Unlocked tx-sender amount)
    (ok amount)
  )
)

;; ========================
;; VIEW FUNCTIONS
;; ========================
(define-read-only (get-minter-status (minter principal))
  (ok (map-get? minters minter))
)

(define-read-only (get-locked-tokens (owner principal))
  (ok (map-get? locked-tokens owner))
)

(define-read-only (get-contract-status)
  (ok {
    enabled: (var-get contract-enabled),
    owner: (var-get contract-owner),
    total-supply: (var-get total-supply),
    max-supply: MAX_SUPPLY
  })
)
