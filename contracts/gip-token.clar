;; ====================================================================================
(impl-trait 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip010-trait)
;; IMPORTANT: The SIP-010 trait is usually defined in a separate file (e.g., sip010-trait.clar)
;; and used via (use-trait .sip010-trait). When you directly (impl-trait 'contract-id.trait-name),
;; you are telling the Clarity checker that THIS contract implements that specific trait
;; deployed at that specific contract ID. This is the correct usage for implementing a SIP.
;; ====================================================================================

;; ========================
;; TOKEN METADATA
;; ========================
(define-constant TOKEN_NAME "Green Impact Points")
(define-constant TOKEN_SYMBOL "GIP")
(define-constant TOKEN_DECIMALS u6)
(define-constant MAX_SUPPLY u1000000000000000) ;; Represents 1 billion (10^9) tokens with 6 decimals

;; ========================
;; ERROR CODES
;; ========================
(define-constant ERR_NOT_MINTER u100)
(define-constant ERR_NOT_OWNER u101)
(define-constant ERR_MINT_LIMIT u102) ;; Still using for consistency, but could be more specific
(define-constant ERR_BURN_LIMIT u103) ;; Still using for consistency, but could be more specific
(define-constant ERR_TRANSFER_FAIL u104)
(define-constant ERR_CONTRACT_DISABLED u105)
(define-constant ERR_NOT_BURNER u106)
(define-constant ERR_ZERO_AMOUNT u107) ;; Added explicit error for amount > u0 checks
(define-constant ERR_ARITHMETIC_OVERFLOW u108) ;; More specific than ERR_MINT_LIMIT/BURN_LIMIT
(define-constant ERR_NO_LOCKED_TOKENS_FOUND u109) ;; For unlock-tokens (u510)
(define-constant ERR_LOCK_PERIOD_NOT_OVER u110) ;; For unlock-tokens (u511)

;; ========================
;; FT DEFINITION AND STATE
;; ========================
(define-fungible-token gip-token)
(define-data-var total-supply uint u0)
(define-data-var contract-owner principal tx-sender)
(define-data-var contract-enabled bool true)
(define-map minters principal bool)
(define-map burners principal bool)
(define-map locked-tokens principal (tuple (amount uint) (release-height uint))) ;; Moved to data definitions

;; ========================
;; IMPORTS (For math-utils)
;; ========================
;; Assuming .math-utils is another deployed contract that defines its own trait
;; You would use: (use-trait .math-utils 'SP...contract-id.math-utils-trait)
;; Or if it's a file in the same project: (use-trait .math-utils)
;; I'll assume it's a local import for now.
(use-trait .math-utils)


;; ========================
;; SIP-010 REQUIRED FUNCTIONS (read-only)
;; ========================

(define-read-only (get-name)
  (ok TOKEN_NAME)
)

(define-read-only (get-symbol)
  (ok TOKEN_SYMBOL)
)

(define-read-only (get-decimals)
  (ok TOKEN_DECIMALS)
)

(define-read-only (get-balance (owner principal))
  (ok (ft-get-balance gip-token owner))
)

(define-read-only (get-total-supply)
  (ok (var-get total-supply))
)

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

(define-public (set-contract-owner (new-owner principal))
  ;; Transfer ownership (e.g., to a DAO contract)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR_NOT_OWNER))
    (var-set contract-owner new-owner)
    (ok true)
  )
)

;; ========================
;; MINT/BURN FUNCTIONS
;; ========================

(define-public (mint (recipient principal) (amount uint))
  (begin
    (asserts! (var-get contract-enabled) (err ERR_CONTRACT_DISABLED))
    (asserts! (is-eq (default-to false (map-get? minters tx-sender)) true) (err ERR_NOT_MINTER)) ;; More explicit check
    (asserts! (> amount u0) (err ERR_ZERO_AMOUNT))

    (let (
      (current-supply (var-get total-supply))
      (new-supply-result (contract-call? .math-utils safe-add current-supply amount))
    )
      (asserts! (is-ok new-supply-result) (err ERR_ARITHMETIC_OVERFLOW)) ;; More specific error
      (let ((new-supply (unwrap! new-supply-result (err ERR_ARITHMETIC_OVERFLOW))))
        (asserts! (<= new-supply MAX_SUPPLY) (err ERR_MINT_LIMIT)) ;; Max supply check still uses ERR_MINT_LIMIT
        (try! (ft-mint? gip-token amount recipient)) ;; Use try! for ft-mint?
        (var-set total-supply new-supply)
        (ok amount)
      )
    )
  )
)

(define-public (burn (amount uint))
  (begin
    (asserts! (var-get contract-enabled) (err ERR_CONTRACT_DISABLED))
    (asserts! (is-eq (default-to false (map-get? burners tx-sender)) true) (err ERR_NOT_BURNER)) ;; More explicit check
    (asserts! (> amount u0) (err ERR_ZERO_AMOUNT))

    (let (
      (balance (ft-get-balance gip-token tx-sender))
      (current-supply (var-get total-supply))
    )
      (asserts! (>= balance amount) (err ERR_BURN_LIMIT))

      (let ((new-supply-result (contract-call? .math-utils safe-sub current-supply amount)))
        (asserts! (is-ok new-supply-result) (err ERR_ARITHMETIC_OVERFLOW)) ;; More specific error
        (let ((new-supply (unwrap! new-supply-result (err ERR_ARITHMETIC_OVERFLOW))))
          (try! (ft-burn? gip-token amount tx-sender)) ;; Use try! for ft-burn?
          (var-set total-supply new-supply)
          (ok amount)
        )
      )
    )
  )
)

;; ========================
;; SIP-010 REQUIRED FUNCTION (public)
;; ========================

(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (var-get contract-enabled) (err ERR_CONTRACT_DISABLED))
    (asserts! (is-eq tx-sender sender) (err ERR_NOT_OWNER)) ;; Ensures only sender can initiate their own transfer
    (asserts! (> amount u0) (err ERR_ZERO_AMOUNT))
    (try! (ft-transfer? gip-token amount sender recipient)) ;; Use try! for ft-transfer?
    (ok true)
  )
)

;; ========================
;; TOKEN LOCKING MECHANISM
;; ========================

(define-public (lock-tokens (amount uint) (release-height uint))
  (begin
    (asserts! (var-get contract-enabled) (err ERR_CONTRACT_DISABLED))
    (asserts! (> amount u0) (err ERR_ZERO_AMOUNT))
    (asserts! (>= (ft-get-balance gip-token tx-sender) amount) (err ERR_TRANSFER_FAIL))
    (asserts! (> release-height block-height) (err ERR_LOCK_PERIOD_NOT_OVER)) ;; Release height must be in future

    ;; Transfer tokens from tx-sender to this contract's balance
    ;; (as-contract tx-sender) effectively makes the call from the contract's context
    (try! (ft-transfer? gip-token amount tx-sender (as-contract tx-sender)))
    (map-set locked-tokens tx-sender {amount: amount, release-height: release-height})
    (ok true)
  )
)

(define-public (unlock-tokens)
  (let (
    (lock-info (unwrap! (map-get? locked-tokens tx-sender) (err ERR_NO_LOCKED_TOKENS_FOUND))) ;; Use constant
    (amount-to-unlock (get amount lock-info))
    (release-block (get release-height lock-info))
  )
    (begin
      (asserts! (var-get contract-enabled) (err ERR_CONTRACT_DISABLED))
      (asserts! (>= block-height release-block) (err ERR_LOCK_PERIOD_NOT_OVER)) ;; Use constant

      ;; Transfer tokens from this contract's balance back to tx-sender
      (try! (as-contract (ft-transfer? gip-token amount-to-unlock (as-contract tx-sender) tx-sender)))
      (map-delete locked-tokens tx-sender)
      (ok amount-to-unlock)
    )
  )
)

;; ========================
;; READ-ONLY FUNCTIONS (Additional)
;; ========================

(define-read-only (get-minter-status (minter principal))
  (ok (default-to false (map-get? minters minter))) ;; Explicitly return false if not found
)

(define-read-only (get-burner-status (burner principal))
  (ok (default-to false (map-get? burners burner))) ;; Added for completeness
)

(define-read-only (get-locked-tokens (owner principal))
  (ok (map-get? locked-tokens owner)) ;; Returns (optional (tuple ...))
)

(define-read-only (get-contract-status)
  (ok {
    enabled: (var-get contract-enabled),
    owner: (var-get contract-owner),
    total-supply: (var-get total-supply),
    max-supply: MAX_SUPPLY
  })
)
