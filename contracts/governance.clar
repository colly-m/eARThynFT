;; ========================
;; IMPORTS
;; ========================
(use .math-utils)
(use-trait .nft-token-trait 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip009-trait)
(use-trait .gip-token-trait 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip010-trait)

;; ========================
;; CONFIG VARIABLES (Mutable)
;; ========================
(define-data-var voting-period uint u4320)         ;; ~3 days
(define-data-var execution-delay uint u1440)       ;; ~1 day
(define-data-var quorum-percent uint u10)          ;; 10%
(define-data-var approval-threshold uint u60)      ;; 60%
(define-data-var min-description-length uint u20)
(define-data-var total-eligible-voters uint u0)
(define-data-var contract-owner principal tx-sender)

;; ========================
;; ERROR CODES
;; ========================
(define-constant ERR_PROPOSAL_NOT_FOUND u100)
(define-constant ERR_NOT_AUTHORIZED u101)
(define-constant ERR_VOTING_CLOSED u102)
(define-constant ERR_INVALID_STATE u103)
(define-constant ERR_QUORUM_NOT_MET u104)
(define-constant ERR_THRESHOLD_NOT_MET u105)
(define-constant ERR_EXECUTION_FAILED u106)
(define-constant ERR_INSUFFICIENT_WEIGHT u107)
(define-constant ERR_ALREADY_VOTED u108)
(define-constant ERR_INVALID_DESC u109)
(define-constant ERR_INVALID_SOURCE u110)

;; ========================
;; STATE
;; ========================
(define-data-var next-proposal-id uint u0)

;; Voting sources
(define-constant VOTE_SOURCE_NFT u0)
(define-constant VOTE_SOURCE_GIP u1)

;; ========================
;; DATA STRUCTURES
;; ========================
(define-map proposals uint
  {
    creator: principal,
    description: (string-utf8 1024),
    start-block: uint,
    end-block: uint,
    execution-block: uint,
    state: uint,
    for-votes: uint,
    against-votes: uint,
    abstain-votes: uint,
    execution-target: (optional principal),
    execution-call: (optional (buff 128))
  }
)

(define-map votes 
  { proposal-id: uint, voter: principal } 
  { weight: uint, vote-type: uint }
)

(define-map vote-weights 
  principal 
  { nft-weight: uint, gip-weight: uint }
)

(define-map snapshots 
  { proposal-id: uint, voter: principal }
  { nft-balance: uint, gip-balance: uint }
)

;; ========================
;; EVENTS
;; ========================
(define-event ProposalCreated (id uint) (creator principal))
(define-event VoteCast (proposal-id uint) (voter principal) (weight uint) (vote-type uint))
(define-event ProposalStateChanged (id uint) (new-state uint))
(define-event ProposalExecuted (id uint) (executor principal))

;; ========================
;; CORE FUNCTIONS
;; ========================

(define-public (create-proposal (desc (string-utf8 1024)) (target (optional principal)) (call (optional (buff 128))))
  (begin
    (asserts! (>= (len desc) (var-get min-description-length)) (err ERR_INVALID_DESC))
    (let (
      (id (var-get next-proposal-id))
      (start (+ block-height u10))
      (end (+ start (var-get voting-period)))
      (exec (+ end (var-get execution-delay)))
    )
      (map-set proposals id {
        creator: tx-sender,
        description: desc,
        start-block: start,
        end-block: end,
        execution-block: exec,
        state: u0,
        for-votes: u0,
        against-votes: u0,
        abstain-votes: u0,
        execution-target: target,
        execution-call: call
      })
      (var-set next-proposal-id (+ id u1))
      (emit-event ProposalCreated id tx-sender)
      (ok id)
    )
  )
)

(define-public (update-vote-weights (voter principal))
  (let (
    (nft (unwrap-panic (contract-call? .nft-token-trait balance-of voter)))
    (gip (unwrap-panic (contract-call? .gip-token-trait get-balance voter)))
    (gip-weight (unwrap-panic (contract-call? .math-utils safe-div gip u1000)))
  )
    (map-set vote-weights voter {nft-weight: nft, gip-weight: gip-weight})
    ;; Optional: automatically increment eligible voter count if first time
    ;; Could include eligibility tracking here
    (ok true)
  )
)

(define-private (get-vote-weights (voter principal))
  (default-to {nft-weight: u0, gip-weight: u0} (map-get? vote-weights voter))
)

(define-private (get-voting-power (voter principal) (source uint))
  (let ((weights (get-vote-weights voter)))
    (match source
      VOTE_SOURCE_NFT (ok (get nft-weight weights))
      VOTE_SOURCE_GIP (ok (get gip-weight weights))
      (err ERR_INVALID_SOURCE)
    )
  )
)

(define-public (vote (proposal-id uint) (vote-type uint) (vote-source uint))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) (err ERR_PROPOSAL_NOT_FOUND)))
    (state (get state proposal))
  )
    (begin
      (asserts! (and (>= block-height (get start-block proposal)) (<= block-height (get end-block proposal))) (err ERR_VOTING_CLOSED))
      (asserts! (is-eq state u1) (err ERR_INVALID_STATE))
      (asserts! (is-none (map-get? votes {proposal-id: proposal-id, voter: tx-sender})) (err ERR_ALREADY_VOTED))
      (let ((power (unwrap! (get-voting-power tx-sender vote-source) (err ERR_INSUFFICIENT_WEIGHT))))
        (asserts! (> power u0) (err ERR_INSUFFICIENT_WEIGHT))
        ;; Record vote
        (map-set votes {proposal-id: proposal-id, voter: tx-sender} {weight: power, vote-type: vote-type})
        ;; Update counts
        (map-set proposals proposal-id (merge proposal {
          for-votes: (if (is-eq vote-type u1) (+ (get for-votes proposal) power) (get for-votes proposal)),
          against-votes: (if (is-eq vote-type u0) (+ (get against-votes proposal) power) (get against-votes proposal)),
          abstain-votes: (if (is-eq vote-type u2) (+ (get abstain-votes proposal) power) (get abstain-votes proposal))
        }))
        (emit-event VoteCast proposal-id tx-sender power vote-type)
        (ok true)
      )
    )
  )
)

(define-public (activate-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals proposal-id) (err ERR_PROPOSAL_NOT_FOUND))))
    (begin
      (asserts! (is-eq (get state proposal) u0) (err ERR_INVALID_STATE))
      (asserts! (>= block-height (get start-block proposal)) (err ERR_VOTING_CLOSED))
      (map-set proposals proposal-id (merge proposal {state: u1}))
      (emit-event ProposalStateChanged proposal-id u1)
      (ok true)
    )
  )
)

(define-public (finalize-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) (err ERR_PROPOSAL_NOT_FOUND)))
    (total-votes (+ (get for-votes proposal) (get against-votes proposal) (get abstain-votes proposal)))
    (quorum (unwrap! (contract-call? .math-utils percentage-of (var-get total-eligible-voters) (var-get quorum-percent)) (err u501)))
    (approval (unwrap! (contract-call? .math-utils percentage (get for-votes proposal) (+ (get for-votes proposal) (get against-votes proposal))) (err u502)))
  )
    (begin
      (asserts! (and (> block-height (get end-block proposal)) (<= (get state proposal) u1)) (err ERR_INVALID_STATE))
      (asserts! (>= total-votes quorum) (err ERR_QUORUM_NOT_MET))
      (asserts! (>= approval (var-get approval-threshold)) (err ERR_THRESHOLD_NOT_MET))
      (map-set proposals proposal-id (merge proposal {state: u2}))
      (emit-event ProposalStateChanged proposal-id u2)
      (ok true)
    )
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals proposal-id) (err ERR_PROPOSAL_NOT_FOUND))))
    (begin
      (asserts! (is-eq (get state proposal) u2) (err ERR_INVALID_STATE))
      (asserts! (>= block-height (get execution-block proposal)) (err ERR_VOTING_CLOSED))
      (match (get execution-call proposal)
        some-call (try! (contract-call? (unwrap! (get execution-target proposal) (err ERR_EXECUTION_FAILED)) some-call))
        none (ok true)
      )
      (map-set proposals proposal-id (merge proposal {state: u3}))
      (emit-event ProposalExecuted proposal-id tx-sender)
      (emit-event ProposalStateChanged proposal-id u3)
      (ok true)
    )
  )
)

;; ========================
;; ADMIN FUNCTIONS
;; ========================
(define-public (set-quorum-percent (percent uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR_NOT_AUTHORIZED))
    (asserts! (and (>= percent u1) (<= percent u100)) (err u600))
    (var-set quorum-percent percent)
    (ok true)
  )
)

(define-public (set-voting-period (period uint))
  (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR_NOT_AUTHORIZED))
  (var-set voting-period period)
  (ok true)
)

(define-public (set-total-eligible-voters (total uint))
  (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR_NOT_AUTHORIZED))
  (var-set total-eligible-voters total)
  (ok true)
)

;; ========================
;; READ-ONLY FUNCTIONS
;; ========================
(define-read-only (get-proposal (proposal-id uint))
  (match (map-get? proposals proposal-id)
    prop (ok prop)
    (err ERR_PROPOSAL_NOT_FOUND))
)

(define-read-only (get-proposal-state (proposal-id uint))
  (match (map-get? proposals proposal-id)
    prop (ok (get state prop))
    (err ERR_PROPOSAL_NOT_FOUND))
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes {proposal-id: proposal-id, voter: voter})
)

(define-read-only (get-voting-power (voter principal) (source uint))
  (get-voting-power voter source)
)
