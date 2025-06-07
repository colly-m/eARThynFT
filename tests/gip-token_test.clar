(impl-trait 'traits/ft-interface.clar)

(define-public (test-gip-minting)
  (let ((recipient tx-sender))
    (let ((mint-result (contract-call? .gip-token mint-gip recipient u100)))
      (asserts! (is-ok mint-result) "Minting GIP should succeed")
      (let ((balance (contract-call? .gip-token get-balance recipient)))
        (asserts! (is-eq (unwrap! balance) u100) "Balance should be 100")))))
