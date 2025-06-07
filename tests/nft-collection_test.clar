(impl-trait 'traits/nft-interface.clar)

(define-public (test-mint-nft)
  (let ((recipient tx-sender))
    (let ((mint-result (contract-call? .nft-collection mint recipient)))
      (asserts! (is-ok mint-result) "Mint should succeed")
      (let ((token-id (unwrap! mint-result u0)))
        (let ((owner (contract-call? .nft-collection get-owner token-id)))
          (asserts! (is-eq (unwrap! owner) recipient) "Owner should be recipient"))))))
