(impl-trait 'traits/whitelist-interface.clar)

(define-public (test-whitelist)
  (let ((user tx-sender))
    (let ((add-result (contract-call? .whitelist add-to-whitelist user)))
      (asserts! (is-ok add-result) "Adding to whitelist should succeed")
      (let ((check-result (contract-call? .whitelist is-whitelisted user)))
        (asserts! (is-eq (unwrap! check-result) true) "User should be whitelisted")))))
