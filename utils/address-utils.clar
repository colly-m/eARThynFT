(define-read-only (is-contract? (who principal))
  (ok (is-eq (contract-principal? who) true)))

(define-read-only (is-same-address? (a principal) (b principal))
  (ok (is-eq a b)))
