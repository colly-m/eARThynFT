(define-read-only (safe-add (a uint) (b uint))
  (ok (+ a b)))

(define-read-only (safe-sub (a uint) (b uint))
  (if (>= a b)
      (ok (- a b))
      (err u100))) ;; Custom error for underflow

(define-read-only (percentage-of (amount uint) (percent uint))
  (ok (/ (* amount percent) u100)))
