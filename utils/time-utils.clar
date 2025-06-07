(define-reaf-only (get-block-height)
  block-height)

(define-read-only (has-time-elapsed? (start uint) (duration uint))
  (ok (>= block-height (+ start duration))))
