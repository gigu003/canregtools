# compare_mv rejects an incidence-only external standard

    Code
      compare_mv(make_mv_quality()[1, ], reference = reference)
    Condition
      Error:
      ! reference must contain either mv or mv_cases.

# compare_mv validates its inputs

    Code
      compare_mv(data.frame())
    Condition
      Error:
      ! `x` must inherit from class 'quality' or 'qualities'.

---

    Code
      compare_mv(quality, alpha = 0)
    Condition
      Error:
      ! `alpha` must be a single number strictly between 0 and 1.

---

    Code
      compare_mv(quality[0, ])
    Condition
      Error:
      ! `x` must contain at least one row.

---

    Code
      compare_mv(structure(quality[, -6], class = class(quality)))
    Condition
      Error:
      ! `x` is missing required columns: mv.

---

    Code
      compare_mv(invalid_percent)
    Condition
      Error:
      ! `mv` must be finite and between 0 and 100 when `fbs` is positive.

---

    Code
      compare_mv(invalid_count)
    Condition
      Error:
      ! `mv_cases` must not exceed `fbs`.

---

    Code
      compare_mv(duplicate)
    Condition
      Error:
      ! Each `regicode` must occur only once within each detected stratum.

