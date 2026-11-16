# compare_mi validates its inputs

    Code
      compare_mi(data.frame())
    Condition
      Error:
      ! `x` must inherit from class 'quality' or 'qualities'.

---

    Code
      compare_mi(quality, alpha = 1)
    Condition
      Error:
      ! `alpha` must be a single number strictly between 0 and 1.

---

    Code
      compare_mi(quality[0, ])
    Condition
      Error:
      ! `x` must contain at least one row.

---

    Code
      compare_mi(structure(quality[, -1], class = class(quality)))
    Condition
      Error:
      ! `x` is missing required columns: regicode.

---

    Code
      compare_mi(missing_count)
    Condition
      Error:
      ! `fbs` must contain finite, non-negative values without missing values.

---

    Code
      compare_mi(fractional_count)
    Condition
      Error:
      ! `sws` must contain whole-number counts.

---

    Code
      compare_mi(duplicate)
    Condition
      Error:
      ! Each `regicode` must occur only once within each detected stratum.

# compare_mi rejects negative counts

    Code
      compare_mi(negative_count)
    Condition
      Error:
      ! `fbs` must contain finite, non-negative values without missing values.

