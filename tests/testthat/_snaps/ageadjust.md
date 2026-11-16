# ageadjust rejects invalid inputs instead of recycling or returning NaN

    Code
      ageadjust(c(1, 2), 100)
    Condition
      Error in `ageadjust()`:
      ! `count` and `pop` must have equal lengths.

---

    Code
      ageadjust(1, 0)
    Condition
      Error in `check_vector()`:
      ! `pop` must be a non-empty finite numeric vector with positive values.

---

    Code
      ageadjust(0, 0)
    Condition
      Error in `check_vector()`:
      ! `pop` must be a non-empty finite numeric vector with positive values.

---

    Code
      ageadjust(1, NA_real_)
    Condition
      Error in `check_vector()`:
      ! `pop` must be a non-empty finite numeric vector with positive values.

---

    Code
      ageadjust(-1, 100)
    Condition
      Error in `check_vector()`:
      ! `count` must be a non-empty finite numeric vector with non-negative values.

---

    Code
      ageadjust(numeric(), numeric())
    Condition
      Error in `check_vector()`:
      ! `count` must be a non-empty finite numeric vector with non-negative values.

---

    Code
      ageadjust(c(1, 2), c(100, 100), stdpop = 1)
    Condition
      Error in `ageadjust()`:
      ! `stdpop` must match `count` in length and have a positive total.

---

    Code
      ageadjust(1, 100, stdpop = 0)
    Condition
      Error in `ageadjust()`:
      ! `stdpop` must match `count` in length and have a positive total.

---

    Code
      ageadjust(1, 100, conf_level = 1)
    Condition
      Error in `ageadjust()`:
      ! `conf_level` must be a finite number between 0 and 1.

---

    Code
      ageadjust(1, 100, rate = 2)
    Condition
      Error in `ageadjust()`:
      ! `rate` must equal `count / pop` for every age group.

# truncated rates reject unaligned boundaries and weights

    Code
      truncrate(rep(0, 18), rep(100, 18), trunc_age = c(36, 64), sep_zero = FALSE)
    Condition
      Error in `truncrate()`:
      ! `trunc_age` must align with complete age groups within the input.

---

    Code
      truncrate(rep(0, 18), rep(100, 18), trunc_age = c(35, 63), sep_zero = FALSE)
    Condition
      Error in `truncrate()`:
      ! `trunc_age` must align with complete age groups within the input.

---

    Code
      truncrate(rep(0, 18), rep(100, 18), stdpop = 1)
    Condition
      Error in `truncrate()`:
      ! Length of 'stdpop' must match 'cases' and 'pop'.

