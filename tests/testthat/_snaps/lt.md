# lt validates inputs

    Code
      lt(mx = c(0.1, 0.2), sage = c(0))
    Condition
      Error in `lt()`:
      ! `sage` must have the same length as `mx`.

---

    Code
      lt(mx = c(0.1, 0.2), sage = c(1, 0))
    Condition
      Error in `lt()`:
      ! `sage` must be strictly increasing.

# expand_lx validates inputs and handles mx-like input

    Code
      expand_lx(c(1, 2), sage = c(0, 5))
    Condition
      Error in `expand_lx()`:
      ! sage must be of the form c(0, 1, seq(5, last, 5)) with 5-year intervals after age 1.

---

    Code
      result <- expand_lx(rep(0.01, 19))
    Condition
      Warning in `expand_lx()`:
      First value is small (< 0.2), assuming input is mx instead of lx.

