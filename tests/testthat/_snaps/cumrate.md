# cumulative rate age intervals are validated

    Code
      cumrate(rate = numeric())
    Condition
      Error:
      ! `rate` must be a non-empty numeric vector.

---

    Code
      cumrate(rate = rep(0.001, 19), eage = 100)
    Condition
      Error:
      ! `eage` must be between 0 and the upper bound of the supplied age intervals.

---

    Code
      cumrate(rate = rep(0.001, 19), agewidth = 0)
    Condition
      Error:
      ! `agewidth` must be positive and greater than 1 when `sep_zero = TRUE`.

