# population errors identify the stratum, variable, age, and value

    Code
      cr_clean(p)
    Condition
      Error in `.f()`:
      ! Invalid `rks` population count(s) for year 2021, sex 1, agegrp 1: missing. `rks` must be complete; counts must be finite, non-negative numbers. Missing optional `death` values are allowed.

---

    Code
      cr_clean(p)
    Condition
      Error in `.f()`:
      ! Invalid `rks` population count(s) for year 2021, sex 1, agegrp 5: NA. `rks` must be complete; counts must be finite, non-negative numbers. Missing optional `death` values are allowed.

---

    Code
      cr_clean(p)
    Condition
      Error in `.f()`:
      ! Invalid `death` population count(s) for year 2021, sex 1, agegrp 10: -1. `rks` must be complete; counts must be finite, non-negative numbers. Missing optional `death` values are allowed.

# population cleaning rejects incomplete and duplicate schedules

    Code
      cr_clean(p[-1, ])
    Condition
      Error in `.f()`:
      ! Population agegrp must contain a complete, unique standard 18- or 19-group schedule (numeric lower bounds).

---

    Code
      cr_clean(rbind(p, p[1, ]))
    Condition
      Error in `.f()`:
      ! Population agegrp must contain a complete, unique standard 18- or 19-group schedule (numeric lower bounds).

---

    Code
      cr_clean(p)
    Condition
      Error in `.f()`:
      ! Population agegrp must contain a complete, unique standard 18- or 19-group schedule (numeric lower bounds).

---

    Code
      cr_clean(canregs[[1]]$POP, age_breaks = c(0, 90))
    Condition
      Error in `cr_clean.POP()`:
      ! Population target breaks must start at 0 and cannot split 85+.

