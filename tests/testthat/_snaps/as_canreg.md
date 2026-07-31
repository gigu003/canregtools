# as_canreg validates required variables early

    Code
      as_canreg(incidence = bad_incidence, mortality = src$SWcases, population = src$
        POP, areacode = src$areacode)
    Condition
      Error:
      ! Missing required variables in `incidence`: icd10

---

    Code
      as_canreg(incidence = src$FBcases, mortality = src$SWcases, population = bad_population,
      areacode = src$areacode)
    Condition
      Error:
      ! Missing required variables in `population`: rks

