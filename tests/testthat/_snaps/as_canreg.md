# as_canreg validates regicode_var grouping inputs

    Code
      as_canreg(inci = src$FBcases, mort = src$SWcases, pop = src$POP, regicode_var = "registry_id")
    Condition
      Error:
      ! `inci` must contain the `registry_id` column.

---

    Code
      as_canreg(inci = add_registry_id(src$FBcases), mort = add_registry_id(src$
        SWcases), pop = add_registry_id(src$POP, "REG_A"), regicode_var = "registry_id")
    Condition
      Error:
      ! All data frames must contain the same set of `registry_id` values.

---

    Code
      as_canreg(inci = incidence, mort = add_registry_id(src$SWcases), pop = add_registry_id(
        src$POP), regicode_var = "registry_id")
    Condition
      Error:
      ! `registry_id` in `inci` must not contain missing or empty values.

---

    Code
      as_canreg(inci = incidence_empty, mort = add_registry_id(src$SWcases), pop = add_registry_id(
        src$POP), regicode_var = "registry_id")
    Condition
      Error:
      ! `registry_id` in `inci` must not contain missing or empty values.

---

    Code
      as_canreg(inci = add_registry_id(src$FBcases), mort = add_registry_id(src$
        SWcases), pop = add_registry_id(src$POP), regicode = "REG_A", regicode_var = "registry_id")
    Condition
      Error:
      ! `regicode` must be NULL when `regicode_var` is provided.

# as_canreg validates required variables early

    Code
      as_canreg(inci = bad_incidence, mort = src$SWcases, pop = src$POP, regicode = src$
        regicode)
    Condition
      Error:
      ! Missing required variables in `incidence`: icd10

---

    Code
      as_canreg(inci = src$FBcases, mort = src$SWcases, pop = bad_population,
      regicode = src$regicode)
    Condition
      Error:
      ! Missing required variables in `population`: rks

