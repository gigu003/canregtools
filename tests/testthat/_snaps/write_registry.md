# write_registry reports invalid inputs clearly

    Code
      write_registry(data.frame(regicode = "REG_A"), dict = "registry")
    Condition
      Error:
      ! `x` must contain columns `regicode` and `registry`.

---

    Code
      write_registry(c(REG_A = "one", REG_A = "two"), dict = "test")
    Condition
      Error:
      ! Registry identifiers must be non-missing, non-empty, and unique.

---

    Code
      write_registry(1, dict = "test")
    Condition
      Error:
      ! `x` must be NULL, a named list, a named character vector, or a data frame.

