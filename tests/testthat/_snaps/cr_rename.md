# cr_rename validates mapping

    Code
      cr_rename(canregs, c("REG_A", "REG_B"))
    Condition
      Error:
      ! `mapping` must be a named character vector.

---

    Code
      cr_rename(canregs, c(UNKNOWN = "REG_A"))
    Condition
      Error:
      ! Unknown registry name(s) in `mapping`: UNKNOWN.

---

    Code
      cr_rename(canregs, c(`410103` = ""))
    Condition
      Error:
      ! Names and values in `mapping` must not be missing or empty.

---

    Code
      cr_rename(canregs, c(`410103` = "REG_A", `410103` = "REG_B"))
    Condition
      Error:
      ! Names in `mapping` must be unique.

---

    Code
      cr_rename(canregs, c(`410103` = "410307"))
    Condition
      Error:
      ! Renaming would create duplicate registry names.

