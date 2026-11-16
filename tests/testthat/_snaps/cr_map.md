# cr_map rejects invalid and derived attributes without writing

    Code
      cr_map(data.frame(regicode = "410103"), attr = "custom")
    Condition
      Error:
      ! `x` must contain columns `regicode` and `custom`.

---

    Code
      cr_map(c(`410103` = "A", `410103` = "B"), attr = "custom")
    Condition
      Error:
      ! Registry identifiers must be non-missing, non-empty, and unique.

---

    Code
      cr_map(c(`410103` = "A"), attr = "province")
    Condition
      Error:
      ! `attr` must be 'registry', 'area_type', or a custom grouping name. Derived attributes cannot be configured; use `write_regicode()` for labels.

