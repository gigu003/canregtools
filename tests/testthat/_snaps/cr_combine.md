# cr_combine validates its input

    Code
      cr_combine(list())
    Condition
      Error:
      ! `object` must be a non-empty list of canregs or fbswicds objects.

---

    Code
      cr_combine(list(canregs, unclass(canregs)))
    Condition
      Error:
      ! Every element of `object` must have the same collection class: all canregs or all fbswicds.

---

    Code
      cr_combine(list(canregs, fbsws))
    Condition
      Error:
      ! Every element of `object` must have the same collection class: all canregs or all fbswicds.

---

    Code
      cr_combine(list(empty))
    Condition
      Error:
      ! Each canregs object must contain at least one registry.

---

    Code
      cr_combine(list(unnamed))
    Condition
      Error:
      ! Every canregs object must have unique, non-missing registry names.

---

    Code
      cr_combine(list(mismatched))
    Condition
      Error:
      ! Registry names must match the `regicode` in each canreg object.

---

    Code
      cr_combine(list(invalid_sf))
    Condition
      Error:
      ! Component `SFcases` must contain data frames.

---

    Code
      cr_combine(list(mismatched_fbsw))
    Condition
      Error:
      ! Registry names must match the `regicode` in each fbswicd object.

---

    Code
      cr_combine(list(invalid_component))
    Condition
      Error:
      ! Component `pop` must be a data frame.

---

    Code
      cr_combine(list(base_fbsws, sparse))
    Condition
      Error:
      ! Component `fbswicd` must have the same columns for matching registries.

