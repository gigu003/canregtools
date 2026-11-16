# as_fbswicd requires only fbs among statistic columns

    Code
      create_quality(result, year, sex, cancer)
    Condition
      Error in `create_quality.fbswicd()`:
      ! `create_quality()` requires count column(s): fbs, sws, mv, dco, ub, sub, mvs. Missing: sws, mv, dco, ub, sub, mvs.

# as_fbswicd reports malformed components and grouping

    Code
      as_fbswicd(x$fbswicd, x$pop)
    Condition
      Error:
      ! `regicode` must be one non-missing registry identifier.

---

    Code
      as_fbswicd(x$fbswicd[, -1], x$pop, regicode = "A")
    Condition
      Error:
      ! Missing required variables in `fbsw`: year

---

    Code
      as_fbswicd(bad, x$pop, regicode = "A")
    Condition
      Error:
      ! Invalid `fbsw$fbs` value(s) at row(s) 1: -1.

---

    Code
      as_fbswicd(x$fbswicd, x$pop, bad_site, regicode = "A")
    Condition
      Error:
      ! `sitemorp$icd10` must be a list-column of named, finite, non-negative numeric vectors.

---

    Code
      as_fbswicd(x$fbswicd, x$pop, regicode_var = "registry_id")
    Condition
      Error:
      ! All data frames must contain the same set of `registry_id` values.

---

    Code
      as_fbswicd(x$fbswicd, x$pop, regicode = "A", regicode_var = "registry_id")
    Condition
      Error:
      ! `regicode` must be NULL when `regicode_var` is provided.

---

    Code
      as_fbswicd(x$fbswicd, x$pop, sitemorp = unknown, regicode_var = "registry_id")
    Condition
      Error:
      ! `sitemorp` contains registry code(s) not found in `fbsw`: B.

# as_fbswicd rejects non-finite optional counts

    Code
      as_fbswicd(x$fbswicd, bad, regicode = "A")
    Condition
      Error:
      ! Invalid `pop$death` value(s) at row(s) 1: NaN.

# as_fbswicd still requires fbs and matching merge schemas

    Code
      as_fbswicd(x$fbswicd[setdiff(names(x$fbswicd), "fbs")], x$pop, regicode = "A")
    Condition
      Error:
      ! Missing required variables in `fbsw`: fbs

---

    Code
      cr_merge(mixed)
    Condition
      Error in `cr_merge.fbswicds()`:
      ! All `fbswicd` objects must contain the same statistic columns.

