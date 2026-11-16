# Sprague rejects non-regular closed age groups

    Code
      expand_age_pop(c(100, 100, 100, 100), source_breaks = c(0, 4, 9, 14), method = "sprague")
    Condition
      Error in `normalize_sprague_input()`:
      ! `method = "sprague"` requires at least five closed, regular five-year groups and a final open group.

# age splitting rejects invalid boundary definitions

    Code
      expand_age_pop(c(1, 2), source_breaks = c(1, 5))
    Condition
      Error in `validate_age_breaks()`:
      ! `source_breaks` must be strictly increasing integer lower bounds that start at 0 and include a final open-group lower bound.

---

    Code
      expand_age_pop(c(1, 2), source_breaks = c(0, 5.5))
    Condition
      Error in `validate_age_breaks()`:
      ! `source_breaks` must be strictly increasing integer lower bounds that start at 0 and include a final open-group lower bound.

---

    Code
      expand_open_age_pop(1, lower = 85, method = "reference", max_age = 85)
    Condition
      Error in `expand_open_age_pop()`:
      ! `max_age` must be an integer greater than `lower` for reference splitting.

# PCLM is an optional dependency

    Code
      expand_age_pop(c(100, 200, 300), source_breaks = c(0, 5, 10), method = "pclm")
    Condition
      Error in `split_closed_age_pop_pclm()`:
      ! `method = "pclm"` requires the optional package `ungroup`. Install it with install.packages("ungroup").

