# add_labels falls back to default names when custom names mismatch

    Code
      res <- add_labels(data, vars = c("sex", "cancer"), names = "bad_name")
    Condition
      Warning in `add_labels()`:
      Length of `names` does not match `vars`; using default names.

