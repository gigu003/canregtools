# cr_reframe propagates missing custom dictionary errors

    Code
      cr_reframe(canregs, strat = dict_name)
    Condition
      Error in `purrr::map()`:
      i In index: 1.
      Caused by error in `.f()`:
      ! Custom dictionary not found. Use `cr_map(..., attr = 'test_cr_reframe_missing_custom')` to create one.

