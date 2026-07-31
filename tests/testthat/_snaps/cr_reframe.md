# cr_reframe propagates missing custom dictionary errors

    Code
      cr_reframe(canregs, strat = dict_name)
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `purrr::map()`:
      i In index: 1.
      Caused by error in `.f()`:
      ! Custom dictionary not found. Use `write_registry(..., dict = 'test_cr_reframe_missing_custom')` to create one.

