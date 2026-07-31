# classify_icd10 validates cancer_type

    Code
      classify_icd10("C15.2", cancer_type = "invalid")
    Condition
      Error in `classify_icd10()`:
      ! Invalid cancer_type. Valid options: small, big, system, gco

# classify_areacode validates attr

    Code
      classify_areacode("110000", attr = character())
    Condition
      Error in `classify_areacode()`:
      ! 'attr' must be a non-empty character vector without NA or empty values.

# classify_areacode errors for missing custom dictionaries

    Code
      classify_areacode("110000", attr = dict_name)
    Condition
      Error in `purrr::map()`:
      i In index: 1.
      Caused by error in `.f()`:
      ! Custom dictionary not found. Use `write_registry(..., dict = 'test_classify_missing_custom')` to create one.

