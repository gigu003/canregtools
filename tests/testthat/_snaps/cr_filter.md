# cr_filter validates fbswicd part names

    Code
      cr_filter(fbsws[[1]], year == "2021", part = "FBcases")
    Condition
      Error:
      ! Unsupported part name(s): FBcases

# character and explicit year expressions keep their semantics

    Code
      mixed <- cr_filter(data, year > 2012 | year == "2012-2016")
    Condition
      Warning:
      There was 1 warning in `dplyr::filter()`.
      i In argument: `|...`.
      Caused by warning:
      ! Numeric year comparison: period or invalid year labels treated as NA: 2012-2016
    Code
      numeric <- cr_filter(data, year %in% 2012:2019)
    Condition
      Warning:
      There was 1 warning in `dplyr::filter()`.
      i In argument: `(function (left, right, op, year_left) ...`.
      Caused by warning:
      ! Numeric year comparison: period or invalid year labels treated as NA: 2012-2016

