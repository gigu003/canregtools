# explicit missing counts and absent population ages are not zero-filled

    Code
      create_quality(missing_count, year, sex, cancer)
    Condition
      Error:
      ! Summary column 'fbs' must contain finite non-negative counts. Missing rows can be completed as zero; supplied NA values cannot.

---

    Code
      create_asr(missing_count, year, sex, cancer)
    Condition
      Error:
      ! Summary column 'fbs' must contain finite non-negative counts. Missing rows can be completed as zero; supplied NA values cannot.

---

    Code
      create_asr(missing_population, year, sex, cancer)
    Condition
      Error in `create_asr()`:
      ! Registry TEST does not cover every declared age group; missing population age-group rows cannot be treated as zero.

