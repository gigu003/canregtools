# inconsistent supplied totals fail before calculation

    Code
      create_asr(bad_counts, year, sex, cancer)
    Condition
      Error:
      ! Inconsistent sex = '0' totals in summary column 'fbs': supplied totals must equal male plus female values when both are present.

---

    Code
      create_quality(bad_pop, year, sex, cancer)
    Condition
      Error:
      ! Inconsistent sex = '0' totals in population column 'rks': supplied totals must equal male plus female values when both are present.

---

    Code
      create_age_rate(bad_counts, year, sex, cancer)
    Condition
      Error:
      ! Inconsistent sex = '0' totals in summary column 'fbs': supplied totals must equal male plus female values when both are present.

