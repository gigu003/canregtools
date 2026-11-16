# detect_anomaly rejects period labels

    Code
      detect_anomaly(data, metrics = "cr")
    Condition
      Error:
      ! `detect_anomaly()` requires single four-digit calendar-year labels; period labels such as `2003-2007` are not supported.

# detect_anomaly validates duplicate years

    Code
      detect_anomaly(data, metrics = "cr")
    Condition
      Error:
      ! Each time series must contain at most one row per year.

