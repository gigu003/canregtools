calc_age <- function(birth_date, onset_date) {
  birth_date <- as.Date(birth_date)
  onset_date <- as.Date(onset_date)
  birth_year <- as.numeric(format(birth_date, "%Y"))
  onset_year <- as.numeric(format(onset_date, "%Y"))
  age <- onset_year - birth_year
  birth_month <- as.numeric(format(birth_date, "%m"))
  onset_month <- as.numeric(format(onset_date, "%m"))
  birth_day <- as.numeric(format(birth_date, "%d"))
  onset_day <- as.numeric(format(onset_date, "%d"))
  age[onset_month < birth_month | 
        (onset_month == birth_month &
           onset_day < birth_day)] <- 
    age[onset_month < birth_month | 
          (onset_month == birth_month &
             onset_day < birth_day)] - 1
  return(age)
}
