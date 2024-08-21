library(dplyr)

# Assuming you have defined days_to_ymd and cal_weeks functions

# Sample data frame
zebra <- data.frame(
  Name = c("John", "Jane", "Michael", "Emily", "David"),
  Date.of.Birth = c("1995-01-15", "1988-07-22", "1993-03-10", "2000-11-05", "1979-09-12")
)

# Calculate age and week based on Date.of.Birth
age <- days_to_ymd(zebra$Date.of.Birth)
week <- cal_weeks(zebra$Date.of.Birth)

modified_data <- zebra %>%
  mutate(
    across(starts_with("Date.of.Birth"), list(Age = ~ days_to_ymd(.))),
    across(starts_with("Date.of.Birth"), list(Week = ~ cal_weeks(.)))  # Remove original Date.of.Birth columns
  )

print(modified_data)