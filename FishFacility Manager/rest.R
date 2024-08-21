zebra<- read.csv(file = "Johnson_room.csv", header = T, fill = T,encoding="UTF-8")
date<-zebra$Date.of.Birth
date1<- as.Date(date, format = "%d-%b-%y")
current_date <- Sys.Date()
x <- ifelse(!is.na(date1), as.numeric(difftime(current_date, date1, units = "days")), NA)


days_to_ymd <- function(DOB) {
  date1<- as.Date(DOB, format = "%d-%b-%y")
  current_date <- Sys.Date()
  days <- ifelse(!is.na(date1), as.numeric(difftime(current_date, date1, units = "days")), NA)
  years <- floor(days / 365.25)
  remaining_days <- days %% 365.25
  months <- floor(remaining_days / 30.4375)
  days <- floor(remaining_days %% 30.4375)
  return(paste(years, "years", months, "months", days, "days"))
}

cal_weeks<- function(DOB){
  date1<- as.Date(DOB, format = "%d-%b-%y")
  current_date <- Sys.Date()
  nweeks <- ifelse(!is.na(date1), as.numeric(difftime(current_date, date1, units = "weeks")), NA)
  return(nweeks)
}

ages_formatted <- sapply(x, days_to_ymd)
