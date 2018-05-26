library(lubridate)
library(BH)

##########################
# Actual intervention dates

first_dx_date            = mdy("11/18/2014")
investigation_begin_date = mdy("01/23/2015")
intvxdate_actual_date    = mdy("03/15/2015") # actual intervention date
emergency_declared_date  = mdy("03/26/2015")
clinic_opened_date       = mdy("03/31/2015") # do we use this? 
sep_started_date         = mdy("04/04/2015")

##########################
# setup and scenario dates

zero_date         = mdy("04/01/2011") # Beginning of HIV incidence calculations
intvx_actual_date = first_dx_date
intvx_mid_date    = mdy("01/01/2013")
intvx_early_date  = zero_date+2
end_date          = mdy("08/11/2015") # this is last date from incidence data

dateseq = seq(zero_date, end_date, by="day")

###########################
# translated days 

first_dx_day            = as.numeric(first_dx_date - zero_date)
investigation_begin_day = as.numeric(investigation_begin_date - zero_date)
emergency_declared_day  = as.numeric(emergency_declared_date - zero_date)
clinic_opened_day       = as.numeric(clinic_opened_date - zero_date)
sep_started_day         = as.numeric(sep_started_date - zero_date)

zero_day         = 0
intvx_actual_day = as.numeric(intvx_actual_date - zero_date)
intvx_mid_day    = as.numeric(intvx_mid_date - zero_date)
intvx_early_day  = as.numeric(intvx_early_date - zero_date)
end_day          = as.numeric(end_date - zero_date)


dayseq = zero_day:end_day

ndays = length(dayseq)


#######################
# load scott county diagnosis data:

casesbyweek = read.csv("data/scott_county_cases_by_week.csv",stringsAsFactors=FALSE) 
cumcases = cumsum(casesbyweek$Cases)
casesbyweek$Date = mdy(casesbyweek$Date)
casesbyweek$day = as.numeric(casesbyweek$Date - zero_date)

dx = approx(casesbyweek$day, cumcases, xout=dayseq, method="constant")$y
dx[is.na(dx)] = 0

########################################
# load estimated incidence 

incidence_extracted = read.csv("data/extracted_infection_curves.csv", header=FALSE)
names(incidence_extracted) = c("Date.Decimal", "Infections")
incidence_extracted$date = date_decimal(incidence_extracted$Date.Decimal)
incidence_extracted$day = difftime(incidence_extracted$date, zero_date, units="days")
incidence_extracted$Infections = pmax(incidence_extracted$Infections,0)
ord = order(incidence_extracted$day)
incidence_extracted = incidence_extracted[ord,]


n_time_points = dim(incidence_extracted)[1]

infections_lo_tmp = rep(0, n_time_points)
infections_hi_tmp = rep(NA, n_time_points)
infections_lo_tmp[1] = incidence_extracted$Infections[1]
infections_hi_tmp[1] = incidence_extracted$Infections[1]

for(i in 2:n_time_points) {
  infections_hi_tmp[i] = max(c(infections_hi_tmp[1:(i-1)], incidence_extracted$Infections[i]))
  infections_lo_tmp[i] = min(incidence_extracted$Infections[i:n_time_points])
}

for(i in 1:(n_time_points-1)) {
  infections_lo_tmp[i] = min(infections_lo_tmp[(i+1):n_time_points])
}

infections_lo = approx(incidence_extracted$day, infections_lo_tmp, xout=dayseq, method="constant")$y
infections_hi = approx(incidence_extracted$day, infections_hi_tmp, xout=dayseq, method="constant")$y

#########################################
# create month markers

monthseq = mdy(c("04/01/2011", "01/01/2012", "01/01/2013", "01/01/2014", "01/01/2015", "01/01/2016"))
monthdayseq = difftime(monthseq, zero_date, units="days")
monthlabseq = format(monthseq, "%B %Y")

detail_monthseq = seq(mdy("11/10/2014"), mdy("09/01/2015"), by="month")
detail_monthdayseq = difftime(detail_monthseq, zero_date, units="days")
detail_monthlabseq = format(detail_monthseq, "%b %Y")



#########################################
# create raw data frame

dat = data.frame(day=dayseq, date=dateseq, cases=dx, infections_lo=infections_lo, infections_hi=infections_hi)


