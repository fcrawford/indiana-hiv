library(lubridate)

##########################
# important dates:

first_dx            = mdy("11/18/2014")
investigation_begin = mdy("01/23/2015")
emergency_declared  = mdy("03/26/2015")
clinic_opened       = mdy("03/31/2015")
sep_started         = mdy("04/04/2015")


#######################
# load scott county diagnosis data:

casesbyweek = read.csv("data/scott_county_cases_by_week.csv",stringsAsFactors=FALSE)[1:51,]
casedates = mdy(casesbyweek$Date)
begindate = casedates[1]
enddate = casedates[length(casedates)]

########################################

zerodate         = mdy("04/01/2011") # plot starts here 
sim_start_date   = begindate #mdy("06/01/2014") # start of simulations, should be before intvxdate
intvxdate_actual = mdy("03/15/2015") # actual intervention date
end_date         = mdy("10/01/2015") 
dateseq = seq(zerodate, end_date, by="day")

dayseq = as.numeric(difftime(dateseq, zerodate, units="days")) # measured in days since zerodate 

monthdateseq = seq(zerodate, end_date, by="month")
monthseq = as.numeric(difftime(monthdateseq, zerodate, units="days")) # for plotting

sim_start_day = as.numeric(difftime(sim_start_date, zerodate, units="days"))
sim_start_idx = which(dayseq==sim_start_day)

first_dx_day = as.numeric(difftime(first_dx, zerodate, units="days"))
first_dx_idx = which(dayseq==first_dx_day)

sep_started_day = as.numeric(difftime(sep_started, zerodate, units="days"))
sep_started_idx = which(dayseq==sep_started_day)

enddate          = casedates[length(casedates)]
casedays         = as.numeric(casedates - zerodate)


dat = read.csv("data/extracted_infection_curves.csv", header=FALSE)
names(dat) = c("date", "infections")

dat$days = difftime(mdy("01/01/2011") + (dat$date - 2011)*365, zerodate, units="days")

dat$infections = pmax(dat$infections,0)

ord = order(dat$date)
dat = dat[ord,]

n = dim(dat)[1]


infections_lo = rep(NA, n)
infections_hi = rep(NA, n)
infections_lo[1] = dat$infections[1]
infections_hi[1] = dat$infections[1]

for(i in 2:n) {
  infections_hi[i] = max(c(infections_hi[1:(i-1)], dat$infections[i]))
  infections_lo[i] = min(dat$infections[i:n])
}

#scalefac = 1.5
#infections_mean = (infections_hi+infections_lo)/2
#infections_lo2 = pmax(0,infections_mean - (infections_mean-infections_lo)*scalefac)
#infections_hi2 = pmin(max(dat$infections), infections_mean + (infections_hi-infections_mean)*scalefac)

#weekseq = seq(min(dayseq), max(dayseq), by=7)

calibration_lo = approx(dat$days, infections_lo, xout=dayseq, method="constant")$y
calibration_hi = approx(dat$days, infections_hi, xout=dayseq, method="constant")$y

#calibration_lo = approx(weekseq, calibration_lo_tmp, xout=dayseq, method="constant")$y
#calibration_hi = approx(weekseq, calibration_hi_tmp, xout=dayseq, method="constant")$y



cumcases = approx(casedays, cumsum(casesbyweek$Cases), xout=dayseq, method="constant")$y





cumcases[1:(sim_start_idx-1)] = 0 # fill in zero cum cases before first case detected. 

Iudx_lo = pmax(1, calibration_lo - cumcases)
Iudx_hi =         calibration_hi - cumcases

dayseq_Iudx = dayseq[!is.na(Iudx_lo)]

dx = approx(dayseq, cumcases, xout=dayseq_Iudx, method="constant")$y

Iudx_lo = Iudx_lo[!is.na(Iudx_lo)]
Iudx_hi = Iudx_hi[!is.na(Iudx_hi)]

dayseq_calibration = dayseq[!is.na(calibration_lo)]
# cumcases = cumcases[!is.na(calibration_lo)]
calibration_lo = calibration_lo[!is.na(calibration_lo)]
calibration_hi = calibration_hi[!is.na(calibration_hi)]



