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


#######################
# load calibration bounds for true cumulative incidence of infection
# these are by month!

calibration_bounds = read.csv("data/simplistic_simResults.csv")
calibration_days = as.numeric(difftime(calibration_bounds$date, zerodate, units="days"))
calibration_scale = 1.0
calibration_lo = pmax(0, calibration_bounds$Size - calibration_scale*(calibration_bounds$Size-calibration_bounds$lowerBound) )
calibration_hi = calibration_bounds$Size + calibration_scale*(calibration_bounds$upperBound - calibration_bounds$Size)





#####################
# put everything on the dayseq time scale

calibration_lo = approx(calibration_days, calibration_lo, xout=dayseq, method="constant")$y
calibration_hi = approx(calibration_days, calibration_hi, xout=dayseq, method="constant")$y

cumcases = approx(casedays, cumsum(casesbyweek$Cases), xout=dayseq, method="constant")$y


cumcases[1:(sim_start_idx-1)] = 0 # fill in zero cum cases before first case detected. 

Iudx_lo = pmax(0, calibration_lo - cumcases)
Iudx_hi =         calibration_hi - cumcases

dayseq_Iudx = dayseq[!is.na(Iudx_lo)]

dx = approx(dayseq, cumcases, xout=dayseq_Iudx, method="constant")$y

Iudx_lo = Iudx_lo[!is.na(Iudx_lo)]
Iudx_hi = Iudx_hi[!is.na(Iudx_hi)]

dayseq_calibration = dayseq[!is.na(calibration_lo)]
# cumcases = cumcases[!is.na(calibration_lo)]
calibration_lo = calibration_lo[!is.na(calibration_lo)]
calibration_hi = calibration_hi[!is.na(calibration_hi)]



