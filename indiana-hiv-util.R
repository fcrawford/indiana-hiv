library(splines)


#############


smoothers = list(list(name="spline",
                      f=function(x,y,v)smooth.spline(y ~ x, df=v)$y,
                      step=1,
                      dxrange=c(5,8,50),
                      Iudxrange=c(2,9,50),
                      Irange=c(2,9,50),
                      Srange=c(2,20,50)),
                 list(name="loess",
                      f=function(x,y,v)predict(loess(y ~ x,span=v)),
                      step=0.01,
                      dxrange=c(0,0.11,1),
                      Iudxrange=c(0,0.4,1),
                      Irange=c(0,0.46,1),
                      Srange=c(0,0.2,1)),
                 list(name="ksmooth",
                      f=function(x,y,v)ksmooth(x,y,kernel="normal",bandwidth=v)$y,
                      step=0.01,
                      dxrange=c(1,20,200),
                      Iudxrange=c(15,100,1000),
                      Irange=c(1,170,1000),
                      Srange=c(1,500,1000)))

smoothernames = unlist(lapply(smoothers,function(s)s$name))



#############



text_cex = 1.0

myred   = rgb(1,0,0,alpha=0.2)
mygreen = rgb(0,1,0,alpha=0.2)
myblue  = rgb(0,0,1,alpha=0.2)
mygray  = rgb(0,0,0,alpha=0.1)


mydarkred   = rgb(1,0,0,alpha=0.8)
mydarkgreen = rgb(0,1,0,alpha=0.8)
mydarkblue  = rgb(0,0,1,alpha=0.8)
mydarkgray  = rgb(0,0,0,alpha=0.3)



days_to_first_dx = as.numeric(first_dx - zerodate)
days_to_investigation_begin = as.numeric(investigation_begin - zerodate)
days_to_emergency_declared = as.numeric(emergency_declared - zerodate)
days_to_clinic_opened = as.numeric(clinic_opened - zerodate)
days_to_sep_started = as.numeric(sep_started - zerodate)

daily_timescale = dayseq_Iudx
ndays = length(daily_timescale)

indices = c(1, 10, 22, 34, 46, 55)
monthlabs = rep(NA, length(monthseq))
monthlabs[indices] = format(monthdateseq[indices], "%B %Y")


#############################################
#############################################
#############################################
#############################################

plot_dx_rate = function(intvxday, dday, smooth_dx, smooth_Iudx, smoother) {

  intvxday = as.numeric(intvxday - zerodate)
  dday = as.numeric(dday - zerodate)


  smooth_method = smoothers[[which(smoothernames==smoother)]]$f

  Iudx_lo_smooth = pmax(0.01,smooth_method(daily_timescale, Iudx_lo, v=smooth_Iudx))
  Iudx_hi_smooth = pmax(0.01,smooth_method(daily_timescale, Iudx_hi, v=smooth_Iudx))

  #dx_smooth      = pmax(0,smooth_method(daily_timescale, dx, v=smooth_dx))
  dx_smooth   =   dx
  dx_smooth[days_to_first_dx:ndays] = smooth_method(daily_timescale[days_to_first_dx:ndays], dx[days_to_first_dx:ndays], v=smooth_dx)
  dx_smooth[dx_smooth<0] = 0

  dxrate_hi_smooth = c(0,diff(dx_smooth))/Iudx_lo_smooth
  dxrate_lo_smooth = c(0,diff(dx_smooth))/Iudx_hi_smooth

  peak_dx_day = min(ndays,days_to_first_dx+dday-intvxday)

  dxrate_lo_smooth2 = rep(0,length(daily_timescale))
  dxrate_lo_smooth2[intvxday:(intvxday+peak_dx_day-days_to_first_dx)] = dxrate_lo_smooth[days_to_first_dx:peak_dx_day]
  dxrate_lo_smooth2[(intvxday+peak_dx_day-days_to_first_dx+1):ndays] = dxrate_lo_smooth[peak_dx_day]

  dxrate_hi_smooth2 = rep(0,length(daily_timescale))
  dxrate_hi_smooth2[intvxday:(intvxday+peak_dx_day-days_to_first_dx)] = dxrate_hi_smooth[days_to_first_dx:peak_dx_day]
  dxrate_hi_smooth2[(intvxday+peak_dx_day-days_to_first_dx+1):ndays] = dxrate_hi_smooth[peak_dx_day]

  dxrate_lo_smooth2 = pmax(dxrate_lo_smooth2, 0)
  dxrate_hi_smooth2 = pmax(dxrate_hi_smooth2, 0)

  par(mar=c(2.7,4.5,1,1.0), bty="n", cex.lab=1.2, cex.axis=1.2)
  text_cex = 1.0
  

  plot(daily_timescale, dxrate_hi_smooth2, 
       type="n", 
       xlim=c(0,max(casedays)), 
       #ylim=c(0,0.7), 
       axes=FALSE, ylab="Diagnosis rate")
  axis(1,at=monthseq[indices], lab=monthlabs[indices])
  axis(2)


  polygon(c(daily_timescale,rev(daily_timescale)), c(dxrate_lo_smooth2, rev(dxrate_hi_smooth2)), col=mygray)

}

############################################
############################################
############################################
############################################

plot_transmission_rate = function(N, smooth_dx, smooth_Iudx, smooth_I, smooth_S, constFOI, smoother){

  smooth_method = smoothers[[which(smoothernames==smoother)]]$f

  I_hi = calibration_hi
  I_lo = pmax(calibration_lo,dx)

  S_hi = pmax(0,N - I_lo)
  S_lo = pmin(N,N - I_hi)

  S_hi_smooth = smooth_method(daily_timescale, S_hi, v=smooth_S)
  S_lo_smooth = smooth_method(daily_timescale, S_lo, v=smooth_S)

  I_hi_smooth = smooth_method(daily_timescale, I_hi, v=smooth_I)
  I_lo_smooth = smooth_method(daily_timescale, I_lo, v=smooth_I)

  Iudx_lo_smooth = smooth_method(daily_timescale, Iudx_lo, v=smooth_Iudx)
  Iudx_hi_smooth = smooth_method(daily_timescale, Iudx_hi, v=smooth_Iudx)
  dx_smooth      = smooth_method(daily_timescale, dx, v=smooth_dx)


  dxrate_hi_smooth = c(0,diff(dx_smooth))/Iudx_lo_smooth
  dxrate_lo_smooth = c(0,diff(dx_smooth))/Iudx_hi_smooth

  dxrate_lo = mean(dxrate_lo_smooth[days_to_first_dx:ndays])
  dxrate_hi = mean(dxrate_hi_smooth[days_to_first_dx:ndays])
  dxrate_mid = mean(c(dxrate_lo, dxrate_hi))

  incidencerate_lo = sum(diff(dx))/(sum(Iudx_hi * S_hi))
  incidencerate_hi = sum(diff(dx))/(sum(Iudx_lo * S_lo))
  incidencerate_mid = mean(c(incidencerate_lo, incidencerate_hi))
  #cat("avg incidencerate = (", incidencerate_lo, ", ", incidencerate_hi, ")\n", sep="")
  #e1 = S_hi_smooth %*% Iudx_hi_smooth #I_hi_smooth 
  #e2 = S_lo_smooth %*% Iudx_hi_smooth #I_hi_smooth 
  #e3 = S_hi_smooth %*% Iudx_lo_smooth #I_lo_smooth 
  #e4 = S_lo_smooth %*% Iudx_lo_smooth #I_lo_smooth 

  #e_lo = pmin(e1, e2, e3, e4)
  #e_hi = pmax(e1, e2, e3, e4)

  e_lo = S_lo_smooth * I_lo_smooth
  e_hi = S_hi_smooth * I_hi_smooth

  if(constFOI) {
    beta_lo = rep(incidencerate_lo,ndays)
    beta_hi = rep(incidencerate_hi,ndays)
  } else {
    beta_lo = c(0,diff(I_hi_smooth))/e_hi
    beta_lo[beta_lo<=0] = 0
    beta_hi = c(0,diff(I_hi_smooth))/e_lo
    beta_hi[beta_hi<=0] = 0
  }


  par(mar=c(2.7,4.5,1,1.0), bty="n", cex.lab=1.2, cex.axis=1.2)

  plot(daily_timescale, beta_hi, type="n", ylim=c(0,1e-4), xlim=c(0,max(casedays)), axes=FALSE, ylab="Infection rate")
  polygon(c(daily_timescale,rev(daily_timescale)), c(beta_lo, rev(beta_hi)), col=mygray)
  axis(1,at=monthseq[indices], lab=monthlabs[indices])
  axis(2)

  #abline(h=c(incidencerate_lo, incidencerate_hi))


}



############################################
############################################
############################################
############################################

get_indiana_bounds = function(N, intvxday, dday, smooth_dx, smooth_Iudx, smooth_I, smooth_S, constFOI, smoother) {

  intvxday = as.numeric(intvxday - zerodate)
  dday = as.numeric(dday - zerodate)

  smooth_method = smoothers[[which(smoothernames==smoother)]]$f

  I_hi = calibration_hi
  I_lo = pmax(calibration_lo,dx)

  S_hi = pmax(0,N - I_lo)
  S_lo = pmin(N,N - I_hi)

  S_hi_smooth = smooth_method(daily_timescale, S_hi, v=smooth_S)
  S_lo_smooth = smooth_method(daily_timescale, S_lo, v=smooth_S)

  I_hi_smooth = smooth_method(daily_timescale, I_hi, v=smooth_I)
  I_lo_smooth = smooth_method(daily_timescale, I_lo, v=smooth_I)

  Iudx_lo_smooth = smooth_method(daily_timescale, Iudx_lo, v=smooth_Iudx)
  Iudx_hi_smooth = smooth_method(daily_timescale, Iudx_hi, v=smooth_Iudx)

  dx_smooth   =   dx
  dx_smooth[days_to_first_dx:ndays] = smooth_method(daily_timescale[days_to_first_dx:ndays], dx[days_to_first_dx:ndays], v=smooth_dx)
  dx_smooth[dx_smooth<0] = 0



  dxrate_hi_smooth = c(0,diff(dx_smooth))/Iudx_lo_smooth
  dxrate_lo_smooth = c(0,diff(dx_smooth))/Iudx_hi_smooth

  dxrate_lo = mean(dxrate_lo_smooth[days_to_first_dx:ndays])
  dxrate_hi = mean(dxrate_hi_smooth[days_to_first_dx:ndays])
  dxrate_mid = mean(c(dxrate_lo, dxrate_hi))

  incidencerate_lo = sum(diff(dx))/(sum(Iudx_hi * S_hi))
  incidencerate_hi = sum(diff(dx))/(sum(Iudx_lo * S_lo))
  incidencerate_mid = mean(c(incidencerate_lo, incidencerate_hi))
  #cat("avg incidencerate = (", incidencerate_lo, ", ", incidencerate_hi, ")\n", sep="")
  #e1 = S_hi_smooth %*% Iudx_hi_smooth #I_hi_smooth 
  #e2 = S_lo_smooth %*% Iudx_hi_smooth #I_hi_smooth 
  #e3 = S_hi_smooth %*% Iudx_lo_smooth #I_lo_smooth 
  #e4 = S_lo_smooth %*% Iudx_lo_smooth #I_lo_smooth 

  #e_lo = pmin(e1, e2, e3, e4)
  #e_hi = pmax(e1, e2, e3, e4)

  e_lo = S_lo_smooth * I_lo_smooth
  e_hi = S_hi_smooth * I_hi_smooth



  if(constFOI) {
    beta_lo = rep(incidencerate_lo, ndays)
    beta_hi = rep(incidencerate_hi, ndays)
  } else {
    beta_lo = c(0,diff(I_hi_smooth))/e_hi
    beta_lo[beta_lo<=0] = 0
    beta_hi = c(0,diff(I_hi_smooth))/e_lo
    beta_hi[beta_hi<=0] = 0
  }



  peak_dx_day = min(ndays,days_to_first_dx+dday-intvxday)

  dxrate_lo_smooth2 = rep(0,length(daily_timescale))
  dxrate_lo_smooth2[intvxday:(intvxday+peak_dx_day-days_to_first_dx)] = dxrate_lo_smooth[days_to_first_dx:peak_dx_day]
  dxrate_lo_smooth2[(intvxday+peak_dx_day-days_to_first_dx+1):ndays] = dxrate_lo_smooth[peak_dx_day]

  dxrate_hi_smooth2 = rep(0,length(daily_timescale))
  dxrate_hi_smooth2[intvxday:(intvxday+peak_dx_day-days_to_first_dx)] = dxrate_hi_smooth[days_to_first_dx:peak_dx_day]
  dxrate_hi_smooth2[(intvxday+peak_dx_day-days_to_first_dx+1):ndays] = dxrate_hi_smooth[peak_dx_day]


  S_lo2 = S_lo
  S_hi2 = S_hi

  I_lo2 = I_lo
  I_hi2 = I_hi

  Iudx_lo2 = Iudx_lo
  Iudx_hi2 = Iudx_hi

  Idx_lo2 = rep(0,length(daily_timescale))
  Idx_hi2 = rep(0,length(daily_timescale))

  newdx_hi = rep(0, length(daily_timescale))
  newdx_lo = rep(0, length(daily_timescale))
  newinfect_hi = rep(0, length(daily_timescale))
  newinfect_lo = rep(0, length(daily_timescale))

  for(i in intvxday:ndays) {

    newdx_hi[i] = dxrate_lo_smooth2[i-1]*Iudx_hi2[i-1] 
    newdx_lo[i] = dxrate_lo_smooth2[i-1]*Iudx_lo2[i-1] 
    #newdx_hi[i] = dxrate_lo*Iudx_hi2[i-1] 
    #newdx_lo[i] = dxrate_lo*Iudx_lo2[i-1] 


    newinfect_hi[i] = beta_hi[i-1]*Iudx_hi2[i-1]*S_hi2[i-1]
    newinfect_lo[i] = beta_lo[i-1]*Iudx_lo2[i-1]*S_lo2[i-1]

    Iudx_lo2[i] = pmin(N,pmax(0,Iudx_lo2[i-1] + newinfect_lo[i] - newdx_lo[i]))
    Iudx_hi2[i] = pmin(N,pmax(0,Iudx_hi2[i-1] + newinfect_hi[i] - newdx_hi[i]))

    Idx_lo2[i] = pmin(N,Idx_lo2[i-1] + newdx_lo[i])
    Idx_hi2[i] = pmin(N,Idx_hi2[i-1] + newdx_hi[i])

    #if(Iudx_lo2[i]<1) Iudx_lo2[i] = 0
    #if(Iudx_hi2[i]<1) Iudx_hi2[i] = 0

    #if(Iudx_lo2[i]>Iudx_hi2[i]) { Iudx_hi2[i] = Iudx_lo2[i] }


    I_lo2[i] = pmin(N,Iudx_lo2[i] + Idx_lo2[i])
    I_hi2[i] = pmin(N,Iudx_hi2[i] + Idx_hi2[i])


    S_lo2[i] = pmax(0,N - I_hi2[i])
    S_hi2[i] = pmax(0,N - I_lo2[i])
  }

  return(list(I_lo=I_lo,
              I_hi=I_hi,
              I_lo2=I_lo2,
              I_hi2=I_hi2,
              S_lo=S_lo,
              S_hi=S_hi,
              S_lo2=S_lo2,
              S_hi2=S_hi2,
              Iudx_lo2=Iudx_lo2,
              Iudx_hi2=Iudx_hi2,
              Idx_lo2=Idx_lo2,
              Idx_hi2=Idx_hi2))
}


###############################################
#############################################

get_indiana_results = function(N, intvxday, dday, smooth_dx, smooth_Iudx, smooth_I, smooth_S, constFOI, smoother) {

  obj = get_indiana_bounds(N, intvxday, dday, smooth_dx, smooth_Iudx, smooth_I, smooth_S, constFOI, smoother)  

  res = paste('When diagnostic scaleup starts on <span style=\"font-weight:bold\">', intvxday, 
              '</span>, cumulative incidence on <span style=\"font-weight:bold\">', enddate, 
              '</span> is projected to be between <span style=\"font-weight:bold\">', round(obj$Idx_lo2[ndays]), 
              '</span> and <span style=\"font-weight:bold\">', round(obj$Idx_hi2[ndays]), 
              '</span> people, compared to the actual number <span style=\"font-weight:bold\">', cumcases[ndays], 
              '</span>.', sep='')

  return(res)
}


#################################################
###################################################



plot_indiana_bounds = function(N, intvxday, dday, showDates, smooth_dx, smooth_Iudx, smooth_I, smooth_S, constFOI, showSusc, smoother) {

  obj = get_indiana_bounds(N, intvxday, dday, smooth_dx, smooth_Iudx, smooth_I, smooth_S, constFOI, smoother)  

  I_lo = obj$I_lo
  I_hi = obj$I_hi

  I_lo2 = obj$I_lo2
  I_hi2 = obj$I_hi2

  S_lo = obj$S_lo
  S_hi = obj$S_hi

  S_lo2 = obj$S_lo2
  S_hi2 = obj$S_hi2

  Iudx_lo2 = obj$Iudx_lo2
  Iudx_hi2 = obj$Iudx_hi2

  Idx_lo2 = obj$Idx_lo2
  Idx_hi2 = obj$Idx_hi2

  par(mar=c(2.7,4.5,1,1.0), bty="n", cex.lab=1.2, cex.axis=1.2)

  if(showSusc) {
    ymax = N
  } else {
    ymax = max(300,I_hi, I_hi2, Iudx_hi, Iudx_hi2, Idx_hi2)
  }

  plot(0, type="n", ylim=c(0,ymax), xlim=c(0,max(casedays)), #xlim=c(dayseq[sim_start_idx],max(casedays)), 
       ylab="People", xlab="", lwd=2, axes=FALSE)


  indices = c(1, 10, 22, 34, 46, 55)
  monthlabs = rep(NA, length(monthseq))
  monthlabs[indices] = format(monthdateseq[indices], "%B %Y")

  axis(1,at=monthseq[indices], lab=monthlabs[indices])
  axis(2)

  if(showSusc) {
    legend(0, 0.9*min(ymax,N),
           c("Susceptible HIV- (actual)", 
             "Undiagnosed HIV+ (actual)", 
             "Total HIV+ (actual)", 
             "Cumulative HIV Diagnoses (actual)"),
           border=c(mydarkgreen,mydarkred, mydarkgray, NA),
           lty=c(0, 0, 0, 1),
           pch=c(22,22,22,NA),
           pt.cex=c(2,2,2,NA),
           lwd=c(1,1,1,2),
           col=c(mydarkgreen, mydarkred, mydarkgray, "blue"),
           pt.bg=c(mydarkgreen, mydarkred, mydarkgray, NA),
           bg="white", bty="n", cex=1.2)

    legend(0, 0.6*min(ymax,N),
           c("Susceptible HIV- (projected)", 
             "Undiagnosed HIV+ (projected)", 
             "Total HIV+ (projected)", 
             "Cumulative HIV Diagnoses (projected)"), 
           fill=c(mygreen, myred, mygray, myblue), 
           border=c("green", "red", "black", "blue"),
           bg="white", bty="n", cex=1.2)




  } else {
    legend(0, 0.9*max(300,ymax),
           c("Undiagnosed HIV+ (actual)", 
             "Total HIV+ (actual)", 
             "Cumulative HIV Diagnoses (actual)"),
           border=c(mydarkgreen,mydarkred, mydarkgray, NA),
           lty=c(0, 0, 1),
           pch=c(22,22,NA),
           pt.cex=c(2,2,NA),
           lwd=c(1,1,2),
           col=c(mydarkred, mydarkgray, "blue"),
           pt.bg=c(mydarkred, mydarkgray, NA),
           bg="white", bty="n", cex=1.2)

    legend(0, 0.7*max(300,ymax),
           c("Undiagnosed HIV+ (projected)", 
             "Total HIV+ (projected)", 
             "Cumulative HIV Diagnoses (projected)"), 
           fill=c(myred, mygray, myblue), 
           border=c("red", "black", "blue"),
           bg="white", bty="n", cex=1.2)
  }

  abline(h=N, lty="dashed")

  # actual data: 
  polygon(c(daily_timescale,rev(daily_timescale)), c(Iudx_lo,rev(Iudx_hi)), col=mydarkred, border=mydarkred)
  if(showSusc) polygon(c(daily_timescale,rev(daily_timescale)), c(S_lo,rev(S_hi)), col=mydarkgreen, border=mydarkgreen)
  polygon(c(daily_timescale,rev(daily_timescale)), c(I_lo,rev(I_hi)), col=mydarkgray, border=mydarkgray)
  lines(daily_timescale, dx, lwd=2, col="blue")

  # projected
  if(showSusc) polygon(c(daily_timescale,rev(daily_timescale)), c(S_lo2,rev(S_hi2)), col=mygreen, border="green")
  polygon(c(daily_timescale,rev(daily_timescale)), c(Iudx_lo2,rev(Iudx_hi2)), col=myred, border="red")
  polygon(c(daily_timescale,rev(daily_timescale)), c(I_lo2,rev(I_hi2)), col=mygray, border="gray")
  polygon(c(daily_timescale,rev(daily_timescale)), c(Idx_lo2,rev(Idx_hi2)), col=myblue, border="blue")


  if(showDates) {
    arrows(days_to_first_dx, ymax*0.4,
           days_to_first_dx, I_hi[daily_timescale==days_to_first_dx], length=0.05)
    text(days_to_first_dx, ymax*0.4,
         "First Diagnosis", pos=2, cex=text_cex, offset=0.3)

    arrows(days_to_investigation_begin, ymax*0.5,
           days_to_investigation_begin, I_hi[daily_timescale==days_to_investigation_begin], length=0.05)
    text(days_to_investigation_begin, ymax*0.5,
         "Investigation Begins", pos=2, cex=text_cex, offset=0.3)

    arrows(days_to_emergency_declared, ymax*0.6,
           days_to_emergency_declared, I_hi[daily_timescale==days_to_emergency_declared], length=0.05)
    text(days_to_emergency_declared, ymax*0.6,
         "Emergency Declared", pos=2, cex=text_cex, offset=0.3)

    arrows(days_to_clinic_opened, ymax*0.7,
           days_to_clinic_opened, I_hi[daily_timescale==days_to_clinic_opened], length=0.05)
    text(days_to_clinic_opened, ymax*0.7,
         "Local HIV clinic opened", pos=2, cex=text_cex, offset=0.3)

    arrows(days_to_sep_started, ymax*0.8,
           days_to_sep_started, I_hi[daily_timescale==days_to_sep_started], length=0.05)
    text(days_to_sep_started, ymax*0.8,
         "SEP Begins", pos=2, cex=text_cex, offset=0.3)
  }


  res = list(Iend_lo=I_lo[ndays], Iend_hi=I_hi[ndays])
  return(res)


  #abline(v=intvxday)
  #arrows(intvxday, 0.5*ymax, intvxday, 0, lwd=2, length=0.3)
  #text(intvxday, 0.5*ymax, "intervention", offset=0.3, pos=4, cex=2)

}


