library(splines)


#############


smoothers = list(list(name="spline",
                      f=function(x,y,v)smooth.spline(y ~ x, df=v)$y,
                      step=1,
                      dxrange=c(5,16,50),
                      Iudxrange=c(2,9,50),
                      Irange=c(2,9,50),
                      Srange=c(2,20,50)),
                 list(name="loess",
                      f=function(x,y,v)predict(loess(y ~ x,span=v)),
                      step=0.01,
                      dxrange=c(0.05,0.11,1),
                      Iudxrange=c(0.05,0.4,1),
                      Irange=c(0.05,0.46,1),
                      Srange=c(0.05,0.2,1)),
                 list(name="ksmooth",
                      f=function(x,y,v)ksmooth(x,y,kernel="normal",bandwidth=v)$y,
                      step=0.01,
                      dxrange=c(1,20,200),
                      Iudxrange=c(15,100,1000),
                      Irange=c(1,170,1000),
                      Srange=c(1,500,1000)))

smoothernames = unlist(lapply(smoothers,function(s)s$name))



text_cex = 1.7

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

indices = c(1, 10, 22, 34, 46, 52)
monthlabs = rep(NA, length(monthseq))
monthlabs[indices] = format(monthdateseq[indices], "%B %Y")


###############################


#############################################
#############################################
#############################################
#############################################

plot_dx_rate = function(obj) { #N, intvxday, dday, showDates, smooth_dx, smooth_Iudx, smooth_I, smooth_S, constFOI, showSusc, smoother) {

  #obj = get_indiana_bounds(N, intvxday, dday, smooth_dx, smooth_Iudx, smooth_I, smooth_S, constFOI, smoother)

  dxrate_hi_smooth2 = obj$dxrate_hi_smooth2
  dxrate_lo_smooth2 = obj$dxrate_lo_smooth2


  

  plot(daily_timescale, dxrate_hi_smooth2, 
       type="n", 
       xlim=c(0,max(ndays)), 
       ylim=range(dxrate_hi_smooth2),
       axes=FALSE, ylab="Diagnosis rate")
  axis(1,at=monthseq[indices], lab=monthlabs[indices])
  axis(2)

  polygon(c(daily_timescale,rev(daily_timescale)), c(dxrate_lo_smooth2, rev(dxrate_hi_smooth2)), col=mygray)

  }

################################
################################

plot_methods_illustration = function(constFOI=FALSE) {

  N = 536
  smooth_dx   = smoothers[[1]]$dxrange[2]
  smooth_Iudx = smoothers[[1]]$Iudxrange[2]
  smooth_I    = smoothers[[1]]$Irange[2]
  smooth_S    = smoothers[[1]]$Srange[2]
  smoother = smoothernames[1]

  intvxday = first_dx #mdy("01/01/2013")
  dday = intvxday + 140
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

  dx_smooth = obj$dx_smooth

  dxrate_hi_smooth = obj$dxrate_hi_smooth
  dxrate_lo_smooth = obj$dxrate_lo_smooth

  dxrate_hi_smooth2 = obj$dxrate_hi_smooth2
  dxrate_lo_smooth2 = obj$dxrate_lo_smooth2

  text_cex = 1.2
  par(mar=c(2.7,4.5,1,1), bty="n", cex.lab=1.2, cex.axis=1.2)
  layout(matrix(c(1,5,
                  2,5,
                  3,5,
                  4,5), nrow=4, byrow=TRUE), widths=c(rep(5,4),2))
  plot(daily_timescale, dx, 
       type="n",
       xlim=c(0,max(ndays)), 
       ylim=c(0,max(dx)), 
       axes=FALSE, ylab="People")
  axis(1,at=monthseq[indices], lab=monthlabs[indices])
  axis(2)

  polygon(c(daily_timescale,rev(daily_timescale)), c(I_lo,rev(I_hi)), col=mydarkgray, border=mydarkgray)
  lines(daily_timescale, dx, lwd=2, col="blue")
  newcases = c(0,diff(dx))
  sapply(which(newcases>0), function(i) lines(c(i,i),c(0,newcases[i]), lwd=2, col="blue"))

  legend(0, 150,
           c("Total HIV+ (actual)", 
             "Cumulative HIV Diagnoses (actual)"),
           border=c(mydarkgray, NA),
           lty=c(0, 1),
           pch=c(22,NA),
           pt.cex=c(3,NA),
           lwd=c(1,2),
           col=c(mydarkgray, "blue"),
           pt.bg=c(mydarkgray, NA),
           bg="white", bty="n", cex=text_cex)
  
  plot(daily_timescale, dx, 
       type="n",
       xlim=c(0,max(ndays)), 
       ylim=c(0,max(dx)), 
       axes=FALSE, ylab="People")
  axis(1,at=monthseq[indices], lab=monthlabs[indices])
  axis(2)
  polygon(c(daily_timescale,rev(daily_timescale)), c(Iudx_lo,rev(Iudx_hi)), col=mydarkred, border=mydarkred)
  polygon(c(daily_timescale,rev(daily_timescale)), c(Iudx_lo2,rev(Iudx_hi2)), col=myred, border="red")
  #lines(daily_timescale, dx, lwd=2, col="blue")
  lines(daily_timescale, dx_smooth, lwd=2, col="blue")

   legend(0, 150,
           c("Undiagnosed HIV+ (actual)", 
             "Undiagnosed HIV+ (projected)", 
             "Cumulative HIV Diagnoses (smoothed)"), 
           border=c("red", "red", NA),
           lty=c(0, 0, 1),
           pch=c(22,22,NA),
           pt.cex=c(3,3,NA),
           col=c("red", "red", "blue"),
           pt.bg=c(mydarkred, myred, NA), 
           bg="white", bty="n", cex=text_cex)

  ymax = 1.2*max(dxrate_hi_smooth)
  plot(daily_timescale, dx, 
       type="n", 
       xlim=c(0,max(ndays)),
       ylim=c(0,ymax),
       axes=FALSE, ylab="Diagnosis rate")
  axis(1,at=monthseq[indices], lab=monthlabs[indices])
  axis(2)

  #lines(daily_timescale, c(0,diff(dx_smooth))/pmax(1,Iudx_lo))
  #lines(daily_timescale, c(0,diff(dx_smooth))/pmax(1,Iudx_hi))
  #polygon(c(daily_timescale,rev(daily_timescale)), c(dxrate_lo_smooth2, rev(dxrate_hi_smooth2)), col=mygray)
  polygon(c(daily_timescale,rev(daily_timescale)), c(dxrate_lo_smooth, rev(dxrate_hi_smooth)), col=mygray)

  arrows(days_to_first_dx, 0.7*max(dxrate_hi_smooth),
         days_to_first_dx, dxrate_hi_smooth[daily_timescale==days_to_first_dx], length=0.05)
  text(days_to_first_dx, 0.7*max(dxrate_hi_smooth),
       "First Diagnosis (actual)", pos=2, cex=text_cex, offset=0.3)
  arrows(days_to_first_dx+140, ymax,
         days_to_first_dx+140, dxrate_hi_smooth[daily_timescale==(days_to_first_dx+140)], length=0.05)
  text(days_to_first_dx+140, ymax,
       "Target diagnostic scaleup", pos=2, cex=text_cex, offset=0.3)


  legend(0, max(dxrate_hi_smooth),
           c("Diagnosis rate (actual)"),
           border=c("gray"),
           lty=c(0),
           pch=c(22),
           pt.cex=c(3),
           col=c("black"),
           pt.bg=c(mygray),
           bg="white", bty="n", cex=text_cex)



  intvxday = mdy("01/01/2013")
  dday = intvxday + 140
  obj2 = get_indiana_bounds(N, intvxday, dday, smooth_dx, smooth_Iudx, smooth_I, smooth_S, constFOI, smoother)

  days_to_intvx = as.numeric(intvxday - zerodate)
  days_to_dday = days_to_intvx+140
  
  ymax = 1.2*max(dxrate_hi_smooth)
  plot(daily_timescale, dx, 
       type="n", 
       xlim=c(0,max(ndays)),
       ylim=c(0,ymax),
       axes=FALSE, ylab="Diagnosis rate (counterfactual)")
  axis(1,at=monthseq[indices], lab=monthlabs[indices])
  axis(2)
  polygon(c(daily_timescale,rev(daily_timescale)), c(obj2$dxrate_lo_smooth2, rev(obj2$dxrate_hi_smooth2)), col=mygray)
  arrows(days_to_intvx, 0.7*max(dxrate_hi_smooth),
           days_to_intvx, obj2$dxrate_hi_smooth2[daily_timescale==days_to_intvx], length=0.05)
    text(days_to_intvx, 0.7*max(dxrate_hi_smooth),
         "First Diagnosis (counterfactual)", pos=2, cex=text_cex, offset=0.3)
  arrows(days_to_dday, ymax,
         days_to_dday, obj2$dxrate_hi_smooth2[daily_timescale==days_to_dday], length=0.05)
  text(days_to_dday, ymax,
       "Target diagnostic scaleup", pos=2, cex=text_cex, offset=0.3)

  # detail plot
  
  ymax = 170
  plot(0, type="n", ylim=c(0,ymax), xlim=c(1200,max(ndays)), #xlim=c(dayseq[sim_start_idx],max(ndays)), 
       ylab="People", xlab="", lwd=2, axes=FALSE)
  detail_indices = 40:52
  detail_monthlabs = rep(NA, length(monthseq))
  detail_monthlabs[detail_indices] = format(monthdateseq[detail_indices], "%B %Y")

  axis(1,at=monthseq[detail_indices], lab=detail_monthlabs[detail_indices])
  axis(2)

  # actual data: 
  polygon(c(daily_timescale,rev(daily_timescale)), c(Iudx_lo,rev(Iudx_hi)), col=mydarkred, border=mydarkred)
  #polygon(c(daily_timescale,rev(daily_timescale)), c(I_lo,rev(I_hi)), col=mydarkgray, border=mydarkgray)
  #lines(daily_timescale, dx, lwd=2, col="blue")

  # projected
  #polygon(c(daily_timescale,rev(daily_timescale)), c(Iudx_lo2,rev(Iudx_hi2)), col=myred, border="red")
  #polygon(c(daily_timescale,rev(daily_timescale)), c(I_lo2,rev(I_hi2)), col=mygray, border="gray")
  #polygon(c(daily_timescale,rev(daily_timescale)), c(Idx_lo2,rev(Idx_hi2)), col=myblue, border="blue")


    arrows(days_to_first_dx, ymax*0.8,
           days_to_first_dx, Iudx_hi[daily_timescale==days_to_first_dx], length=0.05)
    text(days_to_first_dx, ymax*0.8,
         "First Diagnosis", pos=2, cex=text_cex, offset=0.3)

    arrows(days_to_investigation_begin, ymax*0.9,
           days_to_investigation_begin, Iudx_hi[daily_timescale==days_to_investigation_begin], length=0.05)
    text(days_to_investigation_begin, ymax*0.9,
         "Investigation Begins", pos=2, cex=text_cex, offset=0.3)

    arrows(days_to_emergency_declared, ymax*0.7,
           days_to_emergency_declared, Iudx_hi[daily_timescale==days_to_emergency_declared], length=0.05)
    text(days_to_emergency_declared, ymax*0.7,
         "Emergency Declared", pos=4, cex=text_cex, offset=0.3)

    arrows(days_to_clinic_opened, ymax*0.6,
           days_to_clinic_opened, Iudx_hi[daily_timescale==days_to_clinic_opened], length=0.05)
    text(days_to_clinic_opened, ymax*0.6,
         "Local HIV clinic opened", pos=4, cex=text_cex, offset=0.3)

    arrows(days_to_sep_started, ymax*0.5,
           days_to_sep_started, Iudx_hi[daily_timescale==days_to_sep_started], length=0.05)
    text(days_to_sep_started, ymax*0.5,
         "SEP Begins", pos=4, cex=text_cex, offset=0.3)
  
}

############################################
############################################
############################################
############################################

plot_transmission_rate = function(obj) { #N, intvxday, dday, showDates, smooth_dx, smooth_Iudx, smooth_I, smooth_S, constFOI, showSusc, smoother) {

  #obj = get_indiana_bounds(N, intvxday, dday, smooth_dx, smooth_Iudx, smooth_I, smooth_S, constFOI, smoother)

  beta_lo = obj$beta_lo
  beta_hi = obj$beta_hi

  plot(daily_timescale, beta_hi, type="n", ylim=c(0,max(beta_hi)), xlim=c(0,max(ndays)), axes=FALSE, ylab="Infection rate")
  polygon(c(daily_timescale,rev(daily_timescale)), c(beta_lo, rev(beta_hi)), col=mygray)
  axis(1,at=monthseq[indices], lab=monthlabs[indices])
  axis(2)

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

  I_hi_smooth = pmax(1,smooth_method(daily_timescale, I_hi, v=smooth_I))
  I_lo_smooth = pmax(1,smooth_method(daily_timescale, I_lo, v=smooth_I))

  Iudx_lo_smooth = pmax(1,smooth_method(daily_timescale, Iudx_lo, v=smooth_Iudx))
  Iudx_hi_smooth = pmax(1,smooth_method(daily_timescale, Iudx_hi, v=smooth_Iudx))


  dx_smooth   =   dx
  backlag = 200
  dx_smooth[(days_to_first_dx-backlag):ndays] = smooth_method(daily_timescale[(days_to_first_dx-backlag):ndays], dx[(days_to_first_dx-backlag):ndays], v=smooth_dx)
  dx_smooth[dx_smooth<0] = 0

  #cat("length(dx_smooth) =", length(dx_smooth), "\n")


  dx_smooth_diff = c(0,pmax(0,diff(dx_smooth)))
  dxrate_hi_smooth = dx_smooth_diff/Iudx_lo_smooth
  dxrate_lo_smooth = dx_smooth_diff/Iudx_hi_smooth

  #dxrate_lo = mean(dxrate_lo_smooth[days_to_first_dx:ndays])
  #dxrate_hi = mean(dxrate_hi_smooth[days_to_first_dx:ndays])
  #dxrate_mid = mean(c(dxrate_lo, dxrate_hi))

  incidencerate_lo = sum(diff(I_lo))/(sum(Iudx_hi * S_hi))
  incidencerate_hi = sum(diff(I_hi))/(sum(Iudx_lo * S_lo))
  #incidencerate_mid = mean(c(incidencerate_lo, incidencerate_hi))

  cat("avg incidencerate = (", incidencerate_lo, ", ", incidencerate_hi, ")\n", sep="")
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

  #cat("length(dxrate_hi_smooth2) =", length(dxrate_hi_smooth2), "\n")
  #cat("length(daily_timescale) =", length(daily_timescale), "\n")


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
              Idx_hi2=Idx_hi2,
              dx_smooth=dx_smooth,
              dxrate_lo_smooth=dxrate_lo_smooth,
              dxrate_hi_smooth=dxrate_hi_smooth,
              dxrate_lo_smooth2=dxrate_lo_smooth2,
              dxrate_hi_smooth2=dxrate_hi_smooth2,
              beta_lo=beta_lo,
              beta_hi=beta_hi,
              N=N))
}


###############################################
#############################################

plot_infections_by_N = function(constFOI=FALSE) {

  smooth_dx   = smoothers[[1]]$dxrange[2]
  smooth_Iudx = smoothers[[1]]$Iudxrange[2]
  smooth_I    = smoothers[[1]]$Irange[2]
  smooth_S    = smoothers[[1]]$Srange[2]
  smoother = smoothernames[1]
  
  my_daily_timescale = rev(c(2, mdy("01/01/2013")-zerodate, begindate-zerodate) )
  a = 0.3
  mycols = rev(c(rgb(1,0,0,alpha=0.9), rgb(0,1,0,alpha=a), rgb(0,0,1,alpha=a)))

  Ns = seq(215,4000,length.out=50)

  I_hi_end = array(NA,dim=c(length(Ns),length(my_daily_timescale)))
  I_lo_end = array(NA,dim=c(length(Ns),length(my_daily_timescale)))


  par(mar=c(4.0,4.5,1,1.0), bty="n", cex.lab=1.2, cex.axis=1.2)

  ymax = ifelse(constFOI,480,300)

  plot(0, type="n", ylim=c(0,ymax), xlim=c(0,max(Ns)), ylab="Projected HIV infections by October 2015", xlab="Population size N", lwd=2)

  for(j in 1:length(my_daily_timescale)) {
    for(i in 1:length(Ns)) {
   

      idate = zerodate + my_daily_timescale[j]

      obj = get_indiana_bounds(Ns[i], idate, idate+140, smooth_dx, smooth_Iudx, smooth_I, smooth_S, constFOI, smoother)  

      I_lo_end[i,j] = obj$Idx_lo2[ndays]
      I_hi_end[i,j] = obj$Idx_hi2[ndays]
    }
    polygon(c(Ns,rev(Ns)), c(I_hi_end[,j], rev(I_lo_end[,j])), col=mycols[j], border="black")
  }

  points(4000, max(dx), pch=16, cex=1.5)
  text(4000, max(dx),   paste("Actual infections:", max(dx)), pos=2)

  legend(2900,ymax, paste("Intervention on", c(zerodate+2, mdy("01/01/2013"), begindate)), 
         pch=22, pt.cex=2, cex=1, pt.bg=mycols, bty="n")


}

#############################################
#############################################

plot_infections_by_intvx_date = function(constFOI=FALSE) {

  N = 536
  smooth_dx   = smoothers[[1]]$dxrange[2]
  smooth_Iudx = smoothers[[1]]$Iudxrange[2]
  smooth_I    = smoothers[[1]]$Irange[2]
  smooth_S    = smoothers[[1]]$Srange[2]
  smoother = smoothernames[1]
  
  my_daily_timescale = seq(daily_timescale[1]+1, days_to_first_dx, by=10)
  #my_daily_timescale = sort(unique(c(my_daily_timescale, days_to_first_dx, days_to_investigation_begin, 
                                     #days_to_emergency_declared, days_to_clinic_opened, days_to_sep_started)))

  I_hi_end = rep(NA,length(my_daily_timescale))
  I_lo_end = rep(NA,length(my_daily_timescale))

  for(i in 1:length(my_daily_timescale)) {
    #cat("i =", i, "/", length(my_daily_timescale), "\n")

    idate = zerodate + my_daily_timescale[i]
    #print(idate)

    obj = get_indiana_bounds(N, idate, idate+140, smooth_dx, smooth_Iudx, smooth_I, smooth_S, constFOI, smoother)  
    I_lo_end[i] = obj$Idx_lo2[ndays]
    I_hi_end[i] = obj$Idx_hi2[ndays]
  }

  #par(mar=c(4.0,4.5,1,1.0), bty="n", cex.lab=1.2, cex.axis=1.2)
  
  ymax = max(I_hi_end)
  plot(0, type="n", ylim=range(c(I_hi_end,I_lo_end)), xlim=c(0,max(ndays)), #days_to_first_dx+100), #xlim=c(dayseq[sim_start_idx],max(ndays)), 
       ylab="Projected HIV infections by October 2015", xlab="Counterfactual intervention date", lwd=2, axes=FALSE)


  #indices = c(1, 10, 22, 34, 46, 55)
  #monthlabs = rep(NA, length(monthseq))
  #monthlabs[indices] = format(monthdateseq[indices], "%B %Y")

  axis(1,at=monthseq[indices], lab=monthlabs[indices])
  axis(2)


  polygon(c(my_daily_timescale,rev(my_daily_timescale)), c(I_hi_end, rev(I_lo_end)), col=mygray, border="gray")

  points(days_to_first_dx, max(dx), pch=16, cex=1.5)
  text(days_to_first_dx, max(dx), 
       paste("Actual infections:", max(dx)),
       pos=4)

}

###############################################
###############################################




get_indiana_results_text = function(N, intvxday, dday, smooth_dx, smooth_Iudx, smooth_I, smooth_S, constFOI, smoother) {

  obj = get_indiana_bounds(N, intvxday, dday, smooth_dx, smooth_Iudx, smooth_I, smooth_S, constFOI, smoother)  

  res = paste('When diagnostic scaleup starts on <span style=\"font-weight:bold\">', intvxday, 
              '</span>, cumulative incidence on <span style=\"font-weight:bold\">', enddate, 
              '</span> is projected to be between <span style=\"font-weight:bold\">', round(obj$I_lo2[ndays]), 
              '</span> and <span style=\"font-weight:bold\">', round(obj$I_hi2[ndays]), 
              '</span> people, compared to the actual number <span style=\"font-weight:bold\">', cumcases[ndays], 
              '</span>.', sep='')

  return(res)
}


#################################################
#################################################



plot_indiana_bounds = function(N, intvxday, dday, showDates, smooth_dx, smooth_Iudx, smooth_I, smooth_S, constFOI, showSusc, smoother) {

  obj = get_indiana_bounds(N, intvxday, dday, smooth_dx, smooth_Iudx, smooth_I, smooth_S, constFOI, smoother)  

  par(bty="n", cex=text_cex, cex.lab=text_cex, cex.axis=text_cex, 
      mar=c(2.7,4.5,1,0.0))

  layout(matrix(c(1,2,3), nrow=3), heights=c(1,1,3))


  plot_dx_rate(obj)
  plot_transmission_rate(obj)
  plot_epidemic_curves(obj, showDates, showSusc)

}

##################################################
##################################################

plot_epidemic_curves = function(obj, showDates, showSusc) {

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

  N = obj$N

  if(showSusc) {
    ymax = N
  } else {
    ymax = max(300,I_hi, I_hi2, Iudx_hi, Iudx_hi2, Idx_hi2)
  }

  plot(0, type="n", ylim=c(0,ymax), xlim=c(0,max(ndays)), #xlim=c(dayseq[sim_start_idx],max(ndays)), 
       ylab="People", xlab="", lwd=2, axes=FALSE)


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
           pt.cex=c(3,3,3,NA),
           lwd=c(1,1,1,2),
           col=c(mydarkgreen, mydarkred, mydarkgray, "blue"),
           pt.bg=c(mydarkgreen, mydarkred, mydarkgray, NA),
           bg="white", bty="n", cex=text_cex)

    legend(0, 0.6*min(ymax,N),
           c("Susceptible HIV- (projected)", 
             "Undiagnosed HIV+ (projected)", 
             "Total HIV+ (projected)", 
             "Cumulative HIV Diagnoses (projected)"), 
           fill=c(mygreen, myred, mygray, myblue), 
           border=c("green", "red", "black", "blue"),
           pt.cex=c(3,3,3,3),
           bg="white", bty="n", cex=text_cex)

  } else {
    legend(0, 0.9*max(300,ymax),
           c("Undiagnosed HIV+ (actual)", 
             "Total HIV+ (actual)", 
             "Cumulative HIV Diagnoses (actual)"),
           border=c(mydarkgreen,mydarkred, mydarkgray, NA),
           lty=c(0, 0, 1),
           pch=c(22,22,NA),
           pt.cex=c(3,3,3),
           lwd=c(1,1,2),
           col=c(mydarkred, mydarkgray, "blue"),
           pt.bg=c(mydarkred, mydarkgray, NA),
           bg="white", bty="n", cex=text_cex)

    legend(0, 0.7*max(300,ymax),
           c("Undiagnosed HIV+ (projected)", 
             "Total HIV+ (projected)", 
             "Cumulative HIV Diagnoses (projected)"), 
           fill=c(myred, mygray, myblue), 
           border=c("red", "black", "blue"),
           pt.cex=c(3,3,3),
           bg="white", bty="n", cex=text_cex)
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


