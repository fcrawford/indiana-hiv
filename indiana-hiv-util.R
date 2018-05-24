library(splines)
library(RColorBrewer)
source("indiana-hiv-load.R")
source("indiana-hiv-settings.R")

#############################################

plot_dx_rate = function(obj) { 

  dxrate_mid_smooth2 = obj$dxrate_mid_smooth2
  dxrate_hi_smooth = obj$dxrate_hi_smooth
  dxrate_lo_smooth = obj$dxrate_lo_smooth
  
  ymax = max(dxrate_hi_smooth)

  plot(dayseq, dxrate_mid_smooth2, 
       type="n", 
       xlim=range(dayseq),
       ylim=c(0,ymax),
       axes=FALSE, ylab="Diagnosis rate")
  axis(1,at=monthdayseq, lab=monthlabseq)
  axis(2)

  # actual
  polygon(c(dayseq,rev(dayseq)), c(dxrate_lo_smooth, rev(dxrate_hi_smooth)), col=mydarkorange, border=mydarkorange)

  # projected
  lines(dayseq, dxrate_mid_smooth2, col=myorange, lwd=2)

  legend(0, 0.9*ymax,
           c("Diagnosis rate (actual)", "Diagnosis rate (projected)"),
           lty=c(0,1),
           lwd=c(NA,2),
           pch=c(22,NA),
           pt.cex=c(3,NA),
           col=c(mydarkorange, myorange),
           border=c(mydarkorange, myorange),
           pt.bg=c(mydarkorange, myorange),
           bg="white", bty="n", cex=text_cex)
  }

################################
################################

plot_methods_illustration1 = function() {

  smooth_dx   = smoothers[[1]]$dxrange[2]
  smooth_Iudx = smoothers[[1]]$Iudxrange[2]
  smooth_I    = smoothers[[1]]$Irange[2]
  smooth_S    = smoothers[[1]]$Srange[2]
  smoother = smoothernames[1]

  # Get data under actual intvx
  obj_actual = get_indiana_bounds(N_init, intvx_actual_date, end_date, 
                                  smooth_dx, smooth_Iudx, smooth_I, smooth_S, smoother, 
                                  removal_rate_mid, calibration_scale_init) 

  I_lo_actual = obj_actual$I_lo
  I_hi_actual = obj_actual$I_hi

  Iudx_lo_actual = obj_actual$Iudx_lo
  Iudx_hi_actual = obj_actual$Iudx_hi

  Iudx_lo2_actual = obj_actual$Iudx_lo2
  Iudx_hi2_actual = obj_actual$Iudx_hi2

  dxrate_hi_smooth = obj_actual$dxrate_hi_smooth
  dxrate_lo_smooth = obj_actual$dxrate_lo_smooth


  par(mfrow=c(2,1), mar=c(2.7,4.5,1,1), bty="n", cex.lab=1.2, cex.axis=1.2)

  #layout(matrix(c(1,2), byrow=TRUE, nrow=2), heights=c(1,2))

  # Fig 1A

  plot(dayseq, dx, 
       type="n",
       xlim=c(0,max(dayseq)), 
       ylim=c(0,max(I_hi_actual)), 
       axes=FALSE, ylab="People")
  axis(1,at=monthdayseq, lab=monthlabseq)
  axis(2)

  polygon(c(dayseq,rev(dayseq)), c(I_lo_actual,rev(I_hi_actual)), col=mydarkgray, border=mydarkgray)
  lines(dayseq, dx, lwd=2, col=mydarkblue)
  #newcases = c(0,diff(dx))
  #sapply(which(newcases>0), function(i) lines(c(i,i),c(0,newcases[i]), lwd=2, col=mydarkblue))

  legend(0, 150,
           c("Cumulative HIV incidence (Campbell et al, 2017)", 
             "Cumulative HIV Diagnoses (Peters et al, 2016)"),
           lty=c(0, 1),
           pch=c(22,NA),
           pt.cex=c(3,NA),
           lwd=c(1,2),
           col=c(mydarkgray, mydarkblue),
           border=c(mydarkgray,NA),
           pt.bg=c(mydarkgray, NA),
           bg="white", bty="n", cex=text_cex)
  mtext("A", side=3, adj=0, line=-1.2, cex=2)

  # Fig 1B

  plot(dayseq, dx, 
       type="n",
       xlim=range(dayseq),
       ylim=c(0,max(dx)), 
       axes=FALSE, ylab="People")
  axis(1,at=monthdayseq, lab=monthlabseq)
  axis(2)
  polygon(c(dayseq,rev(dayseq)), c(Iudx_lo_actual,rev(Iudx_hi_actual)), col=mydarkred, border=mydarkred)
  #polygon(c(dayseq,rev(dayseq)), c(Iudx_lo2_actual,rev(Iudx_hi2_actual)), col=myred, border=myred)
  #lines(dayseq, dx_smooth, lwd=2, col=mydarkblue)


  ymax = 170

    arrows(first_dx_day, ymax*0.8,
           first_dx_day, Iudx_hi_actual[dayseq==first_dx_day], length=0.05)
    text(first_dx_day, ymax*0.8,
         "First diagnosis", pos=2, cex=text_cex, offset=0.3)

    arrows(investigation_begin_day, ymax*0.9,
           investigation_begin_day, Iudx_hi_actual[dayseq==investigation_begin_day], length=0.05)
    text(investigation_begin_day, ymax*0.9,
         "Investigation begins", pos=2, cex=text_cex, offset=0.3)

    arrows(emergency_declared_day, ymax*1.0,
           emergency_declared_day, Iudx_hi_actual[dayseq==emergency_declared_day], length=0.05)
    text(emergency_declared_day, ymax*1.0,
         "Emergency declared", pos=2, cex=text_cex, offset=0.3)

    arrows(clinic_opened_day, ymax*0.9,
           clinic_opened_day, Iudx_hi_actual[dayseq==clinic_opened_day], length=0.05)
    text(clinic_opened_day, ymax*0.9,
         "HIV clinic opens", pos=4, cex=text_cex, offset=0.3)

    arrows(sep_started_day, ymax*0.8,
           sep_started_day, Iudx_hi_actual[dayseq==sep_started_day], length=0.05)
    text(sep_started_day, ymax*0.8,
         "SEP begins", pos=4, cex=text_cex, offset=0.3)

    legend(0, 150,
           "Undiagnosed HIV+",
           border=mydarkred,
           lty=0,
           pch=22,
           pt.cex=3,
           col=mydarkred,
           pt.bg=mydarkred,
           bg="white", bty="n", cex=text_cex)

  mtext("B", side=3, adj=0, line=-1.2, cex=2)


}


#####################################


plot_methods_illustration2 = function() {

  smooth_dx   = smoothers[[1]]$dxrange[2]
  smooth_Iudx = smoothers[[1]]$Iudxrange[2]
  smooth_I    = smoothers[[1]]$Irange[2]
  smooth_S    = smoothers[[1]]$Srange[2]
  smoother = smoothernames[1]

    # Get data under actual intvx
  obj_actual = get_indiana_bounds(N_init, intvx_actual_date, end_date, 
                                  smooth_dx, smooth_Iudx, smooth_I, smooth_S, smoother, 
                                  removal_rate_mid, calibration_scale_init) 

  I_lo_actual = obj_actual$I_lo
  I_hi_actual = obj_actual$I_hi

  Iudx_lo_actual = obj_actual$Iudx_lo
  Iudx_hi_actual = obj_actual$Iudx_hi

  Iudx_lo2_actual = obj_actual$Iudx_lo2
  Iudx_hi2_actual = obj_actual$Iudx_hi2

  dxrate_hi_smooth = obj_actual$dxrate_hi_smooth
  dxrate_lo_smooth = obj_actual$dxrate_lo_smooth

  #dday = intvxday + scaleup_peak_offset
  obj = get_indiana_bounds(N_init, intvx_mid_date, intvx_mid_date+scaleup_peak_offset, 
                           smooth_dx, smooth_Iudx, smooth_I, smooth_S, smoother, 
                           removal_rate_mid, calibration_scale_init) 
  #I_lo = obj$I_lo
  #I_hi = obj$I_hi

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


  dxrate_hi_smooth2 = obj$dxrate_hi_smooth2
  dxrate_lo_smooth2 = obj$dxrate_lo_smooth2

  par(mfrow=c(2,1), mar=c(2.7,4.5,1,1), bty="n", cex.lab=1.2, cex.axis=1.2)

  # Fig 2A

  plot(dayseq, dx, 
       type="n", 
       xlim=range(dayseq),
       ylim=c(0,max(dxrate_hi_smooth)),
       axes=FALSE, ylab="Diagnosis rate")
  axis(1,at=monthdayseq, lab=monthlabseq)
  axis(2)

  polygon(c(dayseq,rev(dayseq)), c(dxrate_lo_smooth, rev(dxrate_hi_smooth)), col=mydarkorange, border=mydarkorange)

  lines(dayseq, obj_actual$dxrate_mid_smooth2, col=myorange, lwd=2)

  ymax = max(dxrate_hi_smooth)
  arrows(first_dx_day, 0.7*ymax,
         first_dx_day, obj_actual$dxrate_mid_smooth[dayseq==first_dx_day], length=0.05)
  text(first_dx_day, 0.7*ymax,
       "First Diagnosis", pos=2, cex=text_cex, offset=0.3)
  arrows(first_dx_day+scaleup_peak_offset, 0.9*ymax,
         first_dx_day+scaleup_peak_offset, obj_actual$dxrate_mid_smooth[dayseq==(first_dx_day+scaleup_peak_offset)], length=0.05)
  text(first_dx_day+scaleup_peak_offset, 0.9*ymax,
       "Target casefinding scaleup", pos=2, cex=text_cex, offset=0.3)


  legend(0, 0.5*max(obj_actual$dxrate_hi_smooth),
           c("Diagnosis rate bounds (reconstructed from raw data)",
             "Diagnosis rate midpoint"),
           border=c(myorange,NA),
           lty=c(0,1),
           lwd=c(NA,2),
           pch=c(22,NA),
           pt.cex=c(3,NA),
           col=c(mydarkorange,myorange),
           pt.bg=c(mydarkorange,NA),
           bg="white", bty="n", cex=text_cex)
  mtext("A", side=3, adj=0, line=-1.2, cex=2)


  # Fig 2B
  plot(dayseq, dx, 
       type="n", 
       xlim=range(dayseq),
       ylim=c(0,max(dxrate_hi_smooth)),
       axes=FALSE, ylab="Diagnosis rate")
  axis(1,at=monthdayseq, lab=monthlabseq)
  axis(2)

  polygon(c(dayseq,rev(dayseq)), c(dxrate_lo_smooth, rev(dxrate_hi_smooth)), col=mydarkorange, border=mydarkorange)

  lines(dayseq, obj$dxrate_mid_smooth2, col=myorange, lwd=2)

  #ymax = max(dxrate_hi_smooth)
  arrows(intvx_mid_day, 0.7*ymax,
         intvx_mid_day, dxrate_hi_smooth[dayseq==intvx_mid_day], length=0.05)
  text(intvx_mid_day, 0.7*ymax,
       "First Diagnosis (counterfactual)", pos=2, cex=text_cex, offset=0.3)
  arrows(intvx_mid_day+scaleup_peak_offset, 0.9*ymax,
         intvx_mid_day+scaleup_peak_offset, obj$dxrate_mid_smooth[dayseq==(intvx_mid_day+scaleup_peak_offset)], length=0.05)
  text(intvx_mid_day+scaleup_peak_offset, 0.9*ymax,
       "Target casefinding scaleup (counterfactual)", pos=2, cex=text_cex, offset=0.3)

  legend(0, 0.5*max(dxrate_hi_smooth),
          "Diagnosis rate (counterfactual)",
           lty=c(1),
           lwd=2,
           col=c(myorange),
           bg="white", bty="n", cex=text_cex)

  #legend(0, 0.5*max(dxrate_hi_smooth),
           #c("Diagnosis rate (reconstructed from raw data)",
             #"Diagnosis rate (counterfactual)"),
           #border=c(myorange,NA),
           #lty=c(0,1),
           #pch=c(22,NA),
           #pt.cex=c(3,NA),
           #col=c(mydarkorange,myorange),
           #pt.bg=c(mydarkorange,NA),
           #bg="white", bty="n", cex=text_cex)

  mtext("B", side=3, adj=0, line=-1.2, cex=2)

  # Detail plot
  
  
}

############################################
############################################
############################################
############################################

 # NOT USED! 
plot_transmission_rate = function(obj) { 


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

get_indiana_bounds = function(N, intvx_date, intvx_end_date, smooth_dx, 
                              smooth_Iudx, smooth_I, smooth_S, smoother, 
                              removal_rate, calibration_scale, beta_scale=1) {

  intvx_day     = as.numeric(intvx_date - zero_date)
  intvx_end_day = as.numeric(intvx_end_date - zero_date)

  smooth_method = smoothers[[which(smoothernames==smoother)]]$f

  infections_mean = (infections_lo + infections_hi) / 2
  infections_lo_tmp = pmax(0,infections_mean - calibration_scale*(infections_mean-infections_lo))
  infections_hi_tmp = infections_mean + calibration_scale*(infections_hi-infections_mean)

  ylo = rep(NA, length(dayseq))
  yhi = rep(NA, length(dayseq))
  ylo[1] = infections_lo_tmp[1]
  yhi[1] = infections_hi_tmp[1]

  for(i in 2:ndays) {
    yhi[i] = max(c(yhi[1:(i-1)], infections_hi_tmp[i]))
    ylo[i] = min(infections_lo_tmp[i:ndays])
  }

  infections_lo = ylo
  infections_hi = yhi


  I_hi = infections_hi
  I_lo = pmax(infections_lo,dx)

  Iudx_lo = pmax(0,I_lo - dx)
  Iudx_hi = pmin(N,I_hi - dx)


  S_hi = pmax(0,N - I_lo)
  S_lo = pmin(N,N - I_hi)

  S_hi_smooth = smooth_method(dayseq, S_hi, v=smooth_S)
  S_lo_smooth = smooth_method(dayseq, S_lo, v=smooth_S)

  I_hi_smooth = pmax(1,smooth_method(dayseq, I_hi, v=smooth_I))
  I_lo_smooth = pmax(1,smooth_method(dayseq, I_lo, v=smooth_I))

  Iudx_lo_smooth = pmax(1,smooth_method(dayseq, Iudx_lo, v=smooth_Iudx))
  Iudx_hi_smooth = pmax(1,smooth_method(dayseq, Iudx_hi, v=smooth_Iudx))


  dx_smooth = dx
  backlag = 200
  dx_smooth[(first_dx_day-backlag):ndays] = smooth_method(dayseq[(first_dx_day-backlag):ndays], dx[(first_dx_day-backlag):ndays], v=smooth_dx)
  dx_smooth[dx_smooth<0] = 0

  dx_smooth_diff = c(0,pmax(0,diff(dx_smooth)))
  dxrate_hi_smooth = dx_smooth_diff/Iudx_lo_smooth
  dxrate_lo_smooth = dx_smooth_diff/Iudx_hi_smooth

  #print(I_hi_smooth)
  #print(I_lo_smooth)

  # estimate bounds for beta
  daymax = first_dx_day
  beta_lo = (I_lo_smooth[daymax] - I_hi[1])/(I_hi_smooth[1:daymax] %*% (N-I_hi_smooth[1:daymax]))
  beta_hi = (I_hi_smooth[daymax] - I_lo[1])/(Iudx_lo_smooth[1:daymax] %*% (N-Iudx_lo_smooth[1:daymax]))

  beta_mid = rep((beta_lo+beta_hi)/2, ndays)

  # scale reduction in beta
  beta_mid[intvx_day:ndays] = beta_scale*beta_mid[intvx_day:ndays] 


  peak_dx_day = min(end_day,first_dx_day+intvx_end_day-intvx_day)


  dxrate_lo_smooth2 = rep(0,ndays)
  dxrate_lo_smooth2[intvx_day:(intvx_day+peak_dx_day-first_dx_day)] = dxrate_lo_smooth[first_dx_day:peak_dx_day]
  dxrate_lo_smooth2[(intvx_day+peak_dx_day-first_dx_day+1):ndays] = dxrate_lo_smooth[peak_dx_day]

  dxrate_hi_smooth2 = rep(0,ndays)
  dxrate_hi_smooth2[intvx_day:(intvx_day+peak_dx_day-first_dx_day)] = dxrate_hi_smooth[first_dx_day:peak_dx_day]
  dxrate_hi_smooth2[(intvx_day+peak_dx_day-first_dx_day+1):ndays] = dxrate_hi_smooth[peak_dx_day]

  dxrate_mid_smooth2 = (dxrate_lo_smooth2 + dxrate_hi_smooth2)/2


  S_lo2 = S_lo
  S_hi2 = S_hi

  I_lo2 = I_lo
  I_hi2 = I_hi

  Iudx_lo2 = Iudx_lo
  Iudx_hi2 = Iudx_hi

  Idx_lo2 = rep(0,length(dayseq))
  Idx_hi2 = rep(0,length(dayseq))

  R_lo = rep(0, ndays)
  R_hi = rep(0, ndays)

  cumdx_lo = rep(0, ndays)
  cumdx_hi = rep(0, ndays)

  for(i in intvx_day:ndays) {

    new_dx_hi = dxrate_mid_smooth2[i-1]*Iudx_hi2[i-1] 
    new_dx_lo = dxrate_mid_smooth2[i-1]*Iudx_lo2[i-1] 

    new_removal_lo = removal_rate*Idx_lo2[i-1]
    new_removal_hi = removal_rate*Idx_hi2[i-1]

    new_infect_hi = beta_mid[i]*(Iudx_hi2[i-1] + Idx_hi2[i-1])*S_hi2[i-1]
    new_infect_lo = beta_mid[i]*(Iudx_lo2[i-1] + Idx_lo2[i-1])*S_lo2[i-1]


    Iudx_lo2[i] = pmin(N,pmax(0,Iudx_lo2[i-1] + new_infect_lo - new_dx_lo))
    Iudx_hi2[i] = pmin(N,pmax(0,Iudx_hi2[i-1] + new_infect_hi - new_dx_hi))

    Idx_lo2[i] = pmin(N,Idx_lo2[i-1] + new_dx_lo - new_removal_lo)
    Idx_hi2[i] = pmin(N,Idx_hi2[i-1] + new_dx_hi - new_removal_hi)

    cumdx_lo[i] = pmin(N,cumdx_lo[i-1] + new_dx_lo)
    cumdx_hi[i] = pmin(N,cumdx_hi[i-1] + new_dx_hi)

    R_lo[i] = R_lo[i-1] + new_removal_lo
    R_hi[i] = R_hi[i-1] + new_removal_hi

    I_lo2[i] = pmin(N, I_lo2[i-1] + new_infect_lo) #pmin(N,Iudx_lo2[i] + Idx_lo2[i] + R_lo[i])
    I_hi2[i] = pmin(N, I_hi2[i-1] + new_infect_hi) #pmin(N,Iudx_hi2[i] + Idx_hi2[i] + R_hi[i])


    S_lo2[i] = pmax(0,N - I_hi2[i])
    S_hi2[i] = pmax(0,N - I_lo2[i])
  }



  return(list(I_lo=I_lo,
              I_hi=I_hi,
              I_lo2=I_lo2,
              I_hi2=I_hi2,
              I_lo_smooth=I_lo_smooth,
              I_hi_smooth=I_hi_smooth,
              S_lo=S_lo,
              S_hi=S_hi,
              S_lo2=S_lo2,
              S_hi2=S_hi2,
              Iudx_lo_smooth=Iudx_lo_smooth,
              Iudx_hi_smooth=Iudx_hi_smooth,
              Iudx_lo2=Iudx_lo2,
              Iudx_hi2=Iudx_hi2,
              Iudx_lo=Iudx_lo,
              Iudx_hi=Iudx_hi,
              Idx_lo2=Idx_lo2,
              Idx_hi2=Idx_hi2,
              cumdx_lo=cumdx_lo,
              cumdx_hi=cumdx_hi,
              R_lo=R_lo,
              R_hi=R_hi,
              dx_smooth=dx_smooth,
              dxrate_lo_smooth=dxrate_lo_smooth,
              dxrate_hi_smooth=dxrate_hi_smooth,
              dxrate_lo_smooth2=dxrate_lo_smooth2,
              dxrate_hi_smooth2=dxrate_hi_smooth2,
              dxrate_mid_smooth2=dxrate_mid_smooth2,
              beta_lo=beta_lo,
              beta_hi=beta_hi,
              N=N))
}


###############################################
#############################################

plot_infections_by_N = function() {

  smooth_dx   = smoothers[[1]]$dxrange[2]
  smooth_Iudx = smoothers[[1]]$Iudxrange[2]
  smooth_I    = smoothers[[1]]$Irange[2]
  smooth_S    = smoothers[[1]]$Srange[2]
  smoother = smoothernames[1]
  
  intvx_dates = c(intvx_early_date+2, intvx_mid_date, intvx_actual_date)

  a = 0.3
  mycols = rev(c(rgb(1,0,0,alpha=a), rgb(0,1,0,alpha=a), rgb(0,0,1,alpha=a)))

  Ns = seq(215,4000,length.out=50)

  I_hi_end = array(NA,dim=c(length(Ns),length(intvx_dates)))
  I_lo_end = array(NA,dim=c(length(Ns),length(intvx_dates)))

  par(mfrow=c(3,1), mar=c(4.0,4.5,1,1.0), bty="n", cex.lab=1.2, cex.axis=1.2)

  ymax = 450


  for(j in 1:length(intvx_dates)) {
    for(i in 1:length(Ns)) {

      idate = intvx_dates[j]

      obj = get_indiana_bounds(Ns[i], idate, idate+scaleup_peak_offset, smooth_dx, smooth_Iudx, smooth_I, smooth_S, smoother, removal_rate_mid, calibration_scale_init)  

      I_lo_end[i,j] = obj$I_lo2[ndays]
      I_hi_end[i,j] = obj$I_hi2[ndays]
    }
    plot(0, type="n", ylim=c(0,ymax), xlim=c(215,max(Ns)), 
         ylab=paste("HIV infections by", format(end_date, "%B %d, %Y")), 
         xlab="Population size N", lwd=2, axes=FALSE)
    axis(1,at=c(215,1000,2000,3000,4000))
    axis(2)

    polygon(c(Ns,rev(Ns)), c(I_hi_end[,j], rev(I_lo_end[,j])), col=mycols[j], border=mycols[j])
    polygon(c(215,4000,4000,215), c(rep(obj$I_lo[ndays],2), rep(obj$I_hi[ndays],2)), col=mygray, border=mygray)
    text(1000, obj$I_lo[ndays],   
       paste("Actual infections by", format(end_date, "%B %d, %Y"), ":", 
          round(obj$I_lo[ndays]), "-", round(obj$I_hi[ndays])), pos=3, cex=text_cex)

    legend(215,ymax, c(paste("Intervention on", format(intvx_dates[j], "%B %d,%Y")), 
                     paste("Infections on", format(end_date, "%B %d, %Y"), 
                     "under actual circumstances")),
           pch=22, pt.cex=2, cex=text_cex, col=c(mycols[j],mygray), pt.bg=c(mycols[j],mygray), bty="n")
  }




}

#############################################
#############################################

plot_infections_by_intvx_date = function() {

  smooth_dx   = smoothers[[1]]$dxrange[2]
  smooth_Iudx = smoothers[[1]]$Iudxrange[2]
  smooth_I    = smoothers[[1]]$Irange[2]
  smooth_S    = smoothers[[1]]$Srange[2]
  smoother = smoothernames[1]
  
  idates = seq(zero_date+2, first_dx_date, by=1)
  idays  = difftime(idates, zero_date)

  I_hi_end = rep(NA,length(idates))
  I_lo_end = rep(NA,length(idates))

  for(i in 1:length(idates)) {
    idate = idates[i]
    obj = get_indiana_bounds(N_init, idate, idate+scaleup_peak_offset, smooth_dx, 
    smooth_Iudx, smooth_I, smooth_S, smoother, removal_rate_init, calibration_scale_init)  
    I_lo_end[i] = obj$I_lo2[ndays]
    I_hi_end[i] = obj$I_hi2[ndays]
  }

  #par(mar=c(4.0,4.5,1,1.0), bty="n", cex.lab=1.2, cex.axis=1.2)
  
  ymax = max(I_hi_end)
  par(mar=c(4.0,4.5,1,1.0), bty="n", cex.lab=1.2, cex.axis=1.2)

  plot(0, type="n", ylim=range(c(I_hi_end,I_lo_end)), xlim=c(0,first_dx_day+70), 
       ylab=paste("HIV infections by", format(end_date, "%B %d, %Y")), 
       xlab="Counterfactual intervention date", lwd=2, axes=FALSE)


  axis(1,at=monthdayseq[1:5], lab=monthlabseq[1:5])
  axis(2)


  polygon(c(idays,rev(idays)), c(I_hi_end, rev(I_lo_end)), col=mygray, border="gray")

  points(first_dx_day, (obj$I_lo[ndays]+obj$I_lo[ndays])/2, 
         pch=3, cex=2, col="black", bg="black")

  text(first_dx_day, (obj$I_lo[ndays]+obj$I_lo[ndays])/2, 
       paste("Actual infections by", format(end_date, "%B %d, %Y"), ":", 
              round(obj$I_lo[ndays]), "-", round(obj$I_hi[ndays])), 
       cex=text_cex,
       pos=2,offset=1)


}

###############################################
###############################################

plot_infections_by_incidence_factor = function() {

  
  smooth_dx   = smoothers[[1]]$dxrange[2]
  smooth_Iudx = smoothers[[1]]$Iudxrange[2]
  smooth_I    = smoothers[[1]]$Irange[2]
  smooth_S    = smoothers[[1]]$Srange[2]
  smoother = smoothernames[1]

  intvx_dates = c(intvx_early_date+2, intvx_mid_date, intvx_actual_date)
  
  a = 0.3
  mycols = rev(c(rgb(1,0,0,alpha=a), rgb(0,1,0,alpha=a), rgb(0,0,1,alpha=a)))

  incidence_factors = seq(1,2,length.out=50)

  I_hi_end = array(NA,dim=c(length(incidence_factors),length(intvx_dates)))
  I_lo_end = array(NA,dim=c(length(incidence_factors),length(intvx_dates)))

  I_hi_actual_end = rep(NA,length(incidence_factors))
  I_lo_actual_end = rep(NA,length(incidence_factors))

  par(mar=c(4.0,4.5,1,1.0), mfrow=c(3,1), bty="n", cex.lab=1.2, cex.axis=1.2)

  ymax = N_init


  for(j in 1:length(intvx_dates)) {
    for(i in 1:length(incidence_factors)) {

      idate = intvx_dates[j]

      obj = get_indiana_bounds(N_init, idate, idate+scaleup_peak_offset, smooth_dx, 
             smooth_Iudx, smooth_I, smooth_S, smoother, removal_rate_init, incidence_factors[i])  

      I_lo_end[i,j] = obj$I_lo2[ndays]
      I_hi_end[i,j] = obj$I_hi2[ndays]
      I_lo_actual_end[i] = obj$I_lo[ndays] # non-projected incidence 
      I_hi_actual_end[i] = obj$I_hi[ndays]
    }

   plot(0, type="n", ylim=c(0,ymax), xlim=c(1,max(incidence_factors)), 
        ylab=paste("HIV infections by", format(end_date, "%B %d, %Y")),
        xlab="Incidence factor", lwd=2)

    polygon(c(incidence_factors,rev(incidence_factors)), 
            c(I_hi_end[,j], rev(I_lo_end[,j])), col=mycols[j], border=mycols[j])

    polygon(c(incidence_factors,rev(incidence_factors)), 
            c(I_hi_actual_end, rev(I_lo_actual_end)), col=mygray, border=mygray)

    legend(1.04,ymax, 
           c(paste("Infections on", format(end_date, "%B %d, %Y"),
                   "under intervention on", format(intvx_dates[j], "%B %d, %Y")),
             paste("Infections on", format(end_date, "%B %d, %Y"),
                   "under actual circumstances")),
           pch=22,
           cex=text_cex,
           pt.cex=2, 
           col=c(mycols[j], mygray),
           pt.bg=c(mycols[j],mygray), 
           bty="n")
  }


  
  #text(1.05, obj$I_hi[ndays], 
       #paste("Actual infections by", format(end_date, "%B %d, %Y"), ":", 
             #round(obj$I_lo[ndays]), "-", round(obj$I_hi[ndays])),
       #pos=3)



}



###############################################
###############################################

plot_infections_by_rho = function() {

  
  smooth_dx   = smoothers[[1]]$dxrange[2]
  smooth_Iudx = smoothers[[1]]$Iudxrange[2]
  smooth_I    = smoothers[[1]]$Irange[2]
  smooth_S    = smoothers[[1]]$Srange[2]
  smoother = smoothernames[1]

  intvx_dates = c(intvx_early_date+2, intvx_mid_date, intvx_actual_date)
  
  a = 0.3
  mycols = rev(c(rgb(1,0,0,alpha=a), rgb(0,1,0,alpha=a), rgb(0,0,1,alpha=a)))

  rhos = seq(0,0.1,length.out=50)

  I_hi_end = array(NA,dim=c(length(rhos),length(intvx_dates)))
  I_lo_end = array(NA,dim=c(length(rhos),length(intvx_dates)))


  par(mfrow=c(3,1), mar=c(4.0,4.5,1,1.0), bty="n", cex.lab=1.2, cex.axis=1.2)

  ymax = N_init


  for(j in 1:length(intvx_dates)) {
    for(i in 1:length(rhos)) {

      idate = intvx_dates[j]

      obj = get_indiana_bounds(N_init, idate, idate+scaleup_peak_offset, smooth_dx, 
             smooth_Iudx, smooth_I, smooth_S, smoother, rhos[i], calibration_scale_init)  

      I_lo_end[i,j] = obj$I_lo2[ndays]
      I_hi_end[i,j] = obj$I_hi2[ndays]
    }
   plot(0, type="n", ylim=c(0,ymax), xlim=c(0,max(rhos)), 
       ylab=paste("Projected HIV infections by", format(end_date, "%B %d, %Y")),
       xlab="Removal rate", lwd=2)
    polygon(c(rhos,rev(rhos)), c(I_hi_end[,j], rev(I_lo_end[,j])), col=mycols[j], border=mycols[j])
    polygon(c(0,0.1,0.1,0), c(rep(obj$I_lo[ndays],2), rep(obj$I_hi[ndays],2)), col=mygray, border=mygray)
    text(0.05, obj$I_hi[ndays], 
       paste("Actual infections by", format(end_date, "%B %d, %Y"), ":", round(obj$I_lo[ndays]), "-", 
              round(obj$I_hi[ndays]), cex=text_cex),
       pos=3)
    legend(0.04,ymax, c(paste("Intervention on", format(intvx_dates[j],"%B %d, %Y")), 
                        paste("Infections on", format(end_date, "%B %d, %Y"), "under actual circumstances")
                        ),
           pch=22, pt.cex=2, cex=text_cex, col=c(mycols[j],mygray), pt.bg=c(mycols[j],mygray), bty="n")
  }

}

###############################################
###############################################

plot_infections_by_beta_scale = function() {

  
  smooth_dx   = smoothers[[1]]$dxrange[2]
  smooth_Iudx = smoothers[[1]]$Iudxrange[2]
  smooth_I    = smoothers[[1]]$Irange[2]
  smooth_S    = smoothers[[1]]$Srange[2]
  smoother = smoothernames[1]

  intvx_dates = c(intvx_early_date+2, intvx_mid_date, intvx_actual_date)
  
  a = 0.3
  mycols = rev(c(rgb(1,0,0,alpha=a), rgb(0,1,0,alpha=a), rgb(0,0,1,alpha=a)))

  beta_scales = seq(0,1,length.out=50)

  I_hi_end = array(NA,dim=c(length(beta_scales),length(intvx_dates)))
  I_lo_end = array(NA,dim=c(length(beta_scales),length(intvx_dates)))


  par(mfrow=c(3,1), mar=c(4.0,4.5,1,1.0), bty="n", cex.lab=1.2, cex.axis=1.2)

  ymax = N_init


  for(j in 1:length(intvx_dates)) {
    for(i in 1:length(beta_scales)) {

      idate = intvx_dates[j]

      obj = get_indiana_bounds(N_init, idate, idate+scaleup_peak_offset, smooth_dx, 
             smooth_Iudx, smooth_I, smooth_S, smoother, 
             removal_rate=removal_rate_init, 
             calibration_scale=calibration_scale_init,
             beta_scale=beta_scales[i])  

      I_lo_end[i,j] = obj$I_lo2[ndays]
      I_hi_end[i,j] = obj$I_hi2[ndays]
    }
   plot(0, type="n", ylim=c(0,ymax), xlim=c(0,max(beta_scales)), 
       ylab=paste("HIV infections by", format(end_date, "%B %d, %Y")),
       xlab="Transmission rate reduction proportion", lwd=2)
    polygon(c(beta_scales,rev(beta_scales)), c(I_hi_end[,j], rev(I_lo_end[,j])), col=mycols[j], border=mycols[j])
    polygon(c(0,1,1,0), c(rep(obj$I_lo[ndays],2), rep(obj$I_hi[ndays],2)), col=mygray, border=mygray)
    text(0.5, obj$I_hi[ndays], 
       paste("Actual infections by", format(end_date, "%B %d, %Y"), ":", round(obj$I_lo[ndays]), "-", 
              round(obj$I_hi[ndays])),
       pos=3)
    legend(0.00,ymax, c(paste("Intervention on", format(intvx_dates[j],"%B %d, %Y")), 
                        paste("Infections on", format(end_date, "%B %d, %Y"), "under actual circumstances")
                        ),
           pch=22, pt.cex=2, cex=text_cex, col=c(mycols[j],mygray), pt.bg=c(mycols[j],mygray), bty="n")
  }

}



###############################################
###############################################


plot_indiana_bounds = function(N, intvxday, dday, showDates, smooth_dx, 
                               smooth_Iudx, smooth_I, smooth_S, showSusc, smoother,
                               removal_rate, plotType, calibration_scale, beta_scale=1, 
                               print_results=FALSE) {

  obj = get_indiana_bounds(N, intvxday, dday, smooth_dx, smooth_Iudx, smooth_I, 
                           smooth_S, smoother, removal_rate, calibration_scale, beta_scale)  

  if(print_results) {
    cat("Intervention on", format(intvxday, "%B %d, %Y"), "\n")
    cat("  Infections: [", round(obj$I_lo2[ndays]), ",", round(obj$I_hi2[ndays]), "] by ", format(end_date, "%B %d, %Y"), "\n", sep="")
    cat("  actual diagnoses:", dx[ndays], "\n")
    cat("  actual infections: [", obj$I_lo[ndays], ",", obj$I_hi[ndays], "]\n")
    cat("  reduction of at least", 1-round(obj$I_hi2[ndays])/dx[ndays], "in diagnoses\n")
    cat("  reduction of at least", 1-round(obj$I_hi2[ndays])/obj$I_lo[ndays], "in infections\n")
    cat("  infections averted: at least", obj$I_lo[ndays] - obj$I_hi2[ndays], "\n")
    idx = which.max(obj$Iudx_hi)
    cat("  Peak Iudx: [", obj$Iudx_lo[idx], ",", obj$Iudx_hi[idx], "] on", format(dateseq[idx], "%B %d, %Y"), "\n")
    cat("  [", obj$Iudx_lo[emergency_declared_day], ",", obj$Iudx_hi[emergency_declared_day], "] on ", format(emergency_declared_date,"%B %d, %Y"), "\n")
    cat("  beta in [", obj$beta_lo, ",", obj$beta_hi, "]\n")
    cat("  gamma(t) in [", 0, ",", max(obj$dxrate_lo_smooth), "]\n")
  }

  par(bty="n", cex=text_cex, cex.lab=text_cex, cex.axis=text_cex, 
      mar=c(2.7,4.5,1,0.0))

  #layout(matrix(c(1,2,3), nrow=3), heights=c(1,1,3))


  if(plotType!="calibration") {
    layout(matrix(c(1,2), nrow=2), heights=c(1,3))
    plot_dx_rate(obj)
    #plot_transmission_rate(obj)
  }
  plot_epidemic_curves(obj, showDates, showSusc, plotType, calibration_scale)

}

##################################################
##################################################

plot_epidemic_curves = function(obj, showDates, showSusc, plotType, calibration_scale) {

  I_lo = obj$I_lo
  I_hi = obj$I_hi

 
  I_lo_smooth = obj$I_lo_smooth
  I_hi_smooth = obj$I_hi_smooth

  I_lo2 = obj$I_lo2
  I_hi2 = obj$I_hi2

  S_lo = obj$S_lo
  S_hi = obj$S_hi

  S_lo2 = obj$S_lo2
  S_hi2 = obj$S_hi2

  Iudx_lo = obj$Iudx_lo
  Iudx_hi = obj$Iudx_hi

  Iudx_lo2 = obj$Iudx_lo2
  Iudx_hi2 = obj$Iudx_hi2

  Idx_lo2 = obj$Idx_lo2
  Idx_hi2 = obj$Idx_hi2

  cumdx_lo = obj$cumdx_lo
  cumdx_hi = obj$cumdx_hi

  Iudx_lo_smooth = obj$Iudx_lo_smooth
  Iudx_hi_smooth = obj$Iudx_hi_smooth

  R_lo = obj$R_lo
  R_hi = obj$R_hi

  dx_smooth = obj$dx_smooth


  N = obj$N

  if(plotType=="model") {

    if(showSusc) {
      ymax = N
    } else {
      ymax = max(c(I_hi, I_hi2, Iudx_hi, Iudx_hi2, Idx_hi2))
    }

    plot(0, type="n", ylim=c(0,ymax), xlim=c(0,max(ndays)), #xlim=c(dayseq[sim_start_idx],max(ndays)), 
         ylab="People", xlab="", lwd=2, axes=FALSE)


    axis(1,at=monthdayseq, lab=monthlabseq)
    axis(2)

    if(showSusc) {
      legend(0, 0.9*ymax,
             c("Susceptible HIV- (actual)", 
               "Undiagnosed HIV+ (actual)",
               paste("Total HIV+ (actual ", calibration_scale*100,"%)", sep=""), 
               "Cumulative HIV Diagnoses (actual)"), 
             border=c(mydarkgreen,mydarkred,mydarkgray,NA),
             lty=c(0, 0, 0,1),
             pch=c(22,22,22,NA),
             pt.cex=c(3,3,3,NA),
             lwd=c(1,1,1,2),
             col=c(mydarkgreen, mydarkred,mydarkgray,"black"),
             pt.bg=c(mydarkgreen, mydarkred,mydarkgray,NA),
             bg="white", bty="n", cex=text_cex)

      legend(0, 0.7*ymax,
             c("Susceptible HIV- (projected)", 
               "Undiagnosed HIV+ (projected)", 
               "Diagnosed HIV+ infectious (projected)",
               "Diagnosed HIV+ removed (projected)"), 
             col=c(mygreen, myred, mypurple, myblue), 
             border=c(mygreen, myred, mypurple, myblue),
             pt.bg=c(mygreen, myred, mypurple, myblue),
             lty=0,
             pch=22,
             pt.cex=c(3,3,3,3),
             bg="white", bty="n", cex=text_cex)

    } else {

       legend(0, 0.9*ymax,
             c("Undiagnosed HIV+ (actual)", 
               paste("Total HIV+ (actual ", calibration_scale*100,"%)", sep=""), 
               "Cumulative HIV Diagnoses (actual)"), 
             border=c(mydarkred,mydarkgray,NA),
             lty=c(0,0,1),
             pch=c(22,22,NA),
             pt.cex=c(3),
             lwd=c(1,1,2),
             col=c(mydarkred,mydarkgray,"black"),
             pt.bg=c(mydarkred,mydarkgray,NA),
             bg="white", bty="n", cex=text_cex)

      legend(0, 0.7*ymax,
             c("Undiagnosed HIV+ (projected)", 
               "Diagnosed HIV+ infectious (projected)",
               "Diagnosed HIV+ removed (projected)"), 
             col=c(myred, mypurple, myblue), 
             border=c(myred, mypurple, myblue),
             pt.bg=c(myred, mypurple, myblue),
             lty=0,
             pch=22,
             pt.cex=c(3,3),
             bg="white", bty="n", cex=text_cex)
   }

   abline(h=N, lty="dashed")

   # actual data: 
   polygon(c(dayseq,rev(dayseq)), c(Iudx_lo,rev(Iudx_hi)), col=mydarkred, border=mydarkred)
   if(showSusc) polygon(c(dayseq,rev(dayseq)), c(S_lo,rev(S_hi)), col=mydarkgreen, border=mydarkgreen)
   polygon(c(dayseq,rev(dayseq)), c(I_lo,rev(I_hi)), col=mydarkgray, border=mydarkgray)
   lines(dayseq, dx, lwd=2, col="black")

   # projected
   if(showSusc) polygon(c(dayseq,rev(dayseq)), c(S_lo2,rev(S_hi2)), col=mygreen, border=mygreen)
   #polygon(c(dayseq,rev(dayseq)), c(I_lo2,rev(I_hi2)), col=mygray, border=mygray)
   polygon(c(dayseq,rev(dayseq)), c(Iudx_lo2,rev(Iudx_hi2)), col=myred, border=myred)
   polygon(c(dayseq,rev(dayseq)), c(Idx_lo2,rev(Idx_hi2)), col=mypurple, border=mypurple)

   polygon(c(dayseq, rev(dayseq)), c(R_lo, rev(R_hi)), col=myblue, border=myblue)


  } else if(plotType=="raw") { 

    ymax = max(I_hi, I_hi2, Iudx_hi, Iudx_hi2, Idx_hi2)

     plot(0, type="n", ylim=c(0,ymax), xlim=c(0,max(ndays)), #xlim=c(dayseq[sim_start_idx],max(ndays)), 
       ylab="People", xlab="", lwd=2, axes=FALSE)


    axis(1,at=monthdayseq, lab=monthlabseq)
    axis(2)

    legend(0, 0.9*ymax,
           c("Undiagnosed HIV+ (raw data)", 
             "Undiagnosed HIV+ (model projection)", 
             "Cumulative HIV incidence (raw data)",
             "Cumulative HIV incidence (model projection)"),
           border=c(mydarkred, myred,mydarkgray,mygray),
           pch=22,
           pt.cex=3,
           col=c(mydarkred, myred,mydarkgray,mygray),
           pt.bg=c(mydarkred, myred,mydarkgray,mygray),
           bg="white", bty="n", cex=text_cex)



    # actual data: 
    polygon(c(dayseq,rev(dayseq)), c(Iudx_lo,rev(Iudx_hi)), col=mydarkred, border=mydarkred)
    polygon(c(dayseq,rev(dayseq)), c(I_lo,rev(I_hi)), col=mydarkgray, border=mydarkgray)
    #lines(dayseq, dx, lwd=2, col=mydarkpurple)


    # projected
    polygon(c(dayseq,rev(dayseq)), c(I_lo2,rev(I_hi2)), col=mygray, border=mygray)
    polygon(c(dayseq,rev(dayseq)), c(Iudx_lo2,rev(Iudx_hi2)), col=myred, border=myred)
    #polygon(c(dayseq,rev(dayseq)), c(cumdx_lo,rev(cumdx_hi)), col=mypurple, border=mypurple)

    points(ndays, I_lo2[ndays], pch=3, col=mydarkgray2)
    points(ndays, I_hi2[ndays], pch=3, col=mydarkgray2)
    text(ndays, I_lo2[ndays], round(I_lo2[ndays]), pos=4, col=mydarkgray2, cex=text_cex)
    text(ndays, I_hi2[ndays], round(I_hi2[ndays]), pos=4, col=mydarkgray2, cex=text_cex)

  } else if(plotType=="calibration") {

    par(mfrow=c(3,1))

    ymax = max(I_hi, I_hi2, Iudx_hi, Iudx_hi2, Idx_hi2)

    # Fig 3A
    plot(0, type="n", ylim=c(0,ymax), xlim=c(first_dx_day,max(ndays)), 
       ylab="People", xlab="", lwd=2, axes=FALSE)
    axis(1,at=detail_monthdayseq, lab=detail_monthlabseq)
    axis(2)
    polygon(c(dayseq,rev(dayseq)), 
            c(I_lo,rev(I_hi)), 
            col=mydarkgray, border=mydarkgray)
    polygon(c(dayseq,rev(dayseq)), 
            c(I_lo2,rev(I_hi2)), 
            col=mygray, border=mygray)
    legend(first_dx_day, 0.9*ymax,
           c("Cumulative HIV incidence (raw data)", 
             "Cumulative HIV incidence (model projection)"), 
           pch=22,
           pt.cex=3,
           col=c(mydarkgray, mygray),
           pt.bg=c(mydarkgray, mygray, NA),
           bg="white", bty="n", cex=text_cex)


    # fig 3B
    plot(0, type="n", ylim=c(0,ymax), xlim=c(first_dx_day,max(ndays)), 
       ylab="People", xlab="", lwd=2, axes=FALSE)
    axis(1,at=detail_monthdayseq, lab=detail_monthlabseq)
    axis(2)
    lines(dayseq, dx, lwd=2, col=mydarkpurple)
    polygon(c(dayseq,rev(dayseq)), c(cumdx_lo,rev(cumdx_hi)), col=mypurple, border=mypurple)
    legend(first_dx_day, 0.9*ymax,
           c("Cumulative HIV diagnoses (raw data)", 
             "Cumulative HIV diagnoses (model projection)"), 
           pch=c(NA,22),
           pt.cex=c(NA,3),
           lwd=c(2,NA),
           col=c(mydarkpurple, mypurple),
           pt.bg=c(NA, mypurple),
           bg="white", bty="n", cex=text_cex)

    # fig 3C
    plot(0, type="n", ylim=c(0,150), xlim=c(first_dx_day,max(ndays)), 
       ylab="People", xlab="", lwd=2, axes=FALSE)
    axis(1,at=detail_monthdayseq, lab=detail_monthlabseq)
    axis(2)
    polygon(c(dayseq,rev(dayseq)), c(Iudx_lo,rev(Iudx_hi)), col=mydarkred, border=mydarkred)
    polygon(c(dayseq,rev(dayseq)), c(Iudx_lo2,rev(Iudx_hi2)), col=myred, border=myred)

    legend(sep_started_day, 150,
           c("Undiagnosed HIV+ (raw data)", 
             "Undiagnosed HIV+ (model projection)"), 
           pch=22,
           pt.cex=3,
           col=c(mydarkred, myred),
           pt.bg=c(mydarkred, myred, NA),
           bg="white", bty="n", cex=text_cex)



  }


  abline(h=N, lty="dashed")


  if(showDates) {
    arrows(first_dx_day, ymax*0.5,
           first_dx_day, I_hi[dayseq==first_dx_day], length=0.05)
    text(first_dx_day, ymax*0.5,
         "First Diagnosis", pos=2, cex=text_cex, offset=0.3)

    arrows(investigation_begin_day, ymax*0.6,
           investigation_begin_day, I_hi[dayseq==investigation_begin_day], length=0.05)
    text(investigation_begin_day, ymax*0.6,
         "Investigation Begins", pos=2, cex=text_cex, offset=0.3)

    arrows(emergency_declared_day, ymax*0.7,
           emergency_declared_day, I_hi[dayseq==emergency_declared_day], length=0.05)
    text(emergency_declared_day, ymax*0.7,
         "Emergency Declared", pos=2, cex=text_cex, offset=0.3)

    arrows(clinic_opened_day, ymax*0.8,
           clinic_opened_day, I_hi[dayseq==clinic_opened_day], length=0.05)
    text(clinic_opened_day, ymax*0.8,
         "Local HIV clinic opens", pos=2, cex=text_cex, offset=0.3)

    arrows(sep_started_day, ymax*0.9,
           sep_started_day, I_hi[dayseq==sep_started_day], length=0.05)
    text(sep_started_day, ymax*0.9,
         "SEP Begins", pos=2, cex=text_cex, offset=0.3)
  }


  points(ndays, cumcases[ndays], pch=16)
  text(ndays, cumcases[ndays], cumcases[ndays], pos=4, cex=text_cex)



  res = list(Iend_lo=I_lo[ndays], Iend_hi=I_hi[ndays])
  return(res)




}

#############################################


generate_publication_figures_and_results = function() {
  
  smooth_dx   = smoothers[[1]]$dxrange[2]
  smooth_Iudx = smoothers[[1]]$Iudxrange[2]
  smooth_I    = smoothers[[1]]$Irange[2]
  smooth_S    = smoothers[[1]]$Srange[2]
  smoother = smoothernames[1]
  showDates = FALSE
  showSusc = FALSE

  w = 16

  #################
  # main text figures

  pdf("fig1.pdf", width=w, height=7, bg="white")
  plot_methods_illustration1()
  dev.off()

  pdf("fig2.pdf", width=w, height=7, bg="white")
  plot_methods_illustration2()
  dev.off()

  pdf("fig3.pdf", width=w/1.8,height=9, bg="white")
  plot_indiana_bounds(N_init, intvx_actual_date, end_date, showDates, smooth_dx, smooth_Iudx, 
                      smooth_I, smooth_S, showSusc, 
                      smoother, removal_rate_init, "calibration", calibration_scale_init, 
                      print_results=TRUE)
  dev.off()

  pdf("fig4.pdf", width=w,height=9, bg="white")
  d1 = intvx_mid_date
  d2 = d1+scaleup_peak_offset
  plot_indiana_bounds(N_init, d1, d2, showDates, smooth_dx, smooth_Iudx, 
                      smooth_I, smooth_S, showSusc, smoother, removal_rate_init, 
                      "raw", calibration_scale_init, print_results=TRUE)
  dev.off()

  pdf("fig5.pdf", width=w,height=7, bg="white")
  plot_infections_by_intvx_date()
  dev.off()


  ##############
  # supplementary figures:

  pdf("figs1.pdf", width=w,height=9, bg="white")
  d1 = intvx_early_date
  d2 = d1+scaleup_peak_offset
  plot_indiana_bounds(N_init, d1, d2, showDates, smooth_dx, 
                      smooth_Iudx, smooth_I, smooth_S, showSusc, 
                      smoother, removal_rate_init, "raw", calibration_scale_init, 
                      print_results=TRUE)
  dev.off()
  

  pdf("figs2.pdf", width=w,height=12, bg="white")
  plot_infections_by_N()
  dev.off()

  pdf("figs3.pdf", width=w,height=12, bg="white")
  plot_infections_by_rho()
  dev.off()

  pdf("figs4.pdf", width=w,height=12, bg="white")
  plot_infections_by_beta_scale()
  dev.off()

  pdf("figs5.pdf", width=w,height=12, bg="white")
  plot_infections_by_incidence_factor()
  dev.off()

  # convert to png
  system("for i in *.pdf; do sips -s format png $i --out $i.png; done")

}


