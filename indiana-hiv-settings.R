



##########################
# Initial settings


removal_rate_low = 0.0038
removal_rate_mid = 0.024
removal_rate_high = 0.05

removal_rate_min = 0
removal_rate_max = 0.1
removal_rate_init = removal_rate_mid

calibration_scale_min = 1
calibration_scale_max = 2
calibration_scale_init = 1.0

N_min = 215
N_max = 4000
N_init = 536

scaleup_peak_offset = 150

smoothers = list(list(name="spline (default)",
                      f=function(x,y,v)smooth.spline(y ~ x, df=v)$y,
                      step=1,
                      dxrange=c(5,16,50),
                      Iudxrange=c(2,25,100),
                      Irange=c(2,50,100),
                      Srange=c(2,50,100)),
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

make_transparent = function(col,a=1) {
  obj = col2rgb(col)
  rgb(obj[1]/255, obj[2]/255, obj[3]/255, alpha=a)
}

text_cex = 1.3

Reds   = brewer.pal(n=9, "Reds")
Greens = brewer.pal(n=9, "Greens")
Blues  = brewer.pal(n=9, "Blues")
Greys  = brewer.pal(n=9, "Greys")
Oranges = brewer.pal(n=9, "Oranges")
Purples = brewer.pal(n=9, "Purples")

myred   = make_transparent(Reds[6],   0.5)
mygreen = make_transparent(Greens[7], 0.5)
myblue  = make_transparent(Blues[6],  0.5)
mygray  = make_transparent(Greys[6],  0.5)
myorange = make_transparent(Oranges[7], 0.5)
mypurple = make_transparent(Purples[7], 0.5)


mydarkred   = make_transparent(Reds[6],   0.3)
mydarkgreen = make_transparent(Greens[7], 0.3)
mydarkblue  = make_transparent(Blues[6],  0.8)
mydarkgray  = make_transparent(Greys[6],  0.3)
mydarkorange = make_transparent(Oranges[6], 0.2)
mydarkpurple = make_transparent(Purples[7], 0.3)

mydarkgray2  = make_transparent(Greys[7],  1)

