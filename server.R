
library(shiny)

source("indiana-hiv-util.R")

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {

  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 

  observe({ # observe the intvxday variable; this is to correct the width if an impossible combination is entered. 
    if(input$intvxday[1]>begindate) {
      updateSliderInput(session, "intvxday", value=c(begindate, input$intvxday[2]))
    }
  })

  observe({
    d1 = NA
    d2 = NA
    if(input$scenario == "actual") {
      d1 = begindate
      d2 = enddate
    } else if(input$scenario == "mid") {
      d1 = mdy("01/01/2013")
      d2 = d2 = d1+140
    } else if(input$scenario == "early") {
      d1 = zerodate+2
      d2 = d1+140
    } else {
      error("invalid choice")
    }
    updateSliderInput(session, "intvxday", value=c(d1, d2))
  })


  observe({ # observe the smoother selection 
    input$reset # leave this alone.  Makes the smoothers reset
    updateSliderInput(session, "smooth_dx",  step=smoothers[[which(smoothernames==input$smoother)]]$step,
                      value=smoothers[[which(smoothernames==input$smoother)]]$dxrange[2], 
                      min=smoothers[[which(smoothernames==input$smoother)]]$dxrange[1], 
                      max=smoothers[[which(smoothernames==input$smoother)]]$dxrange[3]) 

    updateSliderInput(session, "smooth_Iudx",  step=smoothers[[which(smoothernames==input$smoother)]]$step,
                      value=smoothers[[which(smoothernames==input$smoother)]]$Iudxrange[2], 
                      min=smoothers[[which(smoothernames==input$smoother)]]$Iudxrange[1], 
                      max=smoothers[[which(smoothernames==input$smoother)]]$Iudxrange[3]) 

    updateSliderInput(session, "smooth_I",  step=smoothers[[which(smoothernames==input$smoother)]]$step,
                      value=smoothers[[which(smoothernames==input$smoother)]]$Irange[2], 
                      min=smoothers[[which(smoothernames==input$smoother)]]$Irange[1], 
                      max=smoothers[[which(smoothernames==input$smoother)]]$Irange[3]) 


    updateSliderInput(session, "smooth_S", step=smoothers[[which(smoothernames==input$smoother)]]$step,
                      value=smoothers[[which(smoothernames==input$smoother)]]$Srange[2], 
                      min=smoothers[[which(smoothernames==input$smoother)]]$Srange[1], 
                      max=smoothers[[which(smoothernames==input$smoother)]]$Srange[3]) 
  })

  output$dxPlot <- renderPlot({
    if(input$intvxday[1]<=begindate)  {
      plot_dx_rate(input$intvxday[1], input$intvxday[2], input$smooth_dx, input$smooth_Iudx, input$smoother)
    }
  })
  
  output$transmissionRatePlot <- renderPlot({
    plot_transmission_rate(input$N, input$smooth_dx, input$smooth_Iudx, input$smooth_I, input$smooth_S, input$constFOI, input$smoother)
  })
  

  output$epidemicPlot <- renderPlot({

    if(input$intvxday[1]<=begindate)  {
      plot_indiana_bounds(input$N, input$intvxday[1], input$intvxday[2], input$showDates, input$smooth_dx, 
                          input$smooth_Iudx, input$smooth_I, input$smooth_S, input$constFOI, input$showSusc, input$smoother)
    }
  })

  output$results <- renderText({
    get_indiana_results(input$N, input$intvxday[1], input$intvxday[2], input$smooth_dx, input$smooth_Iudx, input$smooth_I, input$smooth_S, 
                       input$constFOI, input$smoother)


  })



})
