
library(shiny)

source("indiana-hiv-util.R")

shinyServer(function(input, output, session) {

  observe({ # observe the intvxday variable; this is to correct the width if an impossible combination is entered. 
    if(input$intvxday[1]>first_dx_date) {
      updateSliderInput(session, "intvxday", value=c(first_dx_date, input$intvxday[2]))
    }
  })

  observe({
    d1 = NA
    d2 = NA
    if(input$scenario == "actual") {
      d1 = intvx_actual_date
      d2 = end_date
    } else if(input$scenario == "mid") {
      d1 = intvx_mid_date
      d2 = d1+scaleup_peak_offset
    } else if(input$scenario == "early") {
      d1 = intvx_early_date
      d2 = d1+scaleup_peak_offset
    } else {
      error("invalid choice")
    }
    updateSliderInput(session, "intvxday", value=c(d1, d2))
  })

  observe({ 
    if(input$removal_scenario == "low") {
      v = removal_rate_low
    } else if(input$removal_scenario == "moderate") {
      v = removal_rate_mid
    } else if(input$removal_scenario == "high") {
      v = removal_rate_high
    } else {
      error("invalid choice")
    }
    updateSliderInput(session, "removal_rate", value=v)
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

  output$epidemicPlot <- renderPlot({
    if(input$intvxday[1]<=first_dx_date)  {
      plot_indiana_bounds(input$N, input$intvxday[1], input$intvxday[2], input$showDates, input$smooth_dx, 
                          input$smooth_Iudx, input$smooth_I, input$smooth_S, input$showSusc, input$smoother, 
                          input$removal_rate, input$plotType, input$calibration_scale)
    }
  })

  #output$results <- renderText({
    #get_indiana_results_text(input$N, input$intvxday[1], input$intvxday[2], input$smooth_dx, input$smooth_Iudx, input$smooth_I, input$smooth_S, input$smoother, input$removal_rate)
  #})

  output$downloadDiagnoses <- downloadHandler(
    filename = function() {
      "scott_county_cases_by_week.csv"
    },
    content = function(file) {
      file.copy("data/scott_county_cases_by_week.csv", file)
    }
  )

  output$downloadIncidence <- downloadHandler(
    filename = function() {
      "incidence.csv"
    },
    content = function(file) {
      file.copy("data/extracted_infection_curves.csv", file)
    }
  )
})

