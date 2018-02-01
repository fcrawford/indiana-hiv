
library(shiny)

source("indiana-hiv-util.R")

shinyServer(function(input, output, session) {

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
      d2 = d1+140
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

  output$epidemicPlot <- renderPlot({
    if(input$intvxday[1]<=begindate)  {
      plot_indiana_bounds(input$N, input$intvxday[1], input$intvxday[2], input$showDates, input$smooth_dx, 
                          input$smooth_Iudx, input$smooth_I, input$smooth_S, input$constFOI, input$showSusc, input$smoother)
    }
  })

  output$results <- renderText({
    get_indiana_results_text(input$N, input$intvxday[1], input$intvxday[2], input$smooth_dx, input$smooth_Iudx, input$smooth_I, input$smooth_S, 
                             input$constFOI, input$smoother)
  })

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
      file.copy("data/simplistic_simResults.csv", file)
    }
  )

})

