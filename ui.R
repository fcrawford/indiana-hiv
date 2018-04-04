
library(shiny)
library(markdown)

source("indiana-hiv-util.R")

ui = fluidPage(
  # title: 
  titlePanel("Dynamics of the HIV outbreak and response in Scott County, Indiana 2011-2015"),
  # sidebar: 
  sidebarLayout(
    sidebarPanel(
      tabsetPanel( #type="tabs", 
        tabPanel("Main", fluid=TRUE,
          includeMarkdown("content/instructions.md"),
          #hr(),
          radioButtons("scenario", "Intervention date scenarios",
                       c("Early"="early",
                         "Intermediate"="mid",
                         "Actual"="actual"),
                       inline=TRUE,
                       selected="actual"),
          sliderInput(inputId="intvxday", 
            label=NULL, #"Intervention scale-up dates", 
            min=intvx_early_date,
            max=end_date,
            value=c(first_dx_date,end_date),
            dragRange=TRUE),
          radioButtons("plotType", "Plot type", 
                       c("Raw Data"="raw",
                         "Model Compartments"="model"),
                       inline=TRUE,
                       selected="raw"),
          checkboxInput("showDates", "Show actual response dates", value=FALSE),
          checkboxInput("showSusc", "Show susceptible population", value=FALSE) #,
        ),
        tabPanel("Parameters", 
          includeMarkdown("content/parameters.md"),
          sliderInput(inputId="N", "Risk population size", min=N_min, max=N_max, value=N_init),
          radioButtons("removal_scenario", 
                       "Removal scenarios",
                       c("Low"="low",
                         "Moderate"="moderate",
                         "High"="high"),
                       inline=TRUE,
                       selected="moderate"),
          sliderInput(inputId="removal_rate", 
                      label=NULL, #"Removal rate (per diagnosed person per day)", 
                      min=removal_rate_min, max=removal_rate_max, value=removal_rate_init),
          includeMarkdown("content/incidence.md"),
          sliderInput(inputId="calibration_scale", "Incidence uncertainty factor", 
                      min=calibration_scale_min, max=calibration_scale_max, value=calibration_scale_init, step=0.01)),
        tabPanel("Smoothers",
          includeMarkdown("content/smoothers.md"),
          actionButton("reset", "Reset smoothers"),
          selectInput("smoother", "Smoother", choices=smoothernames),
          sliderInput("smooth_dx", "Diagnosis smoother", step=smoothers[[1]]$step,
                      min=smoothers[[1]]$dxrange[1], max=smoothers[[1]]$dxrange[3], value=smoothers[[1]]$dxrange[2]),
          sliderInput("smooth_Iudx", "Undiagnosed HIV infections smoother", step=smoothers[[1]]$step,
                      min=smoothers[[1]]$Iudxrange[1], max=smoothers[[1]]$Iudxrange[3], value=smoothers[[1]]$Iudxrange[2]),
          sliderInput("smooth_I", "Infection smoother", step=smoothers[[1]]$step,
                      min=smoothers[[1]]$Irange[1], max=smoothers[[1]]$Irange[3], value=smoothers[[1]]$Irange[2]),
          sliderInput("smooth_S", "Suscepible smoother", step=smoothers[[1]]$step,
                      min=smoothers[[1]]$Srange[1], max=smoothers[[1]]$Srange[3], value=smoothers[[1]]$Srange[2])
        ),
        tabPanel("About", 
          includeMarkdown("content/about.md"),
          h3("Data"),
          verticalLayout(
            downloadLink("downloadDiagnoses", "Diagnoses by week"), 
            downloadLink("downloadIncidence", "Estimated cumulative incidence") 
          )
        ) #,
        #tabPanel("Help",
          #includeMarkdown("content/instructions.md")
        #)
      )
    ),
  # main panel
  mainPanel(
      plotOutput("epidemicPlot", height="700px")
    )
  ),
  hr(),
  includeMarkdown("content/footer.md")
)




