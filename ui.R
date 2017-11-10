
library(shiny)

source("indiana-hiv-load.R")
source("indiana-hiv-util.R")

# Define UI for application that plots random distributions 


ui = fluidPage(
  titlePanel("Dynamics of the HIV Outbreak and response in Scott County, Indiana 2011-2015"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(type="tabs", 
        tabPanel("Main",
          includeMarkdown("content/instructions.md"),
          hr(),
          radioButtons("scenario", "Intervention scenarios",
                       c("Actual"="actual",
                         "Intermediate"="mid",
                         "Early"="early"),
                       inline=TRUE),
          sliderInput(inputId="intvxday", 
            label="Intervention scale-up dates", 
            min = zerodate+2,
            max = enddate-1,
            value = c(begindate,enddate),
            dragRange=TRUE),
          hr(),
          h4("Display"),
          checkboxInput("showDates", "Show actual response dates", value=FALSE),
          checkboxInput("showSusc", "Show susceptible population", value=FALSE),
          hr(),
          h4("Results"),
          htmlOutput("results")
        ),
        tabPanel("Settings",
          hr(),
          checkboxInput("constFOI", "Constant FOI", value=FALSE),
          sliderInput(inputId="N", "Risk population size", min = 215, max = 4000, value = 536),
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
          includeMarkdown("content/about.md")
        )
      )
      ),
  mainPanel(
    verticalLayout(
      plotOutput("dxPlot", height="200px"),
      plotOutput("transmissionRatePlot", height="200px"),
      plotOutput("epidemicPlot", height="400px")
      )
    )
  ),
  hr(),
  includeMarkdown("content/footer.md")
)



