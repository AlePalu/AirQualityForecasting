library(forecast)
library(data.table)
library(ggplot2)
library(lubridate)
library(forcats)
library(chron)
library(fasttime)
library(shiny)
library(extraoperators)

wadata=fread("data/tsData.csv")
wadata$pot_id=as.factor(wadata$pot_id)
wadata$created_at = fastPOSIXct(wadata$created_at)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("daterange",label="time interval:",start="2020-11-01"),
      radioButtons(inputId="selectedvar",label="choose variable:",choices=colnames(wadata),selected = "pm1SPS"),
      actionButton("updatebutton", "update")
    ),
    mainPanel(
      fluidRow(
      plotOutput("forecastplot")
      ),
      fluidRow(
        verbatimTextOutput("modelsummary")
      )
    )
  )
)


server <- function(input,output) {
  
  observeEvent(input$updatebutton,{
    drange=input$daterange
    columnselected=input$selectedvar
    selpot=wadata[pot_id=="1038" & created_at %gele% c(drange[1],drange[2]),get(columnselected)]
    wamodel=auto.arima(selpot)

  
  output$forecastplot <- renderPlot({
    plot(forecast(wamodel,h=12))
  })
  
  output$modelsummary <- renderPrint({
    summary(wamodel)
    columnselected
  })
  
  })
  
}

shinyApp(ui, server)

