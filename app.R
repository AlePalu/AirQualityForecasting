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

ggplot(wadata[(pot_id==1038) & created_at>"2020-07-01",],aes(x=created_at,y=pm1SPS)) +
  geom_line() +
  scale_x_datetime(date_breaks = "10 days") +
  theme(axis.text.x = element_text(angle = 90)) 

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
    plot(wamodel$x,col="red")
    lines(fitted(wamodel),col="blue")
    plot(forecast(wamodel,h=12))
    # plot(forecast(wamodel,h=12))
  })
  
  output$modelsummary <- renderPrint({
    summary(wamodel)
    columnselected
  })
  
  })
  
}

shinyApp(ui, server)

