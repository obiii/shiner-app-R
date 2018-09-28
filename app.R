library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(ggplot2)
library(devtools)
library(jsonlite)
library(tidyr)

devtools::install_github("obiii/lab5", subdir="viser",  quiet = TRUE)
library(viser)


ui <- dashboardPage(
  dashboardHeader(title="Viser"),
  
  
  dashboardSidebar(
    textOutput("a"),
    # crime picker
    selectInput("crimes", "Choose Crime Type:",
                c("ALL" = 1,
                  "JUVENILE" = 2,
                  "DAMAGE"=3,
                  "SUDDEN"=4,
                  "LARCENY"=5,
                  "BURGLARY"=6,
                  "SEX"=7,
                  "MISSING"=8,
                  "DRIVING"=9,
                  "DRUGS"=10,
                  "LIQUOR"=11,
                  "MENTAL"=12,
                  "LOST"=13,
                  "FORGERY"=14,
                  "RECOVERED"=15,
                  "PUBLIC"=16,
                  "ASSAULT"=17,
                  "ROBBERY"=18,
                  "TRESPASSING"=19,
                  "FAMILY"=20,
                  "POLICE"=21,
                  "AUTO"=22,
                  "LITTERING"=23,
                  "OBSTRUCT"=24,
                  "SUICIDE"=25,
                  "FRAUD"=26,
                  "IDENTITY"=27,
                  "RAPE"=28,
                  "ATTEMPTED"=29,
                  "OVERDOSE"=30,
                  "WEAPON"=31,
                  "FUGITIVE"=32,
                  "ABDUCT"=33,
                  "ESCAPE"=34,
                  "EXTORT"=35,
                  "COUNTERFEITING"=36,
                  "TRAFFIC"=37,
                  "SOLICITATION"=38,
                  "STOLEN"=39,
                  "COMM"=40,
                  "UNAUTHORIZED"=41,
                  "EXTORTION"=42,
                  "ARSON"=43,
                  "KIDNAP"=44,
                  "LOITERING"=45,
                  "FIRE"=46,
                  "CARRYING"=47,
                  "THREAT"=48,
                  "PROPERTY"=49,
                  "EMBEZZLE"=50,
                  "INCOME"=51,
                  "HIT"=52,
                  "OBSCENE"=53,
                  "DRUNKENNESS"=54
                  )),
    #tableOutput("data")
    
    # date picker
    dateRangeInput('dateRange',
                   label = 'Date range: ',
                   start = Sys.Date() - 2, end = Sys.Date() + 2
    ),
    
    # check box
    switchInput(inputId = "id", value = TRUE)
    
  ),
  
  dashboardBody(
    # ---------------------------- Values box KPIs
    fluidRow(
      # A static valueBox
      valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
      
      # Dynamic valueBoxes
      valueBoxOutput("progressBox"),
      
      valueBoxOutput("approvalBox")
    ),
    
    
    
    
   #-------------------------------------------- Main map box
   fluidRow(
     box(leafletOutput("map", height = 520), width = 12)
   ),
   
   # --------------------------------------- Graphs
   sidebarLayout(position = "left",
                 sidebarPanel("sidebar panel",
                              checkboxInput("do2", "Make 2 plots", value = T)
                 ),
                 mainPanel("main panel",
                           fluidRow(
                             splitLayout(style = "border: 1px solid silver:", cellWidths = c(300,200,100), 
                                         plotOutput("plotgraph1"), 
                                         plotOutput("plotgraph2"),
                                         plotOutput("plotgraph3")
                             )
                           )
                 )
   )
   
  )
)


server <- function(input, output) {
  
  # loading data
  data <- reactive({
    getLimitedData(limit = 10)
  })
  
  # ---dropdown choosing crime
  output$crime <- renderText({
    cat(input$crimes)
  })
  
  
  
  
  # --------------------- map
 
  
  output$map <- renderLeaflet({
    df <- data()
    m <- leaflet(data = df) %>%
      addTiles() %>%
      addMarkers(lng = ~longitude,
                 lat = ~latitude,
                 popup = paste("Offense", df$crimeType, "<br>")
                 )
    m
  })
  
  
  # --------- graphs
  pt1 <- qplot(rnorm(500),fill=I("red"),binwidth=0.2,title="plotgraph1")
  pt3 <- qplot(rnorm(600),fill=I("blue"),binwidth=0.2,title="plotgraph3")
  pt2 <- reactive({
    input$do2
    if (input$do2){
      return(qplot(rnorm(500),fill=I("blue"),binwidth=0.2,title="plotgraph2"))
    } else {
      return(NULL)
    }
  })
  output$plotgraph1 = renderPlot({pt1})
  output$plotgraph2 = renderPlot({pt2()})
  output$plotgraph3 = renderPlot({pt3}
  )
  
  output$progressBox <- renderValueBox({
    valueBox(
      paste0(25 + input$count, "%"), "Progress", icon = icon("list"),
      color = "purple"
    )
  })
  
  output$approvalBox <- renderValueBox({
    valueBox(
      "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
}
shinyApp(ui, server)