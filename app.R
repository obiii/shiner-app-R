

ui <- dashboardPage(
  dashboardHeader(title="Viser"),
  dashboardSidebar(),
  dashboardBody()
)






server <- function(input, output) { }
shinyApp(ui, server)