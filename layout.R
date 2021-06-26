library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  header = dashboardHeader(
    title = "NUPEC/PNE"
  ),

  sidebar = dashboardSidebar(
    sidebarMenu(
        menuItem("Informações do Projeto", tabName = "proj"),
        menuItem("Municípios Analisados", tabName = "muni")
          )
  ),

  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "proj",
        h1("Informações Gerais do Projeto")
        )
      ),

      tabItem(
        tabName = "muni",
        fluidRow(
          column(
            width = 12,
            selectInput("variavel", label = "Selecione o município",
                           choices = names(mtcars)))
        )
    )
  ),
  title = "NUPEC/PNE"
)

server <- function(input, output, session) {

}

shinyApp(ui, server)


















