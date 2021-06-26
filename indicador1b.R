library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(lubridate)

meta1 <- readr::read_rds("data/indicador1bBrasil.rds")

ui <- dashboardPage(
  dashboardHeader(title = "PNE-Municípios"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Meta 1", tabName = "meta1", icon = icon("fas fa-chevron-right")),
      menuItem("Meta 2", tabName = "meta2", icon = icon("fas fa-chevron-right"))
    )
  ),
  dashboardBody(
    tabItems(
      #Pagina 1
      tabItem(
        tabName = "meta1",
        fluidRow(
          column(
            width = 12,
            h1("Meta 1 - Plano Nacional de Educação")
          )
        ),
        hr(style = "border-top: 1px solid black;"), # tag horizontal row style para mudar o css
        br(),
        fluidRow(
          box(
            width = 12,
            fluidRow(
              column(
                width = 6,
                selectInput(
                  "uf", "Selecione um estado",
                  choices = meta1 |> pull(nome_uf) |> unique() |> sort(),
                  selected = "Amazonas"
                )
              ),

              column(
                width = 6,
                selectInput(
                  "municipio",
                  "Selecione o município",
                  choices = "",
                  selected = "Carregando..."
                )
              ),
              br(),

            )
          ),

          box(
            width = 4,
            fluidRow(
              box(
                width = 12,
                title = "Indicador 1B",
                solidHeader = TRUE, #cabeçalho colorido
                status = "primary",  # para mudar a cor igual do anterior
                shinycssloaders::withSpinner(
                  plotOutput("graf_indicador1b")

                )
              )
            )
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  meta1 <- readr::read_rds("data/indicador1bBrasil.rds")

  # Pagina 1

  observe({
    municipios <- meta1 |>
      filter(nome_uf == input$uf) |>
      pull("Municipio") |>
      unique() |> sort()

    updateSelectInput(
      session,
      "municipio",
      choices = municipios
    )
  })

  output$graf_indicador1b <- renderPlot({

    meta1 |> filter(nome_uf == input$uf,
                    Municipio == input$municipio) |>
      # req(input$municipio)
      mutate(
        I1B = round(indicador1b, 2)
      )  |>
      ggplot(aes(x = Ano, y = I1B, color=I1B)) +
      geom_line() +
      geom_point(aes(x = Ano, y = I1B)) +
      geom_label(aes(label = I1B ))

  }

  )

}

shinyApp(ui, server)

















