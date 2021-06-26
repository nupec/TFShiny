library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(lubridate)

meta1 <- readr::read_rds("data/indicador1bBrasil.rds")

ui <- dashboardPage(
  dashboardHeader(title = HTML("PNE-Municípios"),
                  disable = FALSE,
                  titleWidth  = 550),
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
            h1("Meta 1: universalizar, até 2016, a educação infantil na pré-escola para as crianças
de 4 (quatro) a 5 (cinco) anos de idade e ampliar a oferta de educação infantil em
creches, de forma a atender, no mínimo, 50% (cinquenta por cento) das crianças de
até 3 (três) anos até o final da vigência deste PNE",
    style = "font-size: 20px; color:#B45F04")
    )

),
        hr(style = "border-top: 1px solid black;"), # tag horizontal row style para mudar o css
        br(),
        fluidRow(
          box(
            width = 6,
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
              box(
                  width = 12,
                  title = "Indicador 1B: Matrícula em Creche (0 a 3 anos)",
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
      mutate(
        indicador1b = round(indicador1b, 2)) |>
       # req(input$municipio)
      ggplot(aes(x = NU_ANO_CENSO, y = indicador1b, color = indicador1b)) +
      geom_line() +
      geom_point() +
      geom_label(aes(label = indicador1b))+
      labs(
        x = "Ano",
        y = "Indicador 1B",
        color = "Indicador 1B"
       # title = "Gráfico de dispersão",
       # subtitle = "Receita vs Orçamento"
      ) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(2014, 2020, 1)) +
      scale_y_continuous(breaks = seq(0, 1, 0.1))




  }

  )

}

shinyApp(ui, server)
















