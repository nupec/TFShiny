library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(lubridate)

# meta1       <- readr::read_rds("data/indicador1bBrasil.rds")
# indicador1a <- readr::read_rds("data/indicador1aBrasil.rds")


ui <- dashboardPage(
  dashboardHeader(title = HTML("PNE-Municípios"),
                  disable = FALSE,
                  titleWidth  = 550),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Meta 1",  tabName = "meta1", icon = icon("fas fa-chevron-right")),
      menuItem("Meta 2",  tabName = "meta2", icon = icon("fas fa-chevron-right")),
      menuItem("Meta 3",  tabName = "meta3", icon = icon("fas fa-chevron-right")),
      menuItem("Meta 4",  tabName = "meta4", icon = icon("fas fa-chevron-right")),
      menuItem("Meta 5",  tabName = "meta5", icon = icon("fas fa-chevron-right")),
      menuItem("Meta 6",  tabName = "meta6", icon = icon("fas fa-chevron-right")),
      menuItem("Meta 7",  tabName = "meta7", icon = icon("fas fa-chevron-right")),
      menuItem("Meta 8",  tabName = "meta8", icon = icon("fas fa-chevron-right")),
      menuItem("Meta 9",  tabName = "meta9", icon = icon("fas fa-chevron-right")),
      menuItem("Meta 10", tabName = "meta10", icon = icon("fas fa-chevron-right"))
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
          width = 12,
          fluidRow(
            column(
              width = 4,
              selectInput(
                "reg", "Selecione uma região",
                choices = c("Centro-Oeste", "Norte", "Nordeste", "Sudeste",
                            "Sul"),
                selected = "Norte"
               )
              ),
            column(
              width = 4,
              selectInput(
                "uf", "Selecione um estado",
                choices = meta1 |> pull(nome_uf) |> unique() |> sort(),
                selected = "Amazonas"
              )
            ),

          column(
            width = 4,
            selectInput(
              "municipio",
              "Selecione o município",
              choices = "",
              selected = "Carregando..."
                )
              ),
              br(),
              box(
                  width = 4,
                  title = "Indicador 1B: Matrícula em Creche (0 a 3 anos)",
                  solidHeader = TRUE, #cabeçalho colorido
                  status = "primary",  # para mudar a cor igual do anterior
                  shinycssloaders::withSpinner(
                    plotOutput("graf_indicador1b")

                )
              ),
              br(),
              box(
                width = 4,
                title = "Indicador 1A: Matrícula em Creche (4 e 5 anos)",
                solidHeader = TRUE, #cabeçalho colorido
                status = "primary",  # para mudar a cor igual do anterior
                shinycssloaders::withSpinner(
                  plotOutput("graf_indicador1a")

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
  indicador1a <- readr::read_rds("data/indicador1aBrasil.rds")

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


  # Gráfico 1B

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
    }  )

  # Gráfico 1A

  output$graf_indicador1a <- renderPlot({

    indicador1a |> filter(nome_uf == input$uf,
                    Municipio == input$municipio) |>
      mutate(
        indice1a = round(indice1a, 2),
        ano_date = readr::parse_date(as.character(ANO), format = "%Y")) |>
      # req(input$municipio)
      ggplot(aes(x = ano_date, y = indice1a, color = indice1a)) +
      geom_line() +
      geom_point() +
      geom_label(aes(label = indice1a))+
      labs(
        x = "Ano",
        y = "Indicador 1A",
        color = "Indicador 1A"
        # title = "Gráfico de dispersão",
        # subtitle = "Receita vs Orçamento"
      ) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(2014, 2020, 1)) +
      scale_y_continuous(breaks = seq(0, 1, 0.1))
  }  )

}

shinyApp(ui, server)
