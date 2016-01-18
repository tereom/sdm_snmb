
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library("shiny")

shinyUI(fluidPage(

  # Application title
  titlePanel("Especies invasoras SNIB/SNMB"),

  # Combobox para seleccionar invasora
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "invasora_seleccionada",
        "Seleccionar invasora:",
        nombres_invasoras
        ),
      sliderInput(
        "anio_seleccionado",
        "Seleccionar año mínimo",
        anio_min,
        anio_max,
        anio_actual,
        step = 1
      )
    ),

    # Mapa de ocurrencias:
    mainPanel(
      plotOutput("mapa_invasoras")
    )
  )
))
