ui_db <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 3,
      radioButtons("select_type", 
                   label = "Select a watershed type:",
                   choices = c("Pilot", "Control"), 
                   selected = "Pilot",
                   width = "100%",
                   inline = TRUE),
      selectInput("select_ws",
                  label = "Select a watershed:",
                  choices = NULL,
                  width = "100%"
      ),
      # sliderInput("select_yr",
      #             label = "Select a year:",
      #             value = max(lulc$YEAR),
      #             min = min(lulc$YEAR),
      #             max = max(lulc$YEAR),
      #             width = "100%"),
      sliderInput("select_mo",
                  label = "Select a month:",
                  value = 1,
                  min = 1,
                  max = 12,
                  width = "100%")
    ),
    mainPanel(
      width = 9,
      fluidRow(
        column(
          width = 6,
          leafletOutput("map_lulc")
        ),
        column(
          width = 6,
          leafletOutput("map_ndvi")
        )
      )
    )
  )
)