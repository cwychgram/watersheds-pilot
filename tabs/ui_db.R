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
      sliderTextInput("select_yr",
                      label = "Select a year:",
                      choices = sort(unique(lulc$YEAR)),
                      selected = max(lulc$YEAR),
                      grid = TRUE,
                      animate = TRUE,
                      width = "100%"
      ),
      sliderTextInput("select_mo",
                      label = "Select a month:",
                      choices = 1:12,
                      selected = 1,
                      grid = TRUE,
                      animate = TRUE,
                      width = "100%"
      )
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
      ),
      fluidRow(
        column(width = 6,
               plotlyOutput("pie")
        ),
        column(width = 6,
               # plotlyOutput("graph")
        )
      )
    )
  )
)