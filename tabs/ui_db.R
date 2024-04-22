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
      selectInput("select_szn",
                  label = "Select a season:",
                  choices = c("Kiremt or Meher (Summer)",
                              "Belg (Autumn)",
                              "Bega (Winter)",
                              "Tseday (Spring)"),
                  selected = "Kiremt or Meher (Summer)",
                  width = "100%"
      )
    ),
    mainPanel(
      width = 9,
      fluidRow(
        column(
          width = 6,
          h4("Land Use/Land Cover"),
          tabsetPanel(
            tabPanel("Map",
                     leafletOutput("map_lulc")),
            tabPanel("Pie Chart",
                     plotlyOutput("pie")),
            tabPanel("Trend")
          )
        ),
        column(
          width = 6,
          h4("NDVI"),
          tabsetPanel(
            tabPanel("Map",
                      leafletOutput("map_ndvi")),
            tabPanel("Season Average",
                     h5(htmlOutput("avg_ndvi")),
                     dataTableOutput("table_ndvi")),
            tabPanel("Time Series",
                     dygraphOutput("graph_ndvi"))
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          h4("Precipitation"),
          tabsetPanel(
            tabPanel("Season Total",
                     h5(htmlOutput("tot_precip")),
                     dataTableOutput("table_precip")),
            tabPanel("Time Series",
                     dygraphOutput("graph_precip"))
          )
        )
      )
      # ,
      # fluidRow(
      #   column(width = 6,
      #          plotlyOutput("pie")
      #   ),
      #   column(width = 6,
      #   )
      # )
      # ,
      # fluidRow(
      #   column(width = 6,
      #          plotlyOutput("graph_lulc")
      #   ),
      #   column(width = 6,
      #   )
      # )
    )
  )
)