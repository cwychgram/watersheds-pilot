ws_choices <- reactive({
  switch(input$select_type,
         "Pilot" = ws$NAME[ws$TYPE == "Pilot"],
         "Control" = ws$NAME[ws$TYPE == "Control"]
  )
})

observe({
  req(ws_choices())
  updateSelectInput(session = session, 
                    inputId = "select_ws", 
                    choices = ws_choices())
})

observeEvent(c(input$select_ws, input$select_yr, input$select_mo), {
  ws2map <- ws %>%
    filter(NAME == input$select_ws)
  
  lulc2map_df <- lulc %>%
    filter(NAME == input$select_ws,
           YEAR == input$select_yr,
           MONTH == input$select_mo) 
  
  if(nrow(lulc2map_df) > 0) {
    lulc2map <- lulc2map_df %>%
      dplyr::select(LULC, X, Y) %>%
      st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
      mutate(LULC = factor(as.character(LULC), levels = c(1, 2, 3, 4, 5, 6))) %>%
      st_rasterize()
    
    pal_lulc <- colorFactor(
      palette = c("#397d49","#88B053","#e49635","#dfc35a","#c4281b","#a59b8f"),
      na.color = "transparent",
      domain = lulc2map$LULC
    )
  }
  
  output$map_lulc <- renderLeaflet({
    if (ws2map$TYPE == "Pilot") {
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addScaleBar() %>%
        addResetMapButton() %>%
        clearShapes() %>%
        fitBounds(lng1 = unname(st_bbox(ws2map)$xmin),
                  lat1 = unname(st_bbox(ws2map)$ymin),
                  lng2 = unname(st_bbox(ws2map)$xmax),
                  lat2 = unname(st_bbox(ws2map)$ymax)
        ) 
    } else {
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addScaleBar() %>%
        addResetMapButton() %>%
        clearShapes() %>%
        fitBounds(lng1 = unname(st_bbox(ws2map)$xmin - .01),
                  lat1 = unname(st_bbox(ws2map)$ymin - .01),
                  lng2 = unname(st_bbox(ws2map)$xmax + .01),
                  lat2 = unname(st_bbox(ws2map)$ymax + .01)
        ) 
    }
  })
  
  if(nrow(lulc2map_df) > 0) {
    leafletProxy("map_lulc", session) %>%
      clearGroup(group = "LULC") %>%
      removeControl(layerId = "LULC_legend") %>%
    addStarsImage(lulc2map, colors = pal_lulc, group = "LULC") %>%
    addLegend(colors = c("#397d49","#88B053","#e49635","#dfc35a","#c4281b","#a59b8f"),
              labels = c("Trees", "Grass", "Crops", "Shrub/Scrub", "Built", "Bare"),
              opacity = 1,
              title = "LULC",
              layerId = "LULC_legend")
  } else {
    leafletProxy("map_lulc", session) %>%
      clearGroup(group = "LULC") %>%
      removeControl(layerId = "LULC_legend")
  }
  
  output$pie <- renderPlotly({
    
    if(nrow(lulc2map_df) > 0) {
      
      lulc2pie <- data.frame(LULC = c("Trees", "Grass", "Crops", "Shrub/Scrub", "Built", "Bare"),
                             PCT = c(sum((lulc2map$LULC == 1) == TRUE, na.rm = TRUE) / sum(!is.na(lulc2map$LULC)) * 100,
                                     sum((lulc2map$LULC == 2) == TRUE, na.rm = TRUE) / sum(!is.na(lulc2map$LULC)) * 100,
                                     sum((lulc2map$LULC == 3) == TRUE, na.rm = TRUE) / sum(!is.na(lulc2map$LULC)) * 100,
                                     sum((lulc2map$LULC == 4) == TRUE, na.rm = TRUE) / sum(!is.na(lulc2map$LULC)) * 100,
                                     sum((lulc2map$LULC == 5) == TRUE, na.rm = TRUE) / sum(!is.na(lulc2map$LULC)) * 100,
                                     sum((lulc2map$LULC == 6) == TRUE, na.rm = TRUE) / sum(!is.na(lulc2map$LULC)) * 100))
      
      lulc2pie$PCT <- round(lulc2pie$PCT, 1)
      
      lulc2pie <- lulc2pie[lulc2pie$PCT != 0, ]
      
      lulc2pie <- lulc2pie %>%
        mutate(PAL = case_when(
          LULC == "Trees" ~ "#397d49",
          LULC == "Grass" ~ "#88B053",
          LULC == "Crops" ~ "#e49635",
          LULC == "Shrub/Scrub" ~ "#dfc35a",
          LULC == "Built" ~ "#c4281b",
          LULC == "Bare" ~ "#a59b8f"
        ))
      
      p <- plot_ly(lulc2pie, 
                   labels = ~LULC, 
                   values = ~PCT, 
                   textinfo = "percent",
                   hoverinfo = "text",
                   text = paste(lulc2pie$LULC,
                                "<br>",
                                lulc2pie$PCT, "%"),
                   marker = list(colors = ~PAL),
                   sort = FALSE,
                   type = "pie")
      
      p
    }
  })

  output$graph_lulc <- renderPlotly({
    lulc4graph_df <- lulc %>%
      filter(NAME == input$select_ws,
             MONTH == input$select_mo)
    
    if(nrow(lulc4graph_df) > 0) {
      func4graph <- function(yr) {
        lulc4graph <- lulc4graph_df %>%
          filter(YEAR == yr) %>%
          dplyr::select(LULC, X, Y) %>%
          st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
          mutate(LULC = factor(as.character(LULC), levels = c(1, 2, 3, 4, 5, 6))) %>%
          st_rasterize()
        lulc2graph <- data.frame(LULC = c("Trees", "Grass", "Crops", "Shrub/Scrub", "Built", "Bare"),
                               PCT = c(sum((lulc4graph$LULC == 1) == TRUE, na.rm = TRUE) / sum(!is.na(lulc4graph$LULC)) * 100,
                                       sum((lulc4graph$LULC == 2) == TRUE, na.rm = TRUE) / sum(!is.na(lulc4graph$LULC)) * 100,
                                       sum((lulc4graph$LULC == 3) == TRUE, na.rm = TRUE) / sum(!is.na(lulc4graph$LULC)) * 100,
                                       sum((lulc4graph$LULC == 4) == TRUE, na.rm = TRUE) / sum(!is.na(lulc4graph$LULC)) * 100,
                                       sum((lulc4graph$LULC == 5) == TRUE, na.rm = TRUE) / sum(!is.na(lulc4graph$LULC)) * 100,
                                       sum((lulc4graph$LULC == 6) == TRUE, na.rm = TRUE) / sum(!is.na(lulc4graph$LULC)) * 100))

        lulc2graph <- lulc2graph %>%
          mutate(PCT = round(PCT, 1),
                 YEAR = yr) %>%
          filter(PCT != 0)
      }
      # lulc2graph_2018 <- func4graph(2018)
      lulc2graph_2019 <- func4graph(2019)
      lulc2graph_2020 <- func4graph(2020)
      lulc2graph_2021 <- func4graph(2021)
      lulc2graph_2022 <- func4graph(2022)
      lulc2graph_2023 <- func4graph(2023)
      
      lulc2graph_all <- rbind(
        # lulc2graph_2018, 
        lulc2graph_2019, lulc2graph_2020, lulc2graph_2021, 
        lulc2graph_2022, lulc2graph_2023)
      
      lulc2graph_all <- lulc2graph_all %>%
        mutate(LULC = factor(LULC, levels = c("Trees", "Grass", "Crops", "Shrub/Scrub", "Built", "Bare")))
      
      x <- list(
        title = "<b>Year</b>"
      )
      y <- list(
        title = "<b>% LULC</b>",
        tickformat = "digits"
      )
      
      p <- lulc2graph_all %>%
        group_by(LULC) %>%
        plot_ly(x = ~YEAR,
                y = ~PCT,
                color = ~LULC,
                colors = c("Trees" = "#397d49",
                           "Grass" = "#88B053",
                           "Crops" = "#e49635",
                           "Shrub/Scrub" = "#dfc35a",
                           "Built" = "#c4281b",
                           "Bare" = "#a59b8f"),
                type = "scatter",
                mode = "lines") %>%
        layout(xaxis = x, yaxis = y)
      p
    }
  })
  
  ndvi2map_df <- ndvi %>%
    filter(NAME == input$select_ws,
           YEAR == input$select_yr,
           MONTH == input$select_mo) 
  
  if(nrow(ndvi2map_df) > 0) {
    if (input$select_ws == "Meta Hawi Gudina") {
      ndvi2map <- ndvi2map_df %>%
        dplyr::select(NDVI, X, Y) %>%
        st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
        st_rasterize(template = read_stars("data/hawi.tif"))
    } else if (input$select_ws == "Meta Finxabas") {
      ndvi2map <- ndvi2map_df %>%
        dplyr::select(NDVI, X, Y) %>%
        st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
        st_rasterize(template = read_stars("data/finxabas.tif"))
    } else {
      ndvi2map <- ndvi2map_df %>%
        dplyr::select(NDVI, X, Y) %>%
        st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
        st_rasterize()
    }
    
    pal_ndvi <- colorNumeric(palette = "Greens",
                             na.color = "transparent",
                             domain = ndvi2map$NDVI)
  }
  
  output$map_ndvi <- renderLeaflet({
    if (ws2map$TYPE == "Pilot") {
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addScaleBar() %>%
        addResetMapButton() %>%
        clearShapes() %>%
        fitBounds(lng1 = unname(st_bbox(ws2map)$xmin),
                  lat1 = unname(st_bbox(ws2map)$ymin),
                  lng2 = unname(st_bbox(ws2map)$xmax),
                  lat2 = unname(st_bbox(ws2map)$ymax)
        ) 
    } else {
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addScaleBar() %>%
        addResetMapButton() %>%
        clearShapes() %>%
        fitBounds(lng1 = unname(st_bbox(ws2map)$xmin - .01),
                  lat1 = unname(st_bbox(ws2map)$ymin - .01),
                  lng2 = unname(st_bbox(ws2map)$xmax + .01),
                  lat2 = unname(st_bbox(ws2map)$ymax + .01)
        ) 
    } 
  })
  
  if(nrow(ndvi2map_df) > 0) {
    leafletProxy("map_ndvi", session) %>%
      clearGroup(group = "NDVI") %>%
      removeControl(layerId = "NDVI_legend") %>%
      addStarsImage(ndvi2map, colors = pal_ndvi, group = "NDVI") %>%
      addLegend(pal = pal_ndvi,
                values = ndvi2map$NDVI,
                opacity = 1,
                title = "NDVI",
                layerId = "NDVI_legend")
  } else {
    leafletProxy("map_ndvi", session) %>%
      clearGroup(group = "NDVI") %>%
      removeControl(layerId = "NDVI_legend")
  }
}, ignoreInit = TRUE)