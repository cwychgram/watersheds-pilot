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
  })
  
  output$map_ndvi <- renderLeaflet({
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
  })
  
  # observeEvent(input$select_lulc, {
  #   lulc_type <-  reactive({
  #     lulc %>%
  #       filter(LULC %in% input$select_lulc & CW %in% input$select_ws)
  #   })
  #   
  #   pal_lulc <- colorFactor(
  #     palette = c("#397d49","#88B053","#e49635","#dfc35a","#c4281b","#a59b8f"),
  #     domain = lulc$LULC,
  #   )
  #   
  #   if(!is.null(input$select_lulc) & nrow(lulc_type()) > 0) {
  #     leafletProxy("map", session) %>%
  #       clearGroup(group = "LULC") %>%
  #       removeControl(layerId = "LULCLegend") %>%
  #       addPolygons(data = lulc_type(),
  #                   label = ~LULC,
  #                   color = ~pal_lulc(LULC),
  #                   fillOpacity = 1,
  #                   stroke = FALSE,
  #                   group = "LULC") %>%
  #       addLegend(position = "topright",
  #                 pal = pal_lulc,
  #                 values = lulc_type()$LULC,
  #                 title = "LULC Type",
  #                 opacity = 1,
  #                 layerId = "LULCLegend"
  #       )
  #   } else {
  #     leafletProxy("map", session) %>%
  #       clearGroup(group = "LULC") %>%
  #       removeControl(layerId = "LULCLegend")
  #   }
  #   
  # }, ignoreNULL = FALSE)
}, ignoreInit = TRUE)