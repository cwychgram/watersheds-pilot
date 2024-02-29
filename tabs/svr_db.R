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

observeEvent(input$select_ws, {
  ws2map <- ws %>%
    filter(NAME == input$select_ws)
  
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