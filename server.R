library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)



# # Leaflet bindings are a bit slow; for now we'll just sample to compensate
# set.seed(100)
# ffdata <- ffinal[sample.int(nrow(ffinal), 10),]
# # By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# # will be drawn last and thus be easier to see
# 



function(input, output, session) {
    
   
  
  

    

  
    # A reactive expression that returns the set of zips that are
    # in bounds right now
    # ffInBounds <- reactive({
    #     if (is.null(input$map_bounds))
    #         return(ffdata[FALSE,])
    #     bounds <- input$map_bounds
    #     latRng <- range(bounds$north, bounds$south)
    #     lngRng <- range(bounds$east, bounds$west)
    #     
    #     subset(ffdata,
    #            latitude >= latRng[1] & latitude <= latRng[2] &
    #                longitude >= lngRng[1] & longitude <= lngRng[2])
    # })
    # 
    # 
    
    # This observer is responsible for maintaining the circles and legend,
    # according to the variables the user has chosen to map to color and size.
    # observe({
        # colorBy <- input$color
        # sizeBy <- input$size
        # 
        # if (colorBy == "superzip") {
        #     # Color and palette are treated specially in the "superzip" case, because
        #     # the values are categorical instead of continuous.
        #     colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
        #     pal <- colorFactor("viridis", colorData)
        # } else {
        #     colorData <- zipdata[[colorBy]]
        #     pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
        # }
        # 
        # if (sizeBy == "superzip") {
        #     # Radius is treated specially in the "superzip" case.
        #     radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
        # } else {
        #     radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
        # }
        
    #     leafletProxy("map", data = ffdata) %>%
    #         clearShapes() %>%
    #         addCircles(~Long, ~Lat, radius=3, layerId=~站代號,
    #                    stroke=FALSE, fillOpacity=0.4, fillColor="red") %>%
    #         addLegend("bottomleft", pal=1, values=5, title=10,
    #                   layerId="colorLegend")
    # })
    
    
    
    
    
    
    ## Data Data Data ###########################################
    output$valuedate <- renderPrint({ input$date })
    output$valuehour <- renderPrint({ input$hour })
    output$valuemin <- renderPrint({ input$min })
   
    output$table1 <- DT::renderDataTable({
       
        df <- cleantable %>% 
            filter(
                Date == input$date,
                Hour == input$hour,
                Min >= input$min[1],
                Min <= input$min[2]
            )
        action <- DT::dataTableAjax(session, df)
        
        DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
        
        
    })
    
    
    ## license license license ###########################################
    output$value_eng <- renderPrint({ input$license_eng })
    output$value_num <- renderPrint({ input$license_num })
    
    output$table2 <- DT::renderDataTable({
        df <- cleantable %>% 
            filter(
                Eng == input$license_eng,
                Num == input$license_num
            )
        action <- DT::dataTableAjax(session, df)
        
        DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
    })

    
    ## Interactive Map ###########################################
   

   
    
    # Create the map   #把addCircleMarkers放入leaflet函數內才跑的出點(不知道原因) #互動式還未成功
    output$valuedate <- renderPrint({ input$date2 })
    output$valuehour <- renderPrint({ input$hour2 })
    output$valuemin <- renderPrint({ input$min2 })
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>%
        setView(lng = 121.55, lat = 25.036255, zoom = 12) %>% 
        addCircleMarkers((det_time({input$date2 } %>% as.character,{ input$hour2 }%>% as.character,{ input$min2 }%>% as.character) %>% create)$經度,
        (det_time({ input$date2 }%>% as.character,{ input$hour2 }%>% as.character,{ input$min2 }%>% as.character) %>% create())$緯度,
        color=getColor((det_time({ input$date2 }%>% as.character,{ input$hour2 }%>% as.character,{ input$min2 }%>% as.character) %>% create)),
        fillColor=getColor((det_time({ input$date2 }%>% as.character,{ input$hour2 }%>% as.character,{ input$min2 }%>% as.character) %>% create)),
        opacity=1, fillOpacity=1,radius=9,
                         popup=paste0("站名：", 
           ( det_time({ input$date2}%>% as.character,{ input$hour2 }%>% as.character,{ input$min2 }%>% as.character) %>% create())$站名,"<br>過去十分鐘車流量：",
           (det_time({ input$date2 }%>% as.character,{ input$hour2 }%>% as.character,{ input$min2 }%>% as.character) %>% create())$交易次數, "<br>擁擠程度：",
           (det_time({ input$date2 }%>% as.character,{ input$hour2 }%>% as.character,{ input$min2 }%>% as.character) %>% create())$擁擠程度))

                         


    })
    # leaflet在shiny內增加更多東西的函數，不知為何一直失敗.....QQ
    
    # leafletProxy("map",data = mmap) %>%
    #   clearShapes() %>%
    #   addCircleMarkers(mmap$經度, mmap$緯度,color=colorr,fillColor=colorr, opacity=1, fillOpacity=1,radius=9,
    #                    popup=paste0("站名：", mmap$站名,"<br>過去十分鐘車流量：", mmap$交易次數, "<br>擁擠程度：",mmap$擁擠程度))

    
    
    
        
}










