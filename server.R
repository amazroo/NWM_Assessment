library(shiny)
library(RColorBrewer)
library(leaflet)
library(lattice)
library(dplyr)
library(raster)
library(rgdal)
library(moments)
library(plotly)
library(reshape2)
library(dygraphs)



metrics.overall <- read.table("./AppDATA/Verification_Metrics_overall_v2", sep=",", header = TRUE, 
                              colClasses = c("siteID"="character"))
colnames(metrics.overall)[-1] <- paste0(colnames(metrics.overall)[-1],"_overall")
metrics.seasonal <- read.table("./AppDATA/Verification_Metrics_seasonal_v2", sep=",", header = TRUE, 
                              colClasses = c("siteID"="character"))
metrics.monthly <- read.table("./AppDATA/Verification_Metrics_monthly_v2", sep=",", header = TRUE, 
                              colClasses = c("siteID"="character"))
metrics.all <- cbind(metrics.overall, metrics.seasonal[,-1], metrics.monthly[,-1])
is.na(metrics.all) <- sapply(metrics.all, is.infinite)

basin_layer <- shapefile(paste0("AppDATA/stations_info.shp"))
basin_boundary_layer <- shapefile(paste0("AppDATA/basins.shp"))
basin_layer.info <- read.table("AppDATA/info_stations.txt", sep=",", header=TRUE,
                               colClasses = c("siteID"="character"))

list.metrics_out <- c("miu_obs" = "QObs-Mean", 
                      "sigma_obs" = "Qobs-Standard Deviation",
                      "miu_est" = "QNWMv2.0-Mean",
                      "sigma_est" = "QNWM2.0-Standard Deviation",
                      "MSE"="MSE", "KGE"="KGE", "NSE"="NSE", 
                      "NSE_A"="NSE_A", "NSE_B"="NSE_B", "NSE_C"="NSE_C")

list.units <- c("miu_obs" = "[m^3/s]", "sigma_obs" = "[m^3/s]", "miu_est" = "[m^3/s]", "sigma_est" = "[m^3/s]",
                  "MSE"="", "KGE"="", "NSE"="", "NSE_A"="", "NSE_B"="", "NSE_C"="")

list.assessment_subnames <- list("Annual" = "overall", 
                                 "Seasonal" = c("winter","spring","summer","fall"),
                                 "Monthly" = month.abb)

function(input, output, session) {

  # Filter the variable/data to display
  tmp.ind1 <- reactive({ basin_layer.info$CLASS %in% input$station_class })
  tmp.var <- reactive({ as.numeric(metrics.all[tmp.ind1(),colnames(metrics.all) == paste0(input$metric,"_",input$assessment)]) })
  colorpal <- reactive({ colorQuantile(input$colors, tmp.var(), n=10, na.color = "gray") })
  data.tab3 <- reactive({ paste0("./AppDATA/Results_Assessment_NWM/STA_",input$tab3_siteID,".rds") %>% readRDS()})
  data.flows <- reactive({ paste0("./Flows_1993_2018_V2/STA_",input$tab3_siteID,".rds") %>% readRDS()})
  
  tmp.ind_RA <- reactive({ basin_layer.info$CLASS %in% input$RA_station_class & basin_layer.info$HUC2 %in% input$RA_huc})
  data.RA_x <- reactive({ as.numeric(basin_layer.info[tmp.ind_RA(), colnames(basin_layer.info) == input$RA_xvar]) })
  data.RA_y <- reactive({ as.numeric(metrics.all[tmp.ind_RA(),colnames(metrics.all) == paste0(input$RA_yvar,"_",input$RA_assessment)]) }) 
    
  ## BASIC MAP and Lables
  labels.short <- sprintf("<strong>siteID#%s</strong><br/> %s",
                    basin_layer.info$siteID,basin_layer$name) %>% lapply(htmltools::HTML)
  labels.long <- sprintf("<strong>siteID#%s</strong><br/>%s<br/>basin class: %s<br/>Drainage Area: %.2f<br/>
                         Aridity Index: %.2f<br/>#upstream dams: %d",
                         basin_layer.info$siteID,
                         basin_layer.info$name,
                         basin_layer.info$CLASS,
                         basin_layer.info$DA,
                         basin_layer.info$AI,
                         basin_layer.info$dams) %>% lapply(htmltools::HTML)
  output$map <- renderLeaflet({
    leaflet(basin_layer) %>% 
        setView(lng = -99, lat = 40, zoom = 4) %>%
        #addProviderTiles("Stamen.TerrainBackground") %>%
        addProviderTiles("CartoDB.DarkMatter") %>% 
    addCircles(group="base", lng = ~lon, lat = ~lat, weight = 1, layerId = rownames(basin_layer.info),
               label = labels.short , popup = labels.long, radius = 2000, 
               color = "gray", fillOpacity = 1)
  })
  
  
  ## Show Basin Boundary if station is selected
  observeEvent(input$map_shape_click, {
    myclick <- input$map_shape_click
    tmp.rowindex <- myclick$id
    tmp.station <- basin_layer.info[tmp.rowindex,]
    tmp.boundary <- basin_boundary_layer[basin_boundary_layer@data$siteID == tmp.station$siteID,]
    leafletProxy("map") %>%
      clearGroup(group = "tmp.layer") %>%
      addPolygons(group = "tmp.layer", data=tmp.boundary, color = "gray", weight = 1, smoothFactor = 0.5,
                  opacity = 1, fillOpacity = .4, fillColor = "gray")
  })
  ## Hide Basin Boundary by clicking nowhere
  observeEvent(input$map_click, {
    leafletProxy("map") %>%
    clearGroup(group = "tmp.layer")
  })

  
  ## Interactive Map TAB ###########################################  
  
  observe({
    # # Lables
    labels <- sprintf("<strong>siteID#%s</strong><br/> %s <br/> %s <br/> %s = %.3f",
                      basin_layer.info$siteID[tmp.ind1()],basin_layer$name[tmp.ind1()],
                      basin_layer.info$CLASS[tmp.ind1()],
                      paste0(list.metrics_out[input$metric]," (",input$assessment,")"),tmp.var()) %>% lapply(htmltools::HTML)
    
    pal <- colorpal()
    if (!all(is.na(tmp.var()))) {   #prevent crash When unchecking all station classes
      leafletProxy("map", data = basin_layer[tmp.ind1(),]) %>%
        clearShapes() %>%
        addCircles(lng = ~lon, lat = ~lat, weight = 1, label = labels, popup = labels.long[tmp.ind1()], 
                   radius = 1+500*as.numeric(input$circlesize)^3,
                   color = pal(tmp.var()), fillOpacity = 1, layerId = rownames(basin_layer.info[tmp.ind1(),]))
    }
  })
  
  observe({
    proxy <- leafletProxy("map", data = basin_layer)
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    pal <- colorpal()
    pal_colors <- unique(pal(sort(tmp.var())))
    pal_labs <- quantile(tmp.var(), seq(0, 1, .1), na.rm = TRUE) # depends on n from pal
    pal_labs <- paste(sprintf("%.2f",lag(pal_labs)), sprintf("%.2f",pal_labs), sep = " - ")[-1] # first lag is NA
    if (!all(is.na(tmp.var()))) {   #prevent crash When unchecking all station classes
      proxy %>% addLegend(position = "bottomright", colors = pal_colors, labels = pal_labs,
                          title = paste(list.metrics_out[input$metric],list.units[input$metric]), opacity = 1)
      #proxy %>% addLegend(position = "bottomright", pal = pal, values = tmp.var(),
      #                    labels = 1:10, title = paste(input$metric,"Percentile"))
    }
  })
  
  
  ## Regional Analyses (RA) TAB ###########################################
  observe({
    if (input$RA_xvar_log){ X <- log(data.RA_x()); X_label <- paste0("log(",input$RA_xvar,")")} else {X <- data.RA_x(); X_label <- input$RA_xvar}
    if (input$RA_yvar_log){ Y <- log(data.RA_y()); Y_label <- paste0("log(",input$RA_yvar,")")} else {Y <- data.RA_y(); Y_label <- input$RA_yvar}
    tmp.dataly <- cbind(data.frame("x"=X, "y"=Y), basin_layer.info[tmp.ind_RA(),]) #c("DA","AI","dams","NID_str","Mx_strg","Nrml_st","siteID","name","CLASS")])
    tmp.dataly <- do.call(data.frame,lapply(tmp.dataly, function(x) replace(x, is.infinite(x),NA)))
    
    #tmp.dataly <- cbind(data.frame("x"=log(basin_layer.info$DA), "y"=metrics.all$NSE_A_overall), basin_layer.info)
    tmp.dataly <- tmp.dataly[rowSums(is.na(tmp.dataly[,unlist(lapply(tmp.dataly, is.numeric))])) == 0,]
    rownames(tmp.dataly) <- NULL

      if(length(input$lm_inputs>0)){
        tmp.lm.expr <- paste("y ~ x+", paste(input$lm_inputs, collapse="+"))
      } else {
        tmp.lm.expr <- "y ~ x"
      }

      all_summaries <- list()
      for (tmp.class in unique(tmp.dataly$CLASS)){
        tmp.lm.data <- tmp.dataly[tmp.dataly$CLASS == tmp.class,]
        tmp.lm <- lm(tmp.lm.expr, tmp.lm.data)
        tmp.dataly[rownames(tmp.lm.data),"fitted_y"] <- tmp.lm$fitted.values
        #print(paste(tmp.class,summary(tmp.lm), collapse = " "))
        #output$lm_summary <- renderPrint({ summary(tmp.lm) })
        tmp.summary <- summary(tmp.lm)
        tmp.lm.expr2 <- tmp.lm.expr
        tmp.lm.expr2 <- sub("x",X_label,tmp.lm.expr2)
        tmp.lm.expr2 <- sub("y",Y_label,tmp.lm.expr2)
        tmp.summary$call <- paste(tmp.lm.expr2, ", n=",dim(tmp.lm.data)[1])
        rownames(tmp.summary$coefficients) <- sub("x",X_label,rownames(tmp.summary$coefficients))
        all_summaries[[tmp.class]] <- tmp.summary
      }
      names(all_summaries) <- paste(toupper(names(all_summaries)),"FLOWS")
      output$lm_summary <- renderPrint({ all_summaries })

    tmp.dataly <- tmp.dataly[order(tmp.dataly$x),]
    output$RA_plot <- renderPlotly({
      plot_ly(type = 'scatter',  mode='markers', tmp.dataly,
              x = ~x, y = ~y, color=~CLASS, colors = c("#17becf","#ff7f0e"),
              text = sprintf("%s\n%s\nBasin Type: %s", paste("siteID=",tmp.dataly$siteID), tmp.dataly$name, tmp.dataly$CLASS),
              hovertemplate = paste(
                "<b>%{text}</b><br>",
                "%{yaxis.title.text}: %{y:.2f}<br>",
                "%{xaxis.title.text}: %{x:.2f}",
                "<extra></extra>"
              ), name = "data_points")%>%
        add_trace(type = "scatter",mode = "lines", name = "fitted_y",
                  x = ~x, y = ~fitted_y, color=~CLASS, colors = c("#17becf","#ff7f0e"),
                  text = sprintf("%s\n%s\nBasin Type: %s", paste("siteID=",tmp.dataly$siteID), tmp.dataly$name, tmp.dataly$CLASS),
                  hovertemplate = paste(
                    "<b>%{text}</b><br>",
                    "%{yaxis.title.text}_fitted: %{y:.2f}<br>",
                    "%{xaxis.title.text}: %{x:.2f}",
                    "<extra></extra>"
                  )) %>%
        layout(title = input$RA_assessment,
              xaxis = list(title = X_label),
              yaxis = list(title = Y_label),
              showlegend = FALSE,
              legend = list(orientation = 'h')) %>%
        config( displayModeBar = "hover", displaylogo = FALSE,
                modeBarButtonsToRemove = c("select2d","lasso2d", "toggleSpikelines","autoScale2d",
                                           "hoverClosestCartesian", "hoverCompareCartesian"))
    })
    

    
    output$download_RA_data <- downloadHandler(
      filename = "data.csv",
      content = function(file) {
        readr::write_csv(tmp.dataly, file)
    })
    
    
  })
  
  ## SINGLE BASIN TAB ###########################################
  ## Tab3 - BASIC INFO
  observe({
    data.tab3_info1 <- basin_layer.info[basin_layer.info$siteID==input$tab3_siteID,]
    data.tab3_info1 <- data.tab3_info1[,!(colnames(data.tab3_info1) %in% c("obs","FID"))]
    output$tab3_info1 <- renderTable(data.tab3_info1[,1:10])
    output$tab3_info11 <- renderTable(data.tab3_info1[,11:17])
    
    data.tab3_info2 <- metrics.overall[metrics.overall$siteID==input$tab3_siteID,-1]
    colnames(data.tab3_info2) <- paste(list.metrics_out,list.units)
    output$tab3_info2 <- renderTable(data.tab3_info2)
  })
  
  #### Tab3 - Time Series PLOT
  observe({
    
    data.tab3_plot <- subset(data.tab3() , names(data.tab3()) %in% list.assessment_subnames[[input$tab3_plot_assessment]]) %>%
      sapply('[[',input$tab3_plot_metric)
    
    flows <- data.flows()
    flows[flows$Qnwm<0 , "Qnwm"] <- 0
    flows_ts <- xts::xts(x=flows[,c("Qobs","Qnwm")], order.by = flows$Date)

    output$tab3_dygraph_flows <- renderDygraph({
      dygraph(flows_ts, ylab = "Streamflow [m^3/s]", main = "NWMv2.0 predictions vs. USGS observations") %>%
        dyRangeSelector()
    })

    #### Tab3 - BOX PLOT    
    output$tab3_plot <- renderPlotly({
      plot_ly(type='box', melt(as.data.frame(data.tab3_plot), id.vars = NULL),
              x = ~variable, y = ~value, col="lightblue") %>%
        layout(title = "26 years of data",
               xaxis = list(title = ""),
               yaxis = list(title = paste(list.metrics_out[input$tab3_plot_metric],list.units[input$tab3_plot_metric])),
               height = 500,
               width = 100*ncol(data.tab3_plot)^.8 + 150) %>%
            config( displayModeBar = "hover", displaylogo = FALSE,
                    modeBarButtonsToRemove = c("select2d","lasso2d", "toggleSpikelines","autoScale2d",
                                               "hoverClosestCartesian", "hoverCompareCartesian"))
    })

    data.tab3_detaileddata <- cbind("year"=as.character(1993:2018), round(data.tab3_plot, digits = 2))
    colnames(data.tab3_detaileddata)[colnames(data.tab3_detaileddata)=="overall"] <- paste(list.metrics_out[input$tab3_plot_metric],list.units[input$tab3_plot_metric])
    output$tab3_table <- renderTable(data.tab3_detaileddata, 
                                     caption = paste(list.metrics_out[input$tab3_plot_metric],list.units[input$tab3_plot_metric]),
                                     caption.placement = getOption("xtable.caption.placement", "top")
                                     )
  })
  
      
}