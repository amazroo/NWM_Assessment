library(shiny)
library(RColorBrewer)
library(leaflet)
library(lattice)
library(dplyr)
library(raster)
library(rgdal)
library(moments)
library(plotly)
#library(smwrBase)
#library(moments)



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

list.units <- c("miu_obs" = "[cms]", "sigma_obs" = "[cms]", "miu_est" = "[cms]", "sigma_est" = "[cms]",
                  "MSE"="", "KGE"="", "NSE"="", "NSE_A"="", "NSE_B"="", "NSE_C"="")

list.assessment_subnames <- list("Annual" = "overall", 
                                 "Seasonal" = c("winter","spring","summer","fall"),
                                 "Monthly" = month.abb)

function(input, output, session) {


  
  
  # Filter the variable/data to display
 
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

  ## Regional Analyses (RA) TAB ###########################################
  observe({
    if (input$RA_xvar_log){ X <- log(data.RA_x()); X_label <- paste0("log(",input$RA_xvar,")")} else {X <- data.RA_x(); X_label <- input$RA_xvar}
    if (input$RA_yvar_log){ Y <- log(data.RA_y()); Y_label <- paste0("log(",input$RA_yvar,")")} else {Y <- data.RA_y(); Y_label <- input$RA_yvar}
    tmp.dataly <- cbind(data.frame("x"=X, "y"=Y), basin_layer.info[tmp.ind_RA(),c("siteID","name","CLASS")])
    output$RA_plot <- renderPlotly({
      plot_ly(type = 'scatter',  mode='markers', tmp.dataly,
              x = ~x, y = ~y, color=~CLASS, colors = c("#17becf","#ff7f0e"),
              text = sprintf("%s\n%s\nBasin Type: %s", paste("siteID=",tmp.dataly$siteID), tmp.dataly$name, tmp.dataly$CLASS),
              hovertemplate = paste(
                "<b>%{text}</b><br>",
                "%{yaxis.title.text}: %{y:.2f}<br>",
                "%{xaxis.title.text}: %{x:.2f}",
                "<extra></extra>"
              ))%>%
       layout(title = input$RA_assessment,
              xaxis = list(title = X_label),
              yaxis = list(title = Y_label),
              legend = list(orientation = 'h')) %>%
        config( displayModeBar = "hover", displaylogo = FALSE,
                modeBarButtonsToRemove = c("select2d","lasso2d", "toggleSpikelines","autoScale2d",
                                           "hoverClosestCartesian", "hoverCompareCartesian"))
    })
  })
      
}