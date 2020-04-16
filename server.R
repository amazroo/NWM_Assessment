library(shiny)
library(RColorBrewer)
library(leaflet)
library(lattice)
library(dplyr)
library(raster)
library(rgdal)
library(moments)

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
  labels <- sprintf("<strong>siteID#%s</strong><br/> %s <br/> %s",
                    basin_layer.info$siteID,basin_layer$name, basin_layer.info$CLASS) %>% lapply(htmltools::HTML)
  output$map <- renderLeaflet({
    leaflet(basin_layer) %>% 
        setView(lng = -99, lat = 40, zoom = 4) %>%
        #addProviderTiles("Stamen.TerrainBackground") %>%
        addProviderTiles("CartoDB.DarkMatter") %>% 
    addCircles(lng = ~lon, lat = ~lat, weight = 1, popup = labels, radius = 1000, 
               color = "gray", fillOpacity = 1)
  })
  

  ## Regional Analyses (RA) TAB ###########################################
  observe({
    if (input$RA_xvar_log){ X <- log(data.RA_x())} else {X <- data.RA_x()}
    if (input$RA_yvar_log){ Y <- log(data.RA_y())} else {Y <- data.RA_y()}
    output$RA_plot <- renderPlot({
      plot(X,Y, col = basin_layer.info[tmp.ind_RA(),"CLASS"], pch=20,
           ylab = input$RA_yvar, xlab = input$RA_xvar, main=input$RA_assessment)
    }, height = 550)
  })
      
}