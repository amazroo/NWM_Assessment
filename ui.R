library(shiny)
library(leaflet)
library(RColorBrewer)


list.assessments <- c("Overall (1993-2018)"="overall",
                      "-Winter"="winter","-Spring"="spring",
                      "-Summer"="summer","-Fall"="fall",
                      "--Jan"="Jan","--Feb"="Feb","--Mar"="Mar",
                      "--Apr"="Apr","--May"="May","--Jun"="Jun",
                      "--Jul"="Jul","--Aug"="Aug","--Sep"="Sep",
                      "--Oct"="Oct","--Nov"="Nov","--Dec"="Dec")
list.metrics <- c("QObs-Mean" = "miu_obs", 
                  "Qobs-Standard Deviation" = "sigma_obs",
                  "QNWM-Mean" = "miu_est",
                  "QNWM-Standard Deviation" = "sigma_est",
                  "MSE"="MSE", "KGE"="KGE", "NSE"="NSE", 
                  "NSE_A"="NSE_A", "NSE_B"="NSE_B", "NSE_C"="NSE_C")
basin_layer.info <- read.table("AppDATA/info_stations.txt", sep=",", header=TRUE,
                               colClasses = c("siteID"="character"))

list.basin_characters <- c("lat","lon","DA","Precip_mmperyear","PET_mmperyear","AI")

list.hucs <- 1:18
names(list.hucs) <- paste0("huc",sprintf("%02d",1:18))

label.siteID_names <- basin_layer.info$siteID
names(label.siteID_names) <- paste(basin_layer.info$siteID, basin_layer.info$name)

navbarPage("Online NWM Performance Analysis Tool", id="nav",


################# THE MAP #################
   tabPanel("Interactive map",
        div(class="outer",
            tags$head(
                includeCSS("styles.css")
            ),
            
            leafletOutput("map", width="100%", height="100%")
            ),

            tags$div(id="cite",
                     tags$em(' Online NWM Performance Analysis Tool; 2020 ')
            )
   ),



################# REGIONAL ANALYSES TAB #################
tabPanel("Regional Analyses",
         sidebarLayout(
            sidebarPanel(selectizeInput("RA_huc", h5("HUC Region(s)"), choices = list.hucs, selected = list.hucs[1],
                                        multiple = TRUE, options = list('plugins' = list('remove_button'),
                                           'create' = TRUE, 'persist' = FALSE)),
                         checkboxGroupInput("RA_station_class", "", c("Natural Basins" = "natural","Controlled Basins" = "non-natural"),
                                            selected = c("natural","non-natural")),
                         selectInput("RA_assessment", h5("Assessment"), choices = list.assessments,
                                     selected = list.assessments[1]),
                         selectInput("RA_xvar", h5("x variable"), choices = list.basin_characters,
                                     selected = "DA"),
                         checkboxInput("RA_xvar_log", h5("x in log space"), value = TRUE),
                         selectInput("RA_yvar", h5("y variable"), choices = list.metrics,
                                     selected = "NSE_A"),
                         checkboxInput("RA_yvar_log", h5("y in log space"), value = FALSE)
            ),
            mainPanel(
               plotOutput("RA_plot")
            )
         )
),


################# DOCUMENTATION TAB #################
   tabPanel("Documentation",
            h5("User guide / methodology")
   ),
   
   conditionalPanel("false", icon("crosshair"))
)
