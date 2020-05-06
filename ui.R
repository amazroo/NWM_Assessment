library(shiny)
library(leaflet)
library(RColorBrewer)
library(plotly)
library(dygraphs)

list.assessments <- c("Overall (1993-2018)"="overall",
                      "-Winter"="winter","-Spring"="spring",
                      "-Summer"="summer","-Fall"="fall",
                      "--Jan"="Jan","--Feb"="Feb","--Mar"="Mar",
                      "--Apr"="Apr","--May"="May","--Jun"="Jun",
                      "--Jul"="Jul","--Aug"="Aug","--Sep"="Sep",
                      "--Oct"="Oct","--Nov"="Nov","--Dec"="Dec")
list.metrics <- c("QObs-Mean" = "miu_obs", 
                  "Qobs-Standard Deviation" = "sigma_obs",
                  "QNWMv2.0-Mean" = "miu_est",
                  "QNWMv2.0-Standard Deviation" = "sigma_est",
                  "MSE"="MSE", "KGE"="KGE", "NSE"="NSE", 
                  "NSE_A"="NSE_A", "NSE_B"="NSE_B", "NSE_C"="NSE_C")
basin_layer.info <- read.table("AppDATA/info_stations.txt", sep=",", header=TRUE,
                               colClasses = c("siteID"="character"))

list.basin_characters <- c("lat","lon","DA","Precip_mmperyear","PET_mmperyear","AI",
                           "dams","NID_str","Mx_strg","Nrml_st")

list.hucs <- 1:18
names(list.hucs) <- paste0("huc",sprintf("%02d",1:18))

label.siteID_names <- basin_layer.info$siteID
names(label.siteID_names) <- paste(basin_layer.info$siteID, basin_layer.info$name)

navbarPage("Online NWMv2.0 Performance Analysis Tool", id="nav",


################# THE MAP #################
   tabPanel("Interactive map",
        div(class="outer",
            tags$head(
                includeCSS("styles.css")
            ),
            
            leafletOutput("map", width="100%", height="100%"),
            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                          draggable = FALSE, top = 65, right = 0 , width = 300, #bottom = 10,
                          #width = "25%", height = "90%",
                          
                          #h5("1) Select a station from the map"),
                          selectInput("assessment", h5("Performance Assessment"), choices = list.assessments,
                                      selected = list.assessments[1]),
                          selectInput("metric", h5("Verification Metric"), choices=list.metrics,
                                      selected = list.metrics[1]),
                          checkboxGroupInput("station_class", "", c("Natural flows (583)" = "natural","Controlled flows (2884)" = "non-natural"),
                                             selected = c("natural","non-natural"))
                          ),
            
            absolutePanel(id = "legend-controls", class = "panel panel-default", fixed = FALSE,
                          draggable = TRUE, top = 90, left = 11 , width = 180, height = 170,
                          selectInput("colors", "Color Scheme",
                                      rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                                      , selected = "RdYlBu"),
                          numericInput("circlesize", "Marker Size", value = 2, min = 0, max = 5, step = 0.1)
                          )
            ),

            tags$div(id="cite",
                     tags$em(' Online NWMv2.0 Performance Analysis Tool; 2020 ')
            )
   ),



################# SINGLE STATION TAB #################
tabPanel("Single Site Analyses",
         fluidRow(
            column(10,
                   selectizeInput("tab3_siteID",label = h4("select a station"),
                                  choices=label.siteID_names, options= list(maxOptions = 4000), width = 580, )
            )
         ),
      
         h3("Basic Info:"),
         tableOutput("tab3_info1"),
         tableOutput("tab3_info11"),
         dygraphOutput("tab3_dygraph_flows"),
         br(),
         h4("Overall Performance of NWMv2.0 flows (1993-2018)"),
         tableOutput("tab3_info2"),
         headerPanel(""),
         
         fluidRow(
         column(3, offset = 0, style='padding-top:0px; padding-bottom:0px;',
                selectInput("tab3_plot_assessment", h5("Assessment Type"), choices = c("Annual","Seasonal","Monthly"),
                            selected = "Annual")),
         column(3,
                selectInput("tab3_plot_metric", h5("Verification Metric"), choices=list.metrics,
                            selected = list.metrics[1]))
         ),

         tabsetPanel(
            tabPanel("Boxplot",
                     br(), br(),
                     plotlyOutput("tab3_plot"),
                     br()),
            
            tabPanel("Detailed Data",
                     br(),
                     tableOutput("tab3_table"),
                     br())
         )
),



################# REGIONAL ANALYSES TAB #################
tabPanel("Regional Analyses",
         sidebarLayout(
            sidebarPanel(selectizeInput("RA_huc", h5("HUC Region(s)"), choices = list.hucs, selected = list.hucs[1],
                                        multiple = TRUE, options = list('plugins' = list('remove_button'),
                                                                        'create' = TRUE, 'persist' = FALSE)),
                         checkboxGroupInput("RA_station_class", "", c("Natural flows" = "natural","Controlled flows" = "non-natural"),
                                            selected = c("natural","non-natural")),
                         selectInput("RA_assessment", h5("Assessment"), choices = list.assessments,
                                     selected = list.assessments[1]),
                         selectInput("RA_xvar", h5("x variable"), choices = list.basin_characters[-(1:2)],
                                     selected = "DA"),
                         checkboxInput("RA_xvar_log", h5("x in log space"), value = TRUE),
                         selectInput("RA_yvar", h5("y variable"), choices = list.metrics,
                                     selected = "NSE_A"),
                         checkboxInput("RA_yvar_log", h5("y in log space"), value = FALSE),
                         hr(), tags$hr(style="border-color: black;"),
                         h5('Fit a Linear Regression Model to "y variable"'),
                         selectizeInput("lm_inputs", h5("additional predictors (besides x variable)"), choices = list.basin_characters[-(1:2)], selected = NULL,
                                        multiple = TRUE, options = list('plugins' = list('remove_button'),
                                                                        'create' = TRUE, 'persist' = FALSE))
                         
            ),
            mainPanel(
               br(),
               plotlyOutput("RA_plot", height="500px"),
               #downloadButton("download_RA_data", label = "Download Data")
               br(),
               h3("Summary of Linear Regression Model:"),
               verbatimTextOutput("lm_summary")
            )
         )
),


################# DOCUMENTATION TAB #################
   tabPanel("Documentation",
            tags$div(
               tags$br(),
               tags$em(tags$h1("Data Description")),
               tags$h4("Observed streamflow data is obtained from USGS that maintains and operates a network of observation stations that regularly monitor and report streamflow conditions to the National Water Information System (NWIS). In total there are ~7000 active gages. While an impressive observation network, these gages only monitor ~0.25% of the 2,700,000 river segments digitized in the National Hydrography Dataset (NHDPlusV2)."),
               #tags$br(),
               tags$h4("Streamflow predictions are obtained form the National Water Model version 2.0 (NWMv2.0) which succeeded versions 1.0 (year 2016) and 1.2 (year 2017). With each new version, NCAR and NOAA produce a multi-decade reanalysis product with NLDAS  {Xia:2019tt} and NARR meteorological products to provide a historic hourly simulation of channel conditions and 3-hour simulations of land surface conditions. The raw output from these simulations are made available to the public on a 'best effort' basis in an Amazon S3 bucket. These reanalysis products are calibrated in a limited number of basins and do not assimilate NWIS observations in the same way that the operational models do. That said, the retrospective rainfall products provided by NLDAS decrease some of the uncertainty that would be evident in the operational product. ",tags$a(href="https://water.noaa.gov/about/nwm","https://water.noaa.gov/about/nwm")),
               #tags$br(),
               tags$h4("Basin boundaries were delinatied using the Network Linked Data Index (NLDI) to identify the coloacted NWIS and NHD identifiers and then the trace the upstream tributary along the NHDPlusV2 networks. The individual NHD catchments along the upstream flowpath were then unioned into a single basin boundary."),
               tags$hr(),
               tags$img(src="Assessment.png", width=1000),
               tags$br(),
               tags$img(src="MSE.png", width=1000),
               tags$br(),
               tags$img(src="NSE.png", width=1000),
               tags$br(),
               tags$img(src="KGE.png", width=1000)
            )
   ),
   
   conditionalPanel("false", icon("crosshair"))
)
