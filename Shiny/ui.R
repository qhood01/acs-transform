library(leaflet)
library(tidyverse)

cities <- c("New York","Boston","Seattle","Dallas")
headers <- read.csv("~/Dropbox/CensusViz/acsHeaders.csv",stringsAsFactors=F)
vars <- headers %>% filter(Label!="") %>% select(Label)

fillPage(title="ACS Data by City",
         titlePanel("ACS Data by City"),
         tags$head(
                  includeCSS("~/Dropbox/CensusViz/Shiny/styles.css")
              ),
         tabsetPanel(
             tabPanel("Map",
                      leafletOutput("map",height="650px"),
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 60, left = "auto",
                                    right = 20, bottom = "auto",
                                    width = 330, height = "auto",

                                    selectInput("city1", "City:",
                                                choices=cities),
                                    selectInput("var", "Indicator:",
                                                choices=vars),
                                    radioButtons("type","",
                                                 c("Number","Percent"),inline=T))),

             tabPanel("Upload",
                      selectInput("city2", "City:",
                                  choices=cities),
                      fileInput(inputId="shpFile", label="Upload Shapefiles (.shp, .prj, .shx & .dbf)", multiple=TRUE),
                      ##actionButton("prop", "Calculate Proportions"),
                      actionButton("create","Create Estimates"),
                      fluidRow(column(3,textOutput("perc",container=div))),
                      downloadButton("downloadData", "Download")

                      ##DT::dataTableOutput("table")
                      )
         )
)

