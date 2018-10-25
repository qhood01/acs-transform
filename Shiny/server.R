library(geosphere)
library(tidyverse)
library(htmlwidgets)
library(htmltools)
library(leaflet)
library(rgdal)
library(RColorBrewer)
library(DT)

source("helpers.R")

setwd("~/Dropbox/CensusViz/Data")
options(scipen=999)
headers <- read.csv("~/Dropbox/CensusViz/acsHeaders.csv",stringsAsFactors=F)
downloadDF <- NA

function(input, output) {

    uploadShpfile <- reactive({
        if (!is.null(input$shpFile)){
            shpDF <- input$shpFile
            prevWD <- getwd()
            uploadDirectory <- dirname(shpDF$datapath[1])
            setwd(uploadDirectory)
            for (i in 1:nrow(shpDF)){
                file.rename(shpDF$datapath[i], shpDF$name[i])
            }
            shpName <- shpDF$name[grep(x=shpDF$name, pattern="*.shp")]
            shpPath <- paste(uploadDirectory, shpName, sep="/")
            setwd(prevWD)
            shpFile <- readOGR(shpPath)
            print("uploaded")
            return(shpFile)
        } else {
            return()
        }
    })

    df <- reactive({
        layer <- strsplit(list.files(paste0(input$city2,"/Shapefiles"),patter=".prj"),"[.]")[[1]][[1]]
        map <- readOGR(paste0(input$city2,"/Shapefiles"),layer=layer)
        id <- names(map@data)[unlist(lapply(map@data,function(x) length(unique(x)) == nrow(map@data)))][1]
        propList <- district_proportions(uploadShpfile(),map,id)
        output$perc <- renderText(paste0(round(propList$areaPerc,2),"% of uploaded file is covered by census tracts"))
        popDF <- district_populations(propList$df,input$city2)
        return(popDF)
    })

    observeEvent(input$create, {
        downloadDF <<- df()
    })

    output$downloadData <- downloadHandler(
        filename = function() {
            paste0(input$city2, ".csv")
        },
        content = function(file) {
            write.csv(downloadDF, file)
        }
    )

    output$map <- renderLeaflet({
        layer <- strsplit(list.files(paste0(input$city1,"/Shapefiles"),patter=".prj"),"[.]")[[1]][[1]]
        map <- readOGR(paste0(input$city1,"/Shapefiles"),layer=layer)
        map$lng <- centroid(map)[,1]
        map$lat <- centroid(map)[,2]
        if (names(map) %in% "ALAND10") {
            map <- map[map@data$ALAND10 != "0",]
        }
        lng <- map$lng[1]
        lat <- map$lat[2]
        table <- as.character(headers[headers$Label==input$var,"Table"][1])
        data <- read.csv(paste0("~/Dropbox/CensusViz/Data/",input$city1,"/ACS/ACS_16_5YR_",table,"_with_ann.csv"),stringsAsFactors=F)
        col <- as.character(headers[headers$Label==input$var,"Name"][1])
        if (input$type=="Percent" & input$var != "Total Population") {
            denominator <- as.character(headers[headers$Label==input$var,"Denominator"][1])
            print(denominator)
            mergedData <- data[,c("GEO.id2",col,denominator)]
            mergedData$VAR <- as.numeric(mergedData[[col]])/as.numeric(mergedData[[denominator]])*100
            mergedData <- mergedData[,c("GEO.id2","VAR")]
            names(mergedData) <- c("GEOID10","VAR")
        } else {
            mergedData <- data[,c("GEO.id2",col)]
            names(mergedData) <- c("GEOID10","VAR")
        }

        title <- ifelse(input$type=="Percent",paste0(input$var," %"),input$var)
        map@data <- left_join(map@data, mergedData)
        map@data$VAR <- as.numeric(map@data$VAR)

        colorScale <- colorNumeric("Reds",map@data$VAR)

        leaflet(map) %>%
            setView(lng=lng,lat=lat,zoom=11) %>%
            addTiles() %>%
            addPolygons(layerId=~GEOID10,
                        popup=~format(VAR,digits=1,big.mark=","),
                        color="black",
                        weight=2,
                        smoothFactor=0.5,
                        fillColor=~colorScale(VAR),
                        fillOpacity=0.5,
                        highlightOptions=highlightOptions(color = "white", weight = 3,
                                                          bringToFront = TRUE)) %>%
            addLegend("bottomright", pal=colorScale, values=~VAR,title=title)
    })

}
