library(tidyverse)
library(rgdal)
library(geosphere)
library(rgeos)
library(sf)
library(lwgeom)

headers <- read.csv("~/Dropbox/CensusViz/acsHeaders.csv")
options(warn=-1)

district_proportions <- function(districts, tracts, id.districts) {
    if (is.na(proj4string(districts))) {
        stop("Distirct shapefile does not have a valid projection")
    }
    if (is.na(proj4string(tracts))) {
        stop("Tract shapefile does not have a valid projection")
    }
    districts <- st_make_valid(st_as_sf(districts))
    tracts <- st_make_valid(st_as_sf(tracts))
    tracts <- st_transform(tracts, "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96")
    districts <- st_transform(districts, "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96")
    totalArea <- sum(st_area(districts))
    propMatrix <- matrix(0,nrow=nrow(districts),ncol=nrow(tracts))
    areaCovered <- 0
    for(i in 1:nrow(tracts)){
        myDenominator <- st_area(tracts[i,])
        print(paste0(round(i/nrow(tracts)*100,1),"% Done"))
        for(j in 1:nrow(districts)) {
            thisIntersection <- st_intersection(tracts[i,],districts[j,])
            if(nrow(thisIntersection)==0) {
                myNumerator <- 0
                propMatrix[j,i] <- 0
            } else {
                myNumerator <- as.numeric(st_area(thisIntersection))
                areaCovered <- areaCovered+myNumerator
                propMatrix[j,i] <- myNumerator/myDenominator
            }
        }
    }
    colnames(propMatrix) <- tracts[["GEOID10"]]
    rownames(propMatrix) <- districts[[id.districts]]
    perc <- areaCovered/totalArea*100
    return(list(df=propMatrix,areaPerc=perc))
}

povertyCols <- c('HC01_EST_VC01','HC02_EST_VC01','HC01_EST_VC03','HC02_EST_VC03','HC01_EST_VC07','HC02_EST_VC07','HC01_EST_VC37','HC02_EST_VC37','HC01_EST_VC38','HC01_EST_VC41','HC01_EST_VC46','HC01_EST_VC47','HC01_EST_VC48','HC01_EST_VC49','HC01_EST_VC52','HC01_EST_VC53','HC01_EST_VC54','HC01_EST_VC55','HC01_EST_VC56','HC01_EST_VC57','HC01_EST_VC58','HC01_EST_VC59')

raceAgeSexCols <- c('HC01_VC03','HC01_VC04','HC01_VC05','HC01_VC08','HC01_VC09','HC01_VC10','HC01_VC11','HC01_VC12','HC01_VC13','HC01_VC14','HC01_VC15','HC01_VC16','HC01_VC17','HC01_VC18','HC01_VC19','HC01_VC20','HC01_VC49','HC01_VC50','HC01_VC51','HC01_VC56','HC01_VC69','HC01_VC70','HC01_VC88','HC01_VC94','HC01_VC95','HC01_VC96','HC01_VC97','HC01_VC99','HC01_VC100')

educAttCols <- c('HC01_EST_VC02','HC01_EST_VC03','HC01_EST_VC04','HC01_EST_VC05','HC01_EST_VC06','HC01_EST_VC08','HC01_EST_VC09','HC01_EST_VC10','HC01_EST_VC11','HC01_EST_VC12','HC01_EST_VC13','HC01_EST_VC14','HC01_EST_VC15')

educAttOver25Cols <- c('HD01_VD01','HD01_VD02','HD01_VD03','HD01_VD04','HD01_VD05','HD01_VD06','HD01_VD07','HD01_VD08','HD01_VD09','HD01_VD10','HD01_VD11','HD01_VD12','HD01_VD13','HD01_VD14','HD01_VD15','HD01_VD16','HD01_VD17','HD01_VD18','HD01_VD19','HD01_VD20','HD01_VD21','HD01_VD22','HD01_VD23','HD01_VD24','HD01_VD25')

ownOccHouCols <- c('HC01_EST_VC01','HC02_EST_VC01','HC03_EST_VC01')

district_populations <- function(districtProp, city) {
    path <- paste0("~/Dropbox/CensusViz/Data/",city,"/ACS/")
    poverty <- read.csv(paste0(path,"ACS_16_5YR_S1701_with_ann.csv"),stringsAsFactors=F)[-1,c("GEO.id2",povertyCols)]
    raceAgeSex <- read.csv(paste0(path,"ACS_16_5YR_DP05_with_ann.csv"),stringsAsFactors=F)[-1,c("GEO.id2",raceAgeSexCols)]
    educAtt <- read.csv(paste0(path,"ACS_16_5YR_S1501_with_ann.csv"),stringsAsFactors=F)[-1,c("GEO.id2",educAttCols)]
    educAttOver25 <- read.csv(paste0(path,"ACS_16_5YR_B15003_with_ann.csv"),stringsAsFactors=F)[-1,c("GEO.id2",educAttOver25Cols)]
    ownOccHou <- read.csv(paste0(path,"ACS_16_5YR_S2502_with_ann.csv"),stringsAsFactors=F)[-1,c("GEO.id2",ownOccHouCols)]

    cityCensus <- list(poverty,raceAgeSex, educAtt, educAttOver25, ownOccHou) %>% reduce(left_join, by = "GEO.id2")

    cityCensus[,-1] <- sapply(cityCensus[,-1],as.numeric)
    cityCensus <- cityCensus[match(colnames(districtProp),cityCensus$GEO.id2),]
    cityCensus[is.na(cityCensus)] <- 0
    cityDistrictPop <- as.data.frame(districtProp %*% as.matrix(cityCensus[,-1]))
    colnames(cityDistrictPop) <- headers$New.Column
    return(cityDistrictPop)
}
