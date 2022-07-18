## ----setup,include = FALSE,warning=FALSE--------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width=8.5, 
  fig.height=6,
  comment = "#>"
)
# Make sure you load all the libraries below before you start using the package.
library(binaryTimeSeries)
library(ggnewscale)
library(reshape2)
library(raster)
library(terra)
library(dplyr)
library(maptools) 

## ---- eval=TRUE, warning=FALSE------------------------------------------------
# Read the raster files from "externa"l folder as terra raster format.
rasstackY <- terra::rast(system.file("external/Example_raster_Y.tif",package="binaryTimeSeries"))

#NB: The package comes with "Example_Data_Y.tif", "Example_Data_Y.cvs", "Example_raster_X.tif", and "Example_Data_X.cvs."

# Set the spatial resolution of the data. 
datares <- c( 1000, 1000)

# Set the crs of the data.
datprj <-"+proj=utm +zone=32 +datum=WGS84 +ellps=GRS80  +units=m +no_defs"


## ---- eval=TRUE, warning=FALSE------------------------------------------------
# Create input data for the presencePlot function and assign the results to a variable, e.g., "a."
a <- changeData (rasstackY,
                 nodata = 2,
                 category = 1,
                 spres = datares,
                 datacrs = datprj)
a

## ---- eval=TRUE, warning=FALSE------------------------------------------------
presencePlot (input = a,
              pltunit = "m",
              dataEpsg = 32632,
              scalePos = "bottomleft",
              narrowPos = "topright",
              narrowSize = 1,
              categoryName = "marsh",
              xAxis = "Longitude (m)",
              yAxis = "Latitude (m)",
              axisText = 1.2,
              axisLabel = 1.2,
              plotTitle = 1)
              

## ---- eval=TRUE, warning=FALSE------------------------------------------------
# Create input data for the trajPlot function and assign the results to a variable, e.g., "b."
b <- trajData(x = rasstackY,
             nodata = 2,
             category = 1,
             spres = datares,
             datacrs = datprj,
             unified = "yes")# if "no", the analysis considers the entire study region.

b

## ---- eval=TRUE, warning=FALSE, echo=FALSE------------------------------------
trajPlot(input = b,
         pltunit = "m",
         dataEpsg = 32632,
         categoryName = "marsh's",
         scalePos = "bottomleft",
         narrowPos = "topright",
         narrowSize = 1,
         xAxis = "Longitude (m)",
         yAxis = "Latitude (m)",
         axisText = 1.2,
         axisLabel = 1.4,
         plotTitle = 1.5,
         legendTex = 0.9)


## ---- eval=TRUE, warning=FALSE------------------------------------------------
# Create a vector variable containing the time points
timepoints <- c(2000,2001,2002,2003,2005)

# Use the dataStack function to create input for the stackbarPlots function.
c <- dataStack(x = rasstackY,
               category = 1,
               nodata = 2,
               unified = "yes",
               timePoints = timepoints,
               categoryName = "marsh",
               regionName = "extent")
c

## ---- eval=TRUE, warning=FALSE, echo=FALSE------------------------------------
# Create the stacked bars 
stackbarPlots(input = c,
              axisSize = 10,
              lbAxSize = 12,
              lgSize = 12,
              titleSize = 12,
              datbreaks = "no",
              upperlym = 60,
              lowerlym = - 50,
              lymby = 5,
              upperlym2 = 35,
              lymby2 = 5,
              xAngle = 0)


