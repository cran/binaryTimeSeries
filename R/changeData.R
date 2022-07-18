#' Creates a list containing the raster data sets concerning the number
#' of times the category of interest is present and the number of times
#' the category of interest changes during the time series.
#' @param x is the data, which must be a RasterStack,RasterBrick,SpatRaster or
#' data frame.
#' @param nodata is alphanumeric, which denotes no data in the data set.
#' @param category is the category of interest. The default is set to 1.
#' @param spres is a 1*2 vector indicating the spatial resolution of the data.
#' The default is set to c(1000,1000).
#' @param datacrs is the Coordinate Reference System (CRS) of the input data.
#' @return The output from \code{\link{changeData}}
#' @importFrom raster raster
#' @import dplyr
#' @import utils
#' @import magrittr
#' @import mice
#' @import rgdal
#' @importFrom terra rast
#' @export
#' @examples
#' example_data <- terra::rast(system.file("external/Example_raster_Y.tif",package="binaryTimeSeries"))
#' no_data <- 2
#' cat_interest <- 1
#' data_res <- c(1000,1000)
#' data_prj <- "+proj=utm +zone=32 +datum=WGS84 +ellps=GRS80  +units=m +no_defs"
#' chdata_output <- changeData(x = example_data,nodata = no_data,
#' category = cat_interest,spres = data_res,datacrs = data_prj)
#'
changeData <- function(x,nodata,category,spres,datacrs = NULL){
  if(!class(x) %in% c("RasterStack","RasterBrick","SpatRaster","data.frame")) {
    stop("This function is intended for rasters and dataframes only", call. = FALSE)
  } else if (class(x) %in% c("RasterStack","RasterBrick")) {
    x[x == nodata] <- NA
    x1 <- sum(x)
    x <- raster::mask(x,x1)
    x[x != category] <- 0
    x[x == category] <- 1
    rasSum2 <- sum(x)
    rasUniq <- raster::unique(rasSum2)
    rasDiff <- abs(raster::subset(x,2:raster::nlayers(x)) - raster::subset(x,1:(raster::nlayers(x)-1)))
    rasSum3 <- sum(rasDiff)
    rasUniq2 <- raster::unique(rasSum3)
    rasSum3 <- sum(rasDiff)
    rasUniq2 <- raster::unique(rasSum3)
  } else if (class(x) %in% "SpatRaster"){
    x[x == nodata] <- NA
    x1 <- sum(x)
    x <- raster::mask(x,x1)
    x[x != category] <- 0
    x[x == category] <- 1
    rasSum2 <- sum(x)
    rasUniq <- raster::unique(rasSum2)
    rasDiff <- abs(terra::subset(x,2:terra::nlyr(x)) - terra::subset(x,1:(terra::nlyr(x)-1)))
    rasSum3 <- sum(rasDiff)
    rasUniq2 <- raster::unique(rasSum3)
  } else if (class(x) %in% c("data.frame")){
    x[x == nodata] <- NA
    y <- stats::na.omit(x)
    xy <- y[, 1:2]
    nCl <- length(y[1, ])
    rasNoxy <- y[, 3:nCl]
    rasNoxy[rasNoxy != as.numeric(category)] <- as.numeric(0)
    rasNoxyBoolean<- rasNoxy
    rasNoxyBoolean[rasNoxyBoolean == as.numeric(category)] <- as.numeric(1)
    datChange <- mutate(presences = rowSums(rasNoxyBoolean))
    datChangexy <- cbind(xy,datChange$presences)
    names(datChangexy) <- c("x","y","presences")
    rastDf <- raster::rasterFromXYZ(datChangexy,spres,crs = datacrs)
    rasSum2 <- rast(rastDf)
    rasUniq <- unique(rastDf)
    rasDiff <- abs(rasNoxyBoolean[-1] - rasNoxyBoolean[-ncol(rasNoxyBoolean)])
    rasSum <- mutate(change = rowSums(rasDiff))
    rasxyChange <- cbind(xy,rasSum$change)
    names(rasxyChange) <- c("x","y","change")
    dfrast <-raster::rasterFromXYZ(rasxyChange,spres,crs = datacrs)
    rasSum3 <- rast(dfrast)
    rasUniq2 <- raster::unique(dfrast)
  }
  return(list("Data for number of presence" = rasSum2,
              "Data for unique number of presence" = rasUniq,
              "Data for number of changes" = rasSum3,
              "Data for unique number of changes" = rasUniq2))
}
