#' Creates two maps: a map showing how many times the category is present during the time series and
#' a map showing how many times the category changes during the time series.
#' @param input is the results from running the "changeData" function.
#' @param pltunit is the unit which the current map is plotted in, one of cm, m, km, in, ft, mi. or lat or lon.
#' This parameter is optional if "dataEpsg" is passed.
#' @param dataEpsg is the projection of the current map. If extents are valid lat or lons,
#' the projection is assumed to be lat and lon (EPSG:4326), or Spherical Mercator otherwise (EPSG:3857).
#' This is done to work seamlessly with "OpenStreetMap" packages.currently it is set to 32632.
#' @param scalePos where to align the scale bar. One of "bottomleft", "bottomright", "topleft", or "topright".
#' @param narrowPos where to align the north arrow. One of "bottomleft", "bottomright", "topleft", or "topright".
#' @param narrowSize is a numeric value indicating the size of the north arrow.
#' @param categoryName is a character representing the name of the category of interest.Default is "marsh".
#' @param xAxis is a character indicating a label for the horizontal axis. The default is "Longitude (m)".
#' @param yAxis is a character indicating a label for the vertical axis.The default is "Latitude (m)".
#' @param axisText is a numeric value controlling the size of the text on the horizontal and vertical ticks.
#' @param axisLabel is a numeric value controlling the size of the  horizontal and vertical labels
#' @param plotTitle is a numeric value controlling the size of the plot title.
#' @return The output from \code{\link{presencePlot}}
#'@importFrom grDevices gray.colors
#' @export
#' @examples
#' example_data <- terra::rast(system.file("external/Example_raster_Y.tif",package="binaryTimeSeries"))
#' no_data <- 2
#' cat_interest <- 1
#' data_res <- c(1000,1000)
#' data_prj <- "+proj=utm +zone=32 +datum=WGS84 +ellps=GRS80  +units=m +no_defs"
#' chdata_output <- changeData(x = example_data,nodata = no_data,
#' category = cat_interest,spres = data_res,datacrs = data_prj)
#' ch_maps <- presencePlot(input = chdata_output,pltunit = "m",dataEpsg = 32632,
#' scalePos = "bottomleft",narrowPos = "topright",narrowSize = 1,
#' categoryName = "category",xAxis = "Horizontal (m)",yAxis = "Vertical (m)",
#' axisText = 1.2,axisLabel = 1.2,plotTitle = 1.5)
#'
presencePlot <- function(input,
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
                         plotTitle = 1.5){
  op <- graphics::par(mgp=c(1.5,1,0))
  on.exit(graphics::par(op))
  raster::plot(input[[1]],
               col = rev(grDevices::gray.colors(length(table(input[[2]])))),
               colNA = "White",
               main = paste("Number of times",categoryName,"is present during the temporal extent. White is NA"),
               xlab = xAxis,
               ylab = yAxis,
               cex.axis =  axisText,
               cex.lab = axisLabel,
               cex.main = plotTitle)
  prettymapr::addscalebar(pltunit,
                          plotepsg  = dataEpsg,
                          pos = scalePos)
  prettymapr::addnortharrow(pos = narrowPos,
                            scale = narrowSize)
  raster::plot(input[[3]],
               col = rev(grDevices::gray.colors(length(table(input[[4]])))),
               colNA = "White",
               main = paste("Number of times",categoryName,"changes during the temporal extent. White is NA"),
               xlab = xAxis,
               ylab = yAxis,
               cex.axis =  axisText,
               cex.lab = axisLabel,
               cex.main = plotTitle)
  prettymapr::addscalebar(pltunit,
                          plotepsg  = dataEpsg,
                          pos = scalePos)
  prettymapr::addnortharrow(pos = narrowPos,
                            scale = narrowSize)
}
