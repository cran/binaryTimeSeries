#' Creates a map and pie chart of the trajectories of the category of interest.
#' @param input is the results from running the "trajData" function.
#' @param pltunit is the unit in which the current map is plotted in, one of cm, m, km, in, ft, mi. or lat or lon.
#' @param dataEpsg is the projection of the current map. If extents are valid lat and lons,
#' the projection is assumed to be lat and lon (EPSG:4326), or Spherical Mercator otherwise.
#' @param categoryName is a character representing the name of the category of interest.Default is "marsh".
#' @param scalePos where to align the scale bar. One of "bottomleft", "bottomright", "topleft", or "topright".
#' @param narrowPos is a numeric value indicating the size of the north arrow.
#' @param narrowSize is a numeric value indicating the size of the north arrow.
#' @param xAxis is a character indicating label for the horizontal axis. default is "Longitude (m)".
#' @param yAxis is a character indicating label for the vertical axis. default is "Latitude (m)".
#' @param axisText is a numeric value controlling the size of the text on the horizontal and vertical ticks.
#' @param axisLabel is a numeric value controlling the size of the  horizontal and vertical labels
#' @param plotTitle is a numeric value controlling the size of the plot title.
#' @param legendTex is a numerical value controlling the size of the legend text.
#' @return The output from \code{\link{trajPlot}}
#' @export
#' @examples
#' example_data <- terra::rast(system.file("external/Example_raster_Y.tif",package="binaryTimeSeries"))
#' no_data <- 2
#' cat_interest <- 1
#' data_res <- c(1000,1000)
#' data_prj <- "+proj=utm +zone=32 +datum=WGS84 +ellps=GRS80  +units=m +no_defs"
#' unified_resp <- "yes"
#' trajdt_output <- trajData(x = example_data,nodata = no_data,
#' category = cat_interest,spres = data_res,datacrs = data_prj,unified = unified_resp)
#' trjplt_output <- trajPlot(input = trajdt_output)
#'
trajPlot <- function(input,
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
                     legendTex = 0.9){
  clon <- data.frame(matrix(9, nrow = 1,ncol = 3))
  clon$X2 <- "#ffffff"
  clon$X3 <- "No Data"
  names(clon) <- c("ID","myCol","cl")
  lengData <- input[[2]]
  lengData <- rbind(lengData,clon)
  op1 <- graphics::par(xpd = FALSE)
  on.exit(graphics::par(op1))
  op2 <- graphics::par(mai = c(1, 1, 1, 3) + 0.1)
  on.exit(graphics::par(op2))
  raster::plot((input[[1]]),
               col = as.character(input[[2]]$myCol),
               legend = F,
               colNA = "White",
               main = paste(categoryName, "trajectories","for",
                            input[[4]],"time points"),
               xlab = xAxis,
               ylab = yAxis,
               cex.axis =  axisText,
               cex.lab = axisLabel,
               cex.main = plotTitle)
  coord <- graphics::par("usr")
  op3 <- graphics::par(xpd = TRUE)
  on.exit(graphics::par(op3))
  graphics::legend(x = coord[2], y = coord[4],
                   title = "Trajectories",
                   legend = lengData$cl,
                   fill = as.character(lengData$myCol),
                   border = TRUE,
                   bty = "n",
                   cex = legendTex)
  prettymapr::addscalebar(pltunit,
                          plotepsg  = dataEpsg,
                          pos = scalePos)
  prettymapr::addnortharrow(pos = narrowPos,
                            scale = narrowSize)
  graphics::pie(input[[3]]$value,
                clockwise = TRUE,
                labels = "",
                col=as.character(input[[3]]$myCol ),
                main = "Size of trajectories",
                cex = plotTitle)
}
