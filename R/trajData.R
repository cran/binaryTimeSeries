#' creates the data which serves as input for the "trajPlot" function.
#' @param x is the data, which must be a RasterStack,RasterBrick,SpatRaster or data frame.
#' @param nodata is alphanumeric, which denotes no data in the data set.
#' @param category is the category of interest. The default is set to 1.
#' @param spres is a 1*2 vector indicating the spatial resolution of the data. The default is c(1000,1000).
#' @param datacrs is the CRS of the input data.
#' @param unified is a string, which can be "yes" or "no" only. If "yes," the change is a percentage of a region's
#' unified area; else, the change is a percentage of the entire region under consideration.
#' @return The output from \code{\link{trajData}}
#' @export
#' @examples
#' example_data <- terra::rast(system.file("external/Example_raster_Y.tif",package="binaryTimeSeries"))
#' no_data <- 2
#' cat_interest <- 1
#' data_res <- c(1000,1000)
#' data_prj <- "+proj=utm +zone=32 +datum=WGS84 +ellps=GRS80  +units=m +no_defs"
#' unified_resp <- "yes"
#' trajdt_output <- trajData(x = example_data,nodata = no_data,
#' category = cat_interest,spres = data_res,datacrs = data_prj,
#' unified = unified_resp)
#'
trajData <- function(x,
                     nodata = 2,
                     category = 1,
                     spres = c(1000,1000),
                     datacrs = NULL,
                     unified = "yes"){
  if (!class(x) %in% c("RasterStack","RasterBrick","SpatRaster","data.frame")){
    stop("This function is intended for rasters and dataframes only", call. = FALSE)
  } else if (class(x) %in% ("data.frame")) {
    x <- x
  } else if (class(x) %in% c("RasterStack","RasterBrick","SpatRaster")){
    x <- as.data.frame(x,xy = TRUE)
  }
  x <- stats::na.omit(x)
  x$x <- as.character(x$x)
  x$y <- as.character(x$y)
  x <- x[apply(x!=nodata,1,all),]
  x$x <- as.numeric(x$x)
  x$y <- as.numeric(x$y)
  xy <- select(x,"x","y")
  nCl <- length(x)
  rasNoxy <- length(x[, 3:nCl])
  m <- rasNoxy + 1
  x[x != as.numeric(category)] <- as.numeric(0)
  input <- x
  input[input == as.numeric(category)] <- as.numeric(1)
  input <- cbind(xy,input[,c(2:m+1)])
  clone1 <- data.frame(matrix(0, nrow = 1,ncol = length(input[,1:3])))
  names(clone1) <- c("x","y","change")
  traj1 <- mutate(input,change = ifelse(rowSums(input[, c(1:rasNoxy + 2)]) == rasNoxy,7,0))
  traj1 <- subset(traj1,traj1$change == 7)
  traj1 <- subset(traj1, select=c("x", "y", "change"))
  if (nrow(traj1) == 0){
    traj1 <- clone1
  }else{
    traj1 <- traj1
  }
  traj2 <- mutate(input,change = ifelse(rowSums(input[, c(1: rasNoxy + 2)]) == 0,8,0))
  traj2 <- subset(traj2,traj2$change == 8)
  traj2 <- subset(traj2, select=c("x", "y", "change"))
  if (nrow(traj2) == 0){
    traj2 <- clone1
  }else{
    traj2 <- traj2
  }
  comp <- input %>% subset(input[, 3] == 1 & input[, rasNoxy + 2] == 0)
  if (nrow(comp) == 0){
    traj3 <- clone1
  }else{
    comp1 <- comp[1:rasNoxy + 2]
    comp2 <- comp1[-1] - comp1[-ncol(comp1)]
    comp2[comp2 == 1] <- NA
    comp3 <- cbind(comp[1:2],comp2)
    traj3 <- mutate(comp3,change = ifelse(rowSums(comp3[, c(3:m)]) == -1,1,0))
    traj3 <- subset(traj3,traj3$change == 1)
    traj3 <- subset(traj3,select=c("x", "y", "change"))
  }
  comp4 <- input %>% subset(input[, 3] == 1 & input[, rasNoxy + 2] == 0)
  if (nrow(comp4) == 0){
    traj4 <- clone1
  }else{
    comp5 <- comp4[1:rasNoxy + 2]
    comp6 <- comp5[-1] - comp5[-ncol(comp5)]
    comp6[comp6 == -1] <- 2
    comp7 <- cbind(comp4[1:2],comp6)
    traj4 <- mutate(comp7,change = ifelse(rowSums(comp7[, c(3:m)]) >= 5,2,0))
    traj4 <- subset(traj4,traj4$change == 2)
    traj4 <- subset(traj4,select=c("x", "y", "change"))
  }
  comp8 <- input %>% subset(input[, 3] == 0 & input[, rasNoxy + 2] == 1)
  if (nrow(comp8) == 0){
    traj5 <- clone1
  }else{
    comp9 <- comp8[1:rasNoxy + 2]
    comp10 <- comp9[-1] - comp9[-ncol(comp9)]
    comp10[comp10 == -1] <- NA
    comp11 <- cbind(comp8[1:2],comp10)
    traj5 <- mutate(comp11,change = ifelse(rowSums(comp11[, c(3:m)]) == 1,3,0))
    traj5 <- subset(traj5,traj5$change == 3)
    traj5 <- subset(traj5,select=c("x", "y", "change"))
  }
  comp12 <- input %>% subset(input[, 3] == 0 & input[, rasNoxy + 2] == 1)
  if (nrow(comp12) == 0){
    traj6 <- clone1
  }else{
    comp13 <- comp12[1:rasNoxy + 2]
    comp14 <- comp13[-1] - comp13[-ncol(comp13)]
    comp14[comp14 == -1] <- 2
    comp15 <- cbind(comp12[1:2],comp14)
    traj6 <- mutate(comp15,change = ifelse(rowSums(comp15[, c(3:m)]) >= 4,4,0))
    traj6 <- subset(traj6,traj6$change == 4)
    traj6 <- subset(traj6,select=c("x", "y", "change"))
  }
  comp16 <- input %>% subset(input[, 3] == 1 & input[, rasNoxy + 2] == 1)
  if (nrow(comp16) == 0){
    traj7 <- clone1
  }else{
    comp17 <- comp16[1:rasNoxy + 2]
    comp18 <- comp17[-1] - comp17[-ncol(comp17)]
    comp18[comp18 == -1] <- 2
    comp19 <- cbind(comp16[1:2],comp18)
    traj7 <- mutate(comp19,change = ifelse(rowSums(comp19[, c(3:m)]) >= 3,5,0))
    traj7 <- subset(traj7,traj7$change == 5)
    traj7 <- subset(traj7,select=c("x", "y", "change"))
  }
  traj8 <- mutate(input,change = ifelse(input[, 3] == 0 & input[, rasNoxy + 2] == 0 & rowSums(input[, c(3:m)]>0),6,0))
  traj8 <- subset(traj8,traj8$change == 6)
  traj8 <- subset(traj8,select=c("x", "y", "change"))
  if (nrow(traj1) == 0){
    traj8 <- clone1
  }else{
    traj8 <- traj8
  }
  trajectories <- rbind(traj3,traj4,traj5,traj6,traj7,traj8,traj1,traj2)
  trajectories<- filter(trajectories, trajectories$change != "0")
  ch <- trajectories[, ncol(trajectories), drop = FALSE]
  pixCount<- table(unlist(ch))
  dfr <- raster::rasterFromXYZ(trajectories,spres,crs = datacrs)
  dfr2 <- raster::as.factor(dfr)
  rat <- raster::levels(dfr2)[[1]]
  cl <-
    c(
      "Presence\u2192Loss\u2192Absence",
      "Presence\u2192Alternation\u2192Loss\u2192Absence",
      "Absence\u2192Gain\u2192Presence",
      "Absence\u2192Alternation\u2192Gain\u2192Presence",
      "Presence\u2192Alternation\u2192Presence",
      "Absence\u2192Alternation\u2192Absence",
      "Presence\u2192Stable\u2192Presence",
      "Absence\u2192Stable\u2192Absence"
    )
  myCol = c('#941004','#FF6666','#020e7a','#14a5e3','#a8a803','#E6E600','#666666','#c4c3c0')
  cl2 <-
    c(
      "Presence\u2192Loss\u2192Absence",
      "Presence\u2192Alternation\u2192Loss\u2192Absence",
      "Presence\u2192Alternation\u2192Presence",
      "Absence\u2192Alternation\u2192Absence",
      "Absence\u2192Gain\u2192Presence",
      "Absence\u2192Alternation\u2192Gain\u2192Presence",
      "Presence\u2192Stable\u2192Presence",
      "Absence\u2192Stable\u2192Absence"
    )
  myCol2 = c('#941004','#FF6666','#a8a803','#E6E600','#14a5e3','#020e7a','#666666','#c4c3c0')
  ID <- c(1,2,3,4,5,6,7,8)
  idmyColcl <- data.frame(ID,myCol,cl)
  df3 <- left_join(rat,idmyColcl , by = "ID")
  dfPie <- df3
  dfPie[["value"]] <- pixCount
  dfPie2 <- dfPie %>%slice(match(cl2, cl))
  updateddfpie <- subset(dfPie, ID!= 8)
  if (!unified %in% c("yes","no")) {
    stop("Input must be a yes or no string", call. = FALSE)
  } else if (unified == "yes") {
    updateddfPie <- subset(dfPie2, ID!= 8)
  } else if ( unified == "no") {
    updateddfPie <- dfPie2
  }
  return(list("Raster data for trajectory plot" = dfr,
              "Attribute data for trajectory plot" = df3,
              "Data for trajectory pie chart" = updateddfPie,
              "Number of time points" = rasNoxy,
              "Dataframe for trajectory" = trajectories))
}
