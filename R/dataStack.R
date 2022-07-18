#' Creates results that serve as input for the stackbarPlot function.
#' @param x is the data, which must be a RasterStack,RasterBrick,SpatRaster or data frame.
#' @param category is the category of interest. The default is set to 1.
#' @param nodata is alphanumeric, which denotes no data in the data set.
#' @param unified is a string, which can be "yes" or "no" only. If "yes," the change is a percentage of a region's
#' unified area; else, the change is a percentage of the  entire region under consideration.
#' @param timePoints is a vector containing the time points under consideration.The default is  c(2000, 2001, 2002, 2003, 2005).
#' @param categoryName is a character representing the name of the category of interest.Default is "category"
#' @param regionName is a string or character the name of the study region.
#' @import dplyr
#' @return The output from \code{\link{dataStack}}
#' @export
#' @examples
#' example_data <- terra::rast(system.file("external/Example_raster_Y.tif",package="binaryTimeSeries"))
#' no_data <- 2
#' cat_interest <- 1
#' unified_resp <- "yes"
#' time_points <- c(2000,2001,2002,2003,2005)
#' categ_name <- "Category"
#' region_name <- "Study Region"
#' datstk_output <- dataStack(x = example_data,nodata = no_data,category = cat_interest,
#' unified = unified_resp,timePoints = time_points,categoryName = categ_name,
#' regionName = region_name)
dataStack <- function(x,
                      nodata = 2,
                      category = 1,
                      unified = "yes",
                      timePoints = c(2000,2001,2002,2003,2005),
                      categoryName = "marsh",
                      regionName = "Study Region"){
  if (!class(x) %in% c("RasterStack","RasterBrick","SpatRaster","data.frame")){
    stop("This function is intended for rasters and dataframes only",
         call. = FALSE)
  } else if (class(x) %in% ("data.frame")) {
    x <- x
  } else if (class(x) %in% c("RasterStack","RasterBrick","SpatRaster")){
    x <- as.data.frame(x,xy = TRUE)
  }
  input <- x
  input[input == nodata] <- NA
  input <- stats::na.omit(input)
  nCl <- length(input)
  dataNoxy <- input[, 3:nCl]
  n <- length(dataNoxy)
  dataNoxy[dataNoxy != as.numeric(category)] <- as.numeric(0)
  dataNoxy1 <- dataNoxy
  dataNoxy1[dataNoxy1 == as.numeric(category)] <- as.numeric(1)
  dataNoxy2<- dataNoxy1[rowSums(dataNoxy1[]) > 0,]
  dataNoxy3<- dataNoxy2[rowSums(dataNoxy2[]) < n,]
  if (unified == "no"){
    spatialExtent <- dataNoxy1
    stackTitle <- paste("Change in presence of",categoryName,"category where extent is",regionName)
  } else if(unified == "yes") {
    spatialExtent <- dataNoxy2
    stackTitle <- paste("Change in extent where any time point is",categoryName,"category in",regionName)
  }
  lengthSpext <- nrow(spatialExtent)
  seg1 <- dataNoxy3 %>% subset(dataNoxy3[, 1] == 1 & dataNoxy3[, n] == 0)
  clone <- data.frame(matrix(0, nrow = 1,ncol = length(seg1)))
  names(clone) <- names(seg1)
  seg1 <- seg1[-1] - seg1[-ncol(seg1)]
  seg1[seg1 == 1] <- NA
  seg1 <- subset(seg1,rowSums(seg1) == -1)
  clone2 <- data.frame(matrix(0, nrow = 1,ncol = length(seg1)))
  names(clone2) <- names(seg1)
  if (nrow(seg1) == 0){
    seg1 <- clone2
  }else{
    seg1 <- seg1
  }
  seg2 <- dataNoxy3 %>% subset(dataNoxy3[, 1] == 1 & dataNoxy3[, n] == 0)
  seg2 <- seg2[-1] - seg2[-ncol(seg2)]
  seg2[seg2 == -1] <- 2
  seg2 <- mutate(seg2,sum_2 = rowSums(seg2))
  seg2<- subset(seg2,seg2$sum_2 >=5)
  seg2 <- seg2[, c(1:n-1)]
  if (nrow(seg2) == 0){
    seg2 <- clone2
  }else{
    seg2 <- seg2
  }
  seg2[seg2 == 2] <- -1
  seg3 <- dataNoxy3 %>% subset(dataNoxy3[, 1] == 0 & dataNoxy3[, n] == 1)
  seg3<- seg3[-1] - seg3[-ncol(seg3)]
  seg3[seg3 == -1] <- NA
  seg3 <- subset(seg3,rowSums(seg3) == 1)
  if (nrow(seg3) == 0){
    seg3 <- clone2
  }else{
    seg3 <- seg3
  }
  seg4 <- dataNoxy3 %>% subset(dataNoxy3[, 1] == 0 & dataNoxy3[, n] == 1)
  seg4 <- seg4[-1] - seg4[-ncol(seg4)]
  seg4[seg4 == -1] <- 2
  seg4 <- mutate(seg4,sum_2 = rowSums(seg4))
  seg4 <- subset(seg4,seg4$sum_2 >=4)
  seg4 <- seg4[, c(1:n-1)]
  if (nrow(seg4) == 0){
    seg4 <- clone2
  }else{
    seg4 <- seg4
  }
  seg4[seg4 == 2] <- -1
  seg5 <- dataNoxy3 %>% subset(dataNoxy3[, 1] == 0 & dataNoxy3[, n] == 0 )

  if (nrow(seg5) == 0){
    seg5 <- clone
  }else{
    seg5 <- seg5
  }
  seg6 <- dataNoxy3 %>% subset(dataNoxy3[, 1] == 1 & dataNoxy3[, n] == 1)
  if (nrow(seg6) == 0){
    seg6 <- clone
  }else{
    seg6 <- seg6
  }
  seg1b <- seg1
  seg2b <- seg2
  seg3b <- seg3
  seg4b <- seg4
  seg5b<- seg5[-1] - seg5[-ncol(seg5)]
  seg6b <- seg6[-1] - seg6[-ncol(seg6)]
  inter_vals <- names(seg1b)
  gains <- c("gainYellow","gainYellow2",
             "gainLblue","gainLred",
             "gainDblue","gainDred")

  #Loss
  lossess <- c("lossYellow","lossYellow2",
               "lossLblue","lossLred",
               "lossDblue","lossDred")
  ls <- list(c("gain","loss"))
  seg1GainRed <- as.data.frame(colSums(seg1b == 1))
  seg2GainRed2 <- as.data.frame(colSums(seg2b == 1))
  seg3GainBlue <- as.data.frame(colSums(seg3b == 1))
  seg4GainBlue2 <- as.data.frame(colSums(seg4b == 1))
  seg5GainBrown <- as.data.frame(colSums(seg5b == 1))
  seg6GainBrown <- as.data.frame(colSums(seg6b == 1))
  seg1LossRed <- as.data.frame(colSums(seg1b == -1))
  seg2LossRed2 <- as.data.frame(colSums(seg2b == -1))
  seg3LossBlue <- as.data.frame(colSums(seg3b == -1))
  seg4LossBlue2 <- as.data.frame(colSums(seg4b == -1))
  seg5LossBrown <- as.data.frame(colSums(seg5b == -1))
  seg6LossBrown <- as.data.frame(colSums(seg6b == -1))
  inet_time <- c(names(seg4GainBlue2))
  gainDf <-as.data.frame(c(seg6GainBrown,seg5GainBrown,
                           seg4GainBlue2,seg2GainRed2,seg3GainBlue,
                           seg1GainRed))
  oldnames <- names(gainDf)
  newnames <- gains
  gainDf2 <- gainDf %>% rename_at(vars(all_of(oldnames)), ~ newnames)
  trans <- data.frame(cbind(timePoints[-length(timePoints)], timePoints[-1]))
  timeIntervals<- trans[-1] - trans[-ncol(trans)]
  trans$transitions <- paste(trans$X1,"-",trans$X2, sep = "")
  transitions <- trans$transitions
  gainDf3 <- cbind(timeIntervals,gainDf2)
  lossDf <-as.data.frame(c(seg6LossBrown,seg5LossBrown,
                           seg4LossBlue2, seg2LossRed2,seg3LossBlue,
                           seg1LossRed))
  oldnamesLoss <- names(lossDf)
  newnamesLoss <- lossess
  lossDf2 <- lossDf %>% rename_at(vars(all_of(oldnamesLoss)), ~ newnamesLoss)
  lossDf3 <- cbind(timeIntervals,lossDf2)
  gainStack <- round((gainDf3[-1]/(gainDf3$X2*lengthSpext))*100,2)
  gainStack$timeIntervals <- gainDf3$X2
  lossStack<- round((lossDf3[-1]*-1/(lossDf3$X2*lengthSpext))*100,2)
  lossStack$timeIntervals <- lossDf3$X2

  trajNames <- c("Presence\u2192Alternation\u2192Presence",
                 "Absence\u2192Alternation\u2192Absence",
                 "Absence\u2192Alternation\u2192Gain\u2192Presence",
                 "Presence\u2192Alternation\u2192Loss\u2192Absence",
                 "Absence\u2192Gain\u2192Presence",
                 "Presence\u2192Loss\u2192Absence",
                 "Time_intervals")
  oldnames <- names(gainStack)
  newnames <- trajNames
  gainStack2 <- gainStack %>% rename_at(vars(oldnames), ~ newnames)
  gainStack2b <- t( gainStack2[0:6])
  oldnames <- names(lossStack)
  newnames <- trajNames
  lossStack2 <- lossStack %>% rename_at(vars(oldnames), ~ newnames)
  lossStack2b <- t(lossStack2[0:6])
  lossgainStacked2b <- rbind(gainStack2b,lossStack2b)
  names(lossgainStacked2b) <- transitions
  mergLossGain <- rbind(gainStack2,lossStack2)
  trajOnly <- mergLossGain[1:6]
  maxGain <- max(trajOnly[trajOnly > 0])
  maxLoss <- max(abs(trajOnly[trajOnly < 0]))
  mergLossGain$interval_2 <- transitions
  mergLossGain3 <- mergLossGain[ , c("Time_intervals",
                       names(mergLossGain)[names(mergLossGain) != "Time_intervals"])]
  mergLossGain4 <- mergLossGain3[1:7]
  transLossGain4 <- t(mergLossGain4)
  transLossGain5 <- transLossGain4[- 1,]
  colnames(transLossGain5) <- c(mergLossGain$interval_2)
  meltLossGain5 <- reshape2::melt(transLossGain5)
  colnames(transLossGain5) <- c(mergLossGain$Time_intervals)
  meltLossGain5b <- reshape2::melt(transLossGain5)
  meltLossGain5$size <- meltLossGain5b$Var2
  prodGainLossInt <- meltLossGain5$value*meltLossGain5$size
  gainLine <- sum(prodGainLossInt[prodGainLossInt > 0])/(timePoints[[n]] - timePoints[[1]])
  lossLine <- sum(prodGainLossInt[prodGainLossInt < 0])/(timePoints[[n]] - timePoints[[1]])
  net <- (gainLine + lossLine)
  if (net < 0){
    Net <- "Quantity Loss"
  } else if (net > 0){
    Net <- "Quantity Gain"
  } else{
    Net <- "Zero Quantity"
  }
  netAbs <- abs(net)
  firstTime <- dataNoxy3[1]
  lastTime <- dataNoxy3[n]
  diffLastFirst <- lastTime - firstTime
  sumLastFirst <- sum(abs(diffLastFirst))
  allocation <- (sumLastFirst * (100/lengthSpext)) / (timePoints[[n]] - timePoints[[1]]) - netAbs
  dfAlt <- dataNoxy3[-1] - dataNoxy3[-ncol(dataNoxy3)]
  dfAltSum <- sum(abs(dfAlt))
  alternation <- (dfAltSum * (100/lengthSpext)) /
    (timePoints[[n]] - timePoints[[1]]) - allocation - netAbs
  nameRegion <- c("Annual Change in region Y",
                  "Annual Change in region Y2",
                  "Annual Change in region Y3")
  compNames <- c(Net,"Allocation","Alternation")
  compVals <- c(netAbs,allocation,alternation)
  dfCompnents <- as.data.frame(compVals,nameRegion)
  dfCompnents$compNames <- compNames
  dfCompnents2 <- reshape2::melt(dfCompnents, id =  "compNames")
  trajNames2 <- c("Absence\u2192Alternation\u2192Absence",
                  "Presence\u2192Alternation\u2192Presence",
                  "Absence\u2192Alternation\u2192Gain\u2192Presence",
                  "Presence\u2192Alternation\u2192Loss\u2192Absence",
                  "Absence\u2192Gain\u2192Presence",
                  "Presence\u2192Loss\u2192Absence")
  trajCol <- c('#e6e600','#a8a803','#14a5e3',
               '#ff6666','#020e7a','#941004')
  nameCol1 <- as.data.frame(cbind(trajNames2,trajCol))
  trajNames3 <- mergLossGain[1:6][, colSums(mergLossGain[1:6] != 0) > 0]
  trajNames3 <- as.data.frame(names(trajNames3))
  names(trajNames3) <- "trajNames2"
  nameCol2 <- left_join(trajNames3,nameCol1,by = "trajNames2")
  return(list("Factor dataframe for trajectory stacke bar plot" = meltLossGain5,
              "Value of gain line" = gainLine,
              "Value of loss line" = lossLine,
              "Dataframe for components of change" = dfCompnents2,
              "Title of stackbar plot" = stackTitle,
              "Size of net component" = Net,
              "Name of category of ineterst" = categoryName,
              "Dataframe for stackbar plot" = mergLossGain,
              "Colors and trajectories for stacked bars" = nameCol2))
}
