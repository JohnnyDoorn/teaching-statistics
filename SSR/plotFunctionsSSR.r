library(plotrix)
plotTstatSamplingDistribution <- function(tVal = 0, myDF = 10, myNCP = 0, altHyp = "Yes", decision = "Nothing", alpha = 0.01, displayT = TRUE, yAxisMod = 1) {
  
  thisSD <- 1
  xVals <- seq(-5, 5, length.out = 1e3)
  
  halfAlpha <- as.numeric(alpha)/2
  
  leftAbLineLoc <- qt(halfAlpha, df = myDF, ncp = 0, lower.tail = TRUE)
  rightAbLineLoc <- qt(1-halfAlpha, df = myDF, ncp = 0, lower.tail = TRUE)
  leftArea <- round(pt(leftAbLineLoc, myDF, ncp = myNCP), 3)
  rightArea <- round(pt(rightAbLineLoc, myDF, ncp = myNCP, lower.tail = FALSE), 3)
  
  if (altHyp == "Yes") {
    lowerTail <- tVal < 0
    
    leftAbLineLoc <- qt(halfAlpha, df = myDF, ncp = 0, lower.tail = TRUE)
    rightAbLineLoc <- qt(1-halfAlpha, df = myDF, ncp = 0, lower.tail = TRUE)
    leftArea <- round(pt(leftAbLineLoc, myDF, ncp = myNCP), 3)
    rightArea <- round(pt(rightAbLineLoc, myDF, ncp = myNCP, lower.tail = FALSE), 3)
  } else if (altHyp == "Negative only"){
    
    leftAbLineLoc <- qt(as.numeric(alpha), df = myDF, ncp = 0, lower.tail = TRUE)
    rightAbLineLoc <- qt(1, df = myDF, ncp = 0, lower.tail = TRUE)
    leftArea <- round(pt(leftAbLineLoc, myDF, ncp = myNCP), 3)
    rightArea <- round(pt(rightAbLineLoc, myDF, ncp = myNCP, lower.tail = FALSE), 3)
    lowerTail <- TRUE
    
  } else if (altHyp == "Positive only"){
    
    leftAbLineLoc <- qt(1, df = myDF, ncp = 0, lower.tail = FALSE)
    rightAbLineLoc <- qt(as.numeric(alpha), df = myDF, ncp = 0, lower.tail = FALSE)
    leftArea <- round(pt(leftAbLineLoc, myDF, ncp = myNCP), 3)
    rightArea <- round(pt(rightAbLineLoc, myDF, ncp = myNCP, lower.tail = FALSE), 3)
    lowerTail <- TRUE
  }
  
  twoCols <- c("darkgreen", "purple")
  if (decision == "Nothing") {
    allCols <- rep("darkgreen", length(xVals))
  } else if (decision == "Reject H0") {
    allCols <- ifelse(xVals > leftAbLineLoc & xVals < rightAbLineLoc, "darkgreen", "purple")
  } else {
    allCols <- ifelse(xVals > leftAbLineLoc & xVals < rightAbLineLoc, "purple", "darkgreen")
  }
  
  par(cex = 1.2, cex.lab = 1.2)
  densValues <- dt(xVals, myDF, ncp = myNCP)
  
  plot(xVals, densValues,
       col  = allCols,
       type = "h",
       lwd = 2,
       las = 1,
       ylab = "Density",
       xlab = "T-Statistic",
       ylim = c(0, 0.5*yAxisMod),
       xlim = c(min(xVals), max(xVals)),
       bty = "n",
       # yaxt = 'n',
       main = paste0('Sampling Distribution\n(df = ', myDF,")")
  )
  
  
  if (decision != "Nothing") {
    abline(v = leftAbLineLoc, lwd = 2, lty = 2)
    abline(v = rightAbLineLoc, lwd = 2, lty = 2)
    
    text(-4, 0.45, leftArea, col = allCols[1], cex = 2)
    text(4, 0.45, rightArea, col = allCols[length(allCols)], cex = 2)
    
    text(0, 0.45, 1 - rightArea - leftArea, col = allCols[round(length(allCols)/2, 0)], cex = 2)
  }
  
  if (displayT) {
    twoSided <- altHyp == "Yes"
    arrows(x0 = tVal, x1 = tVal, y0 = 0, y1 = 0.4*yAxisMod, lwd = 4, col = "darkred")
    if(twoSided) {
      lowerTail <- tVal < 0
      exVals <- xVals[abs(xVals) > abs(tVal)]
    } else if (altHyp == "Negative only"){
      lowerTail <- TRUE
      exVals <- xVals[xVals < tVal]
    } else {
      lowerTail <- FALSE
      exVals <- xVals[xVals > tVal]
    }
    thisP <- round(pt(tVal, df = myDF, lower.tail = lowerTail), 3) * (2 - 1 * !twoSided)
    text(tVal, 0.45*yAxisMod, paste0("p = ",thisP), cex = 1.2)
    lines(exVals, dt(exVals, myDF, ncp = myNCP), type = "h")
  }
}


# plotTstatSamplingDistribution(tVal = 2)



plotSumSquares <- function(data, sumSq = "Total", stats = NULL, plotMean = TRUE, whatDisplay = NULL, whatPred = "Mean", myLimY = c(0, 10)) {
  nGroups <- length(levels(data$group))
  myCols <- palette.colors(n = nGroups+1, palette = "Okabe-Ito")
  # Darkening the colors
  darkCols <- sapply(myCols, darken_color)
  plot(data$dv, col = "black" , pch = 21, bg = myCols[as.numeric(data$group)+1], cex = 1.8, lwd = 3, las = 1, bty = "n", 
       ylab = "Score", xlab = "Participant Nr.", xlim = c(0, length(data$dv)), ylim = myLimY, cex.lab = 1.3, cex.axis=1.3)
  
  totN <- length(data$dv)
  # Calculate the grand mean
  grandMean <- mean(data$dv)
  
  # Calculate the group mean
  groupMeans <- tapply(data$dv, data$group, mean)
  groupN <- tapply(data$dv, data$group, length)[1]
  
  # Decompose distances into model error and predictive accuracy
  nulModelError <- data$dv - grandMean
  altModelError <- data$dv - groupMeans[as.numeric(data$group)]
  modelAccuracy <- groupMeans[as.numeric(data$group)] - grandMean
  
  modelMS <- (sum(modelAccuracy^2) / (nGroups - 1) ) # model variance
  errorMS <- (sum(altModelError^2) /  (totN - nGroups))  # error variance
  totalMS <- (sum(nulModelError^2) / (totN - 1)) # total variance
  
  expVar <- (sum(modelAccuracy^2)) / (sum(nulModelError^2))
  fullModMeanSquareError <- round(stats[['Error.Sum.of.Squares']] / (totN - (nGroups)), 3)
  
  if (sumSq == "Total") {
    abline(h = mean(data$dv), lwd = 3, col = "purple")
    if ("Segments" %in% whatDisplay) {
      segments(x0 = data$pp, x1 = data$pp, y0 = mean(data$dv), y1 = data$dv, lwd = 2)
      if ("Sums" %in% whatDisplay) {
        mtext(paste0("Total Sum of Squares = ", round(sum(nulModelError^2), 2)), cex = 1.8)
      } else {
        mtext("", cex = 1.8)
      }
    }
  } 
  
  if (whatPred == "Group means") {
    predPoints <- groupMeans[data$group]
    modSumSquares <- stats[['Model.Sum.of.Squares']]
    dfMod <- nGroups - 1
    dfError <- totN - (nGroups)
  } else if (whatPred == "Mean") {
    predPoints <- rep(grandMean, totN)
    modSumSquares <- 0
    dfMod <- totN - (1 + nGroups)
    dfError <- 1
  }
  
  predPoints <- groupMeans[data$group]
  
  if (sumSq == "Model") {
    abline(h = mean(data$dv), lwd = 3, col = "purple")
    for (i in 1:nGroups) {
      ablineclip(x1 = which(data$group == levels(data$group)[i])[1]-0.65,
                 x2 = which(data$group == levels(data$group)[i])[groupN]+0.65,
                 h = groupMeans[i], lwd = 7, col = "black")
      ablineclip(x1 = which(data$group == levels(data$group)[i])[1]-0.5,
                 x2 = which(data$group == levels(data$group)[i])[groupN]+0.5,
                 h = groupMeans[i], lwd = 3, col = myCols[i+1])
    }
    # points(x = data$pp, y = predPoints, pch = 23, bg = darkCols[as.numeric(data$group)+1], col = "black", cex = 1.35)
    
    if ("Segments" %in% whatDisplay) {
      segments(x0 = data$pp, x1 = data$pp, y0 = predPoints, y1 = mean(data$dv), lwd = 2, col = myCols[as.numeric(data$group)+1])
      if ("Sums" %in% whatDisplay) {
        if (whatPred == "Mean" | !("F-stat" %in% whatDisplay)) {
          mtext(paste0("Model Sum of Squares = ", round(modSumSquares, 3)), cex = 1.4, line = 0)
        } else {
          mtext(paste0("Model Sum of Squares = ", round(modSumSquares, 3), 
                       "\nMean Square = ", round(modSumSquares/dfMod, 3), 
                       "\nF = ",round(modSumSquares/dfMod, 3),"/",fullModMeanSquareError, 
                       " = ", round(round(modSumSquares/dfMod, 3)/fullModMeanSquareError, 3)) , cex = 1.4, line = 0)
        }
      } else {
        mtext("Model improvement", cex = 1.8)
      }
    }
  }
  
  
  if (sumSq == "Error") {
    for (i in 1:nGroups) {
      ablineclip(x1 = which(data$group == levels(data$group)[i])[1]-0.65,
                 x2 = which(data$group == levels(data$group)[i])[groupN]+0.65,
                 h = groupMeans[i], lwd = 7, col = "black")
      ablineclip(x1 = which(data$group == levels(data$group)[i])[1]-0.5,
                 x2 = which(data$group == levels(data$group)[i])[groupN]+0.5,
                 h = groupMeans[i], lwd = 3, col = myCols[i+1])
    }
    
    # points(x = data$pp, y = predPoints, pch = 23, bg = darkCols[as.numeric(data$group)+1], col = "black", cex = 1.35)
    if ("Segments" %in% whatDisplay) {
      segments(x0 = data$pp, x1 = data$pp, y0 = predPoints, y1 = data$dv, lwd = 2)
      totSumSquares <- round(stats[['Total.Sum.of.Squares']] - modSumSquares, 3)
      if ("Sums" %in% whatDisplay) {
        if (whatPred == "Mean" | !("F-stat" %in% whatDisplay)) {
          mtext(paste0("Error Sum of Squares = ", totSumSquares), cex = 1.4)
        } else {
          mtext(paste0("Error Sum of Squares = ", totSumSquares, "\n Mean Square = ", round(totSumSquares/dfError, 3)), cex = 1.4)
        }
      } else {
        mtext("Model error", cex = 1.8)
      }
    }
  }
  
}

darken_color <- function(color, factor = 3) {
  col_rgb <- col2rgb(color)
  darkened_rgb <- col_rgb / factor
  rgb(darkened_rgb[1,], darkened_rgb[2,], darkened_rgb[3,], maxColorValue = 255)
}
