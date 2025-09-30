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
