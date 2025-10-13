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


plotSumSquaresCov <- function(data, sumSq = "Total", stats = NULL, plotMean = TRUE, whatDisplay = NULL, whatPred = "Mean", myLabY = "Score", myLimY = c(0, 10)) {
  
  nGroups <- nlevels((data$group))
  groupN <- tapply(data$dv, data$group, length)[1]
  totN <- nrow(data)
  groupMeans <- tapply(data$dv, data$group, mean)
  grandMean <- mean(data$dv)
  myFullMod <- lm(dv ~ group + cov, data = data)
  myCovMod <- lm(dv ~ cov, data = data)
  
  predictedOnGroup <- data[["predictedOnGroup"]] <-  groupMeans[as.numeric(data$group)]
  predictedOnCov <- data[["predictedOnCov"]] <- myCovMod$fitted.values
  predictedFull <- data[["predictedFull"]] <- myFullMod$fitted.values
  
  fullModelError <- myFullMod$residuals
  
  sumSquareFull <- sum((predictedFull - grandMean)^2)
  sumSquareCov <- sum((predictedOnCov - grandMean)^2)
  sumSquareGroup <- sum((predictedOnGroup - grandMean)^2)
  
  errorMS <- (sum(fullModelError^2) /  (totN - nGroups - 1))  # error variance
  groupModelMS <- (sumSquareGroup / (nGroups - 1)) 
  covModelMS <- (sumSquareCov / 1)
  nulModelError <- data$dv - grandMean
  
  finalSumSquareCov <- sumSquareFull - sumSquareGroup
  finalSumSquareGroup <- sumSquareFull - finalSumSquareCov
  
  dfGroup <- nGroups -1
  dfCov <- 1
  dfError <-  (groupN*nGroups) - (nGroups + 1)
  
  stats <- data.frame('Total Sum of Squares' = sum(nulModelError^2),
                          'Group Sum of Squares' = finalSumSquareGroup,
                          'Cov Sum of Squares' = finalSumSquareCov,
                          'Full Model Sum of Squares' = sumSquareFull,
                          'Error Sum of Squares' = sum(fullModelError^2),
                          'Group df' = dfGroup,
                          'Cov df' = 1,
                          'Error df' = dfError,
                          'Group Mean Squares' = finalSumSquareGroup / dfGroup,
                          'Cov Mean Squares' = finalSumSquareCov,
                          'Error Mean Squares' = sum(fullModelError^2) / dfError,
                          'F Group' = (finalSumSquareGroup / dfGroup) / (sum(fullModelError^2) / dfError),
                          'F Cov' = (finalSumSquareCov / dfCov) / (sum(fullModelError^2) / dfError)
  )
  
  
  myCols <- palette.colors(n = nGroups+1, palette = "Okabe-Ito")
  darkCols <- sapply(myCols, darken_color)
  
  plot(data$dv, col = "black" , pch = 21, bg = myCols[as.numeric(data$group)+1], 
       cex = 1.8, lwd = 3, las = 1, bty = "n",
       ylab = myLabY, xlab = "Participant Nr.", xlim = c(0, length(data$dv)), 
       ylim = myLimY, cex.lab = 1.3, cex.axis=1.3)
  
  totN <- length(data$dv)
  # Calculate the grand mean
  grandMean <- mean(data$dv)
  
  # Calculate the group mean
  groupMeans <- tapply(data$dv, data$group, mean)
  nulModelError <- data$dv - grandMean
  
  if (sumSq == "Total") {
    if (plotMean) {
      abline(h = mean(data$dv), lwd = 3, col = "purple")
    }
    if ("Segments" %in% whatDisplay) {
      segments(x0 = data$pp, x1 = data$pp, y0 = mean(data$dv), y1 = data$dv, lwd = 2)
      if ("Sums" %in% whatDisplay) {
        mtext(paste0("Total Sum of Squares = ", round(sum(nulModelError^2), 2)), cex = 1.4)
      } else {
        mtext("", cex = 1.8)
      }
    }
  } 
  
  if (whatPred == "Group means") {
    predPoints <- data$predictedOnGroup
    modSumSquares <- stats[['Group.Sum.of.Squares']]
    dfMod <- nGroups - 1
    dfError <- totN - (nGroups)
    
  } else if (whatPred == "Cov") {
    predPoints <- data$predictedOnCov
    modSumSquares <- stats[['Cov.Sum.of.Squares']]
    dfMod <- 1
    dfError <- totN - (1)
  } else if (whatPred == "Group means + cov") {
    predPoints <- data$predictedFull
    modSumSquares <- stats[['Full.Model.Sum.of.Squares']]
    dfMod <- nGroups
    dfError <- totN - (1 + nGroups)
  } else if (whatPred == "Mean") {
    predPoints <- rep(grandMean, totN)
    modSumSquares <- 0
    dfMod <- totN - (1 + nGroups)
    dfError <- 1
  }
  
  fullModMeanSquareError <- round(stats[['Error.Sum.of.Squares']] / (totN - (1 + nGroups)), 3)

  if (sumSq == "Model") {
    abline(h = mean(data$dv), lwd = 3, col = "purple")
    points(x = data$pp, y = predPoints, pch = 23, bg = darkCols[as.numeric(data$group)+1], col = "black", cex = 1.35)
    if ("Segments" %in% whatDisplay) {
      segments(x0 = data$pp, x1 = data$pp, y0 = predPoints, y1 = mean(data$dv), lwd = 2, col = myCols[as.numeric(data$group)+1])
      if ("Sums" %in% whatDisplay) {
        if (whatPred == "Mean" | !("F-stat" %in% whatDisplay)) {
          mtext(paste0("Model Sum of Squares = ", round(modSumSquares, 3), "\nMean Square = ", round(modSumSquares/dfMod, 3)), cex = 1.4, line = 0)
        } else {
          mtext(paste0("Model Sum of Squares = ", round(modSumSquares, 3), "\nMean Square = ", round(modSumSquares/dfMod, 3), 
                       "\nF = ",round(modSumSquares/dfMod, 3),"/",fullModMeanSquareError, " = ", round(round(modSumSquares/dfMod, 3)/fullModMeanSquareError, 3)) , cex = 1.4, line = 0)
        }
      } else {
        mtext("Model improvement", cex = 1.8)
      }
    }
  }
  
  
  if (sumSq == "Error") {
    # abline(h = mean(data$dv), lwd = 3, col = "purple")
    points(x = data$pp, y = predPoints, pch = 23, bg = darkCols[as.numeric(data$group)+1], col = "black", cex = 1.35)
    
    if ("Segments" %in% whatDisplay) {
      
      segments(x0 = data$pp, x1 = data$pp, y0 = predPoints, y1 = data$dv, lwd = 2, col = darkCols[as.numeric(data$group)+1])
      totSumSquares <- round(stats[['Total.Sum.of.Squares']] - modSumSquares, 3)
      if ("Sums" %in% whatDisplay) {
        mtext(paste0("Error Sum of Squares = ", totSumSquares, "\n Mean Square = ", round(totSumSquares/dfError, 3)), cex = 1.4)
      } else {
        mtext("Model error", cex = 1.8)
      }
    }
  } 
}


plotSumSquaresFactorial <- function(data, input, sumSq = "Total", stats = NULL, plotMean = TRUE, myMain = "",
                                    alcColors = NULL, speedSymbols = NULL, overlayFullPred = FALSE) {
  
  mydat <- read.csv("anova_alcohol_speed_daytime.csv")
  grandMean <- mean(mydat$accidents)
  nGroups <- 9
  myAlcMod <-  lm(accidents ~ alcohol, data = mydat)
  mySpeedMod <- lm(accidents ~ speed, data = mydat)
  myMainMod <- lm(accidents ~ alcohol + speed, data = mydat)
  myFullMod <- lm(accidents ~ alcohol * speed, data = mydat)
  
  
  plot(mydat$accidents, col = "black" , pch = 21, bg = "blue", 
       cex = 1.8, lwd = 3, las = 1, bty = "n",
       ylab = "Accidents", xlab = "Participant Nr.", xlim = c(0, length(mydat$accidents)), 
       ylim = c(0, 10), cex.lab = 1.3, cex.axis=1.3)
  totN <- nrow(mydat)
  
  if (input$whatPred == "Alcohol") {
    myMod <-  myAlcMod
    dfMod <- 2
    dfError <- totN - 3
    myCols <- palette.colors(n = 3, palette = "Okabe-Ito")
    myCols <- rep(palette.colors(n = 9, palette = "Okabe-Ito")[2:4], each = 60)
    if (sumSq != "Total")
      legend("topleft", c("noneA", "someA", 'highA'), fill = unique(myCols), col = unique(myCols), bty = "n")
    
    
  } else if (input$whatPred == "Speed") {
    myMod <- mySpeedMod
    dfMod <- 2
    predPoints <- mydat$predictedOnSpeed
    dfError <- totN - 3
    myCols <- palette.colors(n = 1, palette = "Okabe-Ito")
    myCols <- rep(palette.colors(n = 9, palette = "Okabe-Ito")[5:7], each = 20)
    if (sumSq != "Total")
      legend("topleft", c("noneS", "someS", 'highS'), fill = unique(myCols), col = unique(myCols), bty = "n")
    
  } else if (input$whatPred == "Alcohol + Speed") {
    myMod <- myMainMod
    dfMod <- 4
    predPoints <- mydat$predictedOnMain
    dfError <- totN - 6
    
    myCols <- rep(palette.colors(n = 9, palette = "Okabe-Ito"), each = 20)
    if (sumSq != "Total")
      legend("topleft", paste(rep(c("noneA", "someA", 'highA'), each =3), c("noneS", "someS", 'highS'), sep ="+"),
             fill = unique(myCols), col = unique(myCols), bty = "n")
    
  } else if (input$whatPred == "Alcohol + Speed + A:S") {
    myMod <- myFullMod
    dfMod <- 8
    predPoints <- mydat$predictedOnFull
    dfError <- totN - 9
    myCols <- rep(palette.colors(n = 9, palette = "Okabe-Ito")[2:8], each = 20)
    myCols <- rep(palette.colors(n = 9, palette = "Okabe-Ito"), each = 20)
    # if (sumSq != "Total")
    #   legend("topleft", paste(rep(c("noneA", "someA", 'highA'), each =3), c("noneS", "someS", 'highS'), sep ="+"),
    #          fill = unique(myCols), col = unique(myCols), bty = "n")
    # 
  } else if (input$whatPred == "Mean") {
    myMod <- lm(accidents ~ 1, data = mydat)
    predPoints <- rep(mean(mydat$accidents), totN)
    modSumSquares <- 0
    dfMod <- totN - (1 + 9)
    dfError <- 1
    myCols <- rep(palette.colors(n = 9, palette = "Okabe-Ito")[2], each = 180)
  }
  
  
  predPoints <- myMod$fitted.values
  modAccSumSquares <- sum((predPoints - mean(mydat$accidents))^2)
  modAccMeanSquare <- modAccSumSquares / dfMod
  modErrorSumSquares <- sum((predPoints - mydat$accidents)^2)
  
  fullModErrorSumSq <- sum(myFullMod$residuals^2)
  fullModAccSumSq <- sum((myFullMod$fitted.values - mean(mydat$accidents))^2)
  fullModMeanSquareError <- fullModErrorSumSq / (totN - 9)
  nulModError <- sum((mean(mydat$accidents) - mydat$accidents)^2)
  
  if (sumSq == "Total") {
    if (plotMean) {
      abline(h = mean(mydat$accidents), lwd = 3, col = "purple")
    }
    if ("Segments" %in% input$whatDisplay) {
      segments(x0 = mydat$subjects, x1 = mydat$subjects, y0 = mean(mydat$accidents), y1 = mydat$accidents, lwd = 2)
      if ("Sums" %in% input$whatDisplay) {
        mtext(paste0("Total Sum of Squares = ", round(nulModError, 2)), cex = 1.4)
      } else {
        mtext(myMain, cex = 1.8)
      }
    }
  } 
  
  
  if (sumSq == "Model") {
    abline(h = mean(mydat$accidents), lwd = 3, col = "purple")
    points(x = mydat$subjects, y = predPoints, pch = 23, bg = myCols, col = "black", cex = 1.35)
    if ("Segments" %in% input$whatDisplay) {
      segments(x0 = mydat$subjects, x1 = mydat$subjects, y0 = predPoints, y1 = mean(mydat$accidents), lwd = 2, col = "darkgray")
      if ("Sums" %in% input$whatDisplay) {
        if (input$whatPred == "Mean" | !("F-stat" %in% input$whatDisplay)) {
          mtext(paste0("Model Sum of Squares = ", round(modAccSumSquares, 3), "\nMean Square = ", round(modAccSumSquares/dfMod, 3)), cex = 1.4, line = 0)
        } else {
          mtext(paste0("Model Sum of Squares = ", round(modAccSumSquares, 3), "\nMean Square = ", round(modAccSumSquares/dfMod, 3), 
                       "\nF = ",round(modAccSumSquares/dfMod, 3),"/",round(fullModMeanSquareError,3), " = ", round(round(modAccSumSquares/dfMod, 3)/fullModMeanSquareError, 3)) , cex = 1.4, line = 0)
        }
      } else {
        mtext(myMain, cex = 1.8)
      }
    }
  }
  
  
  if (sumSq == "Error") {
    # abline(h = mean(mydat$dv), lwd = 3, col = "purple")
    points(x = mydat$subjects, y = predPoints, pch = 23, bg = myCols, col = "black", cex = 1.35)
    
    if ("Segments" %in% input$whatDisplay) {
      
      segments(x0 = mydat$subjects, x1 = mydat$subjects, y0 = predPoints, y1 = mydat$accidents, lwd = 2, col = "red")
      # s <- round(fullModAccSumSq - modAccSumSquares, 3)
      s <- round(modErrorSumSquares, 3)
      if ("Sums" %in% input$whatDisplay) {
        mtext(paste0("Error Sum of Squares = ", s, "\n Mean Square = ", round(s/dfError, 3)), cex = 1.4)
      } else {
        mtext(myMain, cex = 1.8)
      }
      
      if (overlayFullPred) {
        points(x = mydat$subjects, y = myFullMod$fitted.values, pch = 22, bg = "orange", col = "black", cex = 1.35)
        legend("topleft", c("Full model predictions", "Current model predictions"), pch = c(22,23), bty = "n", cex = 1.15, col = c("orange", "darkgreen"))
      }
    }
  } 
}


plotSumSquaresRM <- function(mydat, input, sumSq = "Total", myMain = "", stats = NULL, plotMean = TRUE, 
                           alcColors = NULL, speedSymbols = NULL, overlayFullPred = FALSE) {
  
  # mydat <- read.csv("RM_ANOVA_AlcoholSpeedTime.csv")
  mydat$alcohol <- as.factor(mydat$alcohol)
  mydat$pp <- as.factor(mydat$pp)
  totN <- nrow(mydat)
  nGroups <- length(unique(mydat$alcohol))
  grandMean <- mean(mydat$accidents)
  nPerGroup <- length(unique(mydat$pp))
  myAlcMod <-  lm(accidents ~ alcohol, data = mydat)
  myPpMod <-  lm(accidents ~ pp, data = mydat)
  myMainMod <- myFullMod <-lm(accidents ~ alcohol + pp, data = mydat)
  
  
  plot(mydat$accidents, col = "black" , pch = 21, bg = "blue", 
       cex = 1.8, lwd = 3, las = 1, bty = "n", main = "",
       ylab = "Accidents", xlab = "Observation Nr.", xlim = c(0, length(mydat$accidents)), 
       ylim = c(3, 8), cex.lab = 1.3, cex.axis=1.3)
  totN <- nrow(mydat)
  
  if (input$whatPred == "Alcohol") {
    myMod <-  myAlcMod
    dfMod <- nGroups -1
    dfError <- (nGroups-1) * (nPerGroup-1)
    myBgs <- palette.colors(n = 4, palette = "Okabe-Ito")[as.numeric(mydat$alcohol)+1]
    myShapes <- 23
    myCols <- "black"
    if (sumSq != "Total")
      legend("topleft", c("noneA", "someA", 'highA'), fill = unique(myBgs), col = unique(myBgs), bty = "n", horiz = TRUE)
    
  } else if (input$whatPred == "ID") {
    myMod <- myPpMod
    dfMod <- nPerGroup-1
    dfError <- (nGroups-1) * (nPerGroup)
    myCols <- palette.colors(n = 1, palette = "Okabe-Ito")
    myBgs <- "darkgreen"
    myShapes <- (1:nPerGroup)[mydat$pp]
    if (sumSq != "Total")
      legend("topleft", legend = 1:nPerGroup, pch = unique(myShapes), bty = "n", horiz = TRUE, cex = 1.2)
    
  } else if (input$whatPred == "Alcohol + ID") {
    myMod <- myMainMod
    dfMod <- (nGroups -1) + (nPerGroup-1)
    dfError <- (nGroups -1) * (nPerGroup-1)
    myShapes <- (1:nPerGroup)[mydat$pp]
    myCols <- palette.colors(n = 4, palette = "Okabe-Ito")[as.numeric(mydat$alcohol)+1]
    myBgs <- "darkgreen"
    if (sumSq != "Total")
      legend("topleft", c("noneA", "someA", 'highA'), fill = unique(myCols), col = unique(myCols), 
             bty = "n", horiz = TRUE)
    
  } else {
    myMod <- lm(accidents ~ 1, data = mydat)
    dfMod <- 0
    dfError <- totN - 1
    myShapes <- 23
    myBgs <- "darkgreen"
    myCols <- "black"
    
  }
  
  predPoints <- myMod$fitted.values
  modAccSumSquares <- sum((predPoints - mean(mydat$accidents))^2)
  modAccMeanSquare <- modAccSumSquares / dfMod
  modErrorSumSquares <- sum((predPoints - mydat$accidents)^2)
  
  fullModErrorSumSq <- sum(myFullMod$residuals^2)
  fullModAccSumSq <- sum((myFullMod$fitted.values - mean(mydat$accidents))^2)
  fullModMeanSquareError <- fullModErrorSumSq / ( (nGroups -1) * (nPerGroup-1))
  nulModError <- sum((mean(mydat$accidents) - mydat$accidents)^2)
  
  if (sumSq == "Total") {
    if (plotMean) {
      abline(h = mean(mydat$accidents), lwd = 3, col = "purple")
    }
    points(x = 1:nrow(mydat), y = mydat$accidents, pch = myShapes, bg = myBgs, col = myCols, cex = 0.9, lwd = 1)
    
    if ("Segments" %in% input$whatDisplay) {
      segments(x0 = 1:nrow(mydat), x1 = 1:nrow(mydat), y0 = mean(mydat$accidents), y1 = mydat$accidents, lwd = 2)
      if ("Sums" %in% input$whatDisplay) {
        mtext(paste0("Total Sum of Squares = ", round(nulModError, 2)), cex = 1.4)
      } else {
        mtext(myMain, cex = 1.8)
      }
    }
  } 
  
  
  if (sumSq == "Model") {
    abline(h = mean(mydat$accidents), lwd = 3, col = "purple")
    points(x = 1:nrow(mydat), y = predPoints, pch = myShapes, bg = myBgs, col = myCols, cex = 1.35, lwd = 3)
    if ("Segments" %in% input$whatDisplay) {
      segments(x0 = 1:nrow(mydat), x1 = 1:nrow(mydat), y0 = predPoints, y1 = mean(mydat$accidents), lwd = 2, col = "darkgray")
      if ("Sums" %in% input$whatDisplay) {
        if (input$whatPred == "Mean" | !("F-stat" %in% input$whatDisplay)) {
          mtext(paste0("Model Sum of Squares = ", round(modAccSumSquares, 3), "\nMean Square = ", round(modAccSumSquares/dfMod, 3)), cex = 1.4, line = 0)
        } else {
          mtext(paste0("Model Sum of Squares = ", round(modAccSumSquares, 3), "\nMean Square = ", round(modAccSumSquares/dfMod, 3), 
                       "\nF = ",round(modAccSumSquares/dfMod, 3),"/",round(fullModMeanSquareError,3), " = ", round(round(modAccSumSquares/dfMod, 3)/fullModMeanSquareError, 3)) , cex = 1.4, line = 0)
        }
      } else {
        mtext(myMain, cex = 1.8)
      }
    }
  }
  
  
  if (sumSq == "Error") {
    # abline(h = mean(mydat$dv), lwd = 3, col = "purple")
    
    if ("Segments" %in% input$whatDisplay) {
      
      segments(x0 = 1:nrow(mydat), x1 = 1:nrow(mydat), y0 = predPoints, y1 = mydat$accidents, lwd = 2, col = "red")
      # s <- round(fullModAccSumSq - modAccSumSquares, 3)
      s <- round(modErrorSumSquares, 3)
      if ("Sums" %in% input$whatDisplay) {
        mtext(paste0("Error Sum of Squares = ", s, "\n Mean Square = ", round(s/dfError, 3)), cex = 1.4)
      } else {
        mtext(myMain, cex = 1.8)
      }
      
      if (overlayFullPred) {
        points(x = 1:nrow(mydat), y = myFullMod$fitted.values, pch = 22, bg = "orange", col = "black", cex = 1.35)
        legend("topleft", c("Full model predictions", "Current model predictions"), pch = c(22,23), bty = "n", cex = 1.1, col = c("orange", "darkgreen"))
      }
      
    }
    points(x = 1:nrow(mydat), y = predPoints, pch = myShapes, bg = myBgs, col = myCols, cex = 1.35, lwd = 3)
    
  } 
}
