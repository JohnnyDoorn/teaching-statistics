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


# Function to visualize chi-square distribution with shaded region
visualize_chisq_base <- function(H, df, section = "upper", col = "skyblue", myLimX =NULL) {
  # Set up x range (wide enough to show distribution)
  x <- seq(0, max(ceiling(H) * 1.5, qchisq(0.999, df)), length = 500)
  y <- dchisq(x, df)
  
  # Plot the chi-square density
  plot(x, y, type = "l", lwd = 2, col = "black", bty = "n", las =1,
       xlab = expression(chi^2), ylab = "Density",
       main = bquote("Chi-square Distribution (" ~ df == .(df) ~ ")"), xlim = myLimX)
  
  # Shade area depending on section
  if (section == "upper") {
    x_shade <- x[x >= H]
    y_shade <- y[x >= H]
  } else if (section == "lower") {
    x_shade <- x[x <= H]
    y_shade <- y[x <= H]
  } else if (section == "both") {
    cutoff <- H
    x_shade <- x[x <= cutoff | x >= qchisq(1 - pchisq(cutoff, df), df)]
    y_shade <- y[x <= cutoff | x >= qchisq(1 - pchisq(cutoff, df), df)]
  } else {
    stop("section must be 'upper', 'lower', or 'both'")
  }
  
  # Shade area under curve
  polygon(c(x_shade, rev(x_shade)),
          c(y_shade, rep(0, length(y_shade))),
          col = col, border = NA)
  
  # Add vertical cutoff line
  abline(v = H, col = "darkred", lwd = 2, lty = 2)
  
  # Add text showing p-value
  if (section == "upper") {
    pval <- pchisq(H, df, lower.tail = FALSE)
    legend("topright",
           legend = bquote("P(" * chi^2 >= .(round(H, 3)) * ") = " * .(round(pval, 4))),
           bty = "n")
  } else if (section == "lower") {
    pval <- pchisq(H, df)
    legend("topright",
           legend = bquote("P(" * chi^2 <= .round(H, 3) * ") = " * .(round(pval, 4))),
           bty = "n")
  }
}

visualize_normal_base <- function(testStat, mu = 0, sd = 1,
                                  side = c("two-sided", "negative", "positive"),
                                  col = "skyblue", myLimX = NULL) {
  side <- match.arg(side)
  
  # Define x range (wide enough to show full curve)
  if (is.null(myLimX)) {
    x <- seq(mu - 4 * sd, mu + 4 * sd, length.out = 500)
  } else {
    x <- seq(myLimX[1], myLimX[2], length.out = 500)
  }
  y <- dnorm(x, mu, sd)
  
  ## ---- Compute p-value and title text ----
  if (side == "positive") {
    pval <- pnorm(testStat, mu, sd, lower.tail = FALSE)
    subtitle <- bquote("P(" * z >= .(round(testStat, 3)) * ") = " * .(round(pval, 4)))
  } else if (side == "negative") {
    pval <- pnorm(testStat, mu, sd, lower.tail = TRUE)
    subtitle <- bquote("P(" * z <= .(round(testStat, 3)) * ") = " * .(round(pval, 4)))
  } else {  # two-sided
    cutoff <- abs(testStat)
    pval <- 2 * pnorm(-cutoff, mu, sd)
    subtitle <- bquote("P(|" * z * "| ≥ " * .(round(cutoff, 3)) * ") = " * .(round(pval, 4)))
  }
  
  ## ---- Base plot ----
  plot(x, y, type = "l", lwd = 2, col = "black", bty = "n", las = 1,
       xlab = expression(z), ylab = "Density",
       main = bquote(atop(
         "Normal Distribution (" ~ mu == .(mu) ~ "," ~ sigma == .(sd) ~ ")",
         .(subtitle)
       )))
  
  ## ---- Shading ----
  if (side == "positive") {
    x_shade <- x[x >= testStat]; y_shade <- y[x >= testStat]
    polygon(c(x_shade, rev(x_shade)), c(y_shade, rep(0, length(y_shade))), col = col, border = NA)
    abline(v = testStat, col = "darkred", lwd = 2, lty = 2)
    
  } else if (side == "negative") {
    x_shade <- x[x <= testStat]; y_shade <- y[x <= testStat]
    polygon(c(x_shade, rev(x_shade)), c(y_shade, rep(0, length(y_shade))), col = col, border = NA)
    abline(v = testStat, col = "darkred", lwd = 2, lty = 2)
    
  } else if (side == "two-sided") {
    cutoff <- abs(testStat)
    # Left tail
    x_left <- x[x <= -cutoff]; y_left <- y[x <= -cutoff]
    polygon(c(x_left, rev(x_left)), c(y_left, rep(0, length(y_left))), col = col, border = NA)
    # Right tail
    x_right <- x[x >= cutoff]; y_right <- y[x >= cutoff]
    polygon(c(x_right, rev(x_right)), c(y_right, rep(0, length(y_right))), col = col, border = NA)
    abline(v = c(-cutoff, cutoff), col = "darkred", lwd = 2, lty = 2)
  }
}


# Example usage
# visualize_chisq_base(H = 6.5, df = 3, section = "upper")


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
          mtext(paste0("Residual Sum of Squares = ", totSumSquares), cex = 1.4)
        } else {
          mtext(paste0("Residual Sum of Squares = ", totSumSquares, "\n Mean Square = ", round(totSumSquares/dfError, 3)), cex = 1.4)
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
    # The covariate-only model's SS, which is what the segments show. The stats
    # column 'Cov Sum of Squares' is the covariate's unique SS (full - group).
    modSumSquares <- sumSquareCov
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
        mtext(paste0("Residual Sum of Squares = ", totSumSquares, "\n Mean Square = ", round(totSumSquares/dfError, 3)), cex = 1.4)
      } else {
        mtext("Model error", cex = 1.8)
      }
    }
  } 
}


# Sums-of-squares plot for the factorial ANOVA applet (FacANOVA) and the SSR lecture
# slides (lecture 17). Two identical copies: FacANOVA/plotSquaresFunction.R in the
# ShinyApps repo and ssr/plotFunctionsSSR.r in teaching-statistics. Edit one, copy it
# to the other, so the slides and the applet stay one thing.
#
# Each point is one participant: fill = alcohol dose, shape = speed dose. The black
# step line is the current model's prediction (one value per cell of 20 participants).
plotSumSquaresFactorial <- function(data, input, sumSq = "Total", myMain = "",
                                    sortBy = "alcohol", overlayFullPred = FALSE,
                                    alcColors = palette.colors(n = 4, palette = "Okabe-Ito")[2:4],
                                    speedSymbols = c(21, 22, 23)) {

  # The lecture labels levels as "none (A)" etc.; strip that so both data sets work.
  doseLevels <- c("none", "some", "much")
  asDose <- function(x) factor(sub(" \\(.\\)$", "", as.character(x)), levels = doseLevels)
  mydat <- data.frame(alcohol   = asDose(data$alcohol),
                      speed     = asDose(data$speed),
                      accidents = data$accidents)
  ord <- if (sortBy == "speed") order(mydat$speed, mydat$alcohol) else order(mydat$alcohol, mydat$speed)
  mydat <- mydat[ord, ]
  totN <- nrow(mydat)
  x <- seq_len(totN)
  grandMean <- mean(mydat$accidents)

  myMod <- switch(input$whatPred,
                  "Mean"                  = lm(accidents ~ 1, data = mydat),
                  "Alcohol"               = lm(accidents ~ alcohol, data = mydat),
                  "Speed"                 = lm(accidents ~ speed, data = mydat),
                  "Alcohol + Speed"       = lm(accidents ~ alcohol + speed, data = mydat),
                  "Alcohol + Speed + A:S" = lm(accidents ~ alcohol * speed, data = mydat))
  # Every F-ratio uses the residual mean square of the full model.
  myFullMod <- lm(accidents ~ alcohol * speed, data = mydat)
  dfMod   <- length(coef(myMod)) - 1
  dfError <- totN - length(coef(myMod))
  predPoints <- unname(fitted(myMod))

  totalSumSquares  <- sum((mydat$accidents - grandMean)^2)
  modSumSquares    <- sum((predPoints - grandMean)^2)
  errorSumSquares  <- sum((mydat$accidents - predPoints)^2)
  fullModMeanSquareError <- sum(residuals(myFullMod)^2) / (totN - length(coef(myFullMod)))

  # Cells (blocks of identical alcohol + speed) and the blocks of the factor sorted on.
  cellRuns   <- rle(paste(mydat$alcohol, mydat$speed))
  cellEnds   <- cumsum(cellRuns$lengths)
  cellStarts <- cellEnds - cellRuns$lengths + 1
  majorSizes <- as.vector(table(if (sortBy == "speed") mydat$speed else mydat$alcohol))
  majorEnds  <- cumsum(majorSizes)

  plot(x, mydat$accidents, type = "n", las = 1, bty = "n", xaxt = "n",
       xlim = c(0, totN), ylim = c(0, 10), cex.lab = 1.3, cex.axis = 1.3,
       ylab = "Accidents", xlab = paste0("Participants, sorted by ", sortBy, " dose"))
  axis(1, at = majorEnds - majorSizes / 2 + 0.5, labels = doseLevels, tick = FALSE, cex.axis = 1.3)
  abline(v = cellEnds[-length(cellEnds)] + 0.5, col = "gray85", lty = 3)
  abline(v = majorEnds[-length(majorEnds)] + 0.5, col = "gray60", lty = 2, lwd = 1.5)

  showSegments <- "Segments" %in% input$whatDisplay
  stepLine <- function(pred, ...) segments(cellStarts - 0.5, pred[cellStarts], cellEnds + 0.5, pred[cellStarts], ...)

  if (sumSq %in% c("Total", "Model"))
    abline(h = grandMean, lwd = 3, col = "purple")

  if (showSegments) {
    if (sumSq == "Total")
      segments(x0 = x, x1 = x, y0 = grandMean, y1 = mydat$accidents, lwd = 2)
    if (sumSq == "Model")
      segments(x0 = x, x1 = x, y0 = predPoints, y1 = grandMean, lwd = 2, col = "darkgray")
    if (sumSq == "Error")
      segments(x0 = x, x1 = x, y0 = predPoints, y1 = mydat$accidents, lwd = 2, col = "red")
  }

  points(x, mydat$accidents, pch = speedSymbols[mydat$speed], bg = alcColors[mydat$alcohol],
         col = "black", cex = 1.8, lwd = 1.5)

  if (sumSq %in% c("Model", "Error"))
    stepLine(predPoints, lwd = 4)

  legAlc <- legend("topleft", legend = doseLevels, fill = alcColors, title = "Alcohol", bty = "n", cex = 1.1)
  legend(legAlc$rect$left + legAlc$rect$w, legAlc$rect$top, legend = doseLevels, pch = speedSymbols,
         pt.bg = "white", pt.cex = 1.5, title = "Speed", bty = "n", cex = 1.1)

  if (sumSq == "Error" && overlayFullPred) {
    stepLine(unname(fitted(myFullMod)), lwd = 3, lty = 2, col = "#CC79A7")
    legend("bottomright", c("Full model predictions", "Current model predictions"), lwd = c(3, 4),
           lty = c(2, 1), col = c("#CC79A7", "black"), bty = "n", cex = 1.1)
  }

  if (showSegments) {
    if ("Sums" %in% input$whatDisplay) {
      if (sumSq == "Total") {
        sumsText <- paste0("Total Sum of Squares = ", round(totalSumSquares, 2))
      } else if (sumSq == "Model") {
        sumsText <- paste0("Model Sum of Squares = ", round(modSumSquares, 3))
        if (dfMod > 0) {
          modMeanSquare <- modSumSquares / dfMod
          sumsText <- paste0(sumsText, "\nMean Square = ", round(modMeanSquare, 3))
          if ("F-stat" %in% input$whatDisplay)
            sumsText <- paste0(sumsText, "\nF = ", round(modMeanSquare, 3), "/", round(fullModMeanSquareError, 3),
                               " = ", round(modMeanSquare / fullModMeanSquareError, 3))
        }
      } else {
        sumsText <- paste0("Residual Sum of Squares = ", round(errorSumSquares, 3),
                           "\nMean Square = ", round(errorSumSquares / dfError, 3))
      }
      mtext(sumsText, cex = 1.4, line = 0)
    } else {
      defaultMain <- c(Total = "", Model = "Model improvement", Error = "Model residual")[[sumSq]]
      mtext(if (nzchar(myMain)) myMain else defaultMain, cex = 1.8)
    }
  }
}


# Variance partition (DSUJ Figure 13.6 with the numbers filled in), for the FacANOVA
# applet's partition tab and the lecture 17 slides; copies kept identical like the one
# above. Box widths are proportional to the sums of squares. The bottom row shows the sums
# of squares an ANOVA table reports for the chosen type, plus what that type leaves to no
# single effect ("shared"):
#   Type I   sequential: the factor entered first also gets the shared part (nothing left)
#   Type II  what a main effect adds to the other main effect, as on the BONUS slides, so
#            alone = unique + shared
#   Type III each effect given all other terms (the default here, as in JASP and SPSS)
# In a balanced design all types agree and shared is 0; positive shared is redundancy,
# negative is suppression (the parts then add up to more than the model).
partitionFactorial <- function(data, type = "III") {
  doseLevels <- c("none", "some", "much")
  asDose <- function(x) factor(sub(" \\(.\\)$", "", as.character(x)), levels = doseLevels)
  d <- data.frame(y = data$accidents, alcohol = asDose(data$alcohol), speed = asDose(data$speed))
  ssOf <- function(fit) sum((fitted(fit) - mean(d$y))^2)
  fitFull <- lm(y ~ alcohol * speed, data = d)
  alone <- c(alcohol = ssOf(lm(y ~ alcohol, data = d)), speed = ssOf(lm(y ~ speed, data = d)))
  main  <- ssOf(lm(y ~ alcohol + speed, data = d))
  model <- ssOf(fitFull)
  total <- sum((d$y - mean(d$y))^2)
  unique <- c(alcohol = main - alone[["speed"]], speed = main - alone[["alcohol"]])
  # Type III: drop each term's columns from the sum-to-zero coded full model.
  X <- model.matrix(~ alcohol * speed, data = d, contrasts.arg = list(alcohol = "contr.sum", speed = "contr.sum"))
  rss <- function(keep) sum(lm.fit(X[, keep, drop = FALSE], d$y)$residuals^2)
  type3 <- sapply(1:3, function(t) rss(attr(X, "assign") != t) - rss(TRUE))
  terms <- switch(type,
    "I-alcohol" = c(alcohol = alone[["alcohol"]], speed = unique[["speed"]], interaction = model - main),
    "I-speed"   = c(alcohol = unique[["alcohol"]], speed = alone[["speed"]], interaction = model - main),
    "II"        = c(unique, interaction = model - main),
    "III"       = setNames(type3, c("alcohol", "speed", "interaction")))
  list(total = total, model = model, residual = total - model, main = main, alone = alone,
       unique = unique, shared = sum(alone) - main, type = type, terms = terms,
       sharedType = model - sum(terms),
       df = c(total = nrow(d) - 1, model = fitFull$rank - 1, residual = fitFull$df.residual,
              alcohol = nlevels(d$alcohol) - 1, speed = nlevels(d$speed) - 1),
       cells = table(Alcohol = d$alcohol, Speed = d$speed),
       r = cor(as.numeric(d$alcohol), as.numeric(d$speed)))
}

plotVariancePartitionFactorial <- function(data, type = "III", cex = 1.4) {
  p <- partitionFactorial(data, type)
  fmt <- function(x) sprintf("%.2f", x)
  par(mar = c(0.5, 0.5, 0.5, 0.5))
  plot.new(); plot.window(xlim = c(0, 1.25), ylim = c(-1, 3))
  w <- function(ss) ss / p$total
  
  # Draws a box; writes its label inside when it fits (on two lines, else with the df on a
  # third line), otherwise returns FALSE.
  partBox <- function(x0, x1, y0, y1, fill, border, label, textCol) {
    rect(x0, y0, x1, y1, col = fill, border = border, lwd = 2)
    for (candidate in unique(c(label, sub(", df", "\ndf", label, fixed = TRUE)))) {
      if (strwidth(candidate, cex = cex) < 0.95 * (x1 - x0) && strheight(candidate, cex = cex) < 0.9 * (y1 - y0)) {
        text((x0 + x1) / 2, (y0 + y1) / 2, candidate, col = textCol, cex = cex)
        return(TRUE)
      }
    }
    FALSE
  }
  callouts <- list()
  addCallout <- function(x, label, col, yFrom = 0.15)
    callouts[[length(callouts) + 1]] <<- list(x = x, label = label, col = col, yFrom = yFrom)
  
  partBox(0, 1, 2.15, 2.95, "#7FB6D9", "#3F7FAF",
          sprintf("Total variability\nSS = %s, df = %d", fmt(p$total), p$df[["total"]]), "white")
  xModel <- w(p$model)
  partBox(0, xModel, 1.15, 1.95, "#4A9E7E", "#2C5E4F",
          sprintf("Model (alcohol, speed, interaction)\nSS = %s, df = %d", fmt(p$model), p$df[["model"]]), "white")
  residualLabel <- sprintf("Residual\nSS = %s\ndf = %d", fmt(p$residual), p$df[["residual"]])
  if (!partBox(xModel, 1, 1.15, 1.95, "#DDAA44", "#C0702E", residualLabel, "white"))
    text(1.015, 1.55, residualLabel, adj = 0, col = "#C0702E", cex = cex)
  
  # Bottom row, left to right: alcohol, (shared), speed, interaction. With suppression a
  # dashed line marks where the model ends; the boxes drawn after it cover it where they overlap.
  shared <- p$sharedType
  if (shared < -0.005) segments(xModel, 0, xModel, 1.15, lty = 2, lwd = 2, col = "gray25")
  sharedLabel <- if (shared > 0.005) "Shared (redundancy)" else if (shared < -0.005) "Shared (suppression)" else "Shared"
  firstEntered <- c("I-alcohol" = "alcohol", "I-speed" = "speed")[type]
  parts <- list(
    list(ss = p$terms[["alcohol"]], fill = "#EDE36E", border = "#D4B23A", textCol = "gray20",
         label = sprintf("Alcohol%s\nSS = %s, df = %d", if (identical(unname(firstEntered), "alcohol")) " (entered first)" else "",
                         fmt(p$terms[["alcohol"]]), p$df[["alcohol"]])),
    # Type I leaves nothing over: the shared part sits in the factor entered first.
    if (shared >= 0 && is.na(firstEntered))
      list(ss = shared, fill = "#BDBDBD", border = "gray40", textCol = "gray20",
           label = sprintf("%s\nSS = %s", sharedLabel, fmt(shared))),
    list(ss = p$terms[["speed"]], fill = "#FCF0D9", border = "#C9773A", textCol = "gray20",
         label = sprintf("Speed%s\nSS = %s, df = %d", if (identical(unname(firstEntered), "speed")) " (entered first)" else "",
                         fmt(p$terms[["speed"]]), p$df[["speed"]])),
    list(ss = p$terms[["interaction"]], fill = "#C44E3C", border = "#8E2E22", textCol = "white",
         label = sprintf("Alcohol × speed\nSS = %s, df = %d", fmt(p$terms[["interaction"]]), p$df[["alcohol"]] * p$df[["speed"]])))
  x0 <- 0
  for (part in Filter(Negate(is.null), parts)) {
    x1 <- x0 + w(part$ss)
    if (!partBox(x0, x1, 0.15, 0.95, part$fill, part$border, part$label, part$textCol))
      addCallout((x0 + x1) / 2, gsub("\n", ", ", part$label), if (part$textCol == "white") part$border else "gray20")
    x0 <- x1
  }
  if (shared < -0.005) {
    # Suppression: the unique parts overshoot the model by the (negative) shared part,
    # shown as a hatched strip under the overshoot.
    rect(xModel, 0, xModel + w(-shared), 0.1, density = 20, col = "gray25", border = "gray25")
    addCallout(xModel + w(-shared) / 2, sprintf("%s, SS = %s", sharedLabel, fmt(shared)), "gray25", yFrom = 0)
  }
  
  # Labels that did not fit go below the bottom row, one line each, with a leader line.
  # Labels right of center end at their leader and run top to bottom by increasing x (left
  # of center: start at it, decreasing x), so no leader crosses a label above it.
  xs <- vapply(callouts, `[[`, numeric(1), "x")
  callouts <- callouts[c(order(-xs)[xs[order(-xs)] <= 0.5], order(xs)[xs[order(xs)] > 0.5])]
  for (k in seq_along(callouts)) {
    co <- callouts[[k]]; yText <- -0.25 - 0.3 * (k - 1)
    right <- co$x > 0.5
    segments(co$x, co$yFrom, co$x, yText, col = co$col, lwd = 1.5)
    text(co$x + if (right) -0.01 else 0.01, yText, co$label, adj = if (right) 1 else 0, col = co$col, cex = cex)
  }
  invisible(p)
}


# Sums-of-squares plot for the repeated-measures ANOVA applet (RM_ANOVA) and the SSR
# lecture slides (lecture 18). Two identical copies: RM_ANOVA/plotSquaresFunction.R in the
# ShinyApps repo and ssr/plotFunctionsSSR.r in teaching-statistics. Edit one, copy it to
# the other, so the slides and the applet stay one thing.
#
# Long data: one row per observation (accidents, alcohol, pp). Each point is one
# observation, fill = alcohol dose; the bands group the observations by participant (or
# by alcohol dose). The short black lines are the current model's predictions.
plotSumSquaresRM <- function(data, input, sumSq = "Total", myMain = "",
                             sortBy = "participant", overlayFullPred = FALSE,
                             alcColors = palette.colors(n = 4, palette = "Okabe-Ito")[2:4]) {

  doseLevels <- c("none", "some", "much")
  mydat <- data.frame(accidents = data$accidents,
                      alcohol   = factor(sub(" \\(.\\)$", "", as.character(data$alcohol)), levels = doseLevels),
                      pp        = factor(data$pp, levels = sort(unique(data$pp))))
  ord <- if (sortBy == "alcohol") order(mydat$alcohol, mydat$pp) else order(mydat$pp, mydat$alcohol)
  mydat <- mydat[ord, ]
  totN <- nrow(mydat)
  x <- seq_len(totN)
  grandMean <- mean(mydat$accidents)

  myMod <- switch(input$whatPred,
                  "Mean"         = lm(accidents ~ 1, data = mydat),
                  "Alcohol"      = lm(accidents ~ alcohol, data = mydat),
                  "ID"           = lm(accidents ~ pp, data = mydat),
                  "Alcohol + ID" = lm(accidents ~ alcohol + pp, data = mydat))
  # The F-ratio for alcohol uses the residual mean square of the full model (alcohol + ID).
  myFullMod <- lm(accidents ~ alcohol + pp, data = mydat)
  dfMod   <- length(coef(myMod)) - 1
  dfError <- totN - length(coef(myMod))
  predPoints <- unname(fitted(myMod))

  totalSumSquares  <- sum((mydat$accidents - grandMean)^2)
  modSumSquares    <- sum((predPoints - grandMean)^2)
  errorSumSquares  <- sum((mydat$accidents - predPoints)^2)
  fullModMeanSquareError <- sum(residuals(myFullMod)^2) / (totN - length(coef(myFullMod)))

  # Blocks of the factor sorted on: participants (3 observations each) or alcohol doses.
  major      <- if (sortBy == "alcohol") mydat$alcohol else droplevels(mydat$pp)
  majorSizes <- as.vector(table(major))
  majorEnds  <- cumsum(majorSizes)

  plot(x, mydat$accidents, type = "n", las = 1, bty = "n", xaxt = "n",
       xlim = c(0, totN), ylim = c(3, 8), cex.lab = 1.3, cex.axis = 1.3,
       ylab = "Accidents",
       xlab = if (sortBy == "alcohol") "Observations, sorted by alcohol dose" else "Observations, grouped by participant")
  axis(1, at = majorEnds - majorSizes / 2 + 0.5, labels = levels(major), tick = FALSE, cex.axis = 1.3)
  if (sortBy == "alcohol") {
    abline(v = majorEnds[-length(majorEnds)] + 0.5, col = "gray60", lty = 2)
  } else {
    # Shade every other participant, so each person's three observations read as one block.
    shade <- seq(2, length(majorEnds), by = 2)
    rect(majorEnds[shade] - majorSizes[shade] + 0.5, 3, majorEnds[shade] + 0.5, 8, col = "gray93", border = NA)
  }

  showSegments <- "Segments" %in% input$whatDisplay
  predLines <- function(pred, ...) segments(x - 0.45, pred, x + 0.45, pred, ...)

  if (sumSq %in% c("Total", "Model"))
    abline(h = grandMean, lwd = 3, col = "purple")

  if (showSegments) {
    if (sumSq == "Total")
      segments(x0 = x, x1 = x, y0 = grandMean, y1 = mydat$accidents, lwd = 2)
    if (sumSq == "Model")
      segments(x0 = x, x1 = x, y0 = predPoints, y1 = grandMean, lwd = 2, col = "darkgray")
    if (sumSq == "Error")
      segments(x0 = x, x1 = x, y0 = predPoints, y1 = mydat$accidents, lwd = 2, col = "red")
  }

  points(x, mydat$accidents, pch = 21, bg = alcColors[mydat$alcohol], col = "black", cex = 1.8, lwd = 1.5)

  if (sumSq %in% c("Model", "Error"))
    predLines(predPoints, lwd = 4)

  legend("topleft", legend = doseLevels, fill = alcColors, title = "Alcohol", bty = "n",
         horiz = TRUE, cex = 1.1)

  if (sumSq == "Error" && overlayFullPred) {
    predLines(unname(fitted(myFullMod)), lwd = 3, lty = 2, col = "#CC79A7")
    legend("bottomright", c("Full model predictions", "Current model predictions"), lwd = c(3, 4),
           lty = c(2, 1), col = c("#CC79A7", "black"), bty = "n", cex = 1.1)
  }

  if (showSegments) {
    if ("Sums" %in% input$whatDisplay) {
      if (sumSq == "Total") {
        sumsText <- paste0("Total Sum of Squares = ", round(totalSumSquares, 2))
      } else if (sumSq == "Model") {
        sumsText <- paste0("Model Sum of Squares = ", round(modSumSquares, 3))
        if (dfMod > 0) {
          modMeanSquare <- modSumSquares / dfMod
          sumsText <- paste0(sumsText, "\nMean Square = ", round(modMeanSquare, 3))
          if ("F-stat" %in% input$whatDisplay)
            sumsText <- paste0(sumsText, "\nF = ", round(modMeanSquare, 3), "/", round(fullModMeanSquareError, 3),
                               " = ", round(modMeanSquare / fullModMeanSquareError, 3))
        }
      } else {
        sumsText <- paste0("Residual Sum of Squares = ", round(errorSumSquares, 3),
                           "\nMean Square = ", round(errorSumSquares / dfError, 3))
      }
      mtext(sumsText, cex = 1.4, line = 0)
    } else {
      defaultMain <- c(Total = "", Model = "Model improvement", Error = "Model residual")[[sumSq]]
      mtext(if (nzchar(myMain)) myMain else defaultMain, cex = 1.8)
    }
  }
}


# --- t sampling distribution, drawn the way the NHST_Continuous applet draws it ---
# Density as a dense set of vertical lines: green where we retain H0, purple where
# we reject, so the continuous case rhymes with the binomial spikes from lecture 3.
# On p-value slides pass showRegion = FALSE: no alpha, no purple, and the observed
# value plus everything more extreme goes black instead.
# Also defined locally in "2026/6. correlation/t-distribution.qmd", which was
# rendered before this was promoted here; that copy shadows this one identically.
tCurve <- function(df, alpha = .05, sided = "two", observed = NULL, main = NULL,
                   showRegion = TRUE) {

  lim     <- max(5, ceiling(abs(c(observed, 0))) + 1)
  xVals   <- seq(-lim, lim, length.out = 1e3)
  twoCols <- c("darkgreen", "purple")   # retain / reject, as in the applet
  yMax    <- .45

  crit <- switch(sided,
                 two   = c(qt(alpha / 2, df), qt(1 - alpha / 2, df)),
                 right = c(-Inf, qt(1 - alpha, df)),
                 left  = c(qt(alpha, df), Inf),
                 none  = c(-Inf, Inf))

  if (!showRegion) crit <- c(-Inf, Inf)   # p-value slides show no decision region
  cols <- ifelse(xVals > crit[1] & xVals < crit[2], twoCols[1], twoCols[2])

  plot(xVals, dt(xVals, df),
       col  = cols,
       type = "h",
       lwd  = 2,
       las  = 1,
       ylab = "Density",
       xlab = "t",
       ylim = c(0, yMax),
       bty  = "n",
       main = main)

  for (v in crit[is.finite(crit)]) abline(v = v, lwd = 2, lty = 2)

  # The observed value and everything more extreme, in black: that area is the
  # p-value, kept distinct from the purple decision region.
  if (!is.null(observed)) {
    more <- if (sided == "two") xVals[abs(xVals) >= abs(observed)] else xVals[xVals >= observed]
    lines(more, dt(more, df), type = "h", lwd = 2)
    arrows(x0 = observed, x1 = observed, y0 = 0, y1 = yMax * .72,
           col = "darkred", lwd = 3, length = 0)

    p    <- pt(abs(observed), df, lower.tail = FALSE) * if (sided == "two") 2 else 1
    labX <- min(max(observed, -lim + 1.2), lim - 1.2)
    text(labX, yMax * .82, paste("t =", round(observed, 2)), cex = .9)
    text(labX, yMax * .92,
         if (p < .001) "p < .001" else paste("p =", round(p, 3)), cex = .9)
  }
  invisible(crit)
}
