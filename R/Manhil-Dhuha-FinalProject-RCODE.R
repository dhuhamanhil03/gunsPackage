#' Reads in a .xlsx file
#' @description Reads in a data file and prints out data
#' @export
#' @import AER
#' @import gdata
#' @param dataFile variable
#' @examples
#' readingInData(dataFile = "Guns.xlsx")
#' @author Dhuha Manhil

readingInData <- function(file) {
  dataSet <- read_excel(file)
  print(dataSet)
}

#' Summarizes a data set
#' @description Summarizes and prints out summary of data
#' @export
#' @import tidyverse
#' @import dplyr
#' @param dataSet variable
#' @examples
#' sumarizeFunction(dataSet = virginiaData)
#' @author Dhuha Manhil

summarizeFunction <- function(dataSet) {
  summaryOfSet <- summary(dataSet)
  print(summaryOfSet)
}

#' Correlation between two variables
#' @description Calculates and prints out correlation
#' @export
#' @param x,y variable
#' @import tidyverse
#' @import dplyr
#' @examples
#' correlationFunction(x = virginiaData$vio, y = virginiaData$mur)
#' @author Dhuha Manhil

correlationFunction <- function(x, y) {
  correlationOfVariables = cor(x, y, method = 'pearson')
  print(correlationOfVariables)
}

#' Calculates linear regression
#' @description Plots linear model with regression line
#' @export
#' @param x variable.
#' @param y variable.
#' @param a variable.
#' @param b numeric
#' @param colorOfPoints character.
#' @param xaxis character.
#' @param yaxis character.
#' @import dplyr
#' @import tidyverse
#' @examples
#' plottingFunction(x = virginiaData$rob, y = virginiaData$avginc, a = virginiaData, b = 16)
#' @author Dhuha Manhil
plottingFunction <- function(x, y, a, b, colorofPoints, xaxis, yaxis) {
  plot(x ~ y, data = a, pch = b)
  linearModel <- lm(x ~ y, data = a)
  print(summary(linearModel))
  plot(x ~ y, data = a, pch = b, col = colorofPoints,
       xlab = xaxis, ylab = yaxis)
  abline(linearModel)
}

#' Produces a bar graph
#' @description Produces and prints out bar graph
#' @export
#' @param virginiaYears data
#' @param yaxis variable.
#' @param yaxisLabel character.
#' @param title character.
#' @import ggplot2
#' @import scales
#' @examples
#' bargraphPlot(virginiaYears, virginiaData$vio, "Violence Crime Rate", "Violence Over Years")
#' @author Dhuha Manhil
bargraphPlot <- function(virginiaYears, yaxis, yaxisLabel, title) {
  plotOfData <- ggplot(virginiaData, aes(x = virginiaYears$year, y = yaxis)) +
    geom_bar(stat = "identity", color = "maroon", fill = "peachpuff") + theme_light() +
    ylab(yaxisLabel) + xlab("Years (1900's)") +
    theme(legend.position = "none") + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  print(plotOfData)
}

#' Produces a histogram plot
#' @description Produces and prints out histogram plot
#' @export
#' @param dataSet data
#' @param xaxis variable
#' @param xaxisLabel character
#' @param yaxisLabel character
#' @param title character
#' @import ggplot2
#' @import scales
#' @examples
#' histogramPlot(virginiaData, virginiaData$totalCrime, "Total Crime Rate of Murders,
#' Violence, and Robbery", "Frequency", "Histogram of Total Crime Rates")
#' @author Dhuha Manhil
histogramPlot <- function(dataSet, xaxis, xaxisLabel, yaxisLabel, title) {
  ggplot(dataSet) +
    aes(x = xaxis) +
    geom_histogram(bins = 11L, fill = "violet") +
    labs(x = xaxisLabel, y = yaxisLabel,
         title = title) + theme(plot.title = element_text(hjust = 0.5))
}

#' Produces scatter plot
#' @description Produces and prints out scatter plot
#' @export
#' @param dataSet data
#' @param xaxis variable
#' @param yaxis variable
#' @param xaxisLabel character
#' @param yaxisLabel character
#' @param title character
#' @import ggplot2
#' @import scales
#' @examples
#' scatterPlot(virginiaData, virginiaData$totalCrime, virginiaData$incarc_rate, "Total Crime",
#' "Incarceration Rate", "Total Crime vs Incarceration Rate")
#' @author Dhuha Manhil
scatterPlot <- function(dataSet, xaxis, yaxis, xaxisLabel, yaxisLabel, title) {
  ggplot(dataSet) +
    aes(x = xaxis, y = yaxis) +
    geom_point(size = 3L, colour = "khaki") +
    labs(x = xaxisLabel, y = yaxisLabel, title = title) +
    facet_wrap(vars(shall)) + theme(plot.title =
                                      element_text(hjust = 0.5))
}

#' Produces a boxplot
#' @description Produces and prints out a boxplot
#' @export
#' @param dataSet data
#' @param xaxis variable
#' @param yaxis variable
#' @param xaxisLabel character
#' @param yaxisLabel character
#' @param title character
#' @import ggplot2
#' @import scales
#' @examples
#' boxPlot(virginiaData, virginiaData$shall == 1, virginiaData$totalCrime, "Shall Law", "Total Crime",
#' "Total Crime vs Shall Law")
#' @author Dhuha Manhil
boxPlot <- function(dataSet, xaxis, yaxis, xaxisLabel, yaxisLabel, title) {
  ggplot(dataSet) + aes(x = xaxis, y = yaxis, fill = xaxis) +
    geom_boxplot() +
    labs(x = xaxisLabel, y = yaxisLabel, title = title) + theme(plot.title =
                                                                  element_text(hjust = 0.5)) + theme(legend.position = "none")
}
