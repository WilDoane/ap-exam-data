# required libraries
library(ggplot2)

loadData <- function(input) {
  read.csv(input,
           header = TRUE,
           stringsAsFactors = FALSE)
}

renameVariables <- function(input) {
  names(input)[1] <- "sex"
  names(input) <- tolower(names(input))
  return(input)
}

augmentWithRatioOfFemalesToMales <- function(input) {
  exam.names <- names(input)[-1]
  output <- t(input[ ,-1])
  output <- data.frame(females = as.numeric(output[, 1]),
                       males   = as.numeric(output[, 2]))
  ratio.of.females.to.males <- output$females / output$males
  log.ratio <- log(ratio.of.females.to.males, base = 2)
  return(data.frame(exam.names,
                    output, 
                    ratio.of.females.to.males, 
                    log.ratio))
}

reorderByGenderDisparity <- function(input) {
  input$exam.names <- reorder(input$exam.names,
                              input$log.ratio,
                              mean)
  return(input)
}

ap <- loadData("AP-gender.csv")
ap <- renameVariables(ap)
ap <- augmentWithRatioOfFemalesToMales(ap)
ap <- reorderByGenderDisparity(ap)

p <- ggplot(aes(y = factor(exam.names),
                x = log.ratio),
            data = ap)
p <- p + geom_point()

print(p)
