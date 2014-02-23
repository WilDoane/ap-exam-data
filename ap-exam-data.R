# required libraries
library(ggplot2)
library(scales)

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

get_x_axis_limits <- function(log.ratios) {
  distances = abs(1 - log.ratios)
  max.distance = max(distances)
  limits = c(2^(-max.distance), 
             2^(max.distance))
  return(limits)
}

ap <- loadData("AP-gender.csv")
ap <- renameVariables(ap)
ap <- augmentWithRatioOfFemalesToMales(ap)
ap <- reorderByGenderDisparity(ap)
ap <- subset(ap, exam.names != "total.exams")

p <- ggplot(aes(y = factor(exam.names),
                x = ratio.of.females.to.males,
                size = females + males),
            data = ap)
p <- p + geom_point()
p <- p + geom_vline(xintercept=1, colour="orange")
p <- p + scale_x_continuous(trans=log2_trans(),
                            breaks=2^(-3:3),
                            limits=get_x_axis_limits(ap$log.ratio),
                            expand=c(-0.1, 0.0),
                            labels=trans_format("log2", math_format(2^.x)))
                            
p <- p + xlab(expression(paste(bgroup("(",
                                      frac(paste("female test takers"), 
                                           paste("male test takers")),
                                      ")"))))
p <- p + ylab("Advanced Placement (AP) Exam")
p <- p + ggtitle("National Totals for Advanced Placement Exams")
p <- p + labs(size = "Total Number\nof Test-Takers")

print(p)
