# required libraries
library(ggplot2)
library(scales)

loadData <- function(input) {
  read.csv(input,
           header = FALSE,
           stringsAsFactors = FALSE)
}

cleanData <- function(input) {
  input <- input[ , -c(1, 2)] # drop aggregate info from Excel spreadsheet
  column.names <- input[1, ]
  females <- input[2, ]
  males <- input[3, ]
  output <- rbind(males, females)
  output <- as.data.frame(output)
  colnames(output) <- column.names
  return(output)
}

augmentWithRatioOfFemalesToMales <- function(input) {
  ratio.of.females.to.males <- input$females / input$males
  log.ratio <- log(ratio.of.females.to.males, base = 2)
  return(data.frame(exam.names = input$exam.names,
                    females = as.numeric(input$females),
                    males = as.numeric(input$males),
                    ratio.of.females.to.males, 
                    log.ratio))
}

augmentWithExamTypes <- function(input) {
  stem.exams <- c("computer.science.a", "physics.c..elec....magnet.", "physics.c..mechanics", 
    "physics.b", "calculus.bc", "chemistry", "calculus.ab", "statistics", 
    "environmental.science", "biology")

  input$exam.type <- "Non-STEM"
  
  input[input$exam.names %in% stem.exams, ]$exam.type <- "STEM"
  input
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
ap <- cleanData(ap)
ap <- augmentWithRatioOfFemalesToMales(ap)
ap <- augmentWithExamTypes(ap)
ap <- reorderByGenderDisparity(ap)
ap <- subset(ap, exam.names != "total.exams")
View(ap)

p <- ggplot(aes(y = factor(exam.names),
                x = ratio.of.females.to.males,
                size = females + males,
                color = exam.type),
            data = ap)
p <- p + geom_point()
p <- p + geom_vline(xintercept=1, colour="orange")
p <- p + scale_x_continuous(trans=log2_trans(),
                            breaks=2^(-2:2),
                            limits=get_x_axis_limits(ap$log.ratio),
                            expand=c(-0.1, 0.0),
                            labels=c("1/4", "1/2", 1, 2, 4))
                            
p <- p + xlab(expression(paste(bgroup("(",
                                      frac(paste("female test takers"), 
                                           paste("male test takers")),
                                      ")"))))
p <- p + ylab("Advanced Placement (AP) Exam")
p <- p + ggtitle("National Totals for Advanced Placement Exams")
p <- p + labs(size = "Total Number\nof Test-Takers")

print(p)
