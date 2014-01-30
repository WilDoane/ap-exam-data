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

ap <- loadData("AP-gender.csv")
ap <- renameVariables(ap)
View(ap)
