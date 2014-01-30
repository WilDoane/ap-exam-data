ap <- read.csv("AP-gender.csv",
               header = TRUE,
               stringsAsFactors = FALSE)

renameVariables <- function(input) {
  names(input)[1] <- "sex"
  names(input) <- tolower(names(input))
  return(input)
}


ap <- renameVariables(ap)
View(ap)
