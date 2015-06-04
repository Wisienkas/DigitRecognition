library.try <- function(name) {
  if(!require(name)) {
    install.packages(name, dependencies = TRUE)
    library(name)
  }
}