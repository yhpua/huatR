#' huatR TOC
#' @description This function gives an overview of the various functions in the \code{huatR} package
#' @export
pmtoc<- function(){
  writeLines("
Yonghao miscellaneous R functions  \n
Descriptive Stats  \n
yhnmsd() generates descriptive stats
yhround() round numbers

\n
Data wrangling. Simple Tests \n
nplus ncomma nfactor yhrecode\n
yhprop(mtcars$hp >= 50) gives single proportion with Wilson's 95%CI  \n
yhfixid() clean up patient_id column \n

\n
Missing values  \n

\n
Number Formatting \n
yhestci() prints est and 95CI pasted together \n


           ")}
