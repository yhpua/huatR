


#' ncomma() returns show names(df) in alphabetical order and in multiple format
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#'\dontrun{
#'mtcars %>% dplyr::select(starts_with("c")  ) %>% names %>% ncomma
#'
#'}
#'
ncomma <- function (x) {

  #' data.frame: ncomma() returns show names(df) in alphabetical order and in multiple format
  #' character: ncomma() returns vector in multiple forms to facilitate cut-and-paste (Yikes! for experts; Yay for PYH)
  #' mtcars %>% ncomma
  #' mtcars %>% dplyr::select(starts_with("c")  ) %>% names %>% ncomma

  if(!any(class(x) %in% Cs(data.frame, character))) {
    stop('please supply a data.frame or character vector')
  }

  if (is.data.frame(x)){

    ## data.frame

    list(
      column_number = cnum(x),
      names_comma = noquote (paste(names(x), collapse=" , ")),
      sorted_names = sort (names(x)),
      sorted_comma = sort (names(x)) %>% str_flatten(", "))

  } else {

    ## character vector

    list(
      original = x,
      v_1 = x %>%  str_flatten (" , ") %>% noquote  ,
      v_2 = x %>%  str_flatten (" , ") %>% noquote %>% {str_glue("   Cs({.})")} ,
      v_3 = x %>%  map_chr(~str_glue(" '{.x}'")) %>% str_flatten (",") %>% noquote ,
      v_4 = x %>%  map_chr(~str_glue(" '{.x}'")) %>% str_flatten (",") %>% noquote %>% {str_glue("   c({.})")},
      v_5 = x %>% str_flatten ("  + ") %>% noquote() ,
      v_6 = x %>% str_flatten ("|") %>% noquote()
    )
  }
}



#' nplus: similar to ncomma
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
nplus <- function (x) {
  # noquote removes " "
  list(
    cnum(x),
    noquote (paste(names(x), collapse=" + ")),
    sort (names(x))
  )
}

#' cnum  to see column numbers (used within ncomma.pmisc and nplus.pmisc)
#'
#' @param x
#'
#' @return
#'
#' @examples
cnum = function(x) {
  ### A function to see column numbers (used with ncomma.pmisc and nplus.pmisc)
  y = rbind(seq(1, ncol(x)))
  colnames(y) = colnames(x)
  rownames(y) = "col.num"
  return(y)
}


#' nfactor used in conjunction with fct_relevel to manually arrange factor levels
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  used in conjunction with fct_relevel to manually arrange factor levels
# used in conjunction with arrange.dplyr to arrange dataframe rows
# used in conjunction with recode.dplyr or yhrecode to rename factor levels (does not alter level)
# nfactor(iris$Species)
# iris %>% mutate(Species = Species %>% fct_relevel(c('setosa','virginica',  'versicolor')))  ## manual fct_relevel
# iris %>% arrange(factor(Species, levels = c('virginica', 'setosa', 'versicolor')))
# iris %>% mutate(species=  recode(Species, setosa = 'Wombie',  versicolor = 'Jerry',  virginica = 'Akow'))
# num to factor: use factor() to preserve num vector order
# mtcars %>% mutate(cyl.recode =  recode(cyl %>% factor,'4' = 'four','6' = 'six','8' = 'eight'))
# mutate(across(c(varA, varB),~ recode(., 'none' = '1', 'mild' = '2')))
# recode: LHS in quotation needed only for numeric values
#'
#' }
nfactor <- function (x){
  ## used in conjunction with fct_relevel to manually arrange factor levels
  ## used in conjunction with arrange.dplyr to arrange dataframe rows
  ## used in conjunction with recode.dplyr or yhrecode to rename factor levels (does not alter level)
  ## nfactor(iris$Species)
  ## iris %>% mutate(Species = Species %>% fct_relevel(c('setosa','virginica',  'versicolor')))  ## manual fct_relevel
  ## iris %>% arrange(factor(Species, levels = c('virginica', 'setosa', 'versicolor')))
  ## iris %>% mutate(species=  recode(Species, setosa = 'Wombie',  versicolor = 'Jerry',  virginica = 'Akow'))
  ## num to factor: use factor() to preserve num vector order
  ## mtcars %>% mutate(cyl.recode =  recode(cyl %>% factor,'4' = 'four','6' = 'six','8' = 'eight'))
  ## mutate(across(c(varA, varB),~ recode(., 'none' = '1', 'mild' = '2')))
  ## recode: LHS in quotation needed only for numeric values


  mylevels <- levels(as.factor(x))
  # mylevels %>% map_chr(~str_glue("  '{.x}'  ")) %>% str_flatten (",") %>% noquote
  cat("Modify the rhs \n")

  list(
    ## noquote(paste0("'", paste(x, collapse="', '") ,"'")),
    ## noquote(paste0("'",paste(x, collapse="' = '',  '"), "' = ''")),
    noquote(paste0("'", paste(levels(as.factor(x)), collapse="', '") ,"'")),
    noquote(paste0("'",paste(levels(as.factor(x)), collapse="' = '',  '"), "' = ''")),
    noquote(sprintf(" '%s' = '%s' ", mylevels, mylevels)) %>% str_flatten(",") %>% noquote()
  )
}



#' yhrecode uses dplyr::recode to recode levels and (i) returns numeric (if num_output =TRUE) (ii) set NA for unspecified levels
#'
#' @param var
#' @param recode_rules
#' @param num_output TRUE FALSE
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'  yhrecode(c(1,2,3), c("2" = "1", "3"="0"))
#'  yhrecode will give NA to unspecified levels
#'  mtcars %>% mutate(fcyl = yhrecode(cyl, c("4" = "four", "6" = "six") ))
#' }
#'
yhrecode <- function(var, recode_rules, num_output=TRUE){
  ## uses dplyr::recode to recode levels and (i) returns numeric (if num_output =TRUE) (ii) set NA for unspecified levels
  ## yhrecode(c(1,2,3), c("2" = "1", "3"="0"))
  ## yhrecode will give NA to unspecified levels
  ## mtcars %>% mutate(fcyl = yhrecode(cyl, c("4" = "four", "6" = "six") ))
  var1 <- recode(var, !!!recode_rules, .default=NA_character_)
  if(num_output) var1 <- var1 %>% yhnum
  return(var1)
}


#' clean up inconsistent ptid
#'
#'
#'
#' @param idvar   assume idvar is coded as ID001
#' @param width
#' @param name_prefix
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' mtcars %>% mutate(carb_id = yhfixid(carb, name_prefix = "mtcars"))
#' }
yhfixid <- function (idvar, width = 3, name_prefix = "ID" ) {
  #' clean up inconsistent ptid
  #' assume idvar is coded as ID001
  #' mtcars %>% mutate(carb_id = yhfixid(carb, name_prefix = "mtcars"))

  ptnum         <- readr::parse_number(as.character(idvar)) %>%
    stringr::str_pad(width = width, side = "left", pad = "0")
  idvar_cleaned <- paste0(name_prefix, ptnum )
  return (idvar_cleaned)

}

