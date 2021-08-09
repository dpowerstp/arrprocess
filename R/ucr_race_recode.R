#' ucr_race_recode
#'
#' Recode UCR race values to more usable values
#'
#' @param string a string for the UCR race value
#'
#' @return A string with recoded race values
#' @export
#'
#' @examples
ucr_race_recode <- function(string){
  case_when(grepl("White alone", string, ignore.case = T) ~ "White",
            grepl("Black", string, , ignore.case = T) ~ "Black",
            grepl("Hispanic", string, ignore.case = T) ~ "Hispanic",
            grepl("Asian alone", string, ignore.case = T) ~ "Asian",
            TRUE ~ "Other")
}
