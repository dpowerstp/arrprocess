#' Function to convert ACS races to police-tracked races
#'
#' @param string representing acs races
#' @param type Either retention, matching ACS values to city's HR demographic categories, or traffic, matching ACS values to police-data race values
#'
#' @return recoded string
#' @export
#'
#' @examples
race_lookup <- function(string, type = "traffic"){

  if (type == "retention"){
    # print(string)
    val <- dplyr::case_when(grepl("black or", string, ignore.case = TRUE) ~ "Black",
                            grepl("asian alone", string, ignore.case = TRUE) ~ "Asian/Pacific Islander",
                            grepl("two or more", string, ignore.case = TRUE) ~ "Other",
                            grepl("some other", string, ignore.case = TRUE) ~ "Other",
                            grepl("white alone", string, ignore.case = TRUE) ~ "White",
                            grepl("hispanic or latin", string, ignore.case = TRUE) ~ "Hispanic",
                            grepl("American Indian", string, ignore.case = TRUE) ~ "Native American",
                            grepl("Native Hawaiian", string, ignore.case = TRUE)~ "Asian/Pacific Islander",
                            TRUE ~ "Missing")
  }

  else if (type == "traffic"){
    val <- dplyr::case_when(grepl("black or", string, ignore.case = TRUE) ~ "Black",
                            grepl("asian alone", string, ignore.case = TRUE) ~ "Asian",
                            grepl("two or more", string, ignore.case = TRUE) ~ "Other",
                            grepl("some other", string, ignore.case = TRUE) ~ "Other",
                            grepl("white alone", string, ignore.case = TRUE) ~ "White",
                            grepl("hispanic or latin", string, ignore.case = TRUE) ~ "Hispanic",
                            grepl("American Indian", string, ignore.case = TRUE) ~ "Native American",
                            grepl("Native Hawaiian", string, ignore.case = TRUE)~ "Other",
                            TRUE ~ "Missing")

  }


#   if (val == "Missing"){
#     warning("Value not recoded; possibly missing or not standard acs format")
#   }

  val
}
