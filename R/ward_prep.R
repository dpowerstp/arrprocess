#' Order race values before plot
#'
#' Function to order race consistently for graph colors. Dataframe must have a race column with values matching those in ward_vector_order. Should run before running plotly.
#'
#' @param df a stops or arrests dataframe with a ward column
#' @param ward_col a column for ward values in the arrest/stops dataframe
#' @param ward_vector_order character vector of how to order wards in visualizations/charts
#'
#' @return df with race column ordered as factor
#' @export
#'
#' @examples
ward_prep <- function(df,
                      ward_col = "a.ward",
                      ward_vector_order = c("Ward 1",
                                            "Ward 2",
                                            "Ward 3",
                                            "Ward 4",
                                            "Ward 5",
                                            "Ward 6",
                                            "OOC",
                                            "UNK")){

  # race cols df
  ward_vector <- df[[ward_col]] %>%
    unique()

  if (!all(ward_vector %in% ward_vector_order)){
    stop("Error; some ward values not found in ordered ward vector; re-define ordered ward vector to capture all")
  }

  ward_present <- ward_vector_order[ward_vector_order %in% ward_vector]

  df <- df %>%
    dplyr::mutate(!!dplyr::sym(ward_col) := factor(!!dplyr::sym(ward_col), ward_present, ward_present))

  return(df)
}
