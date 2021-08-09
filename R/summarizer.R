#' Function to generate group-counts and percentages
#'
#' @param df Data-frame with each row corresponding to an observation you want counted by groups.
#' @param group_cols Character-vector of columns to group the dataframe by, in order of order you want the grouping done.
#' @param output_prefixs Character-vector of equal length as the group_cols with prefixes for output columns corresponding to each group_col; e.g., "type" if you're grouping by type will produce type_num and type_pct.
#'
#' @return A summary dataframe with information on the number of observations per each group/combination of groups in order. E.g., c(animal, breed, likeability) will group by animal, then animal/breed, then animal/breed/likeability, and calculate the percentage of each animal group of all animals, of each animal/breed group of each animal group, and of each animal/breed/likeability group of each animal/breed group.
#' @export
#'
#' @examples
summarizer <- function(df, group_cols, output_prefixs){

  # return error if there's a mismatch between the number of group columns and type columns
  if (length(group_cols) != length(output_prefixs)){
    stop("Error; group_cols argument should be same length as output_prefixs")
  }


  # if just 1 column to group by - group by that and calculate frequencies/percents
  if (length(output_prefixs) == 1){

    num_col <- paste0(output_prefixs, "_num")
    pct_col <- paste0(output_prefixs, "_pct")

    output <- df %>%
      dplyr::group_by() %>%
      dplyr::mutate(total = n()) %>%
      dplyr::group_by(.data[[group_cols]], total) %>%
      dplyr::summarize(!!sym(num_col) := n(),
                       !!sym(pct_col) := tpfuncts::pct_round(!!sym(num_col), total)) %>%
      dplyr::distinct()
  }

  # if multiple group columns - loop through and calculate group totals/percents
  else if (length(output_prefixs) > 1){

    # define empty vector
    cols_list <- c()


    # calculate overall total
    output <- df %>%
      dplyr::group_by() %>%
      dplyr::mutate(grouptotal = n())

    # loop through group columns, calculating subtotals along way
    purrr::walk(1:length(output_prefixs), ~{

      # browser()

      # subset number of columns to position in loop
      group_sub <- group_cols[1:.x]
      prefix_sub <- output_prefixs[1:.x]

      # if at first column - define prefix as total, otherwise position of prefixes - 1
      old_prefix <- dplyr::case_when(.x == 1 ~ "total",
                                     TRUE ~ output_prefixs[1:(.x-1)])

      #  define name of number column for denominator column
      num_col <- paste0(paste(old_prefix, collapse = "_", sep = ""),
                        "_num")

      # define percent column name
      pct_col <- paste0(paste(prefix_sub, collapse = "_", sep = ""),
                        "_pct")

      # if haven't reached end of list of group columns - calculate subtotal for groups so far
      if (.x != length(output_prefixs)){

        # group by all group columns up to that point; calculate percent total of old group and store in higher-level environment so retains for next loop
        output <<- output %>%
          # dplyr::ungroup %>%
          dplyr::group_by(across(group_sub)) %>%
          dplyr::mutate(newtotal = n(),
                        !!sym(pct_col) := tpfuncts::pct_round(newtotal, grouptotal))

        # rename group total to new total - so newtotal becomes denominator in next group, and overall total assigned to new column
        output <<- output %>%
          dplyr::rename(!!sym(num_col) := grouptotal,
                        grouptotal = newtotal)

        # add columns added so far to column list in higher-level environment
        cols_list <<- c(cols_list, num_col, pct_col)

      }

      # if reached end of list of group columns
      else if (.x == length(output_prefixs)){

        # define name of new numeric column
        new_num_col <- paste0(paste(prefix_sub,
                                    sep = "",
                                    collapse = "_"),
                              "_num")

        # group by group columns
        output <<- output %>%
          # dplyr::ungroup %>%
          # preserve subtotals and percents created before, and calculate final totals
          dplyr::group_by(across(group_sub), grouptotal, dplyr::across(cols_list)) %>%
          dplyr::summarise(!!sym(new_num_col) := n(),
                           !!sym(pct_col) := tpfuncts::pct_round(!!sym(new_num_col), grouptotal)) %>%
          dplyr::distinct()

        # rename group total to new total
        output <<- output %>%
          dplyr::rename(!!sym(num_col) := grouptotal)

      }

    })

  }

  return(output)

}

