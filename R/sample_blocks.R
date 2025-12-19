#' Sample a fraction of rows within blocks
#'
#' This function takes a data frame and samples a specified
#' fraction of rows within each block, defined by a given
#' column. Sampling is done without replacement, so each row
#' can appear at most once in the output.
#'
#' @param data A data frame containing the data to sample from.
#' @param frac Numeric. Fraction of rows to sample from each
#' block (default = 0.05).
#' @param block_column Character. The name of the column in
#' `data` that defines blocks.
#'
#' @return A data frame containing the sampled rows, with
#' approximately `frac` rows per block.
#'
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'     block_id = rep(letters[1:4], each = 1000),
#'     value = rnorm(4000)
#' )
#' df_sub <- sample_blocks(
#'     df,
#'     frac = 0.05, block_column = "block_id"
#' )
#' dplyr::count(df_sub, block_id)
#' @importFrom dplyr %>% group_by sample_frac ungroup
#' @importFrom rlang .data
#' @export
sample_blocks <- function(data, frac = 0.05, block_column) {
    # Step 1: Check that the block column exists in the data
    if (!block_column %in% names(data)) {
        stop(glue::glue(
            "Column '{block_column}' not found in data."
        ))
    }

    # Step 2: Group the data by the block column
    result <- data %>%
        dplyr::group_by(.data[[block_column]]) %>%
        # Step 3: Sample the specified fraction of rows within
        # each group
        dplyr::sample_frac(size = frac) %>%
        # Step 4: Remove grouping structure before returning
        dplyr::ungroup()

    # Step 5: Return the sampled data
    return(result)
}
