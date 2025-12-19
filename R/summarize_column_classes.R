#' Generate a Data Frame of Column Classes
#'
#' This function takes a list of data frames and returns a summary
#' data frame showing the classes of each column across all the
#' data frames. Missing columns in some data frames are represented
#' as NA.
#'
#' @param all_data List. A list of data frames.
#' @return A data frame with rows representing the data frames and
#' columns representing the column names from all the data frames. The
#' values are the classes of the columns or NA if the column is
#' missing in a data frame.
#'
#' @example
#' # Example usage:
#' all_data <- list(
#'   df1 = data.frame(a = 1:3, b = letters[1:3]),
#'   df2 = data.frame(a = 4:6, c = c(TRUE, FALSE, TRUE))
#' )
#' column_classes_df <- generate_column_classes(all_data)
#' print(column_classes_df)
#'
summarize_column_classes <- function(all_data) {
    # Step 1: Get column classes for each data frame in all_data
    column_classes <- lapply(all_data, function(df) {
        sapply(df, class)
    })

    # Step 2: Identify all unique column names across the data frames
    all_columns <- unique(unlist(
        lapply(column_classes, names)
    ))

    # Step 3: Create a data frame of column classes
    result <- do.call(rbind, lapply(
        names(column_classes),
        function(df_name) {
            # Step 3.1: Initialize a row with NA for all columns
            row <- setNames(rep(NA, length(all_columns)), all_columns)

            # Step 3.2: Fill in the classes for columns present in the data frame
            col_classes <- column_classes[[df_name]]
            row[names(col_classes)] <- col_classes

            # Step 3.3: Add the data frame name to the row
            c(data_frame = df_name, row)
        }
    ))

    # Step 4: Convert the result to a data frame and return it
    return(as.data.frame(result, stringsAsFactors = FALSE))
}
