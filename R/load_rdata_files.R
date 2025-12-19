#' Load .RData Files into a List
#'
#' This function scans a directory for `.RData` files, loads each file,
#' and stores all objects from the `.RData` files into a single list.
#'
#' @param data_dir Character. The path to the directory containing
#' `.RData` files.
#' @return A list where each element is an object loaded from the
#' `.RData` files.
#'
#' @examples
#' \dontrun{
#' # Example usage of the function
#' data_dir <- "path/to/your/data/directory"
#' all_data <- load_rdata_files(data_dir)
#' print(names(all_data)) # Prints the names of loaded objects
#' }
#'
#' @export
load_rdata_files <- function(data_dir) {
    # Step 1: Get a list of all .RData files in the specified directory
    rdata_files <- list.files(
        data_dir,
        pattern = "\\.RData$",
        full.names = TRUE
    )

    # Step 2: Initialize an empty list to store objects from .RData files
    all_data <- list()

    # Step 3: Loop through each .RData file
    for (file in rdata_files) {
        # Step 3.1: Load objects directly into a named list element
        loaded_objects <- load(file)
        for (obj_name in loaded_objects) {
            all_data[[obj_name]] <- get(obj_name)
        }
    }

    # Step 4: Return the list containing all loaded objects
    return(all_data)
}
