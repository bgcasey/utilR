#' Read TIF Files into a Named List
#'
#' Reads all `.tif` files from a specified directory and stores
#' them as raster objects in a named list. The names of the list
#' elements are derived from the TIF file names (without the `.tif`
#' extension).
#'
#' @param tif_directory Character, the path to the directory
#' containing `.tif` files.
#' @param tif_list List, an optional existing list to append the
#' raster objects to. Defaults to an empty list.
#' @return A list where each element is a raster object, named
#' after the corresponding TIF file.
#'
#' @examples
#' # Example usage of the function
#' tif_directory <- "path/to/tif/files"
#' tif_list <- read_tifs_to_list(tif_directory)
#' print(names(tif_list)) # Display names of list elements
#' @export
read_tifs_to_list <- function(tif_directory, tif_list = list()) {
    # Step 1: List all TIF files in the specified directory
    tif_files <- list.files(
        path       = tif_directory,
        pattern    = "\\.tif$",
        full.names = TRUE
    )

    # Step 2: Loop through each TIF file
    for (tif_file in tif_files) {
        # Step 3: Extract the file name without the `.tif` extension
        tif_name <- gsub("\\.tif$", "", basename(tif_file))

        # Step 4: Read the TIF file as a raster object
        raster_obj <- terra::rast(tif_file)

        # Step 5: Add the raster object to the list using the file name
        tif_list[[tif_name]] <- raster_obj
    }

    # Step 6: Return the populated list of raster objects
    return(tif_list)
}
