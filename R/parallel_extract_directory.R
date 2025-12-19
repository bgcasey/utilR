#' Parallel Extraction of Raster Data Stored in a Directory
#'
#' Executes a parallelized approach to extract values from multi-band
#' raster files using a custom extraction function for each spatial
#' feature in `locations`.
#'
#' @param locations sf or Spatial object. Geographic features for which
#'   raster values are extracted.
#' @param tif_directory Character. The directory path containing the
#'   TIF files.
#' @param bands Character vector. The subset of raster bands to extract
#'   from each TIF file.
#' @param extract_fun Function. The function used to perform the
#'   extraction. Defaults to `extract_by_year` (replace as needed).
#' @param extract_args List. Additional arguments passed on to
#'   `extract_fun`. Defaults to `list(fun = "mean")`.
#' @param extra_exports Character vector. Names of additional objects
#'   or functions to export to the cluster environment, if needed.
#'
#' @return A list of extraction results, one element per feature in
#'   `locations`.
#'
#' @examples
#' # Example usage of the function
#' # Suppose you have a set of locations (e.g., sf polygons or points)
#' # buffers <- sf::read_sf("path/to/shapefile.shp")
#'
#' # result <- parallel_extract_directory(
#' #   locations     = list of location buffers,
#' #   tif_directory = "path/to/tifs",
#' #   bands         = c("SR_B1", "NDVI"),
#' #   extract_fun   = extract_by_year,
#' #   extract_args  = list(fun = "mean")
#' # )
#'
#' # print(result)
#'
#' @importFrom parallel detectCores makeCluster clusterExport clusterEvalQ parLapply stopCluster
#' @export
parallel_extract_directory <- function(locations,
                                       tif_directory,
                                       bands,
                                       extract_fun = extract_by_year,
                                       extract_args = list(fun = "mean"),
                                       extra_exports = NULL) {
    # Step 1: Determine the number of cores
    num_cores <- min(length(locations), detectCores() - 4)

    # Step 2: Initialize the cluster
    cl <- makeCluster(num_cores)

    # Step 3: Export necessary objects to the cluster
    to_export <- c(
        "locations", "read_tifs_to_list", "extract_fun",
        "extract_args", "extra_exports"
    )
    clusterExport(cl, varlist = to_export, envir = environment())

    # Workers will lazily load namespaces when using :: operators.

    # Step 4: Parallel processing
    result <- parLapply(cl, locations, function(buffer) {
        # 4a. Read rasters
        rasters_list <- read_tifs_to_list(tif_directory)

        # 4b. Keep only specified bands
        rasters_list <- lapply(rasters_list, function(rast_obj) {
            rast_obj[[names(rast_obj) %in% bands]]
        })

        # 4c. Perform extraction
        extract_fun(buffer, rasters_list, extract_args)
    })

    # Step 5: Stop the cluster
    stopCluster(cl)

    # Step 6: Return results
    return(result)
}
