#' Style the Active R File
#'
#' Locate the currently active R source file (in RStudio or from
#' the --file command line argument) and run styler::style_file on
#' it using a tidyverse style with 2-space indent.
#'
#' @return Invisibly returns the path to the styled file, or NULL
#'   if no active file was found.
#' @export
style_active_file <- function() {
    # 1. Helper: find the active file path ----
    get_active_file <- function() {
        if (requireNamespace("rstudioapi", quietly = TRUE) &&
            rstudioapi::isAvailable()) {
            rstudioapi::getSourceEditorContext()$path
        } else {
            args <- commandArgs(trailingOnly = FALSE)
            file_arg <- grep("--file=", args, value = TRUE)
            if (length(file_arg)) {
                sub("--file=", "", file_arg[1])
            } else {
                NULL
            }
        }
    }

    # 2. Resolve file path
    file_path <- get_active_file()

    # 3. Handle missing file
    if (is.null(file_path)) {
        message("No active file found.")
        return(invisible(NULL))
    }

    # 4. Ensure styler is available
    if (!requireNamespace("styler", quietly = TRUE)) {
        stop("The 'styler' package is required but not installed.")
    }

    # 5. Run styler on the file
    styler::style_file(file_path)

    # 6. Return the path invisibly
    invisible(file_path)
}
