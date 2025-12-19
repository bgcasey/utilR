#' Format Time Difference
#'
#' This function calculates the difference between two timestamps
#' and formats it as days, hours, minutes, and seconds for
#' readability.
#'
#' @param start_time POSIXct. The starting timestamp.
#' @param end_time POSIXct. The ending timestamp.
#' @return Character. A formatted string showing the time
#' difference in days, hours, minutes, and seconds.
#'
#' @example
#' # Example usage of the function
#' start_time <- Sys.time()
#' Sys.sleep(5)  # Simulate a delay
#' end_time <- Sys.time()
#' format_time_diff(start_time, end_time)
format_time_diff <- function(start_time, end_time) {
    # Calculate time difference in seconds
    diff <- as.numeric(difftime(end_time, start_time,
        units = "secs"
    ))

    # Extract days, hours, minutes, and seconds
    days <- floor(diff / 86400) # 1 day = 86400 seconds
    hours <- floor((diff %% 86400) / 3600)
    minutes <- floor((diff %% 3600) / 60)
    seconds <- round(diff %% 60, 2)

    # Format the time difference as a string
    sprintf(
        "%d days, %d hours, %d minutes, %.2f seconds",
        days, hours, minutes, seconds
    )
}
