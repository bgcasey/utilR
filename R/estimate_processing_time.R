#' Estimate Total Processing Time
#'
#' Calculates an estimated total processing time for a complete
#' dataset based on the time taken to process a subset of its
#' features. The result is returned as a human-readable string
#' indicating days, hours, minutes, and remaining seconds.
#'
#' @param n_features Numeric. The total number of features in the
#'   dataset.
#' @param nsub Numeric. The number of features actually processed in
#'   the partial subset.
#' @param end_time POSIXct. The time the subset processing ended.
#' @param start_time POSIXct. The time the subset processing started.
#'
#' @return Character string. A human-readable duration representing
#'   the estimated total processing time for all features.
#'
#' @examples
#' # Example usage of the function
#'
#' # Simulate a short processing run
#' start_time <- Sys.time()
#' Sys.sleep(2) # stand-in for some real processing
#' end_time <- Sys.time()
#'
#' # Estimate total time if the above was a test on 100 out of
#' # 10,000 features
#' estimate_processing_time(10000, 100, end_time, start_time)
#'
estimate_processing_time <- function(n_features,
                                     nsub,
                                     end_time,
                                     start_time) {
    # Step 1: Define a helper function to convert seconds to a
    #         human-readable string (days, hours, minutes, seconds).
    convert_seconds <- function(total_seconds) {
        # Round to nearest whole second
        total_seconds <- round(total_seconds)

        sec_in_day <- 86400 # 24 * 3600
        sec_in_hour <- 3600 # 60 * 60
        sec_in_minute <- 60

        days <- total_seconds %/% sec_in_day
        remainder <- total_seconds %% sec_in_day

        hours <- remainder %/% sec_in_hour
        remainder <- remainder %% sec_in_hour

        minutes <- remainder %/% sec_in_minute
        seconds <- remainder %% sec_in_minute

        paste0(
            days, " days, ", hours, " hours, ", minutes,
            " minutes, ", seconds, " seconds"
        )
    }

    # Step 2: Convert elapsed time (end_time - start_time) into
    #         numeric seconds.
    elapsed_secs <- as.numeric(end_time - start_time, units = "secs")

    # Step 3: Scale the partial-processing time to the full dataset:
    #         total time = (n_features / nsub) * elapsed_secs
    total_seconds <- (n_features / nsub) * elapsed_secs

    # Step 4: Convert the total seconds to a human-readable format.
    duration_readable <- convert_seconds(total_seconds)

    # Step 5: Return the final string.
    return(duration_readable)
}
