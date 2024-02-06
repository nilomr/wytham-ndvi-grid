#' Extracts flight information from a file path.
#'
#' This function extracts the flight code and flight date from a given file
#' path. The file path should follow the structure
#' "F[flight_code]_[year]_[month]_[day]_[hour]_[minute]_[second].ext", where
#' [flight_code] is a numeric value and [year], [month], [day], and [hour] are
#' integers.
#'
#' @param file_path The file path from which to extract the flight information.
#'
#' @return A list containing the flight code and flight date.
#' @return$flight_code The numeric flight code extracted from the file path.
#' @return$flight_date The flight date extracted from the file path, represented
#' as a Date object.
#'
#' @examples file_path <- "F123_2022_01_01_12_30_00.txt"
#' extract_flight_info(file_path)
#'
#' @export
get_flight_info <- function(file_path) {
    flight_code <- stringr::str_extract(file_path, "F[0-9]+") |>
        stringr::str_remove("F") |>
        as.integer()

    flight_date <- stringr::str_extract(file_path, "F[0-9]+_[0-9]+_[0-9]+_[0-9]+") |>
        stringr::str_remove("F[0-9]+_") |>
        lubridate::ymd()

    return(list(flight_code = flight_code, flight_date = flight_date))
}


#' Process NDVI data
#'
#' This function processes NDVI data by loading the NDVI tif file, thresholding the values,
#' calculating the mean and standard deviation for each cell, and returning a data frame
#' with the results.
#'
#' @param ndvi_file Path to the NDVI tif file.
#' @param wytham_grid Spatial grid representing the study area.
#' @param threshold Numeric vector specifying the threshold values for NDVI.
#'                  Values below the first threshold will be set to the first threshold,
#'                  and values above the second threshold will be set to the second threshold.
#'
#' @return A data frame with the following columns:
#'   - \code{cell_id}: ID of each cell in the grid.
#'   - \code{ndvi_sd}: Standard deviation of NDVI values for each cell.
#'   - \code{ndvi_mu}: Mean of NDVI values for each cell.
#'   - \code{flight_code}: Flight code associated with the NDVI data.
#'   - \code{flight_date}: Date of the flight associated with the NDVI data.
#'
#' @examples
#' # Process NDVI data using a sample NDVI file and grid
#' ndvi_file <- "/path/to/ndvi.tif"
#' wytham_grid <- readRDS("/path/to/wytham_grid.rds")
#' result <- process_ndvi(ndvi_file, wytham_grid, threshold = c(0.3, 1))
#'
#' @importFrom terra rast clamp zonal
#' @importFrom dplyr tibble filter
#' @export
process_ndvi <- function(ndvi_file, wytham_grid, threshold = c(0.3, 1)) {
    # Load the NDVI tif
    ndvi_tif <- terra::rast(ndvi_file)
    flight_info <- get_flight_info(ndvi_file)

    # Threshold the NDVI tif to remove values < 0.3
    ndvi_tif <- terra::clamp(ndvi_tif, threshold[1], threshold[2])

    # Calculate the mean and standard deviation of the NDVI values
    # for each cell
    results <- lapply(c(sd, mean), function(f) {
        terra::zonal(
            ndvi_tif, wytham_grid,
            fun = f, na.rm = TRUE,
            touches = TRUE
        )
    })

    slice_df <- dplyr::tibble(
        cell_id = seq_len(nrow(wytham_grid)),
        ndvi_sd = unlist(results[[1]]),
        ndvi_mu = unlist(results[[2]]),
        flight_code = flight_info$flight_code,
        flight_date = flight_info$flight_date
    ) |>
        dplyr::filter(!is.na(ndvi_sd))

    return(slice_df)
}
