config <- config::get()
terra::terraOptions(progress = 3, memmax = 9)
source(file.path(config$path$source, "plot.R"))

ndvi_tif_paths <- list.files(config$path$stiched_data,
    pattern = "*.tif",
    recursive = TRUE,
    full.names = TRUE
)

# Load Wytham shapefile
wytham_shp <- terra::vect(config$path$shp_data, "perimeter")


# Calcualte the size of each file and open the largest file
ndvi_tif_sizes <- file.size(ndvi_tif_paths)
ndvi_tif_path <- ndvi_tif_paths[which.max(ndvi_tif_sizes)]

# Load the NDVI tif
ndvi_tif <- terra::rast(ndvi_tif_path)

# the path looks like this:
# "/home/nilomr/projects/wytham-ndvi-grid/data/Stitches/Flight
# 1/F1_23_06_01/NDVI.data.tif"

# extract the flight number and date from the path
flight_code <- stringr::str_extract(
    ndvi_tif_path, "F[0-9]+"
) |> # convert to integer (remove the first character)
    stringr::str_remove("F") |>
    as.integer()
flight_date <- stringr::str_extract(
    ndvi_tif_path, "F[0-9]+_[0-9]+_[0-9]+_[0-9]+"
) |> # convert to date (remove the first characters before the underscore)
    stringr::str_remove("F[0-9]+_") |>
    lubridate::ymd()


# downsample the NDVI tif to 5m resolution
# calculate the factor to downsample the NDVI tif
endres <- 2
fact <- endres / terra::res(ndvi_tif)[1]
ndvi_tif <- terra::aggregate(ndvi_tif, fact)
# threshold the NDVI tif to remove values < 0.3
ndvi_tif <- terra::clamp(ndvi_tif, 0.3, 1)


# Reproject the shapefile to the same projection as the NDVI tif
wytham_shp <- terra::project(wytham_shp, ndvi_tif)

# Create a 50x50m grid over the shapefile using st_make_grid
# (This will be used to calculate the mean NDVI for each grid cell):
wytham_grid <- sf::st_make_grid(wytham_shp, square = FALSE, cellsize = 50)
# convert to terra
wytham_grid <- terra::vect(wytham_grid)
# crop wytham_grid to wytham_shp
wytham_grid <- terra::crop(wytham_grid, wytham_shp)


# using this grid, calculate the sd NDVI for each grid cell in the NDVI tif
system.time(ndvi_sd <- terra::zonal(
    ndvi_tif, wytham_grid,
    fun = sd, na.rm = TRUE
))

# create a data frame where one column is the cell number ID and the
# other is the sd NDVI:
slice_df <- dplyr::tibble(
    cell_id = seq_len(nrow(wytham_grid)),
    ndvi_sd = ndvi_sd
)
# remove rows with NA values
slice_df <- dplyr::filter(slice_df, !is.na(ndvi_sd))

# git first 10 rows of ndvi_sd
nrow(wytham_grid)


# add the mean NDVI to the grid, and plot the result
wytham_grid$ndvi_sd <- ndvi_sd

# create a data frame from the grid, where one column is the cell number ID and the
# other is the mean NDVI:
wytham_grid_df <- terra::as.data.frame(wytham_grid, na.rm = FALSE)
# add a column for the cell number ID
wytham_grid_df$cell_id <- seq_len(nrow(wytham_grid_df))

# remove rows with NA values
wytham_grid_df <- dplyr::filter(wytham_grid_df, !is.na(ndvi_sd))

length(terra::values(wytham_grid))


# plot the same using ggplot
ggplot2::ggplot() +
    tidyterra::geom_spatvector(
        data = wytham_shp, colour = "black", linewidth = 1
    ) +
    tidyterra::geom_spatvector(
        data = wytham_grid, ggplot2::aes(
            fill = ndvi_sd, color = ndvi_sd
        )
    ) +
    ggplot2::scale_fill_viridis_c(na.value = "transparent") +
    ggplot2::scale_color_viridis_c(na.value = "transparent") +
    # remove grid lines and tick lines
    titheme()

# get the values from the wytham_grid spatvector
wytham_grid_values <- terra::values(wytham_grid)

# get the unique values
unique_values <- unique(wytham_grid_values)

# Divide the shapefile quadrant into 50m x 50m grid
wytham_grid <- terra::disagg(wytham_shp, square = T, 50)

# plot the grid
terra::plot(wytham_grid)
