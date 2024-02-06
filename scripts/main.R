# ──── CONFIGURATION ──────────────────────────────────────────────────────────

config <- config::get()
terra::terraOptions(progress = 3, memmax = 9)
source(file.path(config$path$source, "plot.R"))
source(file.path(config$path$source, "utils.R"))


# ──── SETTINGS ───────────────────────────────────────────────────────────────

epsg <- "EPSG:32630" # Used by the rasters

# List all files
ndvi_tif_paths <- list.files(config$path$derived_data,
    pattern = "*.tif",
    recursive = TRUE,
    full.names = TRUE
)

# ──── READ SHAPEFILE ─────────────────────────────────────────────────────────


# Load Wytham shapefile and reproject it
wytham_shp <- terra::vect(config$path$shp_data, "perimeter") |>
    terra::project(epsg)

# Create a 50x50m grid over the site
wytham_grid <- sf::st_make_grid(wytham_shp, square = FALSE, cellsize = 50) |>
    terra::vect() |>
    terra::crop(wytham_shp)



# ──── PROCESS NDVI ───────────────────────────────────────────────────────────


all_data <- list()
progressr::with_progress({
    p <- progressr::progressor(steps = length(ndvi_tif_paths))

    execution_time <- system.time({
        for (file in ndvi_tif_paths) {
            fname <- basename(dirname(file))
            p(message = paste0("Processing ", fname))
            file_data <- process_ndvi(file, wytham_grid, threshold = c(0.3, 1))
            all_data[[fname]] <- file_data
            p()
        }
    })
})
execution_time <- execution_time["elapsed"]
print(paste0("Time elapsed: ", round(execution_time, 2), " seconds"))

# Append all data
all_df <- do.call(rbind, all_data)




# remove rows where

all_data[2][[1]]


# arrange all_df by cell_id and average any repeated cell_id rows
all_df <- all_df |>
    dplyr::group_by(cell_id) |>
    dplyr::summarize(
        ndvi_mu = mean(ndvi_mu, na.rm = TRUE),
        ndvi_sd = mean(ndvi_sd, na.rm = TRUE),
        .groups = "drop"
    )


# Reconstruct result, so that there is one row for each cell_id
result <- all_df |>
    dplyr::right_join(dplyr::tibble(cell_id = seq_len(nrow(wytham_grid))), by = "cell_id") |>
    dplyr::arrange(cell_id)

wytham_grid$ndvi_mu <- result$ndvi_mu

# plot the same using ggplot
ggplot2::ggplot() +
    tidyterra::geom_spatvector(
        data = wytham_shp, colour = "black", linewidth = 1
    ) +
    tidyterra::geom_spatvector(
        data = wytham_grid, ggplot2::aes(
            fill = ndvi_mu, color = ndvi_mu
        )
    ) +
    ggplot2::scale_fill_viridis_c(na.value = "transparent") +
    ggplot2::scale_color_viridis_c(na.value = "transparent") +
    # remove grid lines and tick lines
    titheme()
