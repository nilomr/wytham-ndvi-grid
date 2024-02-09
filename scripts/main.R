# ──── CONFIGURATION ──────────────────────────────────────────────────────────

config <- config::get()
terra::terraOptions(progress = 3, memmax = 9)
source(file.path(config$path$source, "plot.R"))
source(file.path(config$path$source, "utils.R"))


# ──── SETTINGS ───────────────────────────────────────────────────────────────

epsg <- "EPSG:32630" # Used by the rasters

# List all files
ndvi_tif_paths <- list.files(config$path$derived_data,
    pattern = "*.tif$",
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

# Load flight path shapefile
flights_shp <- terra::vect(config$path$shp_data, "flight_areas") |>
    terra::project(epsg)


# ──── PROCESS NDVI ───────────────────────────────────────────────────────────


all_data <- list()
progressr::with_progress({
    p <- progressr::progressor(steps = length(ndvi_tif_paths))

    execution_time <- system.time({
        for (file in ndvi_tif_paths) {
            fname <- basename(dirname(file))
            p(message = paste0("Processing ", fname))
            file_data <- process_ndvi(file, wytham_grid, threshold = c(0.4, 1))
            all_data[[fname]] <- file_data
            p()
        }
    })
})
execution_time <- execution_time["elapsed"]
print(paste0("Time elapsed: ", round(execution_time, 2), " seconds"))

# Append all data
all_df <- do.call(rbind, all_data)


# ──── EXTRACT TIME SERIES ────────────────────────────────────────────────────


# Remove May 13th 2023 data - something went wrong with some flights
cell_ts <- all_df |>
    dplyr::filter(flight_date != "2023-05-13")


# filter the last three dates for each cell and average the ndvi_mu values
# across those three dates, then get the top 5% and bottom 5% of the cells with
# the highest and lowest ndvi_mu values and the top and bottom 5% of the cells
# with the highest and lowest ndvi_sd values
pc <- 0.05
pcsd <- 0.15
max_mu <- cell_ts |>
    dplyr::group_by(cell_id) |>
    dplyr::arrange(flight_date) |>
    dplyr::mutate(
        ndvi_mu = zoo::rollmean(ndvi_mu, k = 3, fill = NA, align = "right"),
        ndvi_sd = zoo::rollmean(ndvi_sd, k = 3, fill = NA, align = "right")
    ) |>
    dplyr::filter(dplyr::row_number() > dplyr::n() - 3) |>
    dplyr::group_by(cell_id) |>
    dplyr::summarize(
        ndvi_mu = mean(ndvi_mu, na.rm = TRUE),
        ndvi_sd = mean(ndvi_sd, na.rm = TRUE),
        flight_date = max(flight_date),
        .groups = "drop"
    ) |>
    # get the lower 10% of the cells with the highest ndvi_mu
    dplyr::arrange(desc(ndvi_mu)) |>
    dplyr::mutate(
        mu_class = ifelse(dplyr::row_number() <= round(dplyr::n() * pc), "top", "")
    ) |>
    # get the lower 5% of the cells with the lowest ndvi_mu
    dplyr::arrange(ndvi_mu) |>
    dplyr::mutate(
        mu_class = ifelse(dplyr::row_number() <= round(dplyr::n() * pc), "bottom", mu_class)
    ) |>
    dplyr::arrange(ndvi_sd) |>
    dplyr::mutate(
        sd_class = ifelse(dplyr::row_number() <= round(dplyr::n() * pcsd), "bottom", "")
    ) |>
    dplyr::arrange(desc(ndvi_sd)) |>
    dplyr::mutate(
        sd_class = ifelse(dplyr::row_number() <= round(dplyr::n() * pc), "top", sd_class)
    )




mu_subset <- max_mu |>
    dplyr::filter(mu_class != "") |>
    dplyr::arrange(desc(ndvi_mu)) |>
    dplyr::select(cell_id, mu_class)

# get all the data for the cell_ids in mu_subset
minmax_data <- mu_subset |>
    dplyr::left_join(cell_ts, by = "cell_id")

# get cell_id of cells where sd_class is "top"
sd_subset <- max_mu |>
    dplyr::filter(sd_class == "top") |>
    dplyr::arrange(desc(ndvi_sd)) |>
    dplyr::select(cell_id)




# plot the time series for each cell, grouping by cell_id, x axis is flight_date
# and y axis is ndvi_mu
p_0 <-
    cell_ts |>
    ggplot2::ggplot() +
    ggplot2::geom_line(
        ggplot2::aes(
            x = flight_date, y = ndvi_mu,
            group = cell_id, color = ndvi_mu
        ),
        alpha = 0.02
    ) +
    ggplot2::geom_smooth(
        data = cell_ts, ggplot2::aes(
            x = flight_date, y = ndvi_mu
        ), method = "gam", se = FALSE, color = "#cccccc75"
    ) +
    ggplot2::scale_color_viridis_c(option = "viridis") +
    titheme(aspect_ratio = 1) +
    ggplot2::guides(color = ggplot2::guide_colorbar(ticks.colour = NA)) +
    ggplot2::labs(
        color = "Mean\nNDVI",
        title = "Mean NDVI time series for each cell",
        x = "Date", y = "Mean NDVI"
    )

ggplot2::ggsave(
    file.path(config$path$figures, "ndvi_mu_time_series.png"),
    plot = p_0,
    bg = "transparent",
    width = 15, height = 15, dpi = 300,
    units = "cm"
)




# plot the time series for the cells with the highest ndvi_mu

p_1 <-
    minmax_data |>
    ggplot2::ggplot(ggplot2::aes(
        x = flight_date, y = ndvi_sd,
        group = cell_id, color = mu_class
    )) +
    ggplot2::geom_line(
        alpha = 0.3, linewidth = 0.3
    ) +
    # add a regression line for each mu_class
    ggplot2::geom_smooth(
        ggplot2::aes(
            x = flight_date, y = ndvi_sd,
            group = mu_class, color = mu_class
        ),
        method = "loess", se = FALSE, linewidth = .8, alpha = 0.5
    ) +
    ggplot2::scale_color_manual(
        values = c("top" = "#f4e52a", "bottom" = "#2d748e"),
        labels = c("top" = "Top 5%", "bottom" = "Bottom 5%"),
        guide = ggplot2::guide_legend(reverse = TRUE, size = 20)
    ) +
    # add more informative legend title and labels
    ggplot2::labs(
        color = "Mean NDVI",
        title = "NDVI SD time series for the cells\nwith the Highest and Lowest mean NDVI",
        x = "Date", y = "NDVI SD"
    ) +
    titheme(aspect_ratio = 1)

ggplot2::ggsave(
    file.path(config$path$figures, "ndvi_sd_time_series.png"),
    plot = p_1,
    bg = "transparent",
    width = 15, height = 15, dpi = 300,
    units = "cm"
)


# plot ndvi_sd vs ndvi_mu, colored by date

p_2 <-
    cell_ts |>
    # get a subset with only 10% of the data
    dplyr::sample_frac(0.4) |>
    ggplot2::ggplot() +
    ggplot2::geom_point(
        ggplot2::aes(x = ndvi_mu, y = ndvi_sd, fill = flight_date),
        alpha = 0.3, shape = 21, stroke = NA, size = 1
    ) +
    ggplot2::geom_smooth(
        ggplot2::aes(x = ndvi_mu, y = ndvi_sd),
        method = "loess", se = FALSE, color = "#48848fb9", linewidth = .8
    ) +
    ggplot2::scale_fill_viridis_c(option = "viridis", trans = "date") +
    titheme() +
    # change legend title to "Flight Date"
    ggplot2::labs(
        fill = "Flight Date", x = "Mean NDVI", y = "SD NDVI",
        title = "NDVI SD vs Mean NDVI",
        subtitle = "Colored by flight date"
    ) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(ticks.colour = NA))


ggplot2::ggsave(
    file.path(config$path$figures, "ndvi_sd_vs_ndvi_mu.png"),
    plot = p_2,
    bg = "transparent",
    width = 15, height = 15, dpi = 300,
    units = "cm"
)




# ──── MAP OF THE ENTIRE SITE ─────────────────────────────────────────────────


# Get the difefrent flight_dates for each flight_code in the data
flight_dates <- all_df |>
    dplyr::select(flight_code, flight_date) |>
    dplyr::distinct() |>
    dplyr::arrange(flight_date)

# Find the dates for which there is data for all flight_codes
common_dates <- flight_dates |>
    dplyr::group_by(flight_date) |>
    dplyr::filter(dplyr::n() == length(unique(flight_dates$flight_code))) |>
    dplyr::distinct(flight_date) |>
    dplyr::arrange(flight_date)

# Filter the data to only include the common dates
full_pop <- all_df |>
    dplyr::filter(flight_date %in% common_dates$flight_date)

# arrange full_pop by cell_id and average any repeated cell_id rows
full_pop <- full_pop |>
    dplyr::group_by(cell_id, flight_date) |>
    dplyr::summarize(
        ndvi_mu = mean(ndvi_mu, na.rm = TRUE),
        ndvi_sd = mean(ndvi_sd, na.rm = TRUE),
        .groups = "drop"
    ) |>
    dplyr::ungroup()
# Reconstruct result, so that there is one row for each cell_id
result <- full_pop |>
    dplyr::right_join(dplyr::tibble(
        cell_id = seq_len(nrow(wytham_grid))
    ), by = "cell_id")


# Add this data to the wytham_grid
wytham_grid$cell_id <- seq_len(nrow(wytham_grid))
# remove the flight_date ndvi_mu ndvi_sd columns from wytham_grid, if they
# exist, using R base subsetting
wytham_grid <- wytham_grid[, !names(wytham_grid) %in% c("flight_date", "ndvi_mu", "ndvi_sd")]

wytham_grid <- wytham_grid |>
    tidyterra::left_join(result, by = "cell_id") |>
    # remove where flight_date is NA
    tidyterra::filter(!is.na(flight_date))




# plot proper

# first, only mean ndvi data for 2023-04-25
p_3 <-
    ggplot2::ggplot() +
    tidyterra::geom_spatvector(
        data = wytham_shp, colour = "black", linewidth = .3, fill = "#4f5c51c2"
    ) +
    tidyterra::geom_spatvector(
        data = wytham_grid |> tidyterra::filter(flight_date == "2023-04-25"),
        ggplot2::aes(
            fill = ndvi_mu, color = ndvi_mu
        )
    ) +
    tidyterra::geom_spatvector(
        data = flights_shp, colour = "#1333334f", fill = "transparent",
        linewidth = 0.5
    ) +
    tidyterra::geom_spatvector_text(
        data = flights_shp, ggplot2::aes(label = FlightID),
        size = 3, color = "#133333"
    ) +
    ggplot2::scale_fill_viridis_c(na.value = "transparent") +
    ggplot2::scale_color_viridis_c(na.value = "transparent", guide = "none") +
    titheme() +
    ggplot2::labs(
        title = paste("NDVI values for the entire site"),
        subtitle = "50x50m hex grid, 25th of April 2023",
        fill = "Mean\nNDVI",
        x = NULL, y = NULL
    ) +
    ggplot2::theme(
        axis.ticks = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(ticks.colour = NA))

ggplot2::ggsave(
    file.path(config$path$figures, "ndvi_mu_map.png"),
    plot = p_3,
    bg = "transparent",
    width = 15, height = 15, dpi = 300,
    units = "cm"
)


p_4 <-
    ggplot2::ggplot() +
    tidyterra::geom_spatvector(
        data = wytham_shp, colour = "black", linewidth = .3, fill = "#4f5c51c2"
    ) +
    tidyterra::geom_spatvector(
        data = wytham_grid |> tidyterra::filter(flight_date != "2023-05-13"),
        ggplot2::aes(
            fill = ndvi_mu, color = ndvi_mu
        )
    ) +
    # add facet_grid to split by flight_date into a single row
    ggplot2::facet_grid(~flight_date) +
    ggplot2::scale_fill_viridis_c(na.value = "transparent") +
    ggplot2::scale_color_viridis_c(na.value = "transparent", guide = "none") +
    titheme() +
    ggplot2::labs(
        title = paste("Mean NDVI values"),
        subtitle = "50x50m hex grid, data from flights on the same day\n\n",
        x = NULL, y = NULL,
        fill = "Mean\nNDVI"
    ) +
    ggplot2::theme(
        axis.ticks = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(ticks.colour = NA))

ggplot2::ggsave(
    file.path(config$path$figures, "ndvi_mu_map_facet.png"),
    plot = p_4,
    bg = "transparent",
    width = 25, height = 15, dpi = 300,
    units = "cm"
)

# same but for ndvi_sd

p_5 <-
    ggplot2::ggplot() +
    tidyterra::geom_spatvector(
        data = wytham_shp, colour = "black", linewidth = .3, fill = "#4f5c51c2"
    ) +
    tidyterra::geom_spatvector(
        data = wytham_grid |> tidyterra::filter(flight_date != "2023-05-13"),
        ggplot2::aes(
            fill = ndvi_sd, color = ndvi_sd
        ), na.rm = TRUE
    ) +
    # add facet_grid to split by flight_date into a single row
    ggplot2::facet_grid(~flight_date) +
    ggplot2::scale_fill_viridis_c(na.value = "transparent") +
    ggplot2::scale_color_viridis_c(na.value = "transparent", guide = "none") +
    titheme() +
    ggplot2::labs(
        title = paste("NDVI SD values"),
        subtitle = "50x50m hex grid, data from flights on the same day\n\n",
        x = NULL, y = NULL,
        fill = "SD\nNDVI"
    ) +
    ggplot2::theme(
        axis.ticks = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(ticks.colour = NA))

ggplot2::ggsave(
    file.path(config$path$figures, "ndvi_sd_map_facet.png"),
    plot = p_5,
    bg = "transparent",
    width = 25, height = 15, dpi = 300,
    units = "cm"
)

# Now remove the rows from wytham_grid where cell id is in sd_subset
wytham_grid_sub <- wytham_grid |> tidyterra::filter(!cell_id %in% sd_subset$cell_id)

# plot the distribution of ndvi_sd for the average last three dates for each
# cell

max_mu |>
    ggplot2::ggplot(ggplot2::aes(x = ndvi_sd)) +
    ggplot2::geom_histogram(
        ggplot2::aes(y = ggplot2::after_stat(density)),
        alpha = 0.5, position = "identity",
        bins = 200
    )

# # same but for ndvi_sd using the subset
p_5.1 <-
    ggplot2::ggplot() +
    tidyterra::geom_spatvector(
        data = wytham_shp, colour = "black", linewidth = .3, fill = "#4f5c51c2"
    ) +
    tidyterra::geom_spatvector(
        data = wytham_grid_sub |> tidyterra::filter(flight_date != "2023-05-13"),
        ggplot2::aes(
            fill = ndvi_sd, color = ndvi_sd
        ), na.rm = TRUE
    ) +
    # add facet_grid to split by flight_date into a single row
    ggplot2::facet_grid(~flight_date) +
    ggplot2::scale_fill_viridis_c(na.value = "transparent") +
    ggplot2::scale_color_viridis_c(na.value = "transparent", guide = "none") +
    titheme() +
    ggplot2::labs(
        title = paste("NDVI SD values"),
        subtitle = "50x50m hex grid, removing 5% of the cells\nwith the highest NDVI SD values\n\n",
        x = NULL, y = NULL,
        fill = "SD\nNDVI"
    ) +
    ggplot2::theme(
        axis.ticks = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(ticks.colour = NA))

ggplot2::ggsave(
    file.path(config$path$figures, "ndvi_sd_map_facet_subset.png"),
    plot = p_5.1,
    bg = "transparent",
    width = 25, height = 15, dpi = 300,
    units = "cm"
)

# also remove the rows from wytham_grid where cell id is not in mu_subset
wytham_grid_sub2 <- wytham_grid_sub |> tidyterra::filter(cell_id %in% mu_subset$cell_id)

# plot this new subset (variable: ndvi_sd)
p5.2 <-
    ggplot2::ggplot() +
    tidyterra::geom_spatvector(
        data = wytham_shp, colour = "black", linewidth = .3, fill = "#4f5c51c2"
    ) +
    tidyterra::geom_spatvector(
        data = wytham_grid_sub2 |> tidyterra::filter(flight_date != "2023-05-13"),
        ggplot2::aes(
            fill = ndvi_sd, color = ndvi_sd
        ), na.rm = TRUE
    ) +
    # add facet_grid to split by flight_date into a single row
    ggplot2::facet_grid(~flight_date) +
    ggplot2::scale_fill_viridis_c(na.value = "transparent") +
    ggplot2::scale_color_viridis_c(na.value = "transparent", guide = "none") +
    titheme() +
    ggplot2::labs(
        title = paste("NDVI SD values"),
        subtitle = "50x50m hex grid, keeping 5% highest final mean NDVI\nand removing 15% highest NDVI SD values\n\n",
        x = NULL, y = NULL,
        fill = "SD\nNDVI"
    ) +
    ggplot2::theme(
        axis.ticks = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(ticks.colour = NA))

ggplot2::ggsave(
    file.path(config$path$figures, "ndvi_sd_map_facet_subset2.png"),
    plot = p5.2,
    bg = "transparent",
    width = 25, height = 15, dpi = 300,
    units = "cm"
)
