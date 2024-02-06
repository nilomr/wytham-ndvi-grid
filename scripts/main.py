import os
import threading
import time
from pathlib import Path

import geopandas as gpd
import matplotlib.pyplot as plt
import numpy as np
import pyrootutils

# get the raster values for each grid cell
# Assuming 'grid' is your GeoDataFrame with the grid cells
import rioxarray
import shapely.geometry
from rasterstats import zonal_stats
from shapely.geometry import Point
from tqdm import tqdm

rioxarray.set_options(export_grid_mapping=False)

import pyvips # on ubuntu, also sudo apt install libvips

# ──── FUNCTIONS AND CLASSES ──────────────────────────────────────────────────


def create_grid(shapefile, cell_size, plot=False):
    # total area for the grid
    xmin, ymin, xmax, ymax = shapefile.total_bounds
    # how many cells across and down
    n_cells_x = int((xmax - xmin) / cell_size)
    n_cells_y = int((ymax - ymin) / cell_size)
    # projection of the grid
    crs = shapefile.crs
    # create the cells in a loop
    grid_cells = []
    for i in range(n_cells_x):
        for j in range(n_cells_y):
            x0 = xmin + i * cell_size
            y0 = ymin + j * cell_size
            x1 = x0 + cell_size
            y1 = y0 + cell_size
            grid_cells.append(shapely.geometry.box(x0, y0, x1, y1))

    cell = gpd.GeoDataFrame(grid_cells, columns=["geometry"], crs=crs)

    # Plotting the grid over the data
    if plot:
        ax = shapefile.plot(figsize=(12, 8))
        plt.autoscale(False)
        cell.plot(ax=ax, facecolor="none", edgecolor="grey")
        ax.axis("off")
        plt.show()

    return cell


# this requires pillow, dask, markupsafe
# TODO add as dependencies

# get tif files
root = pyrootutils.find_root()
datadir = Path(root, "data", "Stitches")
files = list(datadir.rglob("*.tif"))

# get area shapefile using geopandas
shapedir = Path(root, "data", "shapefiles")
flight_areas = gpd.read_file(shapedir / "flight_areas.shp")


# Read in wytham shapefile
wytham = gpd.read_file(shapedir / "perimeter.shp")


# REVIEW remove once corrupted files are removed
# order the files by size in mb
filesizes = [os.path.getsize(file) for file in files]
filesizes = [size / (1024 * 1024) for size in filesizes]
# get the smallest file
smallest = min(filesizes)
# remove smallest file from 'files'
files = [
    file for file in files if os.path.getsize(file) / (1024 * 1024) != smallest
]

# get a sample file and flight area shapefile to test
# Get subset of files with 'Flight 1' in the path
file = [f for f in files if "Flight 1/" in str(f)][3]


# REVIEW: Sam needs to change file naming, then review
areacode = file.parents[1].name.split(" ")[1]


# open the raster file with rioxarray
xds = rioxarray.open_rasterio(
    file,
    lock=threading.Lock(),
    chunks=(1, -1, "auto"),
    cache=False,
    masked=True,
)

# Open the raster file with pyvips
xds = pyvips.Image.new_from_file(str(file), access="sequential")

xds = np.asarray(xds)
# get the lower third of the raster file
# xds = xds.isel(y=slice(xds.shape[1] // 3, -1))

xds.shape


# Print some info about the raster file
print(f"Resolution: {xds.rio.resolution()}")
print(f"Shape: {xds.shape}")
print(f"CRS: {xds.rio.crs}")

# # downsample the raster file
# new_res = (5, 5)
# downsampled = xds.rio.reproject(
#     xds.rio.crs,
#     resolution=new_res,
#     resampling=Resampling.nearest,
# )

# Then crop the raster file to the shapefile
# clipped = xds.rio.clip(
#     wytham.geometry.values, wytham.crs, from_disk=True
# )


# then get the shapefile from flight_areas where FlightID == areacode
areashape = flight_areas[flight_areas["FlightID"] == areacode]
# reproject the shapefile to the same CRS as the raster file
areashape = areashape.to_crs(xds.rio.crs)


# Reproject the wytham shapefile to the same CRS as the raster file
wytham = wytham.to_crs(xds.rio.crs)

# TODO: now clip the raster file to the whytham shapefile and
# also minus some metres from the edges of the flight area shapefile

# Divide wytham into a 50x50m grid
grid = create_grid(wytham, 50, plot=True)


# extract around 10 horizontal slices of the raster file (xds) in a way that
# follows horizontal lines in the grid, so that there are no overlaps
# between the slices:

# prepare slices, taking a fifth of the raster file at a time

total_slices = 10  # Change this value to the desired total number of slices
slices = []
slice_size = xds.shape[1] // total_slices
for i in range(0, xds.shape[1], slice_size):
    slices.append(slice(i, i + slice_size))

sliced_geojsons = []

for s in slices:
    # get the slice
    shd = xds[0, s, :].values
    # get the slice's geojson
    slice_geojson = zonal_stats(
        vectors=grid,
        raster=shd,
        stats=["mean", "std", "median"],
        affine=xds.rio.transform(),
        nodata=np.nan,
        all_touched=True,
        geojson_out=True,
    )
    sliced_geojsons.append(slice_geojson)



sl_stats = zonal_stats(
    vectors=grid,
    raster=xds,
    stats=["mean", "std", "median"],
    # affine=xds.rio.transform(),
    nodata=-1000,
    all_touched=True,
    geojson_out=True,
)


# TODO: append these to the grid, careful not to overwrite existing cells:
# check that they are empty first

# Now 'slices' is a list of xarray DataArrays, each one representing a horizontal slice of the raster

# convert the geojson sl_stats to a geodataframe
sl_stats_gdf = gpd.GeoDataFrame.from_features(sl_stats)

# plot these values on the grid
grid["std"] = [s["properties"]["std"] for s in sl_stats]

# crop the grid to the wytham shapefile
cr_grid = grid[grid.intersects(wytham.unary_union)]

# plot the grid
fig, ax = plt.subplots(figsize=(12, 8))
grid.plot(column="std", ax=ax, legend=True)
plt.show()

# Now plot sl_stats_gdf on the full grid, including the wytham shapefile
fig, ax = plt.subplots(figsize=(12, 8))
grid.plot(ax=ax, facecolor="none", edgecolor="grey")
wytham.plot(ax=ax, facecolor="none", edgecolor="red")
sl_stats_gdf.plot(column="std", ax=ax, legend=True)
plt.show()


# Assuming 'grid' is your GeoDataFrame with the grid cells
geometries = (
    grid["geometry"].map(lambda x: shapely.geometry.mapping(x)).tolist()
)

# Initialize an empty list to hold the results
results = []

# Loop over the geometries (grid cells) with tqdm progress bar
for geometry in tqdm(
    geometries[:10],
    desc="Processing",
    ncols=80,
    bar_format="{l_bar}{bar}| {n_fmt}/{total_fmt}",
):
    # Clip the data to the current grid cell
    try:
        clipped = xds.rio.clip([geometry], xds.rio.crs)

        # Compute statistics (or any other processing) on the clipped data
        # For example, compute the mean pixel value
        mean_value = clipped.mean().values.item()

        # Append the result to the list
        results.append(mean_value)

        # Explicitly delete the clipped data to free up memory
        del clipped
    except:
        results.append(np.nan)

# Now 'results' is a list of the computed statistics for each grid cell
clipped
# Find the platform for reference

# Define the coordinates
lon = -1.338616
lat = 51.773304

# Create a GeoDataFrame with a single point geometry
point = Point(lon, lat)
gdf = gpd.GeoDataFrame(geometry=[point], crs="EPSG:4326")

# Transform the coordinates to the CRS of the areashape
gdf_transformed = gdf.to_crs(xds.rio.crs)


# plot the first chunk

# first, get the full extent of the data, then crop the central 1000x1000
# pixels:

#
extent = xds.rio.bounds()
res = 10
center_x = gdf_transformed.geometry.x[0]
center_y = gdf_transformed.geometry.y[0]
xds_crop = xds.rio.clip_box(
    minx=center_x - res / 2,
    miny=center_y - res / 2,
    maxx=center_x + res / 2,
    maxy=center_y + res / 2,
)
xds_crop.plot()
