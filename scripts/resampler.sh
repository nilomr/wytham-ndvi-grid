#!/bin/bash

# Define the source and destination directories
src_dir=$(git rev-parse --show-toplevel)/data/Stitches
dst_dir=$(git rev-parse --show-toplevel)/data/derived/lowres

# Define the resolution and the EPSG code
res=5
epsg=32630

# Create the destination directory if it doesn't exist
mkdir -p "$dst_dir"

# Find all .tif files in the source directory and its subdirectories
find "$src_dir" -name "*.tif" | while read src_file; do
    # Create the destination file path
    dst_file="${src_file/$src_dir/$dst_dir}"

    # Create the destination directory if it doesn't exist
    mkdir -p "$(dirname "$dst_file")"

    # Warp the source file to the destination file
    gdalwarp -tr $res $res -srcnodata -10000 -multi -s_srs EPSG:$epsg -t_srs EPSG:$epsg "$src_file" "$dst_file"
done