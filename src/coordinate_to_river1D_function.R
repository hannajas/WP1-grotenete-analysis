# by Hanna Jaspaert
# inspired by the calculation of the distance matrix in https://github.com/inbo/fish-tracking
# Hanna.Jaspaert@UGent.be

#function to calculate the distance from source to a point in the river
# if the point is not on the river, it finds the nearest point on the river
get.distance.from.source <- function(raster, XY) {
  points_sf_proj <- st_transform(XY, crs = crs(raster))
  # Convert sf to SpatVector
  points_vect <- vect(points_sf_proj)

  # Extract cell values for points
  cell_vals <- extract(raster, points_vect)

  if (inherits(raster, "SpatRaster")) {
    raster <- raster::raster(raster)
  }

  # For points that fall on NA, find the nearest non-NA cell
  for (i in seq_len(nrow(points_sf_proj))) {
    if (is.na(cell_vals[i, 2])) {
      # Find nearest non-NA cell
      nearest_cell <- terra::adjacent(
        raster,
        cells = cellFromXY(
          raster,
          st_coordinates(points_sf_proj[i, ])
        ),
        directions = 8,
        pairs = FALSE
      )
      # Get coordinates of the nearest non-NA cell
      non_na_cells <- which(!is.na(values(raster)))
      nearest_non_na <- non_na_cells[which.min(terra::distance(
        raster,
        cellFromXY(
          raster,
          st_coordinates(points_sf_proj[i, ])
        ),
        non_na_cells
      ))]
      # Replace point coordinates with those of the nearest non-NA cell
      xy <- xyFromCell(raster, nearest_non_na)
      st_geometry(points_sf_proj)[i] <- st_sfc(
        st_point(xy),
        crs = st_crs(points_sf_proj)
      )
    }
  }
  tr <- transition(raster, max, directions = 8)
  tr_geocorrected <- geoCorrection(tr, type = "c")

  cst.dst <- costDistance(tr_geocorrected, as_Spatial(points_sf_proj))
  cst.dst.arr <- as.matrix(cst.dst)
  names <- points_sf_proj$name
  rownames(cst.dst.arr) <- names
  colnames(cst.dst.arr) <- names
  out <- as.data.frame(cst.dst.arr) %>%
    #select all rows that start with "rel"
    select(starts_with("rel"))
  out <- out[!grepl("^rel", rownames(out)), ]
  return(out)
}
