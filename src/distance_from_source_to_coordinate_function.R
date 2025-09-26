# by Hanna Jaspaert
# Hanna.Jaspaert@UGent.be
# get.coordinates <- function(raster, source, target_dist, target_crs) {
#   points_sf_proj <- st_transform(source, crs = crs(raster))

#   #IF source is not on the river, find the nearest point on the river
#   # Convert sf to SpatVector
#   points_vect <- vect(points_sf_proj)
#   # Extract cell values for points
#   cell_vals <- extract(raster, points_vect)

#   if (inherits(raster, "SpatRaster")) {
#     raster <- raster::raster(raster)
#   }

#   # For points that fall on NA, find the nearest non-NA cell
#   for (i in seq_len(nrow(points_sf_proj))) {
#     if (is.na(cell_vals[i, 2])) {
#       # Find nearest non-NA cell
#       nearest_cell <- terra::adjacent(
#         raster,
#         cells = cellFromXY(
#           raster,
#           st_coordinates(points_sf_proj[i, ])
#         ),
#         directions = 8,
#         pairs = FALSE
#       )
#       # Get coordinates of the nearest non-NA cell
#       non_na_cells <- which(!is.na(values(raster)))
#       nearest_non_na <- non_na_cells[which.min(terra::distance(
#         raster,
#         cellFromXY(
#           raster,
#           st_coordinates(points_sf_proj[i, ])
#         ),
#         non_na_cells
#       ))]
#       # Replace point coordinates with those of the nearest non-NA cell
#       xy <- xyFromCell(raster, nearest_non_na)
#       st_geometry(points_sf_proj)[i] <- st_sfc(
#         st_point(xy),
#         crs = st_crs(points_sf_proj)
#       )
#     }
#   }
#   from_coords <- sf::st_coordinates(points_sf_proj)
#   tr <- transition(raster, max, directions = 8)
#   tr_geocorrected <- geoCorrection(tr, type = "c")

#   acc_cost <- accCost(tr_geocorrected, from_coords)
#   # Find cells close to target distance
#   vals <- getValues(acc_cost)
#   idx <- which(
#     !is.na(vals) &
#       abs(vals - target_dist) == min(abs(vals - target_dist), na.rm = TRUE)
#   )

#   # Extract coordinates of those cells
#   coords <- xyFromCell(raster, idx)
#   #coordinates to sf with crs tha one form the raster
#   coords <- st_as_sf(
#     as.data.frame(coords),
#     coords = c("x", "y"),
#     crs = crs(raster)
#   )
#   #transform to target crs
#   coords <- st_transform(coords, crs = target_crs)

#   return(coords)
# }



#####################################################################################
get.coordinates <- function(raster, source, target_dist, target_crs) {
  points_sf_proj <- st_transform(source, crs = crs(raster))

  # Convert sf to SpatVector
  points_vect <- terra::vect(points_sf_proj)
  cell_vals <- terra::extract(raster, points_vect)

  if (inherits(raster, "SpatRaster")) {
    raster <- raster::raster(raster)
  }

  # Snap points that fall on NA to nearest non-NA cell
  for (i in seq_len(nrow(points_sf_proj))) {
    if (is.na(cell_vals[i, 2])) {
      non_na_cells <- which(!is.na(raster::values(raster)))
      nearest_non_na <- non_na_cells[which.min(terra::distance(
        raster,
        raster::cellFromXY(
          raster,
          sf::st_coordinates(points_sf_proj[i, ])
        ),
        non_na_cells
      ))]

      xy <- raster::xyFromCell(raster, nearest_non_na)
      sf::st_geometry(points_sf_proj)[i] <- sf::st_sfc(
        sf::st_point(xy),
        crs = sf::st_crs(points_sf_proj)
      )
    }
  }

  from_coords <- sf::st_coordinates(points_sf_proj)
  tr <- gdistance::transition(raster, max, directions = 8)
  tr_geocorrected <- gdistance::geoCorrection(tr, type = "c")
  acc_cost <- gdistance::accCost(tr_geocorrected, from_coords)

  # Loop through each distance in target_dist
  all_coords <- lapply(target_dist, function(dist) {
    vals <- raster::getValues(acc_cost)
    idx <- which(
      !is.na(vals) &
        abs(vals - dist) == min(abs(vals - dist), na.rm = TRUE)
    )

    coords <- raster::xyFromCell(raster, idx)
    coords <- sf::st_as_sf(
      as.data.frame(coords),
      coords = c("x", "y"),
      crs = raster::crs(raster)
    )
    coords$target_dist <- dist
    coords
  })

  # Combine into one sf object
  coords <- do.call(rbind, all_coords)

  # Transform to target CRS
  coords <- sf::st_transform(coords, crs = target_crs)

  return(coords)
}
