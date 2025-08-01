if (!require(devtools)) {
  install.packages('devtools')
}
library(tidyverse)
library(lubridate)
library(sf)
library(crawl)
library(ggspatial)
library(mapview)
library(prettymapr)
library(dbscan)
library(dplyr)
library(adehabitatLT)
library(factoextra)

############################################################################################
# set up the data structure
data <- read_csv(
  './data/interim/migration_env_filter.csv',
  show_col_types = FALSE
)
data_eels <- data %>% filter(tag_serial_number == "1171746")
#longitude, latitude to UTM
#cord.dec <- SpatialPoints(cbind(data_eels$deploy_latitude, data_eels$deploy_longitude),proj4string=CRS("+proj=longlat"))
#cord.dec <- SpatialPoints(data_eels[,c("deploy_longitude","deploy_latitude")],proj4string=CRS("+proj=longlat"))
#test <- spTransform(cord.dec,CRS("+proj=utm +zone=31 +ellps=WGS84"))
#data_eels[,c("deploy_latitude", "deploy_longitude")] <- coordinates(test)

#distance wordt hier in vogelvlucht berekend MAAR paling gaat langs de rivier
# IK HEB GEEN INFO OVER DE TALLIJN! DUS niet juist te berekenen op deze resolutie
data_eels$middledate <- as.POSIXct(round_date(
  data_eels$arrival + data_eels$residence / 2,
  "15 mins"
)) #PJ werkt op arrival time en niet op middledate
data_eels <- data_eels %>%
  rename(
    x = deploy_longitude,
    y = deploy_latitude
  )
xy <- data_eels[, c("x", "y")]
date <- data_eels$middledate
id <- as.character(data_eels$tag_serial_number)
data_eels$error_radius <- NA
data_eels$error_semi_major_axis <- 0.5 #500
data_eels$error_semi_minor_axis <- 0.5 #500
data_eels$error_ellipse_orientation <- 0 #als minor = major length DAN maakt orientation niet uit denk ik


##################################################################################################################
# try with data preproccessed with RSP
data <- load("./data/analysis/runRSP_out_A69-9006-3945.RData")
colnames <- names(runRSP_out_1$detections[["A69-9006-3945"]])
data_eels <- as.data.frame(
  runRSP_out_1$detections[["A69-9006-3945"]],
  col.names = colnames
)

#timestamp instead of middledate
#data_eels$middledate <- as.POSIXct(round_date(data_eels$arrival+data_eels$residence/2, "15 mins"))#PJ werkt op arrival time en niet op middledate
data_eels <- data_eels %>%
  rename(
    x = Longitude,
    y = Latitude,
    middledate = Timestamp
  ) %>%
  mutate(
    tag_serial_number = "1171746"
  )
xy <- data_eels[, c("x", "y")]
date <- data_eels$middledate
id <- as.character(data_eels$tag_serial_number)
data_eels$error_radius <- NA
data_eels$error_semi_major_axis <- data_eels$Error
data_eels$error_semi_minor_axis <- data_eels$Error
data_eels$error_ellipse_orientation <- 0 #als minor = major length DAN maakt orientation niet uit denk ik

############################################################################################
# Smoothing (crawl: https://jmlondon.github.io/crawl-workshop/crawl-practical.html#fitting-with-crawlcrwmle)
sf_locs <- sf::st_as_sf(data_eels, coords = c("x", "y")) %>%
  sf::st_set_crs(4326) %>%
  sf::st_transform(31370) #Belgian Lambert 72


sf_locs <- sf_locs %>%
  dplyr::group_by(tag_serial_number) %>%
  dplyr::arrange(middledate) %>% #tag_serial_number????
  tidyr::nest() %>% #one row for each group, a column named 'data' is created which contains the rows that used to be there for this group
  dplyr::mutate(data = furrr::future_map(data, sf::st_as_sf)) #convert back into an sf object after the group by and nesting

sf_locs <- sf_locs %>%
  dplyr::mutate(
    diag = purrr::map(
      data,
      ~ crawl::argosDiag2Cov(
        #error range --> usable as a covariance matrix to approximate the location error with a bivariate Gaussian distribution
        .x$error_semi_major_axis,
        .x$error_semi_minor_axis,
        .x$error_ellipse_orientation
      )
    ), #diag is nu een nieuwe kolom (deze kolom bevat ln.sd.x, ln.sd.y, rho)
    data = purrr::map2(data, diag, bind_cols)
  ) %>% #deze kolommen worden ingevoegd bij kolom 'data'
  dplyr::select(-diag)

# fix parameter - initial state of the model
init_params <- function(d) {
  #if d=sf_locs$data[[i]] this works
  if (any(colnames(d) == "x") && any(colnames(d) == "y")) {
    #deze if is het nooit
    ret <- list(a = c(d$x[1], 0, d$y[1], 0), P = diag(c(1, 1, 1, 1)))
  } else if (inherits(d, "sf")) {
    #dit wel inherits(sf_locs$data[[i]],"sf") == TRUE
    ret <- list(
      a = c(
        sf::st_coordinates(d)[[1, 1]],
        0, #a is the starting location of the model: waarom moet die 0 hier tussen?
        sf::st_coordinates(d)[[1, 2]],
        0
      )
    )
  }
  ret
}

fixPar <- c(1, 1, NA, NA) #[e_x, e_y, sigma, beta] if NA it will be estimated by the model
#e_x = x variance, e_y = y variance
#the first two can be one as this is allready provided by the ellips information
#sigma = almost always set to NA
#beta = auto-correlation matrix (almost always set to NA)

sf_locs <- sf_locs %>%
  dplyr::mutate(
    fixpar = rep(
      list(fixPar),
      nrow(.)
    )
  )
##########################################################################################
# crwMLE function
##########################################################################################
fit_crawl <- function(d, fixpar) {
  prior <- function(p) {
    dnorm(p[2], -0.4, 0.1, log = TRUE)
  }
  fit <- crawl::crwMLE(
    mov.model = ~1,
    err.model = list(
      x = ~ ln.sd.x - 1,
      y = ~ ln.sd.y - 1,
      rho = ~error.corr
    ),
    if (any(colnames(d) == "activity")) {
      activity <- ~ I(activity)
    } else {
      activity <- NULL
    },
    fixPar = fixpar, #column name of this in the dataframe?
    data = d,
    drift = FALSE,
    prior = NULL,
    method = "Nelder-Mead",
    Time.name = "middledate",
    attempts = 8, #8
    control = list(
      trace = 0
    ),
    initialSANN = list(
      maxit = 1500,
      trace = 0
    ) #,
    #init = init_params(d)
  )
  fit
}


library(pander)
tbl_locs_fit <- sf_locs %>%
  dplyr::mutate(
    fit = furrr::future_pmap(list(d = data, fixpar = fixpar), fit_crawl), #de functie fit_crawl werkt op sf_locs$data[[i]] (sf_locs staat vooraan dus dan moet je enkel nog data schrijven om sf_locs$data te krijgen en de iteratie gebeurd door furrr!!)
    params = map(fit, crawl::tidy_crwFit)
  )


test <- furrr::future_pmap(
  list(d = sf_locs$data, fixpar = sf_locs$fixpar),
  fit_crawl
)
##########################################################################################
# visualisation
##########################################################################################
.get_sim_tracks <- function(crw_fit, iter) {
  simObj <- crw_fit %>% crawl::crwSimulator(predTime = '1 hour') #Construct a posterior simulation object for the CTCRW state vectors

  sim_tracks = list()
  for (i in 1:iter) {
    sim_tracks[[i]] <- crawl::crwPostIS(simObj, fullPost = FALSE) #Simulate a value from the posterior distribution of a CTCRW model
  }
  return(sim_tracks)
}

tbl_locs_fit <- tbl_locs_fit %>%
  dplyr::mutate(sim_tracks = purrr::map(fit, .get_sim_tracks, iter = 1)) #iter = aantal simulaties die getekend zullen worden

#convert the simulated tracks to a sf object
tbl_locs_fit <- tbl_locs_fit %>%
  dplyr::mutate(
    sim_lines = crawl::crw_as_sf(
      .$sim_tracks,
      ftype = "MULTILINESTRING",
      locType = "p"
    )
  )


#effect van e_x en e_y komt niet door!!
sf_sim_lines <- do.call(rbind, tbl_locs_fit$sim_lines) %>%
  mutate(id = tbl_locs_fit$tag_serial_number) %>%
  rename(deployid = id)

esri_ocean <- paste0(
  'https://services.arcgisonline.com/arcgis/rest/services/',
  'Ocean/World_Ocean_Base/MapServer/tile/${z}/${y}/${x}.jpeg'
)

ggplot() +
  annotation_map_tile(type = esri_ocean, zoomin = 1, progress = "none") +
  layer_spatial(sf_sim_lines, size = 0.75, aes(color = deployid)) +
  scale_x_continuous(expand = expand_scale(mult = c(.6, .6))) +
  scale_y_continuous(expand = expand_scale(mult = c(0.35, 0.35))) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none") +
  ggtitle(
    "Predicted Location Paths",
    subtitle = "harbor seals (n=2), Aleutian Islands, Alaska, USA"
  )

test_map <- mapView(sf_sim_lines)

tbl_locs_fit$params

mapview(sf_locs)

##########################################################################################
# extract simulation results
##########################################################################################
sim_coord_df <- as.data.frame(tbl_locs_fit$sim_tracks[[1]][[1]]$alpha.sim)
sample <- sim_coord_df %>%
  dplyr::mutate(
    middledate = tbl_locs_fit$sim_tracks[[1]][[1]]$middledate,
    tag_serial_number = tbl_locs_fit$tag_serial_number[1]
  ) %>%
  rename(longitude = mu.x, latitude = mu.y)

regular_sample <- sample %>% #select only round hours
  dplyr::filter(
    middledate == lubridate::round_date(middledate, "1 hour")
  ) %>% #to sf object
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 31370)
mapView(regular_sample)

st_write(regular_sample, dsn = "./figures/regular_sample_1171746.shp")

##########################################################################################
# load the projected data
##########################################################################################
sample_proj <- read_csv(
  "C:/Code/fish-tracking/fish-tracking/scripts/receiver_distance_analysis/sample_1171746_proj.csv"
)
sf_sample_proj <- sf::st_as_sf(sample_proj, coords = c("X", "Y")) %>%
  sf::st_set_crs(31370) %>%
  mutate(
    middledate = regular_sample$middledate,
    tag_serial_number = regular_sample$tag_serial_number
  ) %>%
  select(-station_name, -animal_project_code)
mapView(sf_sample_proj)

#add column with the distance between the points
#this is the ABSOLUTE DISTANCE!! schoudl be changed!
sf_sample_proj <- sf_sample_proj %>%
  dplyr::mutate(
    distance = c(
      0,
      sf::st_distance(
        sf_sample_proj[-nrow(sf_sample_proj), ],
        sf_sample_proj[-1, ],
        by_element = TRUE
      )
    ), ## add a column with the speed
    speed = distance /
      as.numeric(difftime(middledate, dplyr::lag(middledate), units = "secs"))
  )

##########################################################################################
#clustering
##########################################################################################
#kmeans
sf_sample_proj_speed <- sf_sample_proj %>%
  dplyr::filter(!is.na(speed))
speed <- sf_sample_proj_speed$speed


result <- kmeans(speed, centers = 2, nstart = 10)
result

ggplot(sf_sample_proj_speed) +
  annotation_map_tile(type = esri_ocean, zoomin = 1, progress = "none") +
  geom_sf(
    data = sf_sample_proj_speed,
    aes(color = as.factor(result$cluster)),
    size = 3
  ) + #manual colors
  scale_color_manual(values = c("red", "#29a11e")) +
  labs(color = "State") +
  ggtitle("Speed clustering eel 1171746")
#save
ggsave("./figures/Clustering/speed_kmeans_1h_1171746.png")

# ook geprobeerd met log speed, maar dit werkt niet!
sf_sample_proj_speed_log <- sf_sample_proj %>%
  dplyr::filter(!is.na(speed) & speed > 0) %>%
  dplyr::mutate(log_speed = log(speed))
speed_log <- sf_sample_proj_speed_log$log_speed
result_log <- kmeans(speed_log, centers = 2, nstart = 10)

ggplot(sf_sample_proj_speed_log) +
  annotation_map_tile(type = esri_ocean, zoomin = 1, progress = "none") +
  geom_sf(
    data = sf_sample_proj_speed_log,
    aes(color = as.factor(result_log$cluster)),
    size = 3
  ) + #manual colors
  scale_color_manual(values = c("red", "#29a11e")) +
  labs(color = "State") +
  ggtitle("Speed clustering eel 1171746 (log speed)")


##########################################################################################
# dbscan
kNNdist <- kNNdistplot(as.matrix(speed), k = 8)
db <- dbscan(as.matrix(speed), eps = 0.025, minPts = 8)
sf_sample_proj_speed$cluster <- db$cluster
sf_sample_proj_speed$cluster[sf_sample_proj_speed$cluster == 0] <- "noise"

print(db)

ggplot() +
  annotation_map_tile(type = esri_ocean, zoomin = 1, progress = "none") +
  geom_sf(
    data = sf_sample_proj_speed,
    aes(color = as.factor(cluster)),
    size = 3
  ) +
  scale_color_manual(values = c("red", "#29a11e", "blue")) +
  labs(color = "Cluster") +
  ggtitle("Speed clustering eel 1171746")
ggsave("./figures/Clustering/speed_dbscan_1h_1171746.png")
#log speed
kNNdist <- kNNdistplot(as.matrix(speed_log), k = 2) #speeds are ordered, then the distance for the lowest speed to the k-th nearest neighbor is plotted
db <- dbscan(as.matrix(speed_log), eps = 0.13, minPts = 8)
db

ggplot(sf_sample_proj_speed_log) +
  geom_sf(aes(color = as.factor(db$cluster))) +
  scale_color_brewer(palette = "Set1") +
  labs(color = "Cluster") +
  ggtitle("Speed Clustering of Eel Migration Data")


##########################################################################################
# simple interpolation of the trajectory
##########################################################################################
data <- read_csv(
  './data/interim/migration_env_filter.csv',
  show_col_types = FALSE
)

cord.dec <- SpatialPoints(
  data[, c("deploy_longitude", "deploy_latitude")],
  proj4string = CRS("+proj=longlat")
)
test <- spTransform(cord.dec, CRS("+proj=utm +zone=31 +ellps=WGS84"))
data[, c("x", "y")] <- coordinates(test)
resolution_s <- "15 min"
minutes <- 15

data$middledate <- as.POSIXct(data$arrival + data$residence / 2) #PJ werkt op arrival time en niet op middledate
data$rounded_date <- floor_date(data$middledate, unit = resolution_s)
data$tag_serial_number <- as.character(data$tag_serial_number)

id <- unique(data$tag_serial_number)
#Store data in an object of class "ltraj"
xy <- data[, c("x", "y")]
date <- data$middledate
id <- as.character(data$tag_serial_number)
id_unique <- unique(id)
traj <- as.ltraj(xy, data$middledate, id) #traj[[1]]$dist --> 21 object for which the last one is NA
#t is one hour in seconds

#data$cluster <- NA

inter_all <- redisltraj(traj, u = minutes * 60, type = "time")

# id toevoegen aan elk traject
for (k in 1:length(inter_all)) {
  inter_all[[k]]$id <- id_unique[k]
}
data_inter <- do.call(rbind.data.frame, inter_all) # %>% filter(!is.na(dist))
data_inter$is_original <- NA
valid_ids <- unique(data_inter$id)
data_inter.eel <- split(data_inter, data_inter$id)
data_inter <- data_inter %>%
  rename(
    tag_serial_number = id
  )

data_filtered <- data[data$tag_serial_number %in% valid_ids, ]
data.eel.filtered <- split(data_filtered, data_filtered$tag_serial_number)

for (k in seq_along(data_inter.eel)) {
  inter.temp <- data_inter.eel[[k]]
  data.temp <- data.eel.filtered[[k]]

  original_times <- data.temp$middledate
  new_times <- inter.temp$date

  #is_original <- floor_date(new_times, unit = resolution_s) %in% floor_date(original_times, unit = resolution_s) # check if the date in inter.temp is in the original data
  is_original <- new_times %in% original_times

  row_ids <- which(data_inter$tag_serial_number == names(data_inter.eel)[k]) #select the row id's in data_inter of id k

  data_inter$is_original[row_ids] <- as.integer(is_original)
}

# with log but nog recommended, because now skewed to lower speeds
log_dist <- data_inter$dist
not_na_idx <- which(!is.na(log_dist))

not_na_idx <- which(!is.na(data_inter$dist))

data_inter_env <- left_join(
  data_inter[, c("x", "y", "date", "dist", "dt", "tag_serial_number")],
  data,
  by = c("tag_serial_number", "date" = "rounded_date")
)

#I want to put values in the data_inter_env$speed_m_s column by filling in all the row above a value with that value

data_inter_env <- data_inter_env %>%
  group_by(tag_serial_number) %>%
  mutate(
    speed_m_s = zoo::na.locf(speed_m_s, fromLast = TRUE, na.rm = FALSE),
    distance_to_source_m = zoo::na.locf(distance_to_source_m, fromLast = TRUE, na.rm = FALSE),
    deploy_latitude = zoo::na.locf(deploy_latitude, fromLast = TRUE, na.rm = FALSE),
    deploy_longitude = zoo::na.locf(deploy_longitude, fromLast = TRUE, na.rm = FALSE)#,
    #station_name = zoo::na.locf(station_name, fromLast = TRUE, na.rm = FALSE)
  ) %>%
  ungroup()

#save as csv
write_csv(data_inter_env, "./data/interim/migration_inter.csv")