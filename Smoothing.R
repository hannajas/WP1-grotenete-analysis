if(!require(devtools)) install.packages('devtools')
library(tidyverse)
library(lubridate)
library(sf)
library(crawl)
library(ggspatial)
library(mapview)
library(prettymapr)

############################################################################################
# set up the data structure
data_eels <- read_csv('./data/interim/migration_env_filter.csv', show_col_types = FALSE)
data_eels <- data_eels %>% filter(tag_serial_number == "1171746")
#longitude, latitude to UTM
#cord.dec <- SpatialPoints(cbind(data_eels$deploy_latitude, data_eels$deploy_longitude),proj4string=CRS("+proj=longlat"))
#cord.dec <- SpatialPoints(data_eels[,c("deploy_longitude","deploy_latitude")],proj4string=CRS("+proj=longlat"))
#test <- spTransform(cord.dec,CRS("+proj=utm +zone=31 +ellps=WGS84"))
#data_eels[,c("deploy_latitude", "deploy_longitude")] <- coordinates(test)

#distance wordt hier in vogelvlucht berekend MAAR paling gaat langs de rivier
# IK HEB GEEN INFO OVER DE TALLIJN! DUS niet juist te berekenen op deze resolutie
data_eels$middledate <- as.POSIXct(round_date(data_eels$arrival+data_eels$residence/2, "15 mins"))#PJ werkt op arrival time en niet op middledate
data_eels <- data_eels %>%
  rename(
    x = deploy_longitude,
    y = deploy_latitude
    )
xy <- data_eels[,c("x", "y")]
date <- data_eels$middledate
id <- as.character(data_eels$tag_serial_number)
data_eels$error_radius <- NA
data_eels$error_semi_major_axis <- 0.5#500
data_eels$error_semi_minor_axis <- 0.5#500
data_eels$error_ellipse_orientation <- 0#als minor = major length DAN maakt orientation niet uit denk ik


##################################################################################################################
# try with data preproccessed with RSP
data <- load("./data/analysis/runRSP_out_A69-9006-3945.RData")
colnames <- names(runRSP_out_1$detections[["A69-9006-3945"]])
data_eels <- as.data.frame(runRSP_out_1$detections[["A69-9006-3945"]],col.names = colnames)

#timestamp instead of middledate
#data_eels$middledate <- as.POSIXct(round_date(data_eels$arrival+data_eels$residence/2, "15 mins"))#PJ werkt op arrival time en niet op middledate
data_eels <- data_eels %>%
  rename(
    x = Longitude,
    y = Latitude,
    middledate = Timestamp
    ) %>%
  mutate(
    tag_serial_number = "1171746")
xy <- data_eels[,c("x", "y")]
date <- data_eels$middledate
id <- as.character(data_eels$tag_serial_number)
data_eels$error_radius <- NA
data_eels$error_semi_major_axis <- data_eels$Error
data_eels$error_semi_minor_axis <- data_eels$Error
data_eels$error_ellipse_orientation <- 0#als minor = major length DAN maakt orientation niet uit denk ik

############################################################################################
# Smoothing (crawl: https://jmlondon.github.io/crawl-workshop/crawl-practical.html#fitting-with-crawlcrwmle)
sf_locs <- sf::st_as_sf(data_eels, coords = c("x","y")) %>%
    sf::st_set_crs(4326) %>%
    sf::st_transform(31370)#Belgian Lambert 72



sf_locs <- sf_locs %>% 
  dplyr::group_by(tag_serial_number) %>% dplyr::arrange(middledate) %>% #tag_serial_number????
  tidyr::nest() %>%#one row for each group, a column named 'data' is created which contains the rows that used to be there for this group
  dplyr::mutate(data = furrr::future_map(data,sf::st_as_sf))#convert back into an sf object after the group by and nesting

sf_locs <- sf_locs %>%
  dplyr::mutate(
    diag = purrr::map(data, ~ crawl::argosDiag2Cov(#error range --> usable as a covariance matrix to approximate the location error with a bivariate Gaussian distribution
                      .x$error_semi_major_axis, 
                      .x$error_semi_minor_axis, 
                      .x$error_ellipse_orientation)),#diag is nu een nieuwe kolom (deze kolom bevat ln.sd.x, ln.sd.y, rho)
    data = purrr::map2(data,diag,bind_cols)) %>% #deze kolommen worden ingevoegd bij kolom 'data'
  dplyr::select(-diag)

# fix parameter - initial state of the model
init_params <- function(d) {#if d=sf_locs$data[[i]] this works
  if (any(colnames(d) == "x") && any(colnames(d) == "y")) {#deze if is het nooit
  ret <- list(a = c(d$x[1], 0,
                    d$y[1], 0),
              P = diag(c(1, 1,
                         1, 1)))
  } else if (inherits(d,"sf")) {#dit wel inherits(sf_locs$data[[i]],"sf") == TRUE
    ret <- list(a = c(sf::st_coordinates(d)[[1,1]], 0,#a is the starting location of the model: waarom moet die 0 hier tussen?
                      sf::st_coordinates(d)[[1,2]], 0))
  }
  ret
} 

fixPar <- c(1,1,NA,NA)#[e_x, e_y, sigma, beta] if NA it will be estimated by the model
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
      mov.model =  ~ 1,
      err.model = list(
          x =  ~ ln.sd.x - 1,
          y =  ~ ln.sd.y - 1,
          rho =  ~ error.corr
        ),
      if (any(colnames(d) == "activity")) {
            activity <- ~ I(activity)
          } else {activity <- NULL},
      fixPar = fixpar,#column name of this in the dataframe?
      data = d,
      prior = NULL,
      method = "Nelder-Mead",
      Time.name = "middledate",
      attempts = 8,#8
      control = list(
        trace = 0
      ),
      initialSANN = list(
        maxit = 1500,
        trace = 0
      )#,
      #init = init_params(d)
    )
    fit
}



library(pander)
tbl_locs_fit <- sf_locs %>% 
  dplyr::mutate(fit = furrr::future_pmap(list(d = data,fixpar = fixpar),
                           fit_crawl),#de functie fit_crawl werkt op sf_locs$data[[i]] (sf_locs staat vooraan dus dan moet je enkel nog data schrijven om sf_locs$data te krijgen en de iteratie gebeurd door furrr!!)
                params = map(fit, crawl::tidy_crwFit))


test <- furrr::future_pmap(list(d = sf_locs$data,fixpar = sf_locs$fixpar),
                           fit_crawl)
##########################################################################################
# visualisation
##########################################################################################
.get_sim_tracks <- function(crw_fit,iter) {
  
  simObj <- crw_fit %>% crawl::crwSimulator(predTime = '1 hour')
  
  sim_tracks = list()
  for (i in 1:iter) {
    sim_tracks[[i]] <- crawl::crwPostIS(simObj, fullPost = FALSE)
  }
  return(sim_tracks)
}

tbl_locs_fit <- tbl_locs_fit %>% 
  dplyr::mutate(sim_tracks = purrr::map(fit,
                                       .get_sim_tracks,
                                       iter = 1))#iter = aantal simulaties die getekend zullen worden

#convert the simulated tracks to a sf object
tbl_locs_fit <- tbl_locs_fit %>% 
  dplyr::mutate(sim_lines = crawl::crw_as_sf(.$sim_tracks, ftype = "MULTILINESTRING",
                                             locType = "p"))


#effect van e_x en e_y komt niet door!!
sf_sim_lines <- do.call(rbind,tbl_locs_fit$sim_lines) %>% 
  mutate(id = tbl_locs_fit$tag_serial_number) %>% 
  rename(deployid = id)

esri_ocean <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
                     'Ocean/World_Ocean_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

ggplot() + 
  annotation_map_tile(type = esri_ocean,zoomin = 1,progress = "none") +
  layer_spatial(sf_sim_lines, size = 0.75,aes(color = deployid)) +
  scale_x_continuous(expand = expand_scale(mult = c(.6, .6))) +
  scale_y_continuous(expand = expand_scale(mult = c(0.35, 0.35))) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none") +
  ggtitle("Predicted Location Paths", 
          subtitle = "harbor seals (n=2), Aleutian Islands, Alaska, USA")

test_map <- mapView(sf_sim_lines)

tbl_locs_fit$params

mapview(sf_locs)

##########################################################################################
# extract simulation results
##########################################################################################
tbl_locs_fit <- tbl_locs_fit %>% 
  dplyr::mutate(sim_points = crawl::crw_as_sf(.$sim_tracks, ftype = "POINT",
                                             locType = "p"))#p = predictions and o = observations
raw_sim_tracks <- tbl_locs_fit$sim_tracks

##########################################################################################
# klad
##########################################################################################
list(a = c(sf::st_coordinates(sf_locs$data[[1]])[[1,1]], 0,sf::st_coordinates(sf_locs$data[[1]])[[1,2]], 0))