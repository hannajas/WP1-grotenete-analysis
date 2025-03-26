if(!require(devtools)) install.packages('devtools')
library(tidyverse)
library(lubridate)
library(sp)
library(sf)
library(crawl)
library(ggspatial)

############################################################################################
# set up the data structure
data_eels <- read_csv('./data/interim/migration_env_filter.csv', show_col_types = FALSE)
data_eels <- data_eels %>% filter(tag_serial_number == "1171746")
#longitude, latitude to UTM
cord.dec <- SpatialPoints(cbind(data_eels$deploy_latitude, data_eels$deploy_longitude),proj4string=CRS("+proj=longlat"))
cord.dec <- SpatialPoints(data_eels[,c("deploy_longitude","deploy_latitude")],proj4string=CRS("+proj=longlat"))
test <- spTransform(cord.dec,CRS("+proj=utm +zone=31 +ellps=WGS84"))
data_eels[,c("deploy_latitude", "deploy_longitude")] <- coordinates(test)
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
data_eels$error_semi_major_axis <- 500
data_eels$error_semi_minor_axis <- 500
data_eels$error_ellipse_orientation <- 0

############################################################################################
# Smoothing
sf_locs <- sf::st_as_sf(data_eels, coords = c("x","y")) %>%
sf::st_set_crs(32631)


sf_locs <- sf_locs %>% 
  dplyr::group_by(tag_serial_number) %>%
  tidyr::nest()

# fix parameter
fixPar <- c(1,1,NA,NA)#[e_x, e_y, sigma, beta]

#prior distribution
prior <- function(p) {
  dnorm(p[2], -4, 2, log = TRUE)# je geeft een waarde in en het geeft de denity terug
}

# error ellipse
sf_locs <- sf_locs %>% 
  dplyr::mutate(
    diag = purrr::map(data, ~ crawl::argosDiag2Cov(
                      .x$error_semi_major_axis, 
                      .x$error_semi_minor_axis, 
                      .x$error_ellipse_orientation)),
    data = purrr::map2(data,diag,bind_cols)) %>% 
  dplyr::select(-diag)

err.model <- list(
    x =  ~ ln.sd.x - 1,
    y =  ~ ln.sd.y - 1,
    rho =  ~ error.corr
)

test <- crwMLE(sf_locs$data,
err.model =err.model,
fixPar= fixPar,
prior = prior,
attempts = 1,
Time.name = "middledate",
control = list(trace = 0),
initialSANN = list(maxit = 1500,trace = 0))