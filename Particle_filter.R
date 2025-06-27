library(truncdist)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(tidyverse)
library(flexsurv)

# Load data
data <- read_csv('./data/interim/migration_env_filter.csv', show_col_types = FALSE)
data_list <- split(data, f = data$tag_serial_number)
eel_1 <- as.data.frame(data_list[[1]]) # Example for one eel, replace with actual tag_serial_number
#write as csv
write_csv(eel_1, './data/interim/eel_1.csv')

distance <- read_csv('./data/actel/distances.csv', show_col_types = FALSE)

# Simulation parameters
N <- 100                       # Number of particles
delta_t <- 30 * 60              # 30 minutes in seconds
#if using gamma distribution
k <- 1                          # Gamma shape
theta <- 0.5                    # Gamma scale
#if using a lognormal distibution
mean <- 1
sd <- 5
max_dist <- 2*delta_t/(60*60)*1000              # Maximum particle movement
a <- 25                          # Detection logistic param
beta <- 0.1                     # Detection logistic param
y_max <- 500                    # Detection range limit

data_receivers <- data %>%
    select(station_name, distance_to_source_m)

# Example receiver positions (replace with actual coordinates)
#receivers <- data.frame(id = 1:3, x = c(100, 300, 500))
receivers <- distance[,c("...1","rel_grotenete1")] %>% #rename
    rename(inter_distance = rel_grotenete1,
    id = ...1)#MOET JE NOG OPDELEN IN VERSCHILLENDE BRANCHES!
receivers <- left_join(receivers, data_receivers, by = c("id" = "station_name"), multiple = "first") %>%
    mutate(distance = as.numeric(distance_to_source_m)) %>%#filter the NA out of distance_to_source_m
    filter(!is.na(distance))
R <- nrow(receivers) # Number of receivers


#observations (end up with a matrix of size T x R), date in 'data' is the avaerga over arrival time and departure
detection_time <- as.POSIXct(round_date(eel_1$arrival+eel_1$residence/2, "30 mins"))
eel_1$detection_time <- detection_time
dates <- seq(detection_time[1], detection_time[length(detection_time)], by = delta_t)
D <- length(dates)
observations <- matrix(0, nrow = D, ncol = R)
for (i in 1:length(eel_1$row_id)){
    col <- which(eel_1$station_name[i] == receivers)
    row <- which(dates == eel_1$detection_time[i]) #find the index of the date in the eel data
    observations[row,col]<- 1 #find the index of the receiver in the eel data
}
#name the columns of the observations matrix
colnames(observations) <- receivers$id


# Initialize particles around release location x=0, with 5m uncertainty
initialize_particles <- function(N) {
  state.init <- data.frame(
    id = 1:N,
    x = runif(N, -2.5, 2.5),
    weight = rep(1 / N, N)
  )
    return(state.init)
}

#movement process
move_particles <- function(particles) {
  #step <- rtrunc(nrow(particles), "gamma", a = 0, b = max_dist, shape = k, scale = theta)
  step <- rtrunc(nrow(particles), "lnorm", a = 0, b = max_dist, mean = 2, sd = 5)

  particles$x <- particles$x + step#* sample(c(-1, 1), nrow(particles), replace = TRUE,prob = c(0.05,0.95)) # Randomly move left or right
  return(particles)
}

# detection probability
p_detect <- function(state, receiver,a,beta) {#(1xN, 1xR, a, beta)
  d <- state - receiver
  prob <- ifelse(d < y_max, 1 / (1 + exp(-(a - beta * d))), 0)
  return(prob)#NxR
}
#update particle weight
update_weights <- function(particles, observations, receivers,a,beta) {#(1xN, 1xR, 1xR, a, beta) Observations should be uploaded voor a certain time stamp!
  for (i in 1:nrow(particles)) {
    p <- particles[i, ]
    likelihood <- 1
    for (j in 1:nrow(receivers)) {
      r <- receivers[j, ]
      obs <- observations[[r$id]]#je wil de eerste rij van de observaties opvragen (van alle receivers maar enkel van tijdstap 1)
      p_yk <- p_detect(p$x, r$distance,a,beta)
      likelihood <- likelihood * (p_yk^obs) * ((1 - p_yk)^(1 - obs))
    }
    particles$weight[i] <- particles$weight[i] * likelihood
  }
  particles$weight <- particles$weight / sum(particles$weight)
  return(particles)
}


update_weights <- function(particles, observations, receivers, a, beta) {
  log_weights <- numeric(nrow(particles))  # Start at log(1)

  for (i in 1:nrow(particles)) {
    log_likelihood <- 0
    p <- particles[i, ]

    for (j in 1:nrow(receivers)) {
      r <- receivers[j, ]
      receiver_id <- as.character(r$id)
      obs <- observations[[receiver_id]]  # Should be 0 or 1

      # Compute detection probability
      p_yk <- p_detect(p$x, r$distance, a, beta)

      # Clamp p_yk to avoid log(0)
      p_yk <- pmin(pmax(p_yk, 1e-10), 1 - 1e-10)

      # Add the receiver contribution to log-likelihood
      log_likelihood <- log_likelihood + obs * log(p_yk) + (1 - obs) * log(1 - p_yk)
    }

    log_weights[i] <- log(p$weight) + log_likelihood
  }

  # Normalize weights on log-scale
  log_weights <- log_weights - max(log_weights)
  weights <- exp(log_weights)
  weights <- weights / sum(weights)

  particles$weight <- weights
  return(particles)
}



# Resample particles based on weights
resample_particles <- function(particles) {
  indices <- sample(1:nrow(particles), nrow(particles), replace = TRUE, prob = particles$weight)
  new_particles <- particles[indices, ]
  new_particles$weight <- new_particles$weight / sum(new_particles$weight)  # Normalize weights
  return(new_particles)
}

# Run the particle filter
# Example observations list (replace with actual data per timestep)
#observations_list <- list(
#  list(r1 = 1, r2 = 0, r3 = 0),
#  list(r1 = 0, r2 = 1, r3 = 0)
#  # Add as needed for each time step
#)

particles <- initialize_particles(N)
#test <- p_detect(particles, receivers$x, a, beta) # Example call to check detection probability
particle_history <- list()
for (t in 1:nrow(observations)) {
  particles <- move_particles(particles)
  particles <- update_weights(particles, observations[t,], receivers,a,beta)
  particles <- resample_particles(particles)
  particle_history[[t]] <- particles
  #particle_history[[paste0("t", t)]] <- particles
}

# Save particle history
saveRDS(particle_history, file = "./data/interim/particlefilter/particle_history.rds")
#load particle history
particle_history <- readRDS("./data/interim/particlefilter/particle_history.rds")
# Example: Visualize particles at time step 5
step <- 3170
ggplot(particle_history[[step]], aes(x = x)) +
  geom_density(alpha = 0.4) +#as title date[3326]
  ggtitle(paste("Particle distribution at time", dates[step]))

###############################################################################################################
# Patter Package
###############################################################################################################




###############################################################################################################
# Visualisation
###############################################################################################################
rtrunc(1000, "gamma", a = 0, b = max_dist, shape = 1, scale = theta)
#plot density
ggplot(data = data.frame(x = rtrunc(4000, "gamma", a = 0, b = max_dist, shape = 1, scale = 5)), aes(x)) +
  geom_density(fill = "blue", alpha = 0.4) +
  labs(title = "Gamma Distribution of Particle Movement", x = "Distance (m)", y = "Density") +
  theme_minimal()


#plot the detection probability
ggplot(data = data.frame(x = seq(0, 500, by = 1)), aes(x)) +
  geom_line(aes(y = 1 / (1 + exp(-(a - beta * x))), color = "Detection Probability")) +
  labs(title = "Detection Probability vs Distance", x = "Distance (m)", y = "Probability") +
  theme_minimal() +
  scale_color_manual(values = "blue")


ggplot(data = data.frame(x = rllogis(1000, shape = 1, scale = 5)), aes(x)) +
  geom_density(fill = "blue", alpha = 0.4) +
  labs(title = "Gamma Distribution of Particle Movement", x = "Distance (m)", y = "Density") +#range x from 1-1000
  xlim(1, 1000) +
  theme_minimal()


ggplot(data = data.frame(x = rtrunc(4000, "lnorm", a = 0, b = max_dist, mean = 1, sd = 5)), aes(x)) +
  geom_density(fill = "blue", alpha = 0.4) +
  labs(title = "Gamma Distribution of Particle Movement", x = "Distance (m)", y = "Density") +
  theme_minimal()


###############################################################################################################
# test
###############################################################################################################
particles <- initialize_particles(N)

observation <- observations[1,] # Example observation for the first time step
p <- particles[2, ]
#save the history of likelihoods
like_list <- list()
likelihood <- 1
for (j in 1:nrow(receivers)) {
  r <- receivers[j, ]
  obs <- observations[[r$id]]#je wil de eerste rij van de observaties opvragen (van alle receivers maar enkel van tijdstap 1)
  p_yk <- p_detect(p$x, r$distance,a,beta)
  likelihood <- likelihood * (p_yk^obs) * ((1 - p_yk)^(1 - obs))
  like_list[[j]]<- likelihood
  #stop if likelihood is NA
  if (is.na(likelihood)) {
    stop(paste("Likelihood is NA at iteration",j))
  }
}
weight <- particles$weight[2] * likelihood
#problem!
#receiver s-9a is 


#time between detection time 1 and detection time 2
time_diff <- as.numeric(difftime(eel_1$detection_time[2], eel_1$detection_time[1], units = "secs"))
speed <- 1048.509 / time_diff
#m per 30 min
m_per_30min <- speed * 30 * 60