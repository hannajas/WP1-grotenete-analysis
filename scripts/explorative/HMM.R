install.packages("momentuHMM")
library(momentuHMM)
library(tidyverse)
library(sf)
library(mapview)
library(ggplot2)
#example ----------------------------------------------------------------------------------------
#muskox <- read.csv("http://www.rolandlangrock.com//Misc//muskox.csv")
#make datset with ID (tag_serial_number?), step, temp (what??) --> is a negative value in muskox,
#x and y (coordinates), + covariate columns

#max speed of eel = 2 m/s = 1800 m/15min
#SO 1800 shuold be the maximal step length!

#----------------------------------------------------------------------------------------
# explore the data
#----------------------------------------------------------------------------------------
eels_inter <- read_csv('./data/interim/migration_env_inter.csv') #%>%
#filter(tag_serial_number == 1171746)
eels_plot <- st_as_sf(eels_inter, coords = c("x", "y"), crs = 31370) #%>%
#filter(tag_serial_number == 1171746)
#visualize tracks
#mapview(eels_plot)
#x and y are not created by using the ookup table
#but dist is! (that's why resolution seems bad)

eels_proc <- eels_inter %>%
  select(-x) %>%
  rename(ID = tag_serial_number, temp = dt, x = distance_to_source_m) %>%
  select(ID, x, temp, Q, Tw, photoperiod) %>%
  mutate(y = 0, step = abs(x - lag(x))) #because of 1D data, we should include step length ourself!
#   filter(ID == 1171746)

#processing the tracking data to be input for fitHMM
data_eel <- as.data.frame(eels_proc)
class(data_eel) <- c("momentuHMMData", "data.frame")

# ------------------------------------------------------------------------------------------
#hmm fitting with multiple initializations avoiding local minima
# ------------------------------------------------------------------------------------------
s <- Sys.time()
llks <- rep(NA, 20) #loglikelihoods
mods <- vector("list", 20)
for (k in 1:20) {
  stepMean0 <- runif(2, 0, 1800) # means of gamma distribution (step lengths)
  stepSD0 <- runif(2, 10, 500) # std. dev. of gamma distribution (step lengths)
  stepPar0 <- c(stepMean0, stepSD0)
  mods[[k]] <- fitHMM(
    data_eel,
    nbStates = 2,
    Par0 = list(step = stepPar0),
    angleDist = "none",
    dist = list(step = "gamma")
  )
  llks[k] <- -mods[[k]]$mod$minimum
}
llks

#beste model save
best_mod <- mods[[7]]
#save(best_mod, file = "./data/analysis/best_HMM.RData") OPGESLAGEN

#------------------------------------------------------------------------------------------
# decoding
#------------------------------------------------------------------------------------------
states <- viterbi(best_mod)
eels_proc$state <- states
eels_proc$time <- eels_plot$date
ggplot(eels_proc, aes(x = time, y = x, color = as.factor(state))) + geom_point()
