# WP1 - Grote Nete Analysis
<mark>last updated on 24-09-2026</mark>

## About
This repo investigates the environmental factors influencing river eel (_Anguilla anguilla_ L.) migration.  To this end acoustic telemetry data of 39 silver eels tagged and released in the Grote Nete River is used. Unlike well-studied regulated systems, this river has an unobstructed water flow with a continuous transition from river to estuary and sea providing a needed reference framework.

## Abbreviations
* HW = heigth water
* LW = low water
* WS = Western Scheldt River
* Q = discharge

## Project structure

### Data

* `/raw:`
    + `/discharge:` contains data on the discharge. All excels are named after the station name
    + `/temperature:` contains water temperature data
    + `/oxygen:` contains dissolved oxygen data
    + `/rainfall:` contains rainfall data
    + `/salinity:` contains salinity data [psu] (data only on Rupel and Scheldt - less reliable)
    + `/tide:` contains tide data (timestamp and heights at HW and LW, nonregular timestamps)
    + `/metadata:` contains the metadata of all environmental data
        - name: station name
        - distance_to_source: distance from release location
        - resolution_unit: unit of the resolution
        - resolution_multiplier: multipier of this unit of resolution
	+ `photoperiod.csv`: dataset containing the daily amount of daylight (unit: minutes)
    + `/level:` contains waterlevel data (to caculate velocity)
    + `/shape:` shape and raster files of the study area (water)
    + `deployments.csv:` receiver metadata from ETN
    + `eel_meta_data/csv`: metadata of eels from ETN
    + `raw_detection_data`: raw detection data from ETN
* `/interim:`
    + `residency.csv`: dataset containing eel tracks (arrival and departure time at receivers)
	+ `migration.csv`: dataset containing eel tracks and alternative speed calculation
    + `migration_env_filter.csv`: dataset containing eel tracks with alterantive speed calculations and the environmental data (data of WS is removed)
    + `\processed`: contains all environmental data again, now processed. E.g.:
        - 0 --> NA-value, if nessecary
        - changing the timestamps of the daily average discharge
    + `\anomaly`: contains the environmental data of temperature (deviation from seasonal average)
    + `metadata.csv`: metadata of environmental data containing their distance along the river (reference = release location)
* `/geo_data:`
	+ `distancematrix_2019_grotenete.csv`: distance matrix of the detection station network (matrices are created at https://github.com/inbo/fish-tracking).
* `/external:`
	+ `distancematrix_2019_grotenete.csv`: distance matrix of the detection station network (matrices are created at https://github.com/inbo/fish-tracking).
    + `release_location_stations.csv`: contains the names of the release locations

### Scripts
Run scripts in the following order or alternatively run `main.R` for the full pipeline.

Configuration
* `config.R`: Store useful variables and configuration

Data download
* `download_data.R`: Download data from silver eel meta-analysis from ETN database via RStudio LifeWatch server
	+ obtain detection dataset `raw_detection_data.csv`
	+ obtain meta-data on tagged eels `eel_meta_data.csv`
	+ obtain meta-data on deployments `deployments.csv` (station names and positions)
* `wateRinfo.R:` Download environmental data making use of the wateRinfo package and save the data at `/raw` (timezone = UCT)


Data preprocessing
* `01_attach_release.R`: Add eel release positions and date-time to detection dataset
* `02_merge_eel_characteristics.R`: Add eel meta data to the detection dataset
* `03_clean_detection_data.R`: Data processing by removing false and irrelevant detections
* `04_extract_network.R`: Extract receiver network based on detection data
	* This serves as input to calculate the distance matrices at https://github.com/inbo/fish-tracking
* `05_smooth_eel_tracks.R`: Smooths duplicates and calculates residencies per eel per station. Therefore, it calls the following two functions:
	+ 5a. `get_nearest_stations.R`: 	function to get the stations which are near a given station (where near means that the distance is smaller than a certain given limit, e.g. detection range).
		- --> Generate residency dataset and store it in `/interim`
	+ 5b. `get_timeline.R`: general function to extract the smoothed track for one eel (via its `transmitter ID`)

* `06_switch_2D_1D.R:`:
    + load the point vector made in QGIS (includes study area, resolution 1m, crs:lambert) --> lookup table
    + process so that to each point a distance_to_source is calculated
    + add in NAAM column the river segment: gn, rp, zes_up, zes_down (lookup table saved as: `./data/geo_data/grotenete_zeeschelde_lookup_Lambert.csv`)
    + calculate the distance to source for each receiver (saved in `./data/geo_data/deployments_distance_to_source.csv`)

* `07_preprocessing_INBO_data.R:` preprocess the telemetry data (starting from `/raw/migration.csv` and saved at `/interim/migration.csv` and `/interim/migration_filter.csv`)
    + timestamps in raw data are in timezone UTC
    + recalcutate the smooth eel track (remove timelimit for which a new track was started)
    + recalculate the distance_to_source (because of higher resolution if the lookup table in comparison to the original distance matrix)
    + calculate the alternative speed by incorporating the residence times at the receivers in the swimtime
        - `\src\calculate_speed_function.R:` function to calculate the speed for a dataframe with data from one eel
    + 'downstream' column: calculate wheter migration is downstream
    + add colums to divide the study area into different zones (boundaries from Keirsebelik et al 2025):
        - tidal: starting at transition Nete to Grote Nete (from gn-3)
        - non-tidal: ending +- at convolution of Grote Nete and Wimp (last receiver: gn-6)
        - transition: between tidal and non-tidal
    + interpolate to find the middle between 2 receivers (interpolation_location)
    + add column to divide in segments: gn (grote-nete), rup (rupel), zes_up (scheldt before confluence with rupel), zes_down (scheldt after confluence with rupel)
    + add coordinates of the interpolation_location (making use of the lookup table)

* `08_environmental_variables:` making some figures of the environmental data and do the preprocessing (save the data at `/interim`). For some datatypes also analyses:
    + `\chemical_var.R:` preprocessing of salinity, turbidity and dissolved oxygen
    + `\discharge.R:` preprocessing of discharge
    + `\rainfall.R:` preprocessing of rainfall. Later in the analysis the accumulated rainfall will be used (release time as startingpoint)
    + `\watertemperature.R`: preprocessing of watertemperature (+ looking at correlations between measurements)
    + `\circadian.R:` arrival/departure analysis + chi-squared test
    + `\tides.R:` arrival/departure analysis. ebb/flood at receiver was decided based on closest measuring point
    + `\metadata.R:`: processing metadata
        - e.g calculated distance to source based on lookup table
    + `\velocity.R:`: evaluating the calculated velocities (from cross_sections/get_velocities) and measured velocity
    + `\lunar_cycle.R`
    + `\photoperiod.R`    


* `09_get_cross_section_velocities.R:` Calculate the water velocity for 5 locations (5 locations with Q-data and cross section data)

    For each location:
    + `\src\get_velocity_function.R:` Making use of waterlevel, discharge and H-A relations (save the data at `/interim`)


* `10_link_env_variables.R:` linking the environmental variables with the telemetry raw data
    
    For each type of environmental data (upload `./data/interim/migration_filter.csv` and save to `./data/interim/migration_env_filter.csv`):

    + For each data point: `\src\concat_env_var_function.R:` function to average the environmental data over the swimtimes of the eels between 2 receivers.
        + for rainfall data the median is used
        + all over variables the mean is calculated
    + `\src\concat_all_env_var_function.R:` the outputs of `concat_env_var_function.R` of the measurement points of each variable is clustered.
    + `\src\inverse_distance_function.R:` get one value out of the different datapoints by performing inverse distance weighting for each point in the migration trajectory
        -  If 1D along river: Closest upstream en downstream environmental data point is used
            - Q is scaled before inverse distance is applied (to correct for increasing discharge more downstream)
        - 2D (Rainfall): involve all datapoints

* `11_smoothing_interpolation.R:` (load `/interim/migration_env_filter.csv`)
    + Interpolation of the trajectory: output saved in `/interim/migration_inter.csv` (now: resolution = 15 min)

* `12_link_env_variables_inter.R:` linking the environmental variables with the telemetry interpolated data (R becomes the accumulated data)

    + Interpolated telemetry data is created in `Smoothing_interpolation.R` (load `/interim/migration_inter.csv`)
    + Here for all environmental data:
        - `align_resolutions_function.R`: The resolutions are fix to a given input resolution (the resolution of the interpolated data)
        - `\src\inverse_distance_function.R:` same as above
            - When datatype = "Q": because of the segments --> discontinuities can occur
    + saved in `/interim/migration_env_inter.csv`


Analysis
* `01_clustering.R:` Clustering (resting + resident) Vs migratory (add column to `/interim/migration__env_filter.csv`)
    + kmeans (different options were tested BUT log raw data, seperate eels and k=2 works best!)
    + working with interpolated data --> even more skewed data (in the other direction)
    + silhouette width method + biological knowledge --> $k = 2$
    + looking at seperate eels (less false resident in straightforward migration part)
    + first value is NA (for each eel)
    + 1 = resident, 2 = migration
* `02_trajectories.R:` Calculate the average migration durations and speeds
* `03_onset_migration.R:` resident vs migration: which environmental variables influence this? Are there short-term triggers to onset migration?
    + data selection: non-tidal, remove the first day after release
        - add column: label --> 'resident' and 'migration' and later for the model: label_bin --> 0 = resident and 1 = migration
    + Boxplot to see difference in conditions
    + Boxplots to see triggers
* `04_during_migration.R:`
    + add column: label --> 'migratory' and 'resting' and later for the model: label_bin --> 0 = migratory and 1 = resting


Explorative
* `\comp_env_distr.R:` compare the distributions of environmental data with the sample distributions (samples = environmental values at arrivals and departures)
* `Smoothing.R:` experimenting with crawl and dbscan
* `BCPA.R:`Behavioural change point analysis
* `trajectory_analysis.R:` Lavielle and GUEGUEN analysis
* `dBBMM.R`
    + Dynamic brownian bridge models: setting the actel input. Work with receiver locations projected on the river center.
* `/visualisation:`
    + `create_eel_track_env_var_plot.R:` Each eel its trajectory plotted together with an environmental variable
    + `correlation_plots.R:` Correlations (saved at `/figures/correlations/`)
        - between an environmental variable and the migration speed
        - between the environmental variables 
    + `Figures_for_presentations.R:` Additional figures for presenations
