# WP1 - Grote Nete Analysis

## About
Linking the eel tracking dataset in the River Grote Nete with environmental variables. This river has an unobstructed water flow with a continuous transition from river to estuary and sea.

This analysis starts from the pre-processing done by Pieterjan Verhelst (https://github.com/PieterjanVerhelst/eel-grotenete-analysis.git). in particular, the dataset "migration.csv" is used as a starting point.

## Project structure

### Data
<mark>Data last updated on 22-05-2025</mark>

* `/raw:`
	+ `migration.csv`: dataset containing eel tracks and speed
    + `/discharge:` contains data on the discharge. All excels are named after the station name
    + `/temperature:` contains temperature data
    + `/oxygen:` contains oxygen data
    + `/rainfall:` contains rainfall data
    + `/salinity:` contains salinity data
    + `/tide:` contains tide data (timestamp and heights at HW and LW, nonregular timestamps)
    + `/metadata:` contains the metadata of all environmental data
        - name: station name
        - distance_to_source: distance from release location
        - resolution_unit: unit of the resolution
        - resolution_multiplier: multipier of this unit of resolution
	+ `photoperiod.csv`: dataset containing the daily amaount of daylight (unit: minutes)
    + `/level:` contains waterlevel data (needed for velocity calcultation)
    + `/shape:` shape and raster files of the study area (water)
* `/interim:`
	+ `migration.csv`: dataset containing eel tracks and alternative speed calculation
    + `migration_env_filter.csv`: dataset containing eel tracks with alterantive speed calculations and the environmental data (data of WS is removed)
    + `\processed`: contains all environmental data again, now processed. E.g.:
        - 0 --> NA-value, if nessecary
        - changing the timestamps of the daily average discharge
* `/external:`
	+ `distancematrix_2019_grotenete.csv`: distance matrix of the detection station network (matrices are created at https://github.com/inbo/fish-tracking).

### Scripts

Data collection and explorative analysis
* `/importing_data:` Making use of the wateRinfo package and save the data at `/raw`
* `/cross_sections/get_velocities.R:` Calculate the water velocity for 5 locations (5 locations with Q-data)

    For each location:
    + `\src\get_velocity_function.R:` Making use of waterlevel, discharge and H-A relations (save the data at `/interim`)
* `/exploration_env_variables:` making some figures of the environmental data and do the preprocessing (save the data at `/interim`). For some datatypes also analyses:
    + `\Circadian.R:` arrival/departure analysis
    + `\Tides.R:` arrival/departure analysis
    + `\comp_env_distr.R:` compare the distributions of environmental data with the sample distributions (samples = environmental values at arrivals and departures)
* `preprocessing_INBO_data.R:` preprocess the telemetry data (starting from `/raw/migration.csv` and stave at `/interim/migration_env_filter.csv`)
    + calulate the alterantive speed by incorporating the residence times at the receivers in the swimtime
        - `\src\calculate_speed_function.R:` function to calculate the speed for a dataframe with data from one eel
    + recalcutate the smooth eel track (remove timelimit for which a new track was started)
    + 'downstream' column: calculate wheter migration is downstream
    + 'downstream_migration' column: ME NIET DUIDELIJK
    + add colums to defide the study area into different zones: tidal, transition and non-tidal (boundaries from Keirsebelik et al 2025)
* `explorative_data_analysis.R:` summaries of migration speeds
* `link_env_variables.R:` linking the environmental variables with the telemetry data (adding to `/interim/migration_env_filter.csv`)
    
    For each type of environmental data:

    + For each data point: `\src\concat_env_var_function.R:` function to average the environmental data over the swimtimes of the eels between 2 receivers.
    + `\src\inverse_distance_function.R:` get one value out of the different datapoints by performing inverse distance weighting for each point in the migration trajectory
        - When datatype = "Q", there are 3 segments defined. To link environmental data with the receivers each receiver can only get information from data-point that are located whitin the same segment.
* `/visualisation:`
    + `create_eel_track_env_var_plot.R:` Each eel its trajectory plotted together with an environmental variable
    + `correlation_plots.R:` Correlations (saved at `/figures/correlations/`)
        - between an environmental variable and the migration speed
        - between the environmental variables 
    + `Figures_for_presentations.R:` Additional figures for presenations

Trajectory smoothing
* `Smoothing.R:` regularistaion of trajectories(crawl package) (GIVES ERRORS FOR NOW)

Labeling
* `Clustering.R:` kmeans (both for "raw" trajectory as for regularizes trajectory)
* `BCPA.R:`Behavioural change point analysis
* `trajectory_analysis.R:` Lavielle and GUEGUEN analysis

Dynamic brownian bridge models
* `dBBMM.R`
    + setting the actel input. Work with receiver locations projected on the river center.