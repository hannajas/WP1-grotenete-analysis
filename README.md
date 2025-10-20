# WP1 - Grote Nete Analysis

## About
Linking the eel tracking dataset in the River Grote Nete with environmental variables. This river has an unobstructed water flow with a continuous transition from river to estuary and sea.

This analysis starts from the pre-processing done by Pieterjan Verhelst (https://github.com/PieterjanVerhelst/eel-grotenete-analysis.git). in particular, the dataset "migration.csv" is used as a starting point.

## Project structure

### Data
<mark>Data last updated on 20-10-2025</mark>

* `/raw:`
	+ `migration.csv`: dataset containing eel tracks and speed
    + `/discharge:` contains data on the discharge. All excels are named after the station name
    + `/temperature:` contains temperature data
    + `/oxygen:` contains oxygen data
    + `/rainfall:` contains rainfall data
    + `/salinity:` contains salinity data [psu] (data only on Rupel and Scheldt - less reliable)
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

Data collection and explorative analysis I
* `/importing_data/waterRinfo.R:` Making use of the wateRinfo package and save the data at `/raw` (timezone = UCT)
* `switch_2D_1D.R:`:
    + load the point vector made in QGIS (includes study area, resolution 1m) --> lookup table
    + process so that to each point a distance_to_source is calculated
    + add in NAAM column the river segment: gn, rp, zes_up, zes_down (lookup table saved as: `./data/geo_data/grotenete_zeeschelde_lookup_Lambert.csv`)
    + calculate the distance to source for each receiver (saved in `./data/geo_data/deployments_distance_to_source.csv`)
* `/cross_sections/get_velocities.R:` Calculate the water velocity for 5 locations (5 locations with Q-data and cross section data)

    For each location:
    + `\src\get_velocity_function.R:` Making use of waterlevel, discharge and H-A relations (save the data at `/interim`)
* `/exploration_env_variables:` making some figures of the environmental data and do the preprocessing (save the data at `/interim`). For some datatypes also analyses:
    + `\chemical_var.R:` preprocessing of salinity, turbidity and dissolved oxygen
    + `\discharge.R:` preprocessing of discharge
    + `\rainfall.R:` preprocessing of rainfall. Later in the analysis the accumulated rainfall will be used (release time as startingpoint)
    + `\Watertemperature.R`: preprocessing of watertemperature (+ looking at correlations between measurements)
    + `\Circadian.R:` arrival/departure analysis + chi-squared test
    + `\Tides.R:` arrival/departure analysis. ebb/flood at receiver was decided based on closest measuring point
    + `\comp_env_distr.R:` compare the distributions of environmental data with the sample distributions (samples = environmental values at arrivals and departures)
    + `\metadata.R:`: processing metadata
        - e.g calculated distance to source based on lookup table
    + `\velocity.R:`: evaluating the calculated velocities (from cross_sections/get_velocities)
* `preprocessing_INBO_data.R:` preprocess the telemetry data (starting from `/raw/migration.csv` and saved at `/interim/migration.csv` and `/interim/migration_filter.csv`)
    + timestamps in raw data are in timezone UTC
    + recalcutate the smooth eel track (remove timelimit for which a new track was started)
    + recalculate the distance_to_source (because of higher resolution if the lookup table in comparison to the original distance matrix)
    + calculate the alternative speed by incorporating the residence times at the receivers in the swimtime
        - `\src\calculate_speed_function.R:` function to calculate the speed for a dataframe with data from one eel
    + 'downstream' column: calculate wheter migration is downstream
    + 'downstream_migration' column: downstream and faster than a certain treshold? ME NIET DUIDELIJK
    + add colums to divide the study area into different zones: tidal, transition and non-tidal (boundaries from Keirsebelik et al 2025)
    + interpolate to find the middle between 2 receivers (interpolation_location)
    + add column to divide in segements: gn, rup, zes_up, zes_down
    + add coordinates of the interpolation_location (making use of the lookup table)
* `explorative_data_analysis.R:` summaries of migration speeds

Clustering (resting + resident) Vs migratory
* `Clustering.R:` (add column to `/interim/migration_filter.csv`)
    + kmeans (different options were tested BUT log raw data, seperate eels and k=2 works best!)
    + working with interpolated data --> even more skewed data (in the other direction)
    + silhouette width method + biological knowledge --> $k = 2$
    + looking at seperate eels (less false resident in straightforward migration part)
    + first value is NA (for each eel)
    + 1 = resident, 2 = migration

Explorative analysis II
* `link_env_variables.R:` linking the environmental variables with the telemetry raw data (R becomes the accumulated data)
    
    For each type of environmental data (upload `./data/interim/migration_filter.csv` and save to `./data/interim/migration_env_filter.csv`):

    + For each data point: `\src\concat_env_var_function.R:` function to average the environmental data over the swimtimes of the eels between 2 receivers.
    + in `\src\concat_all_env_var_function.R:` de outputs van `concat_env_var_function.R` voor verschillende meetlocaties van de zelfde variabele worden gebundeld.
    + `\src\inverse_distance_function.R:` get one value out of the different datapoints by performing inverse distance weighting for each point in the migration trajectory
        -  If 1D along river: Closest upstream en downstream environmental data point is used
            - When datatype = "Q", there are 3 segments defined. To link environmental data with the receivers each receiver can only get information from data-point that are located whitin the same segment.
            - Q is normalised over traject
        - 2D (Rainfall): involve all datapoints

* Trajectory smoothing
    + `Smoothing_interpolation.R:` (load `/interim/migration_env_filter.csv`)
        * Interpolation of the trajectory: output saved in `/interim/migration_inter.csv` (now: resolution = 15 min)
    + `Smoothing.R:` experimenting with crawl and dbscan

* `link_env_variables_inter.R:` linking the environmental variables with the telemetry interpolated data (R becomes the accumulated data)

    + Interpolated telemetry data is created in `Smoothing_interpolation.R` (load `/interim/migration_inter.csv`)
    + Here for all environmental data:
        - `align_resolutions_function.R`: The resolutions are fix to a given input resolution (the resolution of the interpolated data)
        - `\src\inverse_distance_function.R:` same as above
            - When datatype = "Q": because of the segments --> discontinuities can occur
    + saved in `/interim/migration_env_inter.csv`

* Onset of migration
    + `onset_migration.R:` Resident Vs migration: Is there a difference in environmental variables? Are there short-term triggers to onset migration?
        - data selection: non-tidal, remove the first day after release
            - add column: label --> 'resident' and 'migration' and later for the model: label_bin --> 0 = resident and 1 = migration
        - Boxplot to see difference in conditions
        - Boxplots to see triggers
* During migration
    + `during_migration.R:`
        - 
            - add column: label --> 'migratory' and 'resting' and later for the model: label_bin --> 0 = migratory and 1 = resting

* `/visualisation:`
    + `create_eel_track_env_var_plot.R:` Each eel its trajectory plotted together with an environmental variable
    + `correlation_plots.R:` Correlations (saved at `/figures/correlations/`)
        - between an environmental variable and the migration speed
        - between the environmental variables 
    + `Figures_for_presentations.R:` Additional figures for presenations

Try outs
* `BCPA.R:`Behavioural change point analysis
* `trajectory_analysis.R:` Lavielle and GUEGUEN analysis
* `dBBMM.R`
    + Dynamic brownian bridge models: setting the actel input. Work with receiver locations projected on the river center.


### Figures
* `/Trajectory_linked_env_var:`
* `/Trajectory:`
* `/Clustering:`
* `/correlations:`
    + `ggpairs_inter_5min`: environmental variables compared (on data_inter_env)


## Bugs

## Order of migration csv
Raw
* `preprocessing_INBO_data.R:` starting from `/raw/migration.csv` and saved at `/interim/migration_env_filter.csv`
* `link_env_variables.R:` (adding to `/interim/migration_env_filter.csv`)
* `Clustering.R:` (adding to `/interim/migration_env_filter.csv`)

Interpolation
* `Smoothing.R:` starting from `/interim/migration_env_filter.csv` and saved at `/interim/migration_inter.csv`
* `link_env_variables.R:` load `/interim/migration_inter.csv` saved at `/interim/migration_env_inter.csv`
