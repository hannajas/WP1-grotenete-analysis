# WP1 - Grote Nete Analysis

## About
Linking the eel tracking dataset in the River Grote Nete with environmental variables. This river has an unobstructed water flow with a continuous transition from river to estuary and sea.

This analysis starts from the pre-processing done by Pieterjan Verhelst (https://github.com/PieterjanVerhelst/eel-grotenete-analysis.git). in particular, the dataset "migration.csv" is used as a starting point.

## Project structure

### Data
<mark>Data last updated on 19-02-2025</mark>

* `/raw:`
	+ `migration.csv`: dataset containing eel tracks and speed
    + `/discharge:` contains data on the discharge. All excels are named after the station name
    + `/temperature:` contains temperature data
    + `/metadata:` contains the metadata of all environmental data
        - name: station name
        - distance_to_source: distance from release location
        - resolution_unit: unit of the resolution
        - resolution_multiplier: multipier of this unit of resolution
	+ `photoperiod.csv`: dataset containing the daily amaount of daylight (unit: minutes)
* `/interim:`
	+ `migration.csv`: dataset containing eel tracks and alterantive speed calculation
    + `\processed`: contains all environmental data again, now processed. E.g.:
        - 0 --> NA-value, if nessecary
        - changing the timestamps of the daily average discharge
* `/external:`
	+ `distancematrix_2019_grotenete.csv`: distance matrix of the detection station network (matrices are created at https://github.com/inbo/fish-tracking).

### Scripts

* `/importing_data:` Making use of the wateRinfo package and save the data at `/raw`
* `/exploration_env_variables:` making some figures of the environmental data and do the preprocessing
* `/visualisation:`
    + `create_eel_track_env_var_plot.R:` Each eel its trajectory plotted together with an environmental variable
    + `Figures_for_presentations.R:` Additional figures for presenations
Sequence of processing:
* `calculate_speed.R:` calulate the alterantive speed by incorporating the residence times at the receivers in the swimtime
    + `\src\calculate_speed_function.R:` function to calculate the speed for a dataframe with data from one eel
* `link_env_variables.R:` linking the environmental variables with the telemetry data
    
    For each type of environmental data:

    + For each data point: `\src\concat_env_var_function.R:` function to average the environmental data over the swimtimes of the eels between 2 receivers.
    + `\src\inverse_distance_function.R:` get one value out of the different datapoints by performing inverse distance weighting for each point in the migration trajectory

    Plotting correlation functions
