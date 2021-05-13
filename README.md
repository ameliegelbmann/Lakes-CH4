# Methane in Swiss Lakes

Despite their small share of land surface, lakes and impoundments emit a significant amount of methane (CH4), a potent greenhouse gas, to the atmosphere. Aquatic productivity (i.e., eutrophication) has been found to be a major driver of CH4 emission from freshwaters. This study aims to investigate if different levels of biological productivity (i.e., trophic state) relates to CH4 accumulation and emission in Alpine lakes and to detect potential drivers or mechanisms behind any relationship found between CH4 dynamics and trophic state. One part of this study is presented here and consists in data analysis of CH4 drivers (biological, physical, chemical) in lakes using R.

Methodology:

Data:
- Biological parameters (e.g. groups and species distribution)
- Physical parameters (e.g. water column stability (Kz))
- Chemical parameters (e.g. nitrite and nitrate concentration)
- Methane accumulation and emission in lakes

After having these data for each lakes and at each periods of time, we can process and visualize data using R (see script).

"N2,Kd,Ch4..."(physical data), "Profiles_CTD..."(chemical data) and "Fluoro_plots..." (biological data) scripts are used to create lakes profiles for 3 different periods (June 2018, September 2018 and June 2019). See image Rplot122 for a result exemple.
The purpose of the "Loop_Linear_Reg" script is to create linear regression of the different parameters in each lakes at each periods of time. Two plots have been created: one with the average of the data point and the other with the raw data point. See image Rplot148 for a result exemple.
A loop has been used in each script to facilitate graphics creation.
