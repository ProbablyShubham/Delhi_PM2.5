# Delhi_PM2.5
Spatiotemporal Analysis of Traffic and PM 2.5 levels in Delhi (2006–2018)

The idea behind this project is to plot a map of the PM 2.5 data collected in Delhi across its air quality monitoring stations between 2006-2018 and perform statistical analysis on the same to derive insights using R. The key questions that this analysis intends to answer are:

- 1. Does traffic time matter specifically at choke points?
- 2. Are choke point stations generally more polluted than others?
- 3. Is the difference between Peak and Off-Peak significantly larger at choke points compared to control stations?

To do this, air quality monitoring stations across Delhi are plotted alongside a shortlisted set of traffic choke points in Delhi. The PM 2.5 monitoring stations near (within a 2 km radius) these choke points are identified and compared against the rest of the dataset using a linear model regression.
