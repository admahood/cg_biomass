# cg_biomass



[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4705469.svg)](https://doi.org/10.5281/zenodo.4705469)


This is the data and code for our paper in Journal of Arid Environments, **"Cover-based allometric estimate of aboveground biomass of a non-native, invasive annual grass (*Bromus tectorum* L.) in the Great Basin, USA"**. https://doi.org/10.1016/j.jaridenv.2021.104582

The main scripts are `R/a_data_prep.R` and `R/b_lm_figures.R`.

`R/ee_day_of_year_analysis.R` uses google earth engine data to get the peak NDVI dates. The raw data are too big for github, but are available upon request.

`R/precip_extract.R` extracts the precipitation data from PRISM. Also too big for github but available upon request. It is also easily downloaded from PRISM's website.

The script to generate the ndvi time series on google earth engine is here:

https://code.earthengine.google.com/66ea244543735df254d4ba9a937784f5
