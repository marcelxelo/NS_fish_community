# North Sea HMSC fish occurrence model

This repository contains the scripts used in the manuscript [Community assembly processes and drivers shaping marine fish community structure in the North Sea](https://doi.org/10.1111/ecog.06642) by Marcel Montanyès, Benjamin Weigel, and Martin Lindegren.

Main contact: Marcel Montanyès Solé [mamont@dtu.dk](mailto:mamont@dtu.dk)


## Scripts
* [Download and clean survery data from DATRAS](https://github.com/marcelxelo/NS_fish_community/blob/main/scripts/1-Download%20and%20clean%20survery%20data%20from%20DATRAS.R), modified from [CleanTrawlNAmEUr](https://github.com/AquaAuma/CleanTrawlNAmEUr/blob/main/code/cleanDATRAS.R)
* [Workflow data preparation](https://github.com/marcelxelo/NS_fish_community/blob/main/scripts/2-Workflow%20data%20preparation.R): Retrieve environmental covariates from NEMO-MEDUSA, sediment type from EMODnet and fishing effort associated to each haul, and reformat data.
* [Environment and traits exploration and selection](https://github.com/marcelxelo/NS_fish_community/blob/main/scripts/3-Environment%20and%20traits%20exploration%20and%20selection.R)
* [Prepare data input for HPC run](https://github.com/marcelxelo/NS_fish_community/blob/main/scripts/4-Prepare%20data%20input%20for%20HPC%20run.R): Prepare all the inputs for fitting the HMSC model, i.e, environmental data (X), community data (Y), trait data (T), taxonomy (C).
* [Model fitting](https://github.com/marcelxelo/NS_fish_community/blob/main/scripts/5-Model%20fitting.R)
* [Explanatory and predictive powers](https://github.com/marcelxelo/NS_fish_community/blob/main/scripts/6-Explanatory%20and%20predictive%20powers.R)
* [Plots](https://github.com/marcelxelo/NS_fish_community/blob/main/scripts/7-Plots.R)
* [Comparison of field environmental data vs NEMO-MEDUSA model data](https://github.com/marcelxelo/NS_fish_community/blob/main/scripts/8-Comparison%20of%20field%20environmental%20data%20vs%20NEMO-MEDUSA%20model%20data.R)


## Data sources
* The North Sea International Bottom Trawl Survey data are available from the [DATRAS](https://datras.ices.dk/Data_products/Download/Download_Data_public.aspx)
* Trait data were collected from available [Beukhof et al. 2019](https://doi.org/10.1594/PANGAEA.900866) trait data base, supplemented with information from recent literature [Coulon et al., 2023](https://doi.org/10.1111/geb.13731)
* Fishing effort data were collected from [Couce et al., 2019](https://doi.org/10.14466/)
* Seabed substrate composition data retrieved from [EMODnet](https://www.emodnet-geology.eu) (European Marine Observation
and Data network; ver. September 2021)
* Environmental data was retrieved from the NEMOMEDUSA coupled hydro-geochemical model runs data from [Yool et al. 2013](https://doi.org/10.5194/gmd-6-1767-2013), [Gurvan et al. 2022](https://doi.org/10.5281/zenodo.6334656) & [Laverick 2022](https://github.com/Jack-H-Laverick/nemomedusR)
