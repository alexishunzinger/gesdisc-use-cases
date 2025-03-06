# following example: https://nasa-openscapes.github.io/earthdata-cloud-cookbook/how-tos/find-data/find-r.htmlall_po_collections

# install packages --------
#install.packages(c("rstac", "terra", "getPass", "earthdatalogin", "RColorBrewer"))
#install.packages(c("rstac", "terra", "getPass", "RColorBrewer"))
#install.packages("pak")
# load packages ----------
#pak::pak("boettiger-lab/earthdatalogin")
library(rstac) #interact with stac catalogs
library(terra) #work with raster data
library(RColorBrewer) #color pallets from Color Brewer
library(getPass) #allows us to give the code our password without it printing to the console
library(earthdatalogin) #lets us authenticate to access NASA data

# EDL --------
#earthdata_username <- getPass("Earthdata User:")
#earthdata_password <- getPass("Earthdata Password:")
#earthdatalogin::edl_netrc(username = earthdata_username, password = earthdata_password)

# desired data, time, bounds ---------
short_name <- 'OCO2_L2_Lite_FP'
start <- '2020-07-05T00:00:00Z'
end <- '2020-07-07T00:00:00Z'
#tmstr <- paste(start_time,end_time,sep=",")

# query using shortname and earthdatalogin -----
granules <- edl_search(
  short_name = short_name,
  temporal = c(start, end), 
  parse_results = FALSE
)
#granules

#granules[[1]]

# See the granule titles
sapply(granules, `[`, "title")

# See granule urls
granule_urls <- edl_extract_urls(granules)
#granule_urls
