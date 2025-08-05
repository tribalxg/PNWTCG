# load data (assumes current working directory is top of 'TCG' repo)
monitoring_locations <- read.table('./code_snippets/huc_from_gps/exdata.tsv', sep = '\t', header=TRUE)

# convert locations to sf
sf_monitoring_locations <- sf::st_as_sf(monitoring_locations, coords = c("Long", "Lat"), crs=4326)

#Subset Puget Sound HUCs
huc6s <- c('Coast' = '171001', 'Puget Sound' = '171100')
wwa <- nhdplusTools::get_huc(id=huc6s, type='huc06') # manually define a restricted search area
huc_lvl <- 'huc12'
wb_poly <- nhdplusTools::get_huc(sf::st_union(wwa), type = huc_lvl)

# rows in wb_poly are huc polygons
# find indexes for the rows in wb_poly that contain the points in sf_monitoring_locations
wb_poly_idx <- sf::st_intersects(sf_monitoring_locations, wb_poly)

# convert wb_poly_idx to simpler vector object
wb_poly_idx <- unlist(wb_poly_idx)

# convert wb_poly to simpler data.frame object
wb_info <- as.data.frame(wb_poly)

# get the data from the corresponding rows and columns
huc_ids <- wb_info[wb_poly_idx, huc_lvl]
huc_names <- wb_info[wb_poly_idx, 'name']

# add IDs to original dataset
monitoring_locations[,'HUC12'] <- huc_ids

# or to the sf version
sf_monitoring_locations[,'HUC12'] <- huc_ids
