# Install package to get USDA data
# devtools::install_github("rdinter/usdarnass")
library(usdarnass)
# Key to access USDA NASS service
nass_set_key('AE78F2A0-8EE8-3F02-BD5D-74BB6272DFE5')

# Get code for all states
all_states <- nass_param('state_ansi',
                         source_desc = 'SURVEY', 
                         sector_desc = 'CROPS',
                         group_desc = 'FIELD CROPS',
                         commodity_desc = 'CORN',
                         util_practice_desc = 'GRAIN',
                         statisticcat_desc = 'YIELD',
                         agg_level_desc = 'COUNTY')

# Get data
ndata <- list()
for (i in seq_along(all_states)) {
  print(i)
  ndata[[i]] <- try(nass_data(
    source_desc = 'SURVEY', 
    sector_desc = 'CROPS',
    group_desc = 'FIELD CROPS',
    commodity_desc = 'CORN',
    util_practice_desc = 'GRAIN',
    statisticcat_desc = 'YIELD',
    agg_level_desc = "COUNTY",
    year = '>=2014',
    state_ansi = all_states[i]), silent = T)
  n = 1
  while(class(ndata[[i]]) == 'try-error' & n < 50) {
    print(paste0('i = ', i, ', try = ', n))
    ndata[[i]] <- try(nass_data(
      source_desc = 'SURVEY', 
      sector_desc = 'CROPS',
      group_desc = 'FIELD CROPS',
      commodity_desc = 'CORN',
      util_practice_desc = 'GRAIN',
      statisticcat_desc = 'YIELD',
      agg_level_desc = "COUNTY",
      year = '>=2014',
      state_ansi = all_states[i]), silent = T)
    n <- n + 1
  }
}
# Format data
ndata1 <- ndata[unlist(sapply(ndata, class)) == 'data.frame']
nass_data <- do.call(rbind, ndata1)
nass_data$Value <- as.numeric(as.character(nass_data$Value))
# 25.40117272 kg in one bushel of corn
# 0.40468564 hectares in one acre
nass_data$yield <- nass_data$Value * 25.40117272 / 0.40468564

# Save this file
# write.csv(nass_data, file = 'data/nass_data.csv')
nass_data <- read.csv('data/nass_data.csv')

# Read means table
mdata <- read.csv('data/year_location_summary_till_2022.csv')

# Format table of means
mdata$state <- substr(mdata$location, 1, 2)

# Convert lat-long to county using sp package
library(sp)
library(maps)
library(maptools)

lonlat_to_county <- function(pointsDF) {
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  indices <- over(pointsSP, counties_sp)
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}

mdata$county0 <- lonlat_to_county(data.frame(x = mdata$longitude, y = mdata$latitude))
mdata$county1 <- sapply(strsplit(mdata$county0, ','), function(x) x[2])

mdata$usda_yield <- NA
for (i in 1:nrow(mdata)) {
  
  # Match state
  tmp <- nass_data[nass_data$state_alpha %in% mdata$state[i],]
  # Match county
  tmp <- tmp[grep(mdata$county1[i], tmp$county_name, ignore.case = T),]
  # Match year
  tmp <- tmp[tmp$year %in% mdata$year[i],]
  # Match irrigation
  tmp1 <- tmp[tmp$prodn_practice_desc %in% switch(mdata$irrigated[i] == 'yes', 'IRRIGATED', 'NON-IRRIGATED'),]
  # If tmp1 does not have data, use tmp
  if (nrow(tmp1) == 0) tmp1 <- tmp
  
  mdata$usda_yield[i] <- mean(tmp1$yield, na.rm = T)
}


plot(mdata$yield, mdata$usda_yield)
cor(mdata$yield, mdata$usda_yield, use = 'pairwise.complete.obs')

write.csv(mdata, file = 'data/mdata.csv')



