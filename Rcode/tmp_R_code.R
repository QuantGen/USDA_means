### OPTION 1 ###
devtools::install_github("rdinter/usdarnass")
library(usdarnass)
nass_set_key('AE78F2A0-8EE8-3F02-BD5D-74BB6272DFE5')

all_states <- nass_param('state_ansi',
                         source_desc = 'CENSUS', 
                         sector_desc = 'CROPS',
                         group_desc = 'FIELD CROPS',
                         commodity_desc = 'CORN',
                         util_practice_desc = 'GRAIN',
                         statisticcat_desc = 'PRODUCTION',
                         agg_level_desc = "COUNTY")

ndata <- list()
for (i in seq_along(all_states)) {
  print(i)
  ndata[[i]] <- try(nass_data(
    source_desc = 'SURVEY', 
    sector_desc = 'CROPS',
    group_desc = 'FIELD CROPS',
    commodity_desc = 'CORN',
    util_practice_desc = 'GRAIN',
    statisticcat_desc = 'PRODUCTION',
    agg_level_desc = "COUNTY",
    year = '>=2014',
    state_ansi = all_states[i]), silent = T)
}

ndata1 <- ndata[unlist(sapply(ndata, class)) == 'data.frame']
ndata1 <- do.call(rbind, ndata1)

table(ndata1$year)



