#####################################
#Total Cost to Convert the Electric to Renewable Energy in 3 Steps
#1-28-16
#Andrew Bray
#####################
library('httr')

#Personal API Key stored in seperate file that is .gitignored
eia_api_key = readLines('eia.api.key.config')
#################################
##Step 1 - Fossil Fuel Output and LCOE of Renewables

###############################
##Step 2 - Match Hourly Demand and Renewable Production
##############################

start=2015
end=2015

#Get single run of data
series <- 'EBA.US48-ALL.D.H'
api_call <- GET('http://api.eia.gov/series/', query = list(api_key = eia_api_key, series_id = series,start=start,end=end))
results <- content(api_call)
#Format dates to data frame
dates <- sapply(results$series[[1]]$data, function(x) x[[1]])
dates_formatted <- as.Date(dates,format='%Y%m%d')
#Pull out data
values <- sapply(results2$series[[1]]$data, function(x) x[[2]])
#Monthly TOtal US Generation
data <-data.frame(row.names=dates_formatted,values)




########
#End of Step 2 Analysis
####################################

##Step 3 - Add location Data
