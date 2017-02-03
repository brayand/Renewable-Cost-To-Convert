#####################################
#Total Cost to Convert the Electric to Renewable Energy in 3 Steps
#1-28-16
#Andrew Bray
#####################
library('httr')

#Personal API Key stored in seperate file that is .gitignored
eia_api_key = readLines('eia.api.key.config')

###############################
##Step 1 - Fossil Fuel Output and LCOE of Renewables
##############################
#Find which data to get
#Category 3 is by fueltype
category_id <- '3'
#Make API call with GET then extract content from httr package into json format
api_call <- GET('http://api.eia.gov/category/', query = list( api_key = eia_api_key, category_id = category_id))
results <- content(api_call)
#See series available in this set
sapply(results$category$childseries, function(x) x$name)

##########################
#Get single run of data
series <- 'ELEC.GEN.ALL-US-99.M'
api_call2 <- GET('http://api.eia.gov/series/', query = list(api_key = eia_api_key, series_id = series))
results2 <- content(api_call2)
#Format dates to data frame
dates <- sapply(results2$series[[1]]$data, function(x) paste0(x[[1]],'01'))
dates_formatted <- as.Date(dates,format='%Y%m%d')
#Pull out data
values <- sapply(results2$series[[1]]$data, function(x) x[[2]])
#Monthly TOtal US Generation
data <-data.frame(row.names=dates_formatted,values)

########################3
#Get actual data
#2

#Series according to http://www.eia.gov/electricity/data/browser/#/topic/0?agg=2,0,1&fuel=vtvv&geo=g&sec=g&linechart=ELEC.GEN.ALL-US-99.M~ELEC.GEN.COW-US-99.M~ELEC.GEN.NG-US-99.M~ELEC.GEN.NUC-US-99.M~ELEC.GEN.HYC-US-99.M~ELEC.GEN.WND-US-99.M~ELEC.GEN.TSN-US-99.M&columnchart=ELEC.GEN.ALL-US-99.M~ELEC.GEN.COW-US-99.M~ELEC.GEN.NG-US-99.M~ELEC.GEN.NUC-US-99.M~ELEC.GEN.HYC-US-99.M~ELEC.GEN.WND-US-99.M&map=ELEC.GEN.ALL-US-99.M&freq=M&start=200101&end=201611&ctype=linechart&ltype=sourcekey&rtype=s&maptype=0&rse=0&pin=
#Coal, Petroleum liquids, Petroleum Coke, NG, Other Gas, Biomass-Wood, Biomass-other
#(7 carbon-emitting sources)
seriesToGet <- c('ELEC.GEN.ALL-US-99.M','ELEC.GEN.COW-US-99.M','ELEC.GEN.PEL-US-99.M',
                 'ELEC.GEN.PC-US-99.M','ELEC.GEN.NG-US-99.M','ELEC.GEN.OOG-US-99.M',
                 'ELEC.GEN.WWW-US-99.M','ELEC.GEN.WAS-US-99.M'
                 )
start=2015
end=2015

#Create function to loop and get all data
get_api <- function(seriesToGet){
    #Create initial matrix for 12 monthes and append any number of generation sources
    allData = data.frame(matrix(nrow=12,ncol=0))

    for(series in seriesToGet){
        api_call2 <- GET('http://api.eia.gov/series/', query = list(api_key = eia_api_key, series_id = series,start=start,end=end))
        results2 <- content(api_call2)
        #Format dates
        dates <- sapply(results2$series[[1]]$data, function(x) paste0(x[[1]],'01'))
        dates_formatted <- as.Date(dates,format='%Y%m%d')
        #Get values
        values <- sapply(results2$series[[1]]$data, function(x) x[[2]])
        #Strip out name of source to put in header
        type <- trimws(strsplit(results2$series[[1]]$name,":")[[1]][2])
        #Put in data.frame
        data <-data.frame(row.names=dates_formatted, values)
        colnames(data) <- type
        allData = cbind(allData,data)
    }
    return(allData)
}
#Monthly totals
all = get_api(seriesToGet)
#Grand fossil fuel total
#All except first column total which won't be a sum of the other columns
#anyway because it excludes non-carbon generating sources
totalEnergyofFF = sum(all[,-1])


#Average LCOE Cost of SOlar and Wind Renewables
#Generation source percentages based on 2015 total share
#https://www.eia.gov/tools/faqs/faq.cfm?id=427&t=3

solarPerc = .006
windPerc = .047
solarShare = solarPerc/(solarPerc+windPerc)
windShare = windPerc/(solarPerc+windPerc)

#LCOE Prices Estimated 2022
#https://www.eia.gov/outlooks/aeo/pdf/electricity_generation.pdf
solarLCOE = 58.2 #$/MWh
windLCOE = 50.9  #$/MWh

renewableLCOE = 1000*(solarLCOE*solarShare +
                windLCOE*windShare) #in $/GWh

costToSwitch1 = totalEnergyofFF*renewableLCOE

paste("$",format(costToSwitch1, big.mark=","),sep="")
########
#End of Step 1 Analysis
####################################

#MWh of Energy per Year of Fossil Fuel

##Step 2 - Add TOU data

##Step 3 - Add location Data
