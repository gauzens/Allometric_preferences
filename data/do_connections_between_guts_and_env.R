rm(list = ls())
# library(rstudioapi)
# setwd(dirname(getActiveDocumentContext()$path))
# set.seed(1)
# fish.diet: data on stomach contents
fish.diet = read.csv("../data/Fish_Diet.csv", sep = ' ')
# benthic.community: data on prey in the environment
benthic.community = read.csv("../data/Benthic_Community.csv", sep = ' ')
# trawlings: data on fish sampling (date, location,...)
trawlings = read.csv("../data/trawlings.csv", sep = ' ')
# environment: data on environmental conditions in the kiel bay
environment = read.csv("../data/environment.csv", sep = ' ')

head(fish.diet)
unique(sort(fish.diet$dates))

##############################################################################################
# temporal aggregation
# when fishing events were made at closely related dates, they are merged to the same date
##############################################################################################

# define the maximum accepted distance between gut content analysis and environment sampling.
time.dif = 31



######### fish diet ####################
# aggreg: merge to the same date trwalings that occurs X days in a row
# if trawlings occur on the 1st, 2nd and 3rd day of the month, final date will be the 2nd
# first column: actual dates, 2nd: aggregated ones
aggreg = read.csv('../data/fish_date_aggregation.csv', header = F)
sort(unique(fish.diet$dates[!fish.diet$dates %in% aggreg[,1]]))

# check if some dates from fish.diet are not mentioned in the aggreg array
# should return 0 and TRUE
sum(!fish.diet$dates %in% aggreg[,1])
sum(fish.diet$dates %in% aggreg[,1]) == length(fish.diet$dates)

fish.diet$aggregated.dates = fish.diet$dates

# get row correspondances between the two files
xx = match(fish.diet$dates, aggreg[,1])
sum(is.na(xx))

# aggregated.dates where all the actual dates, replace them by aggregated ones
fish.diet$aggregated.dates = aggreg[xx,2]


# -------------------------------------------------------------------------------
# -------compare dates from fish.diet to dates from benthic----------------------
# -------------------------------------------------------------------------------

# for each date from fish.diet, find the closest date from benthic.community

# discard trawlings from stations without info on prey
fish.diet = fish.diet[fish.diet$station_name %in% benthic.community$station_name,]

fishdate = as.Date(fish.diet$aggregated.dates)
fish.diet$ID_sampling = as.numeric(as.factor(paste(fish.diet$aggregated.dates, fish.diet$station_name)))
fish.diet$key.to.environment = NA

benthicdate = as.Date(benthic.community$dates)
benthic.uniques = unique(benthicdate)

fish.diet$corresponding.benthic.date = NA
fish.diet2 = c()

# classic  loop approach
# loop over all sampling stations and associate a fish sampling to a 
# prey sampling if time difference < threshold = time.dif
indices2 = fish.diet$corresponding.benthic.date
for (station in as.character(unique(fish.diet$station))){
  # cat('\n!!!!!!!!!!!!!!! station = ', station, ' !!!!!!!!!!!!!!!!\n')
  bent = benthic.community[benthic.community$station_name == station, ]
  fish = fish.diet[fish.diet$station_name == station, ]
  fishdate = as.Date(fish$aggregated.dates)
  benthic.uniques = unique(as.Date(bent$dates))
  loopsize = length(fish$date)
  # loopsize = 136
  for (i in 1:loopsize){
    min.value = min(abs(benthic.uniques - fishdate[i]))
    if (min.value > time.dif){
      # no association between fish sampling and prey sampling possible
      fish$corresponding.benthic.date[i] = NA
      fish$key.to.environment[i] = NA
    }else{
      # association found, select the samplings that are the closest in time
      date = which(abs(benthic.uniques - fishdate[i]) == min.value)
      if (length(date) > 1){
        date = sample(date, 1)
      }
      fish$corresponding.benthic.date[i] = as.character(benthic.uniques[date])
      fish$key.to.environment[i] = unique(bent$ID_sampling[as.character(benthic.uniques[date]) == bent$dates])
    }
  }
  fish.diet2 = rbind.data.frame(fish.diet2, fish)
}


# fish.diet$corresponding.aggregated.benthic.date = benthic.uniques[indices2]
# visual check of the results
head(fish.diet2$corresponding.benthic.date)
summary(fish.diet2$corresponding.benthic.date)
sum(is.na(fish.diet2$corresponding.benthic.date))
head(fish.diet2$aggregated.dates)
# fish.diet$aggregated.dates - fish.diet$corresponding.aggregated.benthic.date
as.Date(fish.diet2$aggregated.dates) - as.Date(fish.diet2$corresponding.benthic.date)
unique(as.Date(fish.diet2$aggregated.dates) - as.Date(fish.diet2$corresponding.benthic.date))
# max(as.Date(fish.diet2$aggregated.dates) - as.Date(fish.diet2$corresponding.aggregated.benthic.date))
difs = as.Date(fish.diet2$aggregated.dates) - as.Date(fish.diet2$corresponding.benthic.date)
fish.diet2$time.difference = difs
# distribution of differences between guts and environment sampling (in days)
tab = unique(fish.diet2[,c('ID_Predators_individuals', 'aggregated.dates', 'corresponding.benthic.date')])
table(as.Date(tab$aggregated.dates) - as.Date(tab$corresponding.benthic.date))

#save everything in fish.diet
fish.diet = fish.diet2

################ connections between fish and prey is done #################
################ now combine with information on environemnt ###############

fishdates = as.Date(fish.diet$aggregated.dates)
envDates = as.Date(environment$dates)

xx = match(format(fishdates, '%m%Y'), format(envDates, '%m%Y'))
indices = rep(NA, length(fishdates))

loopsize = length(fish.diet$aggregated.dates)
fish.diet$temperature = NA
fish.diet$Salinity = NA
for (i in 1:loopsize){
  date = which(abs(envDates - fishdates[i]) == min(abs(envDates - fishdates[i])))
  if (length(date) > 1){
    if (length(unique(environment$Temperature[date])) == 1){
      indices[i] = date[1]
      fish.diet$temperature[i] = environment$Temperature[date[1]]
      fish.diet$Salinity[i] = environment$Salinity[date[1]]
    }else{
      # if several closest date (in day differences), take the mean temperature
      indices[i] = NA
      fish.diet$temperature[i] = mean(environment$Temperature[date])
      fish.diet$Salinity[i] = mean(environment$Salinity[date])
    }
  }else{
  indices[i] = date
  fish.diet$temperature[i] = environment$Temperature[date]
  fish.diet$Salinity[i] = environment$Salinity[date]
  }
}



# associate the environment sampling to local conditions
benthic.dates = as.Date(benthic.community$dates)
envDates = as.Date(environment$dates)

xx = match(format(benthic.dates, '%m%Y'), format(envDates, '%m%Y'))
benthic.community$Temperature = NA
benthic.community$Salinity = NA
for (i in 1:dim(benthic.community)[1]){
# for (i in 1:10){
  date = which(abs(envDates - benthic.dates[i]) == min(abs(envDates - benthic.dates[i])))
  if (length(unique(environment$Temperature[date])) != 1){
    benthic.community$Temperature[i] = mean(environment$Temperature[date])
  }else{
    benthic.community$Temperature[i] = unique(environment$Temperature[date])
  }
  benthic.community$Salinity[i] = mean(environment$Salinity[date])
}


# --------------------------------------------------------------------------------------------------
# ---------------- now remove data from fish and environment without correspondences ---------------
# --------------------------------------------------------------------------------------------------

fish.diet = fish.diet[!is.na(fish.diet$key.to.environment),]
benthic.community = benthic.community[benthic.community$ID_sampling %in% fish.diet$key.to.environment, ]

# and save :)
write.csv(fish.diet, paste('../generated_files/fish_diet_analyses_', time.dif,'.csv', sep = ''), row.names = F)
write.csv(benthic.community, paste('../generated_files/benthic_for_analyses_', time.dif, '.csv', sep = ''), row.names = F)




