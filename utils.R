remove_preds_few_items = function(df, limit){
  # remove predators from df with less than 'limit' food items
  to.keep = c()
  for (i in unique(df$ID_statistical_fish)){
    tab = df[df$ID_statistical_fish == i, ]
    if (dim(tab)[1] >= limit) {
      to.keep = c(to.keep, i)
     }
  }
  df = subset(df, ID_statistical_fish %in% to.keep)
  return(df)
}


remove_envs_few_items = function(df, limit){
  # remove environmental samplings from df with less than 'limit' sampled items
  to.keep = c()
  df = subset(df, df$Density>0)
  for (i in unique(df$ID_sampling)){
    tab = df[df$ID_sampling == i, ]
    if (dim(tab)[1] >= limit) {
      to.keep = c(to.keep, i)
    }
  }
  df = subset(df, ID_sampling %in% to.keep)
  return(df)
}



prepare_fish = function(df, limit, shape = 'FALSE'){
  # prepare the dataset for analyses
  # 1) remove NA or fish without information (i.e. weigth of 0)
  # 2) remove fish with too few information
  df$ind_weight_g = df$total_weight_g/df$number_of_prey
  # remove NAs in number_of_prey and total_weight_g
  df = df[!is.na(df$number_of_prey), ] # maybe reassign ID_Predators_individuals to 1:n.total
  df = df[!is.na(df$total_weight_g), ] # maybe reassign ID_Predators_individuals to 1:n.total
  df = df[!is.na(df$Length), ] # maybe reassign ID_Predators_individuals to 1:n.total
  df = df[df$ind_weight_g>0, ] # maybe reassign ID_Predators_individuals to 1:n.total
  
  # generate the "statistical fish": group by species x length x ...
  if (shape){
    df$ID_statistical_fish = as.factor(paste0(df$ID_species,"_",df$Length,"_",df$station_name,"_",df$aggregated.dates, '_', df$shape))
  }else{
    df$ID_statistical_fish = as.factor(paste0(df$ID_species,"_",df$Length,"_",df$station_name,"_",df$aggregated.dates))  
  }
  
  df = remove_preds_few_items(df, limit)
  df$ID_Pred_new = as.numeric(droplevels(df$ID_statistical_fish))
  
  return(df)
  
}


prepare_environment = function(df, limit){
  # prepare dtaframe with environment data for analyses
  # 1) remove NA
  # 2) remove samplings with too few datapoints
  
  df = subset(df, df$Density>0)
  df = remove_envs_few_items(df, limit)
  # ids: vector if unique identifyer of sampling events 
  ids = unique(df$ID_sampling)
  
  # ID_env same as ID_sampling, but we can use it as index 1:length(ids)
  df$ID_env = as.numeric(as.factor(df$ID_sampling))
  
  # density is extrapolated to per square meters, explaining non integer values
  df$bodymass = df$Biomass / df$Density
  
  return(df)
}

size2mass = function(size, fish.id){
  # convert fish size to body mass
  # equation from fish base for the baltic sea
  BM = rep(NA, length(size))
  BM[fish.id == 1] = 0.00708 * size[fish.id == 1]^3.08
  BM[fish.id == 3] = 0.00776 * size[fish.id == 3]^3.08
  BM[fish.id == 2] = 0.00631 * size[fish.id == 2]^3.05
  BM[fish.id == 4] = 0.00776 * size[fish.id == 4]^3.06
  BM[fish.id == 5] = 0.00776 * size[fish.id == 5]^3.07
  BM[fish.id == 6] = 0.00562 * size[fish.id == 6]^3.09
  BM[fish.id == 11] = 0.00389 * size[fish.id == 11]^3.08
  return(BM)
}


weighted.median = function(x, weigth){
  calc.median = cbind(x, weigth)
  calc.median = calc.median[order(x), ]
  
  cumulated = cumsum(calc.median[,2])
  median.index = which(cumulated < max(cumulated/2))
  if (length(median.index) == 0){
    median.index = 1
  }else{
    median.index = c(max(median.index), max(median.index)+1)
  }
  w.median = mean(calc.median[median.index, 1])
  return(w.median)
}
