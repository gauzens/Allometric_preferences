library(MuMIn)

select.best.model = function(index, df = df.pref, select = BIC){
  global.model.shape = lm(index ~ pred.BM*temperature*shape*productivity, data = df, na.action = "na.fail")
  global.model.species = lm(index ~ pred.BM*temperature*species_name*productivity, data = df, na.action = "na.fail")
  
  list.shape = get.models(dredge(global.model.shape), subset = NA)
  list.species =get.models(dredge(global.model.species), subset = NA)
  
  AIC.shape = unlist(lapply(list.shape, select))
  AIC.species =  unlist(lapply(list.species, select))
  
  best.shape = list.shape[[names(which(AIC.shape == min(AIC.shape)))]]
  best.species = list.shape[[names(which(AIC.species == min(AIC.species)))]]
  
  if (select(best.shape) == select(best.species)){
    cat("AIC/BIC values are similar, comparing models (selecting best.shape by default): \n")
    cat("best for species: \n")
    print(best.species$call)
    cat("best for shape: \n")
    print(best.shape$call)
    best.model = best.shape$call$formula
  }else{
    if (select(best.shape) <= select(best.species)){
      cat('best model uses shape\n')
      best.model = best.shape$call$formula
    }else{
      cat('best model uses species identity\n')
      best.model = best.species$call$formula
    }
  }
  return(best.model)
}
  