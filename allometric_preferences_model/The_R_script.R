args <- commandArgs(TRUE)
cat(args, '\n')
id = as.numeric(args[1])
S = as.numeric(args[2])
rep = as.numeric(args[3])
q = as.numeric(args[4])


library(sn)
library(ATNrPrefs)

setwd('/home/gauzens/allometric_preferences_model/')


#define the FW parameters
nb_s = 50
nb_b = 20
nb_a = nb_s - nb_b
nb_n = 2




run = function(temperature, fw, BM, rep, q = 1.2, S = 20){
  # i = i+2
  
  cat('aaa')
  model <- create_model_Unscaled_nuts(nb_s, nb_b, nb_n, BM, fw)
  model$ext = 1e-6
  model2 <- new(ATNrPrefs:::Schneider_arma_pref, nb_s, nb_b, nb_n)
  # THIS WE CAN EVEN PUT IN THE CONSTRUCTOR, PERHAPS?
  model2[["BM"]] <- BM
  model2[["log_BM"]] <- log10(BM)
  model2[["fw"]] <- fw
  
  model <- initialise_default_Unscaled_nuts(model, L, temperature = temperature)
  model$q <- q
  model$S <- rep(S, nb_n)
  # model$V[1,] = 0.5
  # model$V[2,] = 1
  model2$temperature = temperature
  model2$D = model$D
  model2$K = model$K
  model2$S = model$S
  model2$X = model$X
  model2$V = model$V
  model2$b = model$b
  model2$c = model$c
  model2$e = model$e
  model2$ext = model$ext
  model2$h = model$h
  model2$q = model$q
  model2$r = model$r
  model2$w = model$w
  model2$q <- model$q
  model2$S <- model$S
  
  model$initialisations()
  model2$initialisations()
  # times <- seq(0, 150000, 1500)
  times <- seq(0, 1500, 15)
  sol <-  tryCatch(lsoda_wrapper(times, biomasses, model),
                   error = function(e){return(matrix(NA, length(biomasses)*2, nrow = 2))},
                   warning = function(w) {return(matrix(NA, length(biomasses)*2, nrow = 2))}
  )
  
  sol2 <- tryCatch(lsoda_wrapper(times, biomasses, model2),
                   error = function(e){return(matrix(NA, length(biomasses)*2, nrow = 2))},
                   warning = function(w) {return(matrix(NA, length(biomasses)*2, nrow = 2))}
  )
  
  extinct = sum(sol[nrow(sol), (nb_n+2):ncol(sol)] < model$ext)
  extinct2 = sum(sol2[nrow(sol2), (nb_n+2):ncol(sol2)] < model$ext)
  
  extinct.nb = sum(sol[nrow(sol), (nb_n+2 + nb_b):ncol(sol)] < model$ext)
  extinct2.nb = sum(sol2[nrow(sol2), (nb_n+2 + nb_b):ncol(sol2)] < model$ext)
  
  res = rbind.data.frame(c(temperature, S, q, rep, 0, extinct, extinct.nb), 
                         c(temperature, S, q, rep, 1, extinct2, extinct2.nb))
  # results[c(i, i+1), ] = res
  names(res) = c('temperature', 'S', 'q', 'rep', 'prefs', 'extinctions', 'extinctions_nn_basal')
  return(res)
  
}

nb_s = 50
nb_b = 20
nb_a = nb_s - nb_b
nb_n = 2

isolated = TRUE
no.herbivore = TRUE
comp = -1
while (isolated & no.herbivore){
  BM = c(sort(runif(nb_b, 1, 6)), sort(runif(nb_a, 2, 9)))
  BM = 10^BM
  # Ropt = 1.62559
  # Ropt = 71.67541
  L = create_Lmatrix(BM ,nb_b, Ropt = 71.67541, gamma = 2, th = 0.01)
  
  # check the presence of isolated species or consumers without prey:
  isolated = any(colSums(L) + rowSums(L) == 0) | any(colSums(L[,(nb_b+1):nb_s])==0)
  # create the binary FW structure
  fw = L
  fw[L>0] = 1

  comp = comp + 1
  # colSums(L[,(nb_b+1) :nb_s])
}
cat('number of rejected webs = ', comp, '\n')

biomasses <- runif(nb_s + nb_n, 2, 3)

temperatures = seq(0,26, by = 2)
xx = lapply(temperatures, run, fw = fw,  BM = BM, rep = rep, q = q, S = S)
results = do.call(rbind, xx)

names(results) = NULL
write.csv(results,
          file = paste('~/allometric_preferences_model/results/results_S_', id, '.txt', sep = ''),
          row.names = F)



