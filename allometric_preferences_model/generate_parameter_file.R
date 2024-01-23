library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

i = 0
params = list()
nuts = c(1, 2, 5, 10, 20, 40, 80, 160, 240)
reps = 1:50
# behaviour = c(TRUE, FALSE)
hill = c(0,0.1,0.2,0.5)
hill = hill + 1
for (rep in reps){
    for (nut in nuts){
      for (q in hill){        
        # for (behav in behaviour){
          i = i+1
          params[[i]] = list()
          params[[i]]$id = i
          params[[i]]$S = nut
          params[[i]]$temperature = t
          # params[[i]]$behav = behav
          params[[i]]$replicate = rep
          params[[i]]$q = q
        # }
      }
    }

}


sink('parameters.txt')
for (parameter in params){
  line = c(parameter$id, parameter$S, parameter$rep)
  cat(parameter$id, parameter$S, parameter$rep, parameter$q, sep = " ")
  cat('\n')
}
sink()

