# diet ~ environment * preference
# ==> preference ~ diet / environment
#
# generate kernel density estimate for environment 
# calculate preference distribution from diet / environment
# 
# use env densitity values as inverse weights for weighted mean, var, skew for diet observations
# these are properties of the preference distribution
# generate a skew normal preference distr from these values

rm(list = ls())
# dev.off()

library("sn")
library("Hmisc")
library("RColorBrewer")



# increase rule of thumb kernel bandwidth by scaling factor to smooth out density
bw.scale.env = 1.5 
bw.scale.gut = 1.0 # only for visualization, gut density is not required

# cols = brewer.pal(3, "Dark2")
# cols = cols[c(3,1,2)]

# cols = brewer.pal(3, "Set1")
# cols = cols[c(2,3,1)]

cols=c("red", "blue", "purple")

kern = function(x,xi,h){
  z=(x-xi)/h
  # Gaussian:
  out=dnorm(z, mean=0, sd=1)
  return(out)
}

library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

source("../utils.R")

# prepare environment ----------------------------------------------------------

df.env = read.csv('../generated_files/benthic_for_analyses_31.csv', sep = ',')
head(df.env)

df.env = prepare_environment(df.env, limit=5)

df.env$log10.bodymass = log10(df.env$bodymass)

n.env = max(df.env$ID_env)

# prepare gut ----------------------------------------------------------------- 

df.gut = read.csv("../generated_files/fish_diet_analyses_31.csv", sep=",")
nrow(df.gut)
length(unique(df.gut$ID_Predators_individuals))

# add species name information
df.gut$species.names = NA
unique(df.gut$ID_species)
df.gut$species.names[df.gut$ID_species == 1] = "Gadus morhua"
df.gut$species.names[df.gut$ID_species == 2] = "Merlangius merlangius"
df.gut$species.names[df.gut$ID_species == 3] = "Limanda limanda"
df.gut$species.names[df.gut$ID_species == 4] = "Pleuronectes platessa"
df.gut$species.names[df.gut$ID_species == 5] = "Platichthys flesus"
df.gut$species.names[df.gut$ID_species == 6] = "Hippoglossoides platessoides"
df.gut$species.names[df.gut$ID_species == 11] = "Enchelyopus cimbrius"

# and shape
df.gut$shape = NA

df.gut$shape[df.gut$species.names %in% c("Limanda limanda", "Pleuronectes platessa", "Platichthys flesus", "Hippoglossoides platessoides")] = 'flat'
df.gut$shape[df.gut$species.names %in% c("Gadus morhua", "Merlangius merlangius", "Enchelyopus cimbrius")] = 'fusiform'

# remove non identified items (bivalva siphons, unidentified remains...)
sort(unique(df.gut$latin_name))
df.gut = subset(df.gut, !(latin_name %in% 
                            c("Various Groups", "Unidentified crustacean remains", "Clupeidae spp. Reste", "Fish offal (Fischereireste)", "Not animals, div.", 
                              "Siphons", "Unidentified crustacean remains", "Unidentified Polychaeta remains")))

df.gut = prepare_fish(df.gut, limit=7, shape = TRUE)
df.gut$mass.pred = size2mass(df.gut$Length, df.gut$ID_species)
df.gut$log10.pred = log10(df.gut$mass.pred)
df.gut$log10.prey = log10(df.gut$ind_weight_g)

# remove prey that are too big (likely to be errors)
df.gut$pred_prey_ratio = df.gut$ind_weight_g / df.gut$mass.pred
# no such cases apparently:
# df.gut$pred_prey_ratio[df.gut$pred_prey_ratio > 0.8] 
df.gut = subset(df.gut, pred_prey_ratio < 0.8)

df.gut = prepare_fish(df.gut, limit=7, shape = TRUE)

plot(df.gut$number_of_prey)
hist(df.gut$number_of_prey)
head(df.gut)

df.gut$ID_Pred_new = as.integer(as.factor(df.gut$ID_Pred_new))

df.gut$biom.ratios = df.gut$total_weight_g / df.gut$mass.pred

n.total = nrow(df.gut)
n.total
sum(df.gut$number_of_prey)  # number of interactions recorded
n.pred  = length(unique(df.gut$ID_Pred_new))
n.pred

# apply some correction to take into account prey detectability. 
# as some preys (hard bodied, like shells) are more likely to be detected 
# in comparisons to soft bodied prey (worms)

weigths = read.csv("../data/detection_weigths.csv", header = TRUE)
df.gut$weigths = 1
weigth0.8 = weigths$GUTS[weigths$weight == 0.8]
df.gut$weigths[df.gut$latin_name %in% weigth0.8] = 0.8
df.gut$weighted_nb_prey = df.gut$number_of_prey * df.gut$weigths

df.gut$ID_Pred_new = as.integer(as.factor(df.gut$ID_Pred_new)) # code IDs again, that they run from 1:n.pred

# checks differences in species list in gut and env
association.tab = read.csv('gut_env_association_susanne.csv', header = TRUE)
association.tab = association.tab[,-2]
presence = rowSums(association.tab[-1]) !=0
gut_in_env = association.tab[,1][presence]

all.prey = unique(c(as.character(unique(df.env$latin_name)), as.character(unique(df.gut$latin_name))))
unique(df.env$latin_name)
unique(df.gut$latin_name)
prey.in.env = unique(df.env$latin_name)
props.in.env = c()
for (i in 1:n.pred){
  gut.prey = subset(df.gut, ID_Pred_new == i)
  total.prey.biom = sum(gut.prey$total_weight_g)
  total.prey.biom.also.in.env = sum(gut.prey$total_weight_g[gut.prey$latin_name %in% gut_in_env])
  props.in.env = c(props.in.env, total.prey.biom.also.in.env / total.prey.biom)
}


hist(props.in.env, nclass = 50, main = '', 
     xlab = "Proportion of species found in fish stomachs \n also observed in the corresponding environment")
mean(props.in.env)
summary(props.in.env)
# remove fish for which we have a bad match between prey in gut and in env. 
# 80 % of what is found in gut should be found in the prey samples.
# would work without it, but I would be doubtful of the quality here
to.remove = which(props.in.env < 0.8)



# calculate new numbers
n.total = nrow(df.gut)
n.total
sum(df.gut$number_of_prey)  # number of interactions recorded
n.pred  = length(unique(df.gut$ID_Pred_new))
n.pred

# prepare loop ----------------------------------------------------------------- 

x.min = -3.0
x.max =  3.0
x.dens = seq(x.min,x.max,by=0.01)

breaks = seq(from=x.min, to=x.max, length.out=20) # only for plotting histograms
df.env.hist = data.frame(min = breaks[1:(length(breaks)-1)],
                         max = breaks[2:length(breaks)],
                         dens = 0)
df.gut.hist = df.env.hist

pdf("../kernel_densities/preferences.pdf")
# par(mfrow=c(2,2))

# data frame for output
df.pref = data.frame(ID_Pred_new = rep(NA,n.pred),
                     key.to.env = rep(NA,n.pred),
                     ID_env = rep(NA,n.pred),
                     pred.BM = rep(NA, n.pred),
                     shape = rep(NA, n.pred),
                     species_name = rep(NA, n.pred),
                     gut.mean = rep(NA, n.pred),
                     gut.sd = rep(NA, n.pred),
                     gut.median = rep(NA, n.pred),
                     env.mean = rep(NA, n.pred), 
                     env.sd = rep(NA, n.pred), 
                     env.median = rep(NA, n.pred), 
                     productivity = rep(NA, n.pred),
                     temperature = rep(NA, n.pred),
                     p.mean = rep(NA,n.pred),
                     p.sdev = rep(NA,n.pred),
                     p.skew = rep(NA,n.pred),
                     p.median = rep(NA,n.pred),
                     p.xi = rep(NA,n.pred),
                     p.omega = rep(NA,n.pred),
                     p.alpha = rep(NA,n.pred)
)

for(i in 1:n.pred){
  
  # gut subset -----------------------------------------------------------------
  
  df.gut.sub = subset(df.gut, ID_Pred_new==i)
  
  pred.mass = df.gut.sub$log10.pred[1]
  shape = df.gut.sub$shape[1]
  pred.species = df.gut.sub$species.names[1]
  
  w.mean.gut = weighted.mean(df.gut.sub$log10.prey, df.gut.sub$weighted_nb_prey)
  w.sdev.gut = sqrt(wtd.var(df.gut.sub$log10.prey, df.gut.sub$weighted_nb_prey))
  w.median.gut = weighted.median(df.gut.sub$log10.prey, df.gut.sub$weighted_nb_prey)

  
  plot(NA,NA, 
       xlim=c(-1, 3),
       ylim=c(0,1.5),
       xlab="log10.mass.g",
       ylab="density"
  )
  
  title(paste0("statistical fish = ",i))
  
  # initialize density vector and histogram
  y.dens.gut = rep(0, length(x.dens))
  df.gut.hist$dens=0
  
  # histograms
  for(j in 1:nrow(df.gut.sub)){
    for(k in 1:nrow(df.gut.hist)){
      if( df.gut.sub$log10.prey[j]>df.gut.hist$min[k] & 
          df.gut.sub$log10.prey[j]<df.gut.hist$max[k] ){
        df.gut.hist$dens[k] = df.gut.hist$dens[k] + df.gut.sub$weighted_nb_prey[j]
      }
    }
  }
  # normalize
  df.gut.hist$dens = df.gut.hist$dens/sum(df.gut.hist$dens*(x.max-x.min)/length(breaks))
  
  for(j in 1:nrow(df.gut.hist)){
    rect( df.gut.hist$min[j], 0, df.gut.hist$max[j], df.gut.hist$dens[j],
          col=adjustcolor(cols[1], alpha.f=0.3), border=NA)
  }
  
  # densities
  h.gut = 1.06*w.sdev.gut*nrow(df.gut.sub)^(-1/5) # Scott's rule bandwidth
  h.gut = h.gut*bw.scale.gut # increase bandwidth for smoothing
  h.gut=max(h.gut, 0.1)
  
  for(j in 1:nrow(df.gut.sub)){
    y.dens.gut = y.dens.gut + df.gut.sub$weighted_nb_prey[j] * kern(x=x.dens, xi=df.gut.sub$log10.prey[j], h=h.gut)
  }
  # cut off lower and upper boundaries
  # y.dens.gut[x.dens<min(df.gut.sub$log10.prey)-2*h.gut]=0
  # y.dens.gut[x.dens>max(df.gut.sub$log10.prey)+2*h.gut]=0
  y.dens.gut[x.dens<min(df.gut.sub$log10.prey)-0.5]=0
  y.dens.gut[x.dens>max(df.gut.sub$log10.prey)+0.5]=0
  
  # normalize
  y.dens.gut = y.dens.gut / sum(y.dens.gut*(x.max-x.min)/length(x.dens) )
  
  lines(x.dens, y.dens.gut, type="l", col="white", lwd=4)
  lines(x.dens, y.dens.gut, type="l", col=cols[1], lwd=2, lty=2)
  
  rug(df.gut.sub$log10.prey, col=cols[1])
  
  points(w.mean.gut, 0, col="white", lwd=2, pch=18, cex=2)
  points(w.mean.gut, 0, col=cols[1], lwd=2, pch=18, cex=1.5)
  
  # env subset -----------------------------------------------------------------
  
  key.to.env = df.gut.sub$key.to.environment[1]
  
  df.env.sub = subset(df.env, ID_sampling==key.to.env)
  ID_env = df.env.sub$ID_env[1]
  productivity = sum(df.env.sub$Biomass, na.rm = TRUE)
  temperature = df.env.sub$Temperature[1]
  
  w.mean.env = weighted.mean(df.env.sub$log10.bodymass, df.env.sub$Density)
  w.sdev.env = sqrt(wtd.var(df.env.sub$log10.bodymass, df.env.sub$Density))
  w.median.env = weighted.median(df.env.sub$log10.bodymass, df.env.sub$Density)
  
  # initialize density vector and histogram
  y.dens.env = rep(0, length(x.dens))
  df.env.hist$dens=0
  
  # histograms
  for(j in 1:nrow(df.env.sub)){
    for(k in 1:nrow(df.env.hist)){
      if( df.env.sub$log10.bodymass[j]>df.env.hist$min[k] &
          df.env.sub$log10.bodymass[j]<df.env.hist$max[k] ){
        df.env.hist$dens[k] = df.env.hist$dens[k] + df.env.sub$Density[j]
      }
    }
  }
  # normalize
  df.env.hist$dens = df.env.hist$dens/sum(df.env.hist$dens*(x.max-x.min)/length(breaks))
  
  # for(j in 1:nrow(df.env.hist)){
  #   rect( df.env.hist$min[j], 0, df.env.hist$max[j], df.env.hist$dens[j],
  #         col=adjustcolor(cols[2], alpha.f=0.3), border=NA)
  # }
  
  # densities
  h.env = 1.06*w.sdev.env*nrow(df.env.sub)^(-1/5) # Scott's rule bandwidth
  h.env = h.env*bw.scale.env  # increase bandwidth for smoothing
  
  for(j in 1:nrow(df.env.sub)){
    y.dens.env = y.dens.env + df.env.sub$Density[j] * kern(x=x.dens, xi=df.env.sub$log10.bodymass[j], h=h.env)
  }
  # normalize
  y.dens.env = y.dens.env / sum(y.dens.env*(x.max-x.min)/length(x.dens) )
  
  lines(x.dens, y.dens.env, type="l", col="white", lwd=4)
  lines(x.dens, y.dens.env, type="l", col=cols[2], lwd=2)
  
  # preference -----------------------------------------------------------------
  
  # y.dens.pref = y.dens.gut / y.dens.env
  # y.dens.pref = y.dens.pref/ (sum(y.dens.pref)*(x.max-x.min)/length(x.dens))
  # 
  # lines(x.dens, y.dens.pref, type="l", col="white", lwd=4)
  # lines(x.dens, y.dens.pref, type="l", col=cols[3], lwd=2, lty=2)
  # 
  # pref.mean = sum(y.dens.pref*x.dens)*(x.max-x.min)/length(x.dens)
  # points(pref.mean, 0, col="white", lwd=2, pch=18, cex=2)
  # points(pref.mean, 0, col=cols[3], lwd=2, pch=18, cex=1.5)
  
  # print(c(h.gut,h.env))
  
  # title(main=paste0("h.env = ",format(round(h.env,2),nsmall=2),"  h.gut = ",format(round(h.gut,2),nsmall=2)), 
  #       line=0.5,
  #       font.main = 1)  
  
  # preference empirical -------------------------------------------------------
  
  # empirical pref mean
  env.pdf = rep(0, length(df.gut.sub$log10.prey))
  for(j in 1:length(df.gut.sub$log10.prey)){
    # which x.dens is closest
    env.pdf[j] = y.dens.env[ which.min(abs(df.gut.sub$log10.prey[j]-x.dens)) ]
  }
  
  # weighted means and sdev. wights are inverse of environment density
  # pref.mean = weighted.mean(df.gut.sub$log10.prey, df.gut.sub$weighted_nb_prey/env.pdf)
  # pref.sdev = sqrt(wtd.var(df.gut.sub$log10.prey, df.gut.sub$weighted_nb_prey/env.pdf))
  
  weights = df.gut.sub$weighted_nb_prey/env.pdf # distribution of the preferences?
  
  V1 = sum(weights)
  
  pref.mean = sum( weights*df.gut.sub$log10.prey )/V1
  pref.sdev = sqrt(sum( weights*(df.gut.sub$log10.prey-pref.mean)^2 )/V1)
  pref.skew = sum( weights*(df.gut.sub$log10.prey-pref.mean)^3 )/(V1*pref.sdev^3)

  library(spatstat.geom)
  pref.median = weighted.median(df.gut.sub$log10.prey, weights)

  # print(c(pref.var, pref.var.1))
  # print(c(pref.skew, pref.skew.1))
  
  pref.skew = min(pref.skew, 0.95)
  pref.skew = max(pref.skew, -0.95)
  
  # go from centered parametrization to direct parametrization for skew normal distr
  dp = cp2dp(c(pref.mean, pref.sdev, pref.skew), family="SN")
  y.dens.pref.1 = dsn(x.dens, xi=dp[1], omega=dp[2], alpha=dp[3])
  lines(x.dens, y.dens.pref.1, type="l", col="white", lwd=4)
  lines(x.dens, y.dens.pref.1, type="l", col=cols[3], lwd=2, lty=1)
  
  points(pref.mean, 0, col="white", lwd=2, pch=18, cex=2)
  points(pref.mean, 0, col=cols[3], lwd=2, pch=18, cex=1.5)
  
  # save output
  df.pref[i, ] = c(i, key.to.env, ID_env, # IDs
                   pred.mass, shape, pred.species, # consumer information
                   w.mean.gut, w.sdev.gut, w.median.gut, # gut information
                   w.mean.env, w.sdev.env, w.median.env, productivity, temperature, # environment information
                   pref.mean, pref.sdev, pref.skew, pref.median, # moments
                   cp2dp(c(pref.mean, pref.sdev, pref.skew), family="SN") # skew-normal parameters 
  )
}

# plot(pred.BM ~ temperature, data = df.pref)
# summary(lm(pred.BM ~ as.numeric(temperature)*as.numeric(productivity), data = df.pref))
# 
# plot(env.sd ~ temperature, data = df.pref)
# summary(lm(env.sd ~ as.numeric(temperature)*as.numeric(productivity), data = df.pref))
# plot(10^as.numeric(gut.sd) ~ temperature, data = df.pref)
# summary(lm(gut.sd ~ as.numeric(temperature)*as.numeric(productivity), data = df.pref))
# plot(p.skew ~ temperature, data = df.pref)
# summary(lm(p.skew ~ as.numeric(temperature)*as.numeric(productivity), data = df.pref))

names(df.pref)
dev.off()

save(df.pref, file="../generated_files/preferences_weigths_0.8.RData")

