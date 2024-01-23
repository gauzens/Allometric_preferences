rm(list=ls())
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

library(ggplot2)
library(viridis)


# histogram of the distribution of fidh body length

df.gut = read.csv("../generated_files/fish_diet_analyses_31.csv", sep=",")

# associate fish IDs to proper species names
df.gut$species.names = NA
unique(df.gut$ID_species)
df.gut$species.names[df.gut$ID_species == 1] = "Gadus morhua"
df.gut$species.names[df.gut$ID_species == 2] = "Merlangius merlangus"
df.gut$species.names[df.gut$ID_species == 3] = "Limanda limanda"
df.gut$species.names[df.gut$ID_species == 4] = "Pleuronectes platessa"
df.gut$species.names[df.gut$ID_species == 5] = "Platichthys flesus"
df.gut$species.names[df.gut$ID_species == 6] = "Hippoglossoides platessoides"
df.gut$species.names[df.gut$ID_species == 11] = "Enchelyopus cimbrius"

# and shape
df.gut$shape = NA

df.gut$shape[df.gut$species.names %in% c("Limanda limanda", "Pleuronectes platessa", "Platichthys flesus", "Hippoglossoides platessoides")] = 'flat'
df.gut$shape[df.gut$species.names %in% c("Gadus morhua", "Merlangius merlangus", "Enchelyopus cimbrius")] = 'fusiform'

# histograms of fish body length

g_BL = ggplot(df.gut, aes(x = Length))+
  geom_histogram(bins = 20)+
  labs(title="Distribution of fish body length",x="Length (cm)", y = "Count")+
  theme(plot.title = element_text(hjust = 0.5, size = 22))

ggsave('~/projects/allometric_preferences/paper/figures/fig_SI_hist_fish_length.png', g_BL)

# representation of dates

# for environments

16

df.time.difs = unique(df.gut[,c("ID_Predators_individuals", "time.difference", "species.names")])
names(df.time.difs) = c("ID_Predators_individuals", "time.difference", "Species")
time.difs = ggplot(df.time.difs, aes(x = time.difference, fill = Species))+
  geom_histogram(bins = 62)+
  scale_fill_viridis(discrete = TRUE, 
                     guide = guide_legend(label.theme = element_text(face = "italic")))+
  xlab("difference in days between\nfish and environmental samplings")
time.difs
ggsave('~/projects/allometric_preferences/paper/figures/days_differences.jpeg')

# for fish
rm(list = ls())

tab = read.csv('../data/Fish_Diet.csv', header = T, sep = ' ')
str(tab)
tab$dates = as.Date(tab$dates)
unique(tab$dates)
tab$species.names = NA
tab$species.names[tab$ID_species == 1] = "Gadus morhua"
tab$species.names[tab$ID_species == 2] = "Merlangius merlangus"
tab$species.names[tab$ID_species == 3] = "Limanda limanda"
tab$species.names[tab$ID_species == 4] = "Pleuronectes platessa"
tab$species.names[tab$ID_species == 5] = "Platichthys flesus"
tab$species.names[tab$ID_species == 6] = "Hippoglossoides platessoides"
tab$species.names[tab$ID_species == 11] = "Enchelyopus cimbrius"

head(tab)


tab2 = unique(cbind.data.frame(tab$species.names, tab$dates))
names(tab2) = c('Species', 'Dates')
g_date = ggplot(tab2, aes(x = Dates, fill = Species))+
  geom_histogram()+
  labs(title="Distribution of fish species accross dates",x="Date", y = "Count")+
  theme(plot.title = element_text(hjust = 0.2, size = 22), 
        axis.text.x=element_text(angle=60, hjust=1, size = 8))+
  scale_fill_viridis(discrete = TRUE, 
                     guide = guide_legend(label.theme = element_text(face = "italic")))+
  scale_x_date(date_breaks = "6 month")

ggsave('~/projects/allometric_preferences/paper/figures/hist_SI_date_species.png', g_date)



######### histogram of the match in species between guts and env



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
df.gut$species.names[df.gut$ID_species == 2] = "Merlangius merlangus"
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
prey.species.list = c()
biomass.prey = c()
for (i in 1:n.pred){
  gut.prey = subset(df.gut, ID_Pred_new == i)
  total.prey.biom = sum(gut.prey$total_weight_g)
  total.prey.biom.also.in.env = sum(gut.prey$total_weight_g[gut.prey$latin_name %in% gut_in_env])
  props.in.env = c(props.in.env, total.prey.biom.also.in.env / total.prey.biom)
  if (total.prey.biom.also.in.env / total.prey.biom>0.9){
    prey.species.list = c(prey.species.list, gut.prey$latin_name)
    biomass.prey = c(biomass.prey, gut.prey$total_weight_g)
  }
}

fishes= c('Gobiidae spp.', 'Limanda limanda', 
          'Pomatoschistus minutus', 'Pleuronectiformes spp.')
sum(biomass.prey[prey.species.list %in% fishes])/ sum(biomass.prey)

# check stomach composition of consumers kept:
xx = tapply(biomass.prey, prey.species.list, 'mean')

jpeg('~/projects/allometric_preferences/paper/figures/SI_hist_corresp_species_gut_env.jpeg')
hist(props.in.env, nclass = 50, main = '', 
     xlab = "Proportion of prey biomass in fish stomachs \n explained by species also observed in the corresponding environment",
     cex.lab = 1.2)
dev.off()

quantile(props.in.env, c(0, 0.01 ,0.5, 0.10, 0.90, 0.95, 0.99, 1))

# prop of fish for which all species found in stomachs are also found in the environment
sum(props.in.env == 1) / length(props.in.env)

# prop of fish for which 90% of stomach biomassis explained by species found in the environment
sum(props.in.env >0.9) / length(props.in.env)

mean(props.in.env)
summary(props.in.env)

###################################################################################################





######### histogram of the match in species between guts and env FOR THE DATA PAPER
# difference here is that we don't aggregate fish in "statistical fish


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
df.gut$species.names[df.gut$ID_species == 2] = "Merlangius merlangus"
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

# df.gut = prepare_fish(df.gut, limit=7, shape = TRUE)
df.gut$mass.pred = size2mass(df.gut$Length, df.gut$ID_species)

df.gut$ind_weight_g = df.gut$total_weight_g / df.gut$number_of_prey

df.gut$log10.pred = log10(df.gut$mass.pred)
df.gut$log10.prey = log10(df.gut$total_weight_g / df.gut$number_of_prey)

# remove prey that are too big (likely to be errors)
df.gut$pred_prey_ratio = df.gut$ind_weight_g / df.gut$mass.pred
# no such cases apparently:
# df.gut$pred_prey_ratio[df.gut$pred_prey_ratio > 0.8] 
df.gut = subset(df.gut, pred_prey_ratio < 0.8)

# df.gut = prepare_fish(df.gut, limit=7, shape = TRUE)

plot(df.gut$number_of_prey)
hist(df.gut$number_of_prey)
head(df.gut)

df.gut$ID_Pred_new = as.integer(as.factor(df.gut$ID_Pred_new))

df.gut$biom.ratios = df.gut$total_weight_g / df.gut$mass.pred

n.total = nrow(df.gut)
n.total
sum(df.gut$number_of_prey)  # number of interactions recorded
n.pred  = length(unique(df.gut$ID_Predators_individuals))
n.pred

# apply some correction to take into account prey detectability. 
# as some preys (hard bodied, like shells) are more likely to be detected 
# in comparisons to soft bodied prey (worms)
# 
# weigths = read.csv("../data/detection_weigths.csv", header = TRUE)
# df.gut$weigths = 1
# weigth0.8 = weigths$GUTS[weigths$weight == 0.8]
# df.gut$weigths[df.gut$latin_name %in% weigth0.8] = 0.8
# df.gut$weighted_nb_prey = df.gut$number_of_prey * df.gut$weigths
# 
df.gut$ID_Pred_new = as.integer(as.factor(df.gut$ID_Predators_individuals)) # code IDs again, that they run from 1:n.pred

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
prey.species.list = c()
biomass.prey = c()
for (i in 1:n.pred){
  gut.prey = subset(df.gut, ID_Pred_new == i)
  total.prey.biom = sum(gut.prey$total_weight_g)
  total.prey.biom.also.in.env = sum(gut.prey$total_weight_g[gut.prey$latin_name %in% gut_in_env])
  props.in.env = c(props.in.env, total.prey.biom.also.in.env / total.prey.biom)
  if (total.prey.biom.also.in.env / total.prey.biom>0.9){
    prey.species.list = c(prey.species.list, gut.prey$latin_name)
    biomass.prey = c(biomass.prey, gut.prey$total_weight_g)
  }
}

fishes= c('Gobiidae spp.', 'Limanda limanda', 
          'Pomatoschistus minutus', 'Pleuronectiformes spp.')
sum(biomass.prey[prey.species.list %in% fishes])/ sum(biomass.prey)

# check stomach composition of consumers kept:
xx = tapply(biomass.prey, prey.species.list, 'mean')

jpeg('~/projects/allometric_preferences/paper/figures/SI_hist_corresp_species_gut_env.jpeg')
hist(props.in.env, nclass = 50, main = '', 
     xlab = "Proportion of prey biomass in fish stomachs \n explained by species also observed in the corresponding environment",
     cex.lab = 1.2)
dev.off()

quantile(props.in.env, c(0, 0.01 ,0.5, 0.10, 0.90, 0.95, 0.99, 1))

# prop of fish for which all species found in stomachs are also found in the environment
sum(props.in.env == 1) / length(props.in.env)

# prop of fish for which 90% of stomach biomassis explained by species found in the environment
sum(props.in.env >0.9) / length(props.in.env)

mean(props.in.env)
summary(props.in.env)






