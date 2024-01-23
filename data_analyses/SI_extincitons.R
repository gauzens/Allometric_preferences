rm(list = ls())
library(rstudioapi)
library(ggplot2)
setwd(dirname(getActiveDocumentContext()$path))

results = read.csv('../allometric_preferences_model/results.csv', header = FALSE)
head(results)
names(results) = c('temperature', 'nutrients', 'q', 'replicate', 'behaviour', 'nb_extinctions', 'nb_extinctions_nnb')
# pars = read.table('../allometric_preferences_model/parameters.txt')

results$nb_extinctions_b = results$nb_extinctions - results$nb_extinctions_nnb

# stick to the temperature conditions form the dataset to not extrapolate
results = subset(results, temperature <= 18 & nutrients < 50 & temperature > 0)

head(results, 10)

results$behaviour[results$behaviour == 0] = 'Non Flexible'
results$behaviour[results$behaviour == 1] = 'Flexible'

results$persistence = 1 - results$nb_extinctions/50

# number of knots
k = 4
##############3 all species ########################

gfacet.nut = ggplot(results, aes(x = temperature, y = nb_extinctions, color = behaviour))+
  geom_point(alpha = 0.4, shape=20, size = 1)+
  geom_smooth(method = "gam", formula = y ~ s(x, k = k)) +
  facet_wrap(~nutrients)+
  ylab("Number of extinctions, all species")+
  xlab("Temperature, °C")+
  scale_color_discrete(name = "Foraging\nbehaviour")

gfacet.nut

ggsave('~/projects/allometric_preferences/paper/figures/SI_nutrientWrap.jpeg')

gfacet.q = ggplot(results, aes(x = temperature, y = nb_extinctions, color = behaviour))+
  geom_point(alpha = 0.4, shape=20, size = 1)+
  geom_smooth(method = "gam", formula = y ~ s(x, k = k)) + 
  # theme(legend.position = c(0.91, 0.05), legend.justification = c(1, 0))+
  facet_wrap(~as.factor(q))+
  ylab("Number of extinctions, all species")+
  xlab("Temperature, °C")+
  scale_color_discrete(name = "Foraging\nbehaviour")

gfacet.q
ggsave('~/projects/allometric_preferences/paper/figures/SI_HillWrap.jpeg')

one_nut = subset(results, nutrients == 20)
ggplot(one_nut, aes(x = temperature, y = nb_extinctions, color = behaviour))+
  geom_point(alpha = 0.4, shape=20, size = 1)+
  geom_smooth()+
  labs(y = 'Number of extinctions', x = 'Temperature, °C')+
  facet_wrap(~as.factor(q))


one_q = subset(results, q == 1.5)
ggplot(one_q, aes(x = temperature, y = nb_extinctions, color = behaviour))+
  geom_point(alpha = 0.4, shape=20, size = 1)+
  geom_smooth()+
  labs(y = 'Number of extinctions', x = 'Temperature, °C')+
  facet_wrap(~nutrients)

oneq.onenut = subset(results, q == 1.5 & nutrients == 20)
ggplot(oneq.onenut, aes(x = temperature, y = persistence, color = behaviour))+
  geom_point(alpha = 0.4, shape=20, size = 1.5)+
  labs(y = 'Number of extinctions', x = 'Temperature, °C')+
  stat_smooth(method = 'gam', 
              method.args = list(family = 'binomial'),
              formula = y ~ s(x, bs = "cs", fx = TRUE, k=5))

oneq.onenut = subset(results, q == 1.2 & nutrients == 20)
ggplot(oneq.onenut, aes(x = temperature, y = nb_extinctions, color = as.factor(behaviour)))+
  geom_point(alpha = 0.4, shape=20, size = 1.5)+
  labs(y = 'Number of extinctions', x = 'Temperature, °C')+
  stat_smooth()

ggplot(oneq.onenut, aes(x = temperature, y = nb_extinctions_nnb, color = behaviour))+
  geom_point(alpha = 0.4, shape=20, size = 1.5)+
  labs(y = 'Number of extinctions', x = 'Temperature, °C')+
  stat_smooth()


pdf('first_results_model.pdf', width =10)
gfacet
dev.off()


####################3 non basalspecies #######################

k = 4

gfacet.nut = ggplot(results, aes(x = temperature, y = nb_extinctions_nnb, color = behaviour))+
  geom_point(alpha = 0.4, shape=20, size = 1)+
  geom_smooth(method = "gam", formula = y ~ s(x, k = k))+
  facet_wrap(~nutrients)

gfacet.nut

gfacet.q = ggplot(results, aes(x = temperature, y = nb_extinctions_nnb, color = behaviour))+
  geom_point(alpha = 0.4, shape=20, size = 1)+
  geom_smooth() +
  facet_wrap(~as.factor(q))

gfacet.q


one_nut = subset(results, nutrients == 20)
ggplot(one_nut, aes(x = temperature, y = nb_extinctions_nnb, color = behaviour))+
  geom_point(alpha = 0.4, shape=20, size = 1)+
  geom_smooth()+
  labs(y = 'Number of extinctions', x = 'Temperature, °C')+
  facet_wrap(~as.factor(q))


one_q = subset(results, q == 1.2)
ggplot(one_q, aes(x = temperature, y = nb_extinctions_nnb, color = behaviour))+
  geom_point(alpha = 0.4, shape=20, size = 1)+
  geom_smooth()+
  labs(y = 'Number of extinctions', x = 'Temperature, °C')+
  facet_wrap(~nutrients)


oneq.onenut = subset(results, q == 1.2 & nutrients == 40)
ggplot(oneq.onenut, aes(x = temperature, y = nb_extinctions_nnb, color = behaviour))+
  geom_point(alpha = 0.4, shape=20, size = 1.5)+
  labs(y = 'Number of extinctions', x = 'Temperature, °C')+
  stat_smooth()





# 
# # select specific nutrient turn over rates:
# 
# results_D0.15 = subset(results, nutrients == 0.15)
# g15 = ggplot(results_D0.15, aes(x = temperature, y = nb_extinctions, color = behaviour))+
#   geom_point()+
#   geom_smooth()
# g15
# 
# 
# results_D0.25 = subset(results, nutrients == 0.25)
# g25 = ggplot(results_D0.25, aes(x = temperature, y = nb_extinctions, color = behaviour))+
#   geom_point()+
#   geom_smooth()
# g25
# 
# 
# results_D0.35 = subset(results, nutrients == 0.35)
# g35 = ggplot(results_D0.35, aes(x = temperature, y = nb_extinctions, color = behaviour))+
#   geom_point()+
#   geom_smooth()
# g35