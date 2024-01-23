rm(list = ls())
library(rstudioapi)
library(ggplot2)
setwd(dirname(getActiveDocumentContext()$path))
results = read.csv('../allometric_preferences_model/results.csv')
names(results) = c('temperature', 'nutrients', 'q', 'replicate', 'behaviour', 'nb_extinctions', 'nb_extinctions_nnb')
# names(results) = c('temperature', 'nutrients', 'behaviour', 'replicate', 'q', 'nb_extinctions', 'non_basal')
results$nb_extinctions_basals = results$nb_extinctions - results$nb_extinctions_nnb

# restrict to dataset temperature to not extrapolate
results = subset(results, temperature <= 20)

pars = read.table('../allometric_preferences_model/parameters.txt')

non.basal = 30
basals = 20

################################################################################

one_nut = subset(results, q == 1.2 & nutrients == 20)

# plot(non_basal ~ temperature, data=one_nut)
plot(nb_extinctions_basals ~ temperature, data=one_nut)

df.true = subset(one_nut, behaviour == TRUE)
df.false = subset(one_nut, behaviour == FALSE)

################################################################################

gam.model.false = mgcv::gam(cbind(nb_extinctions_basals,basals-nb_extinctions_basals) ~ s(temperature, k=3), 
                            family = binomial, 
                            method = "REML",
                            data = df.false)

summary(gam.model.false)

# plot(gam.model.false,
#      trans=plogis,
#      shift = coef(gam.model.false)[1],
#      seWithMean = TRUE)

newdata = data.frame(temperature=seq(from = min(one_nut$temperature),
                                     to = max(one_nut$temperature),
                                     length.out = 100))

predictions = predict(gam.model.false, type="link", newdata=newdata, se.fit=TRUE)

preds.mean = basals*plogis(predictions$fit)
preds.upper = basals*plogis(predictions$fit + 2*predictions$se.fit)
preds.lower = basals*plogis(predictions$fit - 2*predictions$se.fit)

plot(jitter(nb_extinctions_basals, factor=0.5) ~ temperature, 
     data=df.false, 
     # ylim=c(-0.1, 3.1),
     col="darkgrey")

abline(0,0)

lines(newdata$temperature, preds.mean, col="red", lwd=2)
lines(newdata$temperature, preds.lower, col="red", lty=2, lwd=2)
lines(newdata$temperature, preds.upper, col="red", lty=2, lwd=2)

data.false = data.frame(temperature = newdata$temperature,
                        preds.mean = preds.mean,
                        preds.lower = preds.lower,
                        preds.upper = preds.upper, 
                        behaviour = rep('Non Flexible', length(preds.mean))
)

################################################################################

gam.model.true = mgcv::gam(cbind(nb_extinctions_basals,basals-nb_extinctions_basals) ~ s(temperature, k=3), 
                           family = binomial(link="logit"), 
                           method = "REML",
                           data = df.true)

summary(gam.model.true)

# plot(gam.model.true,
#      trans=plogis,
#      shift = coef(gam.model.true)[1],
#      seWithMean = TRUE)

newdata = data.frame(temperature=seq(from = min(one_nut$temperature),
                                     to = max(one_nut$temperature),
                                     length.out = 100))

predictions = predict(gam.model.true, type="link", newdata=newdata, se.fit=TRUE)

preds.mean = basals*plogis(predictions$fit)
preds.upper = basals*plogis(predictions$fit + 2*predictions$se.fit)
preds.lower = basals*plogis(predictions$fit - 2*predictions$se.fit)

plot(jitter(nb_extinctions_basals, factor=0.5) ~ temperature, 
     data=df.true, 
     # ylim=c(-0.1, 3.1),
     col="darkgrey")

abline(0,0)

lines(newdata$temperature, preds.mean, col="red", lwd=2)
lines(newdata$temperature, preds.lower, col="red", lty=2, lwd=2)
lines(newdata$temperature, preds.upper, col="red", lty=2, lwd=2)

data.true = data.frame(temperature = newdata$temperature,
                       preds.mean = preds.mean,
                       preds.lower = preds.lower,
                       preds.upper = preds.upper, 
                       behaviour = rep('Flexible', length(preds.mean))
)

extinctions = rbind.data.frame(data.false, data.true)
raw.data = one_nut
raw.data$Behaviour = 'Non Flexible'
raw.data$Behaviour[one_nut$behaviour == 1]= 'Flexible'
# names(extinctions)[5] = 'Behaviour'
extinctions$Behaviour = extinctions$behaviour
head(extinctions)

library(sjPlot)
set_theme(
  base = theme_classic()+
    theme(strip.background = element_rect(fill = "grey90", color = NA), 
          strip.text = element_text(colour = 'black'),
          panel.grid.major = element_line(colour = 'grey95'),
          axis.title.x=element_text(size=16),
          axis.title.y=element_text(size=16)
    )
)

# ggplot(data = tibble::as_tibble(extinctions),
#        aes(x = temperature, y = preds.mean, fill = Behaviour, color = Behaviour)) +
#   geom_line()  +
#   geom_ribbon(aes(ymin = preds.lower, ymax = preds.upper), alpha = 0.2, linetype = 0, show.legend = FALSE)+
#   geom_point(data = raw.data, aes(x = temperature, y = nb_extinctions_basals, color = Behaviour),
#              alpha = 0.3, size = 0.8, show.legend = FALSE)+
#   xlab('Temperature, °C') + ylab('Number of extinctions')

head(raw.data)
tail(raw.data)
# 
g = ggplot(dplyr::mutate(extinctions, Behaviour = behaviour),
       aes(x = temperature, y = preds.mean, fill = Behaviour, color = Behaviour)) +
  geom_line()  +
  # geom_point()
  geom_ribbon(aes(ymin = preds.lower, ymax = preds.upper), alpha = 0.2, linetype = 0)+
  geom_point(data = raw.data, aes(x = temperature, y = nb_extinctions_basals), 
             alpha = 0.3, size = 0.8, show.legend = FALSE)+
  xlab('Temperature, °C') + ylab('Number of extinctions, basal species')+
  scale_color_discrete(name = "Foraging\nbehaviour")+
  scale_fill_discrete(name = "Foraging\nbehaviour")

g
ggsave('~/projects/allometric_preferences/paper/figures/SI_nb_ext_basals.jpeg', g)


