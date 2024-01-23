##### analysis of environmental effect on the realised distribution ######
# I compare 3 models: 
#   - one based on what I hypothesised
#   - one without the effect of shape to test for the generality of the results
#   - one with all 2way interactions, just to feel more secure..
setwd('~/idiv-mount/groups/tib/allometric_preferences_0/kernel_density_est/data_analyses/')
library(brms)
library(sjPlot)
library(gridExtra)
library(ggpubr)
library(cowplot)

set_theme(
  base = theme_classic()+
    theme(strip.background = element_rect(fill = "grey90", color = NA), 
          strip.text = element_text(colour = 'black'),
          panel.grid.major = element_line(colour = 'grey95'),
          axis.title.x=element_text(size=16),
          axis.title.y=element_text(size=16)
    )
)

rm(list = ls())
# load('../generated_files/preferences_weigths_0.8.Rdata')
load('../generated_files/preferences_no_weigths.Rdata')
df.pref[, c(1:4, 7:18)] <- sapply(df.pref[, c(1:4, 7:18)], as.numeric)
df.pref$productivity = log10(df.pref$productivity)



model.all = brm(gut.median ~ pred.BM+
                  temperature+
                  productivity+
                  shape+
                  pred.BM*productivity+
                  pred.BM*temperature+
                  temperature*productivity+
                  temperature*shape,
                data = df.pref,
)
model.all = add_criterion(model.all, "loo")

model.relevant = brm(gut.median ~ pred.BM+
                       temperature+
                       productivity+
                       shape+
                       temperature*shape+
                       temperature*productivity,
                     data = df.pref,
)
model.relevant = add_criterion(model.relevant, "loo")
summary(model.relevant)
model.noshape = brm(gut.median ~ pred.BM+
                      temperature+
                      productivity+
                      temperature*productivity,
                    data = df.pref,
)
model.noshape = add_criterion(model.noshape, "loo")

loo_compare(model.all, model.relevant, model.noshape)

summary(model.relevant)
tab_model(model.noshape)
rhat(model.relevant)

predictors = c(productivity = 'Productivity', temperature = 'Temperature', 
               pred.BM = 'Predator body mass')
predictors = c(predictors, 'Productivity:Temperature', 'temperature:shapefusiform')
names(predictors)[4:5] = c('productivity:temperature', 'Temperature:Shapefusiform')

tab_model(model.relevant, digits = 6, pred.labels = predictors, 
          dv.labels = 'Response of the median prey body mass\n of the realised distribution to', 
  file = '~/projects/allometric_preferences/paper/figures/tab_guts_weights.doc')

b = plot_model(model.relevant, type = "pred", terms = c("pred.BM", 'shape'),
               axis.title = c("Consumer body mass, log10(g)", ""), title = "", legend.title = "Functional\ngroup")+
  geom_point(data = df.pref, mapping = aes(y = gut.mean, x = pred.BM, color = shape), inherit.aes = FALSE)

t = plot_model(model.relevant, type = "pred", terms = c("temperature", 'shape'),
               axis.title = c("Temperature, °C", ""), 
               title = "", show.data = FALSE, legend.title = "Functional\ngroup")+
  geom_point(data = df.pref, mapping = aes(y = gut.median, x = temperature, color = shape), inherit.aes = FALSE)

g = grid.arrange(b, t, nrow = 2, top = '', left = "Median body mass,\nrealised distribution, log10(g)")

ggsave('~/projects/allometric_preferences/paper/figures/SI_realised_median_dists.jpeg', g)

