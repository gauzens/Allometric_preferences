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
load('../generated_files/preferences_weigths_0.8.Rdata')
# load('../generated_files/preferences_no_weigths.Rdata')
df.pref[, c(1:4, 7:18)] <- sapply(df.pref[, c(1:4, 7:18)], as.numeric)
df.pref$productivity = log10(df.pref$productivity)



model.all = brm(env.median ~ 
                  temperature+
                  productivity+
                  temperature*productivity,
                data = df.pref,
)

summary(model.all)
tab_model(model.all)
rhat(model.all)

median = plot_model(model.all, type = "pred", terms = c("temperature", "productivity [1.9, 2.7]"),
                    axis.title = c("Temperature, °C", "Median body mass,\n environmental distribution, log10(g)"),
                    show.legend = FALSE, title = "", )+ 
  scale_fill_manual(name = 'xxx', labels = c("low", "high"), values =c("brown", "green"))+
  scale_colour_manual(values =c("brown", "green"), labels = c("low", "high"))



model.all.sd = brm(env.sd ~ 
                  temperature+
                  productivity+
                  temperature*productivity,
                data = df.pref,
)


sd = plot_model(model.all.sd, type = "pred", terms = c("temperature", "productivity [1.9, 2.7]"),
                axis.title = c("Temperature, °C", "Standard deviation,\nenvironmental distribution"),
                colors = c("black", "green"), 
                legend.title = 'Resource\navailability', title = "", )+
  scale_fill_manual(name = 'xxx', labels = c("low", "high"), values =c("brown", "green"))+
  scale_colour_manual(values =c("brown", "green"), labels = c("low", "high"))

g = grid.arrange(median, sd, ncol = 2, widths = c(1,1.2))
ggsave('~/projects/allometric_preferences/paper/figures/SI_env_dists.jpeg', g)

predictors = c(productivity = 'Productivity', temperature = 'Temperature')
tab_model(model.all, model.all.sd, 
          dv.labels = c('Median of BM', 'Standard deviation of BM'), pred.labels = predictors, 
          file = '~/projects/allometric_preferences/paper/figures/tab_SI_env_dist.doc')




