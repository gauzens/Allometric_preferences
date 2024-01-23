
##### analysis of environmental effect on the trait based distribution ######
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
library(viridis)
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

weights = TRUE
if (weights){
  load('../generated_files/preferences_weigths_0.8.Rdata')
}else{
  load('../generated_files/preferences_no_weigths.Rdata')
}

df.pref[, c(1:4, 7:18)] <- sapply(df.pref[, c(1:4, 7:18)], as.numeric)
df.pref$productivity = log10(df.pref$productivity)



model.all = brm(p.median ~ pred.BM+
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

model.relevant = brm(p.median ~ pred.BM+
                  temperature+
                  productivity+
                  shape+
                  temperature*shape+
                  temperature*productivity,
                  data = df.pref,
                  )
model.relevant = add_criterion(model.relevant, "loo")
summary(model.relevant)
model.noshape = brm(p.median ~ pred.BM+
                       temperature+
                       productivity+
                       temperature*productivity,
                       data = df.pref,
                       
                    )
model.noshape = add_criterion(model.noshape, "loo")

loo_compare(model.all, model.relevant, model.noshape)


# So here, it is completely fine to select the model without shape, based on loo
plot(model.noshape)
summary(model.noshape)
rhat(model.noshape)



tab_model(model.noshape)
predictors = c(productivity = 'Productivity', temperature = 'Temperature', 
               pred.BM = 'Predator body mass')
predictors = c(predictors, 'Productivity:Temperature')
names(predictors)[4] = 'productivity:temperature'

tab_model(model.noshape, digits = 6, pred.labels = predictors, 
          dv.labels = 'Response of the median prey body mass\n of the preference distribution to') 
          # file = '~/projects/allometric_preferences/paper/figures/tab_prefs_weights.doc')

# plot the model predictions

b = plot_model(model.noshape, type = "pred", terms = c("pred.BM"),
               axis.title = c("Predator body mass, log10(g)", ""), title = '')+
  geom_point(data = df.pref, mapping = aes(y = p.median, x = pred.BM, colour = shape), 
             inherit.aes = FALSE)+
  scale_color_viridis_d(begin = 0, end = 0.8, name = "Functional\ngroup")
  # scale_colour_gradient(low = "green", high = "brown", name = 'Resource\navailability',
  #                       labels = c("low", "high"), breaks = c(1.8,3.2))
  # scale_colour_discrete()
b

legend = get_legend(b)
b = b + theme(legend.position = "none")


t = plot_model(model.noshape, type = "pred", terms = c("temperature", "productivity [1.9, 2.7]"),
               axis.title = c("Temperature, °C", ""), 
               title = '')
t = t + scale_fill_manual(name = 'Resource\navailability', 
                          values = c(c("brown", "green")), labels = c("low", "high"))
t = t + scale_color_manual(name = 'Resource\navailability', 
                          values = c(c("brown", "green")), labels = c("low", "high"))
legend2 = get_legend(t)

t = t +  geom_point(data = df.pref, mapping = aes(y = p.median, x = temperature, color = shape),
                   inherit.aes = FALSE)

t = t + scale_color_manual(name = 'Resource\navailability', 
                           values = c(c("brown", "green"), viridis(n = 2, begin = 0, end = 0.8)),
                           labels = c("low", "high", "flat", "fusiform"))
t = t + theme(legend.position = "none")
t

lay = rbind(c(1,1,1,2,2,2,NA),
            c(1,1,1,2,2,2,3),
            c(1,1,1,2,2,2,4),
            c(1,1,1,2,2,2,NA))
  
g = grid.arrange(b, t, legend, legend2, layout_matrix = lay,
                 top = '', left = 'Median body mass,\npreference distribution, log10(g)')
gg = as_ggplot(g) + draw_plot_label(label = c("a)", "b)"), x = c(0,0.5), y = c(1,1) )
gg + theme(plot.background = element_rect(fill="white", color = NA))


if (weights){
  ggsave('~/projects/allometric_preferences/paper/figures/fig2_median_pref.jpeg')
}else{
  
  ggsave('~/projects/allometric_preferences/paper/figures/figSI_median_pref_noweights.jpeg')
}



# Now, check how the fish response to temperature changes depending on productivity levels
posteriors = as_draws_df(model.noshape)
names(posteriors)
head(posteriors)
int.seq = seq(1.5, 3.5, by = 0.1)
interaction.matrix = outer(posteriors$`b_temperature:productivity`, int.seq, '*')

predictions = sweep(interaction.matrix, 1, posteriors$b_temperature, '+')
head(predictions)

pred.mean = colMeans(predictions)
pred.IC = apply(predictions, 2, FUN = quantile, probs = c(0.025, 0.975))

temp.effect = cbind.data.frame(int.seq, pred.mean, t(pred.IC))
names(temp.effect) = c('Productivity', 'Estimate', 'CI25', 'CI975')


thr = temp.effect$Productivity[temp.effect$CI975<0][1]

ggplot(temp.effect, aes(x = Productivity, y = Estimate))+
  geom_line()+
  geom_ribbon(aes(ymin = CI25, ymax = CI975), alpha = 0.1)+
  geom_vline(xintercept = 2.73, lty = 2, color = 'darkgrey')+
  ylab('Estimate of the temperature effect')+
  xlab("Resource availability, log10(g)")

if (weights){
  ggsave('~/projects/allometric_preferences/paper/figures/FIg_03_Temp_effect_pref_median.jpeg')
}


##### analysis of environmental effect on the sd of the trait based distribution #### 
# I compare 3 models: 
#   - one based on what I hypothesised
#   - one without the effect of shape to test for the generality of the results
#   - one with all 2way interactions, just to feel more secure..
##### -------------------------------------------------------------------------- #### 


df.pref$log.pref.sd = log10(df.pref$p.sdev)
model.all.sd = brm(p.sdev ~ pred.BM+
                  temperature+
                  productivity+
                  shape+
                  pred.BM*productivity+
                  pred.BM*temperature+
                  temperature*productivity+
                  temperature*shape,
                data = df.pref,
                cores = 4
)
model.all.sd = add_criterion(model.all.sd, "loo")

model.relevant.sd = brm(p.sdev ~ pred.BM+
                       temperature+
                       productivity+
                       shape+
                       temperature*productivity+
                       temperature*shape,
                     data = df.pref,
                     cores = 4
)
model.relevant.sd = add_criterion(model.relevant.sd, "loo")

model.noshape.sd = brm(p.sdev ~ pred.BM+
                      temperature+
                      productivity+
                      temperature*productivity,
                    data = df.pref,
                    cores = 4
)
model.noshape.sd = add_criterion(model.noshape.sd, "loo")
summary(model.noshape.sd)
rhat(model.noshape.sd)

loo_compare(model.all.sd, model.relevant.sd, model.noshape.sd)




tab_model(model.noshape.sd)
predictors = c(productivity = 'Productivity', temperature = 'Temperature', 
               pred.BM = 'Predator body mass')
predictors = c(predictors, 'Productivity:Temperature')
names(predictors)[4] = 'productivity:temperature'
tab_model(model.noshape.sd, digits = 2, pred.labels = predictors, 
          dv.labels = 'Response of standard deviation of the the preference body mass distribution to')
# , 
          # file = '~/projects/allometric_preferences/paper/NCC/tab_prefs_sd.doc')
# save()

# plot the model predictions

b = plot_model(model.noshape.sd, type = "pred", terms = c("pred.BM"),
               axis.title = c("Predator body mass, log10(g)", ""), title = '')+
  geom_point(data = df.pref, mapping = aes(y = p.sdev, x = pred.BM), inherit.aes = FALSE)

t = plot_model(model.noshape.sd, type = "pred", terms = c("temperature", "productivity [1.9, 2.7]"),
               axis.title = c("Temperature, °C", ""), colors = c("brown", "green"), 
               title = '', legend.title = 'Productivity')+
  geom_point(data = df.pref, mapping = aes(y = p.sdev, x = temperature), inherit.aes = FALSE)

t = t + scale_fill_manual(name = 'xxx', labels = c("low", "high"), values =c("brown", "green"))+
  scale_colour_manual(values =c("brown", "green"), labels = c("low", "high"))

g = grid.arrange(b, t, ncol = 2, widths = c(2, 2.5),
                 top = '', left = 'sd body mass of the trait based niche, log10(g)')
gg = as_ggplot(g) + draw_plot_label(label = c("a)", "b)"), x = c(0,0.5), y = c(1,1) )
gg


# plot(model.noshape.sd)
# summary(model.noshape.sd)
# Now, check how the fish response to temperature changes depending on productivity levels
posteriors.sd = as_draws_df(model.noshape.sd)
names(posteriors.sd)
head(posteriors.sd)
int.seq = seq(1.5, 4, by = 0.1)
interaction.matrix.sd = outer(posteriors.sd$`b_temperature:productivity`, int.seq, '*')

predictions.sd = sweep(interaction.matrix.sd, 1, posteriors.sd$b_temperature, '+')
head(predictions.sd)

pred.mean.sd = colMeans(predictions.sd)
pred.IC.sd = apply(predictions.sd, 2, FUN = quantile, probs = c(0.025, 0.975))

temp.effect.sd = cbind.data.frame(int.seq, pred.mean.sd, t(pred.IC.sd))
names(temp.effect.sd) = c('Productivity', 'Estimate', 'CI25', 'CI975')


ggplot(temp.effect.sd, aes(x = Productivity, y = Estimate))+
  geom_line()+
  geom_ribbon(aes(ymin = CI25, ymax = CI975), alpha = 0.1)+
  geom_vline(xintercept = 2.84, lty = 2, color = 'darkgrey')+
  ylab('Estimate of the temperature effect\ns.d. of preferred distribution')+
  xlab('Resource availability, log10(g)')

if (weigths){
  ggsave('~/projects/allometric_preferences/paper/figures/SI_X_prod_temp_int_sd_pref.jpeg')
}



