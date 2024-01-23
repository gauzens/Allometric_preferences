# fit parameters of snd to for the model of population dynamics

library(brms)

load('../generated_files/preferences_weigths_0.8.Rdata')
df.pref[, c(1:4, 7:21)] <- sapply(df.pref[, c(1:4, 7:21)], as.numeric)
df.pref$productivity = log10(df.pref$productivity)
names(df.pref)

# mean pred prey ratio
mean(10^df.pref$pred.BM / 10^df.pref$gut.mean)

head(df.pref)
str(df.pref)

model.xi = brm(p.xi ~ pred.BM+
                      temperature+
                      productivity+
                      temperature*productivity,
                     iter = 5000,
                     cores = 4,
                    data = df.pref,
)
plot(model.xi)
summary(model.xi)$fixed

model.omega = brm(p.omega ~ pred.BM+
                 temperature+
                 productivity+
                 temperature*productivity,
                 family = gaussian(link = 'log'),
                 iter = 5000,
                 cores = 4,
                 data = df.pref
)

# plot(model.omega)
summary(model.omega)$fixed

plot(conditional_effects(model.omega), 
     points=TRUE,
     ask=FALSE)

cred.model.omega = fitted(model.omega)
plot(cred.model.omega[,1], df.pref$p.omega-cred.model.omega[,1], xlab="predicted", ylab="residual")
abline(0,0)

model.alpha = brm(p.alpha ~ pred.BM+
                    temperature+
                    productivity+
                    temperature*productivity,
                  iter = 5000,
                  cores = 4,
                  data = df.pref,
)


# plot(model.alpha)
summary(model.alpha)$fixed

