library(ggplot2)
x = seq(0,1, by = 0.01)
colors = c("Consumers" = "red", 'Resources' = "blue")

ggplot(as.data.frame(x), aes(x = x))+
  stat_function(aes(col = "Consumers"), fun = dnorm, args = list(mean =0.47, sd = 0.04))+
  stat_function(aes(col = "Resources"), fun = dnorm, args = list(mean =0.15, sd = 0.03))+
  labs(y = 'Density', color= '')+
  scale_color_manual(values = colors)+
  theme(legend.key = element_rect(fill = "white"))
