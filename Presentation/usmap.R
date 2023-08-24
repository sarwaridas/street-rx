library(usmap)
library(ggplot2)
`%ni%` = function(x,y) !(x %in% y)

dat <- read_csv("Presentation/usmap.csv")

dat2 = dat[dat$state %ni% c("USVI","USA","Guam", "Puerto Rico", "Washington DC"), c("state", "ppm")]
dat3 = dat[dat$state %in% c("USVI","USA","Guam", "Puerto Rico", "Washington DC"), c("state", "ppm")]
dat3$state = c("US Virgin Islands", "Other", "Washington DC", "Puerto Rico", "Guam")
dat3$x = -3e6
dat3$y = -4e5 * c(0,4,2,3,1)


plot_usmap(regions="states", data=dat2, values="ppm") + 
  scale_fill_viridis_c(option="A") +
  theme(legend.position = "right", legend.key.height = unit(3, "cm")) +
  labs(fill='LEGEND TITLE') +
#  theme_classic() +
  geom_point(data = dat3, aes(x = x, y=y, fill=ppm, group=NULL), shape = 23, size = 6) +
  geom_text(data = dat3, aes(x=x, y=y, label=state), nudge_x=5e5)
