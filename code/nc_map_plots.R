library(ggplot2)
library(sp)
library(rgdal)
library(magrittr)
library(broom)

nc_shape <- readOGR(dsn = "Data/part2/nc", layer = "nc")

ran25 <- c("CALDWELL","YANCEY","STOKES","PENDER","PERQUIMANS", "GUILFORD","GREENE","GRANVILLE","MARTIN","ROBESON","CLAY","ALAMANCE","JACKSON","POLK","LINCOLN",    
           "WASHINGTON","CHEROKEE","NORTHAMPTON","DAVIE","JONES",      
           "HYDE","HALIFAX","WARREN","MITCHELL","ONSLOW")

ran25 = tolower(ran25)
nc_shape$County = tolower(nc_shape$County)
nc_shape_tidy = tidy(nc_shape,region = "County")

example_dataset = data.frame(county=ran25, variable_to_plot = 1:25)

nc_shape_tidy = merge(nc_shape_tidy, example_dataset,by.x="id", by.y="county", all.x=TRUE)
nc_shape_tidy = nc_shape_tidy[order(nc_shape_tidy$order, nc_shape_tidy$group),]

ggplot(nc_shape_tidy) + 
  aes(x=long,y=lat,fill=variable_to_plot,group=group) + 
  geom_polygon(color='black', alpha=0.7) + 
  scale_fill_viridis_c(na.value = "white") +
  theme_void() +
  theme(legend.position = "right")
  
  
  
  
  
  
  
  