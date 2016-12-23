library(tmap)
library(ggplot2)
library(tidyverse)

orig <- getwd()

setwd("Z:\\DEC\\Millstream_RiperanVegMonitoring\\Working\\Millstream2016\\data\\trends")
mill <- read_shape("millstream_15_16_pfc_dowbins_mga50.shp")

setwd(orig)


mill1 <- tm_shape(mill)+
  tm_polygons("DoW_Class", palette="-Reds", contrast = 0.5, title="Change Class")

colours <- c("#0000FF", "#00FF00", "#FF9900", "#FF0000", "#00CCFF", "#CCCCCC")

milldf <- mill@data%>%
  group_by(DoW_Class)%>%
  summarise(area=sum(Area))

millplot <- ggplot(milldf, aes(x=DoW_Class, y=area, fill= DoW_Class))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=colours, guide=FALSE)+
  theme_bw()+
  theme(axis.ticks=element_blank(),
        axis.text.x=element_blank())+
  ylab("Area (ha)")+
  xlab("")

m1 <- tm_shape(mill)+
  tm_fill("DoW_Class", palette= colours, title="Change Class")+
  tm_compass(position = c(.02, .5), color.light = "grey90") +
  tm_scale_bar(position=c("right", "top"))+
  #tm_grid(n.x=2, n.y=0, labels.inside.frame = FALSE)+
  tm_credits("Produced by DPaW - RSSA", position = c(.75, 0)) 
print(millplot, vp=grid::viewport(x=.32, y=.35, width=.35, height=.2))


# Saving the viewport not working
save_tmap(m2, "Millstream_example.png", width=1920, height=1080)

jpeg(filename = "Millstream_example.jpeg", width=1920, height=1080, units= 'mm', res= 100, quality = 100)
print(m1)
#print(millplot, vp=grid::viewport(x=.32, y=.35, width=.35, height=.2))
dev.off()

