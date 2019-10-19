library("sp")
library("rgdal")
library("rgeos")
library("GISTools")
library("scales")
library("spdep")
library("GWmodel")
library("scales")
library('maptools')
library('lattice')
library('plyr')
library('ggplot2')

data.nb <- read.gwt2nb('MergedECOC3.gwt')
temp <- read.gwt2nb('example.gwt')
ecoc <- read.csv('MergedECOC3.csv')
temp <- read.csv('MergedECOC_distance.csv')

ecoc <- join(ecoc, temp[,c('City','airport','log_airport')], by='City')

names(data.nb) <- ecoc$City


ecoc$ECoC <- as.factor(ecoc$ECoC)

ggplot(ecoc, aes(x=longitude, y=latitude)) +
  geom_point(aes(colour = ECoC), size=3.5) +
  geom_text(aes(label=City),hjust=1, vjust=-0.7) +
  ggtitle("ECoC") +
  theme_bw()


ecoc$budget_quantile <- cut(ecoc$Median_cult_budg,5)
ggplot(ecoc, aes(x=longitude, y=latitude)) +
  geom_point(aes(colour = budget_quantile), size=3.5) +
  geom_text(aes(label=City),hjust=1, vjust=-0.7) +
  ggtitle("Median Budget Grouped Quantiles") +
  theme_bw()

distances <- attr(data.nb,'GeoDa')[[1]]

# What's the max distances? (for bell curve)
max_dist <- max(ldply(distances,max))

# Bell curve
bell_dists <- lapply(distances,function(x, max_d = max_dist) (1 - (x/max_d)^2)^2)


bell.nb <- nb2listw(data.nb, glist=bell_dists, style="S", zero.policy=TRUE)


# We now calculate Moran's I

joincount.test(ecoc$ECoC, bell.nb)

moran.test(ecoc$airport, bell.nb)
moran.test(ecoc$log_airport, bell.nb)

moran.test(ecoc$road_speed, bell.nb) 
moran.test(ecoc$norm_distance, bell.nb) 
moran.test(ecoc$museum_vis, bell.nb)
moran.test(ecoc$population, bell.nb)
moran.test(ecoc$educ_perc, bell.nb)
moran.test(ecoc$Median_cult_budg, bell.nb)
moran.test(ecoc$Latest_cult_budg, bell.nb)

moran.plot(ecoc$Median_cult_budg, bell.nb, 
           main = "Moran Plot of Median Culture Budget", xlab = "Median Culture Budg", ylab = "Spatially Lagged Culture Budg", col=rgb(0,100,0,90,maxColorValue=255), pch=16) 

moran.plot(ecoc$airport, bell.nb, 
           main = "Moran Plot of Airport Disance", xlab = "Ariport Distance", ylab = "Spatially Lagged Airport Distance", col=rgb(0,100,0,90,maxColorValue=255), pch=16) 

moran.plot(ecoc$log_airport, bell.nb, 
           main = "Moran Plot of Log(Airport Disance)", xlab = "Log(Airport Distance)", ylab = "Spatially Lagged Log(Airport Distance)", col=rgb(0,100,0,90,maxColorValue=255), pch=16) 

ecoc$City[c(8,5)]
ecoc$City[c(11,12)]

ggplot(ecoc, aes(x=museum_vis, y=Median_cult_budg)) + geom_point() +
  geom_smooth(method=lm, se=FALSE) +theme_bw()


Map.lI <- localmoran(ecoc$log_airport, bell.nb) 


quadrant <- vector(mode="numeric",length=nrow(Map.lI))


# centers the number of Tweets around the mean
m.ecoc <- ecoc$log_airport - mean(ecoc$log_airport)     

# centers the local Moran's around the mean
m.local <- Map.lI[,1] - mean(Map.lI[,1])    

# significance threshold, we'll use a 90% level, to be cautious
signif <- 0.1

# builds a data quadrant
quadrant[m.ecoc >0 & m.local>0] <- 4  
quadrant[m.ecoc <0 & m.local<0] <- 1      
quadrant[m.ecoc <0 & m.local>0] <- 2
quadrant[m.ecoc >0 & m.local<0] <- 3
quadrant[Map.lI[,5]>signif] <- 0  

ecoc$City[quadrant == 2]

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","purple4","mediumpurple1","palegreen1", "darkgreen")
plot(Map,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])

legend("bottomright",legend=c("insignificant","very low","low","high","very high"),
       fill=colors,bty="n")

title(main = "Strength and Significance of Local Moran's I for Tweets Per Area")