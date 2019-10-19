library(plyr)
library(ggplot2)


distance <- read.csv('ecoc.csv')
distance$ECoC <- as.factor(distance$ECoC)




distance$norm_dist <- distance$Distance/distance$Country_size
hist(distance$norm_dist,20)



# ggplot(x, aes(x=log_distance)) +
#   geom_histogram(alpha = 0.5) + theme_classic()
# 
# ggplot(x, aes(x=log_distance, group=ECoC, color=ECoC)) +
#   geom_histogram(fill = "white", alpha=0.5, position = "identity") + theme_classic()
# 
# ggplot(x, aes(x=log_time)) +
#   geom_histogram(alpha = 0.5) + theme_classic()
# 
# ggplot(x, aes(x=log_time, group=ECoC, color=ECoC)) +
#   geom_histogram(fill = "white", alpha=0.5, position = "identity") + theme_classic()
# 

# Joining the information

population <- read.csv('POPULATION DATA.csv', header=F, col.names=c('City', 'population'))

data <- join(distance, population, by='City')

# Population Checks
data$City[!is.na(data$museum_vis) & is.na(data$population)]


# Coords Checks
loglat <- read.csv('loglat.csv')

data <- join(data, loglat[,c(1,2,3)], by='City')

table(is.na(data$latitude))
table(is.na(data$longitude))

data$City[is.na(data$latitude)]
data$City[is.na(data$longitude)]

data[data$City == 'Valletta',]

data <- data[data$City != 'Valletta',]

# Education
education <- read.csv('high education rate.csv')

data <- join(data, education[,names(education) != "Country"], by='City')

data$City[!(data$City %in% education$City)]
data$City[!is.na(data$museum_vis) & is.na(data$educ_perc)]
#Aarhus

# Culture budget
budget <- read.csv('culture_budget.csv')

data <- join(data, budget, by='City')

data$City[!(data$City %in% budget$City)]
data$City[!is.na(data$museum_vis) & is.na(data$Median)]
# Burgos     Las Palmas Zaragoza   Katowice   Cagliari   Pula       Waterford 


write.csv(data, file = "MergedECOC.csv",row.names=FALSE)
