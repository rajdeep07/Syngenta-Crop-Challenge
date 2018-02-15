
setwd("******************* File Location************************")
rm(list=ls(all=TRUE))

# Reading all the files in the system
performance <- read.csv("D:/UTD/Competitions/Syngeta/Training_Performance_Dataset.csv")
soil <- read.csv("D:/UTD/Competitions/Syngeta/Training_Soil_Dataset.csv")
weather <- read.csv('D:/UTD/Competitions/Syngeta/Training_Weather_Dataset.csv')
genetics <- read.csv("D:/UTD/Competitions/Syngeta/Training_Genetic_Dataset.csv")
n <- nrow(genetics)

dim(genetics)
# [1]  2267 19466

# Exploratory Analysis - Genetics Data Set
# Dividing markers locations in 1000 buckets for further analysis

genetics1 <-  genetics[,2:1000]
for(i in 1:n)
{
  genetics1$AA1[i] <- sum(genetics1[i,] ==  1, na.rm = TRUE)
  genetics1$AC1[i]  <- sum(genetics1[i,] ==  0, na.rm = TRUE)
  genetics1$CC1[i]  <- sum(genetics1[i,] == -1, na.rm = TRUE)
  sum <- genetics1$AA1[i] + genetics1$AC1[i] + genetics1$CC1[i]
  genetics1$AA1[i]  <- genetics1$AA1[i]/sum
  genetics1$AC1[i]  <- genetics1$AC1[i]/sum
  genetics1$CC1[i]  <- genetics1$CC1[i]/sum
}
write.csv(genetics1, file = paste0("new_genetics1.csv"))


j=1001
for(i in seq(2:19))
{
  new_gene<-genetics[,j:(j+999)]
  for(i in 1:n)
  {
    new_genetics$AA1[i] <- sum(new_genetics[i,] ==  1, na.rm = TRUE)
    new_genetics$AC1[i]  <- sum(new_genetics[i,] ==  0, na.rm = TRUE)
    new_genetics$CC1[i]  <- sum(new_genetics[i,] == -1, na.rm = TRUE)
    sum <- new_genetics$AA1[i] + new_genetics$AC1[i] + new_genetics$CC1[i]
    new_genetics$AA1[i]  <- new_genetics$AA1[i]/sum
    new_genetics$AC1[i]  <- new_genetics$AC1[i]/sum
    new_genetics$CC1[i]  <- new_genetics$CC1[i]/sum
  }
  write.csv(new_genetics, file = paste0("new_genetics",i,".csv"))
  j=j+1000
}

genetics20 <- genetics[,19001:19466]
for(i in 1:n)
{
  genetics20$AA1[i] <- sum(genetics20[i,] ==  1, na.rm = TRUE)
  genetics20$AC1[i]  <- sum(genetics20[i,] ==  0, na.rm = TRUE)
  genetics20$CC1[i]  <- sum(genetics20[i,] == -1, na.rm = TRUE)
  sum <- genetics20$AA1[i] + genetics20$AC1[i] + genetics20$CC1[i]
  genetics20$AA1[i]  <- genetics20$AA1[i]/sum
  genetics20$AC1[i]  <- genetics20$AC1[i]/sum
  genetics20$CC1[i]  <- genetics20$CC1[i]/sum
}
write.csv(genetics20, file ="new_genetics20.csv")


f1<-read.csv("new_genetics.csv")
f1<-f1[c((dim(f1)[2]-2):(dim(f1)[2]))]
f1<-cbind(genetics[1],f1)

for(i in seq(2:20))
{
  f2<-read.csv(paste0("new_genetics",i,".csv"))
  f2<-f2[c((dim(f2)[2]-2):(dim(f2)[2]))]
  f1<-cbind(f1,f2)
} 
# write.csv(genetics_sub,file = "genetics_sub.csv")

########

#############################################
# Genetic Clusters
############################################

fit.km <- kmeans(gene[,2:61],10)
fit.km$withinss
# [1] 5.417335 4.090355 5.906796 6.883076 5.283874 2.229639 1.826349 3.066706 4.380734 4.041790
fit.km$betweenss
# [1] 223.2494
colnames(gene)

# Determining optimal number of clusters
wss <- 1:15
number <- 1:15

for (i in 1:15)
{
  wss[i]<-kmeans(gene[,2:61],i)$tot.withinss
}


# ggplot2

library(ggplot2)
data<-data.frame(wss,number)
p<-ggplot(data,aes(x=number,y=wss),color="red")
p+geom_point()+scale_x_continuous(breaks=seq(1,20,1))

# Build 5 cluster model

set.seed(100)
fit.km<-kmeans(gene[,2:61],5) # We can define number of clusters here
colnames(sample)

# Merging the cluster output with original data

gene$cluster1<-fit.km$cluster
max(gene$cluster1)
colnames(sample)
head(sample)

# Profiling of the new clusters formed

#Cluster wise Aggregates
cmeans<-aggregate(gene[,2:61],by=list(gene$cluster1),FUN=mean)
cmeans

###########################################################################################################################################################
# Till now we have created 60 new parameter for genetics data set which will represents the AA, AC, CC marker position concentration 
# for 1000 marker positions. These parameters we will use for further analysis of Model building.
##########################################################################################################################################################

summary(performance)

# Hybrid            Year      Maturity_Group   Location_ID      Latitude       Longitude           Yield         Check_Yield    
# P1304:P2030:   830   Min.   :2008   Min.   :0.000   Min.   :   1   Min.   :26.15   Min.   :-121.54   Min.   : 20.07   Min.   : 30.12  
# P1212:P2030:   683   1st Qu.:2012   1st Qu.:4.000   1st Qu.: 860   1st Qu.:40.08   1st Qu.: -98.22   1st Qu.:101.61   1st Qu.:114.73  
# P1311:P2144:   599   Median :2013   Median :6.000   Median :1321   Median :41.11   Median : -94.14   Median :120.01   Median :130.55  
# P1157:P1320:   577   Mean   :2013   Mean   :5.409   Mean   :1270   Mean   :41.23   Mean   : -93.29   Mean   :116.51   Mean   :128.28  
# P1168:P1295:   502   3rd Qu.:2014   3rd Qu.:7.000   3rd Qu.:1739   3rd Qu.:43.16   3rd Qu.: -88.84   3rd Qu.:134.78   3rd Qu.:144.76  
# P1142:P1240:   493   Max.   :2016   Max.   :8.000   Max.   :2249   Max.   :49.50   Max.   : -71.82   Max.   :278.32   Max.   :210.46  
# (Other)    :144768                                                                                                                    
# Yield_Difference  
# Min.   :-128.237  
# 1st Qu.: -19.491  
# Median : -10.514  
# Mean   : -11.765  
# 3rd Qu.:  -2.529  
# Max.   : 107.329 

summary(soil)
# Location_ID        Latitude       Longitude             s1              s2              s3              s4              s5       
# Min.   :   1.0   Min.   :26.15   Min.   :-121.54   Min.   : 6.80   Min.   : 9.30   Min.   : 0.00   Min.   :11.00   Min.   :4.700  
# 1st Qu.: 564.5   1st Qu.:39.91   1st Qu.: -96.58   1st Qu.:20.20   1st Qu.:38.00   1st Qu.:21.20   1st Qu.:21.95   1st Qu.:6.100  
# Median :1126.0   Median :41.55   Median : -91.62   Median :23.50   Median :45.00   Median :30.00   Median :23.45   Median :6.300  
# Mean   :1125.9   Mean   :41.31   Mean   : -90.80   Mean   :23.46   Mean   :46.32   Mean   :30.23   Mean   :23.09   Mean   :6.398  
# 3rd Qu.:1687.5   3rd Qu.:43.53   3rd Qu.: -86.10   3rd Qu.:26.50   3rd Qu.:54.80   3rd Qu.:38.20   3rd Qu.:24.75   3rd Qu.:6.700  
# Max.   :2249.0   Max.   :49.50   Max.   : -71.49   Max.   :39.80   Max.   :74.20   Max.   :83.30   Max.   :29.40   Max.   :7.900  
# s6               s7             s8        
# Min.   : 1.500   Min.   : 7.2   Min.   : 0.500  
# 1st Qu.: 3.800   1st Qu.:21.8   1st Qu.: 1.400  
# Median : 5.000   Median :24.7   Median : 2.200  
# Mean   : 5.612   Mean   :24.2   Mean   : 2.757  
# 3rd Qu.: 6.800   3rd Qu.:27.2   3rd Qu.: 3.500  
# Max.   :28.000   Max.   :43.2   Max.   :14.700

# install.packages("corrgram")
# install.packages("corrplot")
library(corrplot)

# Computing correlation Matrix
X <- cor(soil)
head(round(X,3))

# Taking required columns
X1 <- X[4:11,4:11]


# Visualizing correlation matrix
corrplot(X1,method="circle",type = "upper")
corrplot(X1,method="pie",type = "upper")
corrplot(X1,method="number",type = "upper")

# Analyzing historical performance
library(data.table)
library(sandwich) 
library(lmtest) # If lmtest isn't working use the following:
# coeftest <- function(x,vcov.=vcov) {
#   estim <- coef(x)
#   sterr <- diag(vcov.(x))
#   tstat <- estim/sqrt(sterr)
#   pval  <- dnorm(-abs(tstat))*2
#   return(cbind(estim,sterr,tstat,pval))
# }

weather1 <- weather[,4:76]
head(weather, 2) #this returns the number of rows indicated in a data frame format
df <- data.frame(head(weather, 2))

summary(weather)
head(weather)
colnames(weather)

# Converting matrix into a data frame
install.packages("reshape2")
require(reshape2)
weather2 = melt(weather,id.vars = c("Location_ID","Latitude","Longitude","Year"))
colnames(weather2)[5] <- "weather"

# Splitting column in characteristics (characteristics and months are different)
install.packages("splitstackshape")
library(splitstackshape)
weather3 <- cSplit(weather2,"weather",sep = "_", type.convert = FALSE)
colnames(weather3)[8] <- "month"
colnames(weather3)[7] <- "characterics"
weather3 <- weather3[,-6]
head(weather3)

# Getting weather variable in the right format
install.packages("reshape")
library(reshape)
weather3 = melt(weather3,id.vars = c("Location_ID","Latitude","Longitude"))
weather3 <- cast(weather3, Location_ID + Latitude + Longitude ~ characterics, mean, value = 'value')
colnames(weather3)[4] <- "w1"
colnames(weather3)[5] <- "w2"
colnames(weather3)[6] <- "w3"
colnames(weather3)[7] <- "w4"
colnames(weather3)[8] <- "w5"
colnames(weather3)[9] <- "w6"

# Getting required data.frame = Combining Soil and weather data together
colnames(weather3)
colnames(soil)
mix=merge(weather3,soil,by.x = "Location_ID",by.y = "Location_ID",all.x = FALSE)
mix <- mix[,-10]
mix <- mix[,-10]
head(mix)

dim(mix)
# [1] 3683     17

#Subsetting the data
sample <- mix[,4:17]

#Scaling the values for k-means clustering algorithm
list <- names(sample)
scaled_data <- data.frame(rownum <- 1:3683)
for(i in 1:length(list))
{
  y<-(sample[,i]-mean(sample[,i]))/(sd(sample[,i]))
  scaled_data<-cbind(scaled_data,y)
  names(scaled_data)[i+1]<-paste("scaled_",list[i])
  print(list[i])
}

colnames(scaled_data)  


#Deleting redundant column    
scaled_data <- scaled_data[,-1]

#Fix the seeds
set.seed(200)

# Combining scaled data and sample data
sample<-cbind(sample,scaled_data)
names(sample)
# [1] "w1"         "w2"         "w3"         "w4"         "w5"         "w6"         "s1"         "s2"         "s3"        
# [10] "s4"         "s5"         "s6"         "s7"         "s8"         "scaled_ w1" "scaled_ w2" "scaled_ w3" "scaled_ w4"
# [19] "scaled_ w5" "scaled_ w6" "scaled_ s1" "scaled_ s2" "scaled_ s3" "scaled_ s4" "scaled_ s5" "scaled_ s6" "scaled_ s7"
# [28] "scaled_ s8"

fit.km <- kmeans(sample[,15:28],10)

fit.km$withinss
# [1]  802.2023 1193.7503  863.6778 2387.3477  697.0540 1800.1317 1564.6361 1925.4849  345.6601 2858.5075
fit.km$betweenss
# [1] 37109.55
colnames(sample)

# Determining optimal number of clusters
wss <- 1:15
number <- 1:15


for (i in 1:15)
{
  wss[i]<-kmeans(sample[,15:28],i)$tot.withinss
}


# ggplot2

library(ggplot2)
data<-data.frame(wss,number)
p<-ggplot(data,aes(x=number,y=wss),color="red")
p+geom_point()+scale_x_continuous(breaks=seq(1,20,1))

# Build 6 cluster model

set.seed(100)
fit.km<-kmeans(sample[,1:14],6) # We can define number of clusters here
colnames(sample)

# Merging the cluster output with original data

sample$cluster<-fit.km$cluster
max(sample$cluster)
colnames(sample)
head(sample)

# Profiling of the new clusters formed

#Cluster wise Aggregates
cmeans<-aggregate(sample[,1:14],by=list(sample$cluster),FUN=mean)
cmeans
# 
# Group.1            w1          w2           w3          w4          w5           w6       s1       s2       s3       s4       s5       s6       s7        s8
# 1       1 -0.0211719827  0.09846443  0.410721717  0.70351062  0.80310640  0.666600335 14.58819 23.32047 62.05354 16.31772 5.859843 4.794488 15.54409 6.3370079
# 2       2 -0.0055273037  0.10320057 -0.006335121  0.13071341  0.07624416  0.121678503 23.23051 52.69213 24.08450 23.78202 6.262228 5.313559 24.12506 1.7638015
# 3       3 -0.0101198748  0.11718401  0.103860863  0.29695349  0.27497068  0.316135523 25.76366 62.47398 11.71599 25.30988 6.160465 4.323110 25.47602 0.9572674
# 4       4 -0.0002614023 -0.18101420 -0.016369014  0.05276228 -0.02663594  0.007018308 29.02848 41.40970 29.61212 24.56970 6.720606 5.732879 27.70697 2.3771212
# 5       5  0.0016774373 -0.32048254  0.197722500 -0.39644145  0.02958301 -0.262328938 19.58776 33.73403 46.68413 20.11606 6.644551 5.371319 21.48356 5.6726577
# 6       6  0.0051242602 -0.06863602 -0.191841293 -0.24358087 -0.28090589 -0.238856965 21.75332 41.70186 36.54808 22.40384 6.465658 6.782189 23.93958 3.2367870
# # 

dim(cmeans)
## Visualise the clusters

#Plotting groups across two dimensions
install.packages("ggplot2")
library(ggplot2)
mix<-cbind(mix,sample)
colnames(mix)
mix <- mix[,-c(18:31)]
mix <- mix[,-c(18:31)]
colnames(mix)

# write.csv(mix,"mix.csv")
# mix <- read.csv("mix.csv")

#For 6 clusters
#Year vs Weather
p<-ggplot(mix,aes(x=Year,y=w1))
p+geom_point(aes(colour=as.factor(cluster)))

#Latitude vs Weather
p<-ggplot(mix,aes(x=Latitude.x,y=value))
p+geom_point(aes(colour=as.factor(cluster)))

#Longitude vs Weather
p<-ggplot(mix,aes(x=Longitude.x,y=value))
p+geom_point(aes(colour=as.factor(cluster)))

#Longitude vs Latitude
p<-ggplot(mix,aes(x=Longitude.x,y=Longitude.x))
p+geom_point(aes(colour=as.factor(cluster)))


# location vs Soil ingredients
p<-ggplot(mix1,aes(x=Location_ID,y=s1))
p+geom_point(aes(colour=as.factor(cluster)))


p<-ggplot(mix,aes(x=Location_ID,y=s2))
p+geom_point(aes(colour=as.factor(cluster)))


p<-ggplot(mix,aes(x=Location_ID,y=s3))
p+geom_point(aes(colour=as.factor(cluster)))


p<-ggplot(mix,aes(x=Location_ID,y=s4))
p+geom_point(aes(colour=as.factor(cluster)))


p<-ggplot(mix,aes(x=Location_ID,y=s5))
p+geom_point(aes(colour=as.factor(cluster)))


p<-ggplot(mix,aes(x=Location_ID,y=s6))
p+geom_point(aes(colour=as.factor(cluster)))


p<-ggplot(mix,aes(x=Location_ID,y=s7))
p+geom_point(aes(colour=as.factor(cluster)))


p<-ggplot(mix,aes(x=Location_ID,y=s8))
p+geom_point(aes(colour=as.factor(cluster)))

# combining performance, soil, weather data
colnames(performance)
colnames(mix_set)
# mix <- read.csv("mix.csv")
max(mix$cluster)
colnames(mix)

# mix_set <- mix[,c(1:15,25)]
# mix_set <- mix[,3:22]
mix_set2=merge(performance,mix,by.x = c("Location_ID"),by.y = c("Location_ID"),all.x = FALSE, all.y = FALSE)
colnames(mix_set2)

# Removing weather variable (For further analysis)
mix_set3 <- mix_set2[,c(1:9,12:26)]
mix_set3 <- mix_set3[!duplicated(mix_set3),]
write.csv(mix_set3,"mix_set3.csv")
mix_set3 <- read.csv("mix_set3.csv")

# Combining with the gene values
colnames(gene)
colnames(mix_set3)
master=merge(mix_set3,gene,by.x = "Hybrid",by.y = "Hybrid",all.x = FALSE)
write.csv(master,"master.csv")
# rm(list=ls(all=TRUE))

# Making clusters based on the kmeans method
master <- read.csv("master.csv")
states <- read.csv("Soil_PCI.CSV")
master1=merge(master,states,by.x = "Location_ID",by.y = "Location_ID",all.x = FALSE)
colnames(master1)
master1 <- master1[,-c(3:4,87:96)]
master1 <- master1[!duplicated(master1),]
write.csv(master1,"master1.csv")
master1 <- read.csv("master1.csv")

library(reshape)
colnames(master1)

sample <- master1[,c(4,8,12:86,90)]
#colnames(sample)
sample <- cast(sample, cluster ~ cluster1, mean, value = 'Yield')
sample

#################################################################################
# Creating 6 x 5 matrix of genes and clusters of yield. By way of example first cell of the matrix represents Cluster = 1 for Genetics and 
# Cluster = 1 for environment data set
################################################################################
# cluster        1        2        3        4        5    
# 1       1 111.6870 102.2423 114.4108 114.2518 117.8112
# 2       2 122.7636 118.4840 125.1480 123.4681 123.4573
# 3       3 122.3266 121.3926 122.8216 122.4363 122.1392
# 4       4 112.2858 108.7391 111.2211 110.9516 109.3761
# 5       5 109.3975 108.8441 114.0752 113.2090 115.8184
# 6       6 113.7179 110.0023 115.0036 113.9175 115.1676



#################################################################################################################
# Using external data set for socio-economic paramters 
################################################################################################################
install.packages("RJSONIO")
reverseGeoCode <- function(latlng) {
  latlngStr <- gsub(' ','%20', paste(latlng, collapse=","))         #Collapse and Encode URL Parameters
  library("RJSONIO")                                            #Load Library
  #Open Connection
  connectStr <- paste('http://maps.google.com/maps/api/geocode/json?sensor=false&latlng=',latlngStr, sep="")
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
  
  #Flatten the received JSON
  data.json <- unlist(data.json)
  if(data.json["status"]=="OK")
    address <- data.json["results.formatted_address"]
  return (address)
}
rownum <- nrow(soil)
for(i in 1:rownum) (soil$address[i] <- reverseGeoCode(c(soil$Latitude[i],soil$Longitude[i])))
write.csv(soil, file = "soil_address.csv")



#################################################################################################################
# Subsetting data for further analysis on the basis of cluster
#################################################################################################################


cluster1 <- subset(master1, cluster == 1)
# write.csv(cluster1,"train_cluster1.csv")
# 
cluster2 <- subset(master1, cluster == 2)
# write.csv(cluster2,"train_cluster2.csv")
# 
cluster3 <- subset(master1, cluster == 3)
# write.csv(cluster3,"train_cluster3.csv")
# 
cluster4 <- subset(master1, cluster == 4)
# write.csv(cluster4,"train_cluster4.csv")
# 
cluster5 <- subset(master1, cluster == 5)
# write.csv(cluster5,"train_cluster5.csv")
# 
cluster6 <- subset(master1, cluster == 6)
# write.csv(cluster6,"train_cluster6.csv")

####################################################################################################################
# Prediction Model : Linear Regression Model
####################################################################################################################

cluster1<- cluster1[,c(4,7,10:84,89)]
colnames(cluster1)

#Prediction Model for different clusters
model1 <- lm(Yield ~ ., data = cluster1)
summary(model1)
step(model1)

###############

cluster2 <- subset(master1, cluster == 2)
colnames(cluster2)
cluster2 <- cluster2[,c(4,7,10:84,89)]

colnames(cluster2)

#Prediction Model for different clusters
model2 <- lm(Yield ~ ., data = cluster2)
summary(model2)
step(model2)

##############

cluster3 <- subset(master1, cluster == 3)
colnames(cluster3)
cluster3 <- cluster3[,c(4,7,10:84,89)]

colnames(cluster3)

#Prediction Model for different clusters
model3 <- lm(Yield ~ ., data = cluster3)
summary(model3)
step(model3)

##############

cluster4 <- subset(master1, cluster == 4)
colnames(cluster4)
cluster4 <- cluster4[,c(4,7,10:84,89)]

colnames(cluster4)

#Prediction Model for different clusters
model4 <- lm(Yield ~ ., data = cluster4)
summary(model4)
step(model4)
model4<-lm(formula = Yield ~ Maturity_Group + w1 + w2 + w3 + w4 + w5 + 
             w6 + s1.x + s2.x + s3.x + s5.x + s6.x + s7.x + s8.x + AA1 + 
             AC1 + CC1 + CC2 + AA3 + AA4 + AC4 + AC5 + CC5 + AA6 + AC6 + 
             CC6 + AA7 + AC7 + CC7 + AA8 + AC8 + CC8 + AA9 + AC9 + AA10 + 
             AC10 + CC10 + AA12 + AC12 + CC12 + AA13 + AC13 + CC13 + AA14 + 
             AC14 + CC14 + CC15 + AA16 + AC16 + CC16 + AA17 + AC17 + AA18 + 
             CC18 + AA19 + AC19 + CC19 + AA20 + AC20 + CC20 + PCI, data = cluster4)

##############

cluster5 <- subset(master1, cluster == 5)
colnames(cluster5)
cluster5 <- cluster5[,c(4,7,10:84,89)]

colnames(cluster5)

#Prediction Model for different clusters
model5 <- lm(Yield ~ ., data = cluster5)
summary(model5)
step(model5)
model5<-lm(formula = Yield ~ Maturity_Group + w1 + w2 + w3 + w4 + w5 + 
             w6 + s1.x + s2.x + s3.x + s4.x + s5.x + s6.x + s7.x + s8.x + 
             AA1 + AC1 + CC2 + AA3 + AA4 + AC4 + CC4 + AC5 + CC5 + CC8 + 
             AA9 + AC9 + CC9 + AA10 + AC10 + CC10 + AA12 + AC12 + CC12 + 
             AA13 + AC13 + CC13 + AA14 + AC14 + AC15 + AA16 + AC16 + CC16 + 
             CC17 + AA19 + AC19 + CC19 + AA20 + AC20 + CC20 + PCI, data = cluster5)

###############

cluster6 <- subset(master1, cluster == 6)
colnames(cluster6)
cluster6 <- cluster6[,c(4,7,10:84,89)]

colnames(cluster6)

#Prediction Model for different clusters
model6 <- lm(Yield ~ ., data = cluster6)
summary(model6)
step(model6)
model6<-lm(formula = Yield ~ Maturity_Group + w1 + w2 + w3 + w4 + w5 + 
             w6 + s1.x + s2.x + s3.x + s4.x + s5.x + s7.x + s8.x + AA1 + 
             AC1 + CC1 + AA2 + AC2 + CC2 + AA3 + AC3 + CC3 + AA5 + AC5 + 
             CC5 + AA6 + AC6 + CC6 + AA7 + AC7 + AA8 + AC8 + AC9 + AC10 + 
             AA11 + AC11 + CC11 + AA12 + AC12 + CC12 + AA13 + AA14 + AC14 + 
             CC14 + AC15 + AA16 + AC16 + CC16 + AA17 + AC17 + CC17 + AA19 + 
             AC19 + CC19 + AA20 + AC20 + CC20 + PCI, data = cluster6)


##########################################
# Validating the linear models 
########################################

library(caret)
inTrain <- createDataPartition(y=cluster1$Yield, p=0.8, list=FALSE)
myTraining1 <- cluster1[inTrain, ]
myTesting1 <- cluster1[-inTrain, ]

one<-lm(formula = Yield ~ Maturity_Group + w1 + w2 + w3 + w4 + w5 + 
          w6 + s1.x + s2.x + s3.x + s4.x + s5.x + s6.x + s8.x + AC1 + 
          CC2 + AA3 + AA4 + AC4 + CC4 + CC5 + AA8 + AC8 + CC8 + AA9 + 
          AC9 + CC9 + CC10 + AA11 + AA12 + AC12 + AA13 + AA14 + CC14 + 
          AA15 + CC15 + AA17 + AA18 + AC18 + CC18 + AA19 + AC19 + CC19 + 
          PCI, data =myTraining1)


p1 <- predict(one, myTesting1[,-2])
p1<-as.data.frame(p1)
mean(myTesting1[,2]/p1$p1)-1
# 0.003151214
RMSE(p1,myTesting1[,2])
# 28.52754

#######################

library(caret)
inTrain <- createDataPartition(y=cluster2$Yield, p=0.8, list=FALSE)
myTraining2 <- cluster2[inTrain, ]
myTesting2 <- cluster2[-inTrain, ]

two <- lm(formula = Yield ~ Maturity_Group + w1 + w2 + w3 + w4 + w5 + 
            w6 + s1.x + s2.x + s3.x + s5.x + s6.x + s7.x + s8.x + AC1 + 
            CC2 + AA3 + AC3 + CC3 + AC4 + CC4 + AA5 + CC5 + AA6 + AC6 + 
            CC6 + AA7 + AC7 + CC7 + AA8 + AC8 + AA9 + AC9 + CC9 + AC10 + 
            CC10 + AA11 + AC11 + CC11 + AA12 + AC12 + CC12 + AA13 + AC13 + 
            CC13 + AA14 + AC14 + CC14 + AA15 + AC15 + CC15 + AA16 + AC16 + 
            CC16 + AA17 + AC17 + CC17 + AA18 + AC18 + AA19 + AC19 + CC19 + 
            AA20 + AC20 + CC20 + PCI, data = myTraining2)
p2 <- predict(two, myTesting2[,-2])
p2<-as.data.frame(p2)
mean(myTesting2[,2]/p2$p2)-1
#-0.000153115

RMSE(p2,myTesting2[,2])
#22.41517

#######################

inTrain <- createDataPartition(y=cluster3$Yield, p=0.8, list=FALSE)
myTraining3 <- cluster3[inTrain, ]
myTesting3 <- cluster3[-inTrain, ]

three<- lm(formula = Yield ~ Maturity_Group + w1 + w4 + w5 + w6 + s1.x + 
             s2.x + s3.x + s4.x + s5.x + s6.x + s7.x + s8.x + AA1 + AC1 + 
             CC1 + AA2 + CC3 + CC4 + AA6 + AC6 + CC6 + AA7 + AC7 + CC7 + 
             AC8 + CC8 + AA9 + AC9 + AA11 + AC11 + CC11 + AA12 + AC12 + 
             CC12 + AA13 + AC13 + CC13 + AA14 + AC14 + CC14 + AA15 + AC15 + 
             AA16 + AC16 + CC16 + AA17 + AC17 + CC17 + CC18 + AC19 + AA20 + 
             AC20 + CC20 + PCI, data =myTraining3)

p3 <- predict(three, myTesting3[,-2])
p3<-as.data.frame(p3)
mean(myTesting3[,2]/p3$p3)-1
#  0.0006213427

RMSE(p3,myTesting3[,2])
# 21.9084

#######################

inTrain <- createDataPartition(y=cluster4$Yield, p=0.8, list=FALSE)
myTraining4 <- cluster4[inTrain, ]
myTesting4 <- cluster4[-inTrain, ]

four <- lm(formula = Yield ~ Maturity_Group + w1 + w2 + w3 + w4 + w5 + 
             w6 + s1.x + s2.x + s3.x + s5.x + s6.x + s7.x + s8.x + AA1 + 
             AC1 + CC1 + CC2 + AA3 + AA4 + AC4 + AC5 + CC5 + AA6 + AC6 + 
             CC6 + AA7 + AC7 + CC7 + AA8 + AC8 + CC8 + AA9 + AC9 + AA10 + 
             AC10 + CC10 + AA12 + AC12 + CC12 + AA13 + AC13 + CC13 + AA14 + 
             AC14 + CC14 + CC15 + AA16 + AC16 + CC16 + AA17 + AC17 + AA18 + 
             CC18 + AA19 + AC19 + CC19 + AA20 + AC20 + CC20 + PCI, data = myTraining4)

p4 <- predict(four, myTesting4[,-2])
p4<-as.data.frame(p4)
mean(myTesting4[,2]/p4$p4)-1
# 0.0005974652

RMSE(p4,myTesting4[,2])
#20.71956

#######################

inTrain <- createDataPartition(y=cluster5$Yield, p=0.8, list=FALSE)
myTraining5 <- cluster5[inTrain, ]
myTesting5 <- cluster5[-inTrain, ]

five <- lm(formula = Yield ~ Maturity_Group + w1 + w2 + w3 + w4 + w5 + 
             w6 + s1.x + s2.x + s3.x + s4.x + s5.x + s6.x + s7.x + s8.x + 
             AA1 + AC1 + CC2 + AA3 + AA4 + AC4 + CC4 + AC5 + CC5 + CC8 + 
             AA9 + AC9 + CC9 + AA10 + AC10 + CC10 + AA12 + AC12 + CC12 + 
             AA13 + AC13 + CC13 + AA14 + AC14 + AC15 + AA16 + AC16 + CC16 + 
             CC17 + AA19 + AC19 + CC19 + AA20 + AC20 + CC20 + PCI, data = myTraining5)
p5 <- predict(five, myTesting5[,-2])
p5<-as.data.frame(p5)
mean(myTesting5[,2]/p5$p5)-1
# -0.001128739

RMSE(p5,myTesting5[,2])
#31.96476

#######################

inTrain <- createDataPartition(y=cluster6$Yield, p=0.8, list=FALSE)
myTraining6 <- cluster6[inTrain, ]
myTesting6 <- cluster6[-inTrain, ]

six<-lm(formula = Yield ~ Maturity_Group + w1 + w2 + w3 + w4 + w5 + 
          w6 + s1.x + s2.x + s3.x + s4.x + s5.x + s7.x + s8.x + AA1 + 
          AC1 + CC1 + AA2 + AC2 + CC2 + AA3 + AC3 + CC3 + AA5 + AC5 + 
          CC5 + AA6 + AC6 + CC6 + AA7 + AC7 + AA8 + AC8 + AC9 + AC10 + 
          AA11 + AC11 + CC11 + AA12 + AC12 + CC12 + AA13 + AA14 + AC14 + 
          CC14 + AC15 + AA16 + AC16 + CC16 + AA17 + AC17 + CC17 + AA19 + 
          AC19 + CC19 + AA20 + AC20 + CC20 + PCI, data=myTraining6)

p6 <- predict(six, myTesting6[,-2])

p6<-as.data.frame(p6)
mean(myTesting6[,2]/p6$p6)-1
# -0.002086126

RMSE(p6,myTesting6[,2])
#26.59691
#26.41076 

###################################################################
# Prediction on Testing Dataset on the basis of Linear Models
#################################################################
r1<-predict(model1,t1)
r2<-predict(model2,t2)
r3<-predict(model3,t3)
r4<-predict(model4,t4)
r5<-predict(model5,t5)
r6<-predict(model6,t6)

r1<-predict(one,t1)
r2<-predict(two,t2)
r3<-predict(three,t3)
r4<-predict(four,t4)
r5<-predict(five,t5)
r6<-predict(six,t6)

##########################################################################################################################
# Prediction Model :  Random Forest
#############################################################################################################################

d1<-cluster1[,c(5,8,11:85,90)]
d2<-cluster2[,c(5,8,11:85,90)]
d3<-cluster3[,c(5,8,11:85,90)]
d4<-cluster4[,c(5,8,11:85,90)]
d5<-cluster5[,c(5,8,11:85,90)]
d6<-cluster6[,c(5,8,11:85,90)]


head(train1)
t1<-test1[,c(5,8:83,87)]
t2<-test2[,c(5,8:83,87)]
t3<-test3[,c(5,8:83,87)]
t4<-test4[,c(5,8:83,87)]
t5<-test5[,c(5,8:83,87)]
t6<-test6[,c(5,8:83,87)]

library(caret)
library(randomForest)

rf_1 <- randomForest(Yield~., data=d1,
                     ntree = 100,
                     mtry = 8,
                     importance = TRUE,
                     proximity = TRUE)

rf_2 <- randomForest(Yield~., data=d2,
                     ntree = 100,
                     mtry = 8,
                     importance = TRUE,
                     proximity = TRUE)

rf_3 <- randomForest(Yield~., data=d3,
                     ntree = 100,
                     mtry = 8,
                     importance = TRUE,
                     proximity = TRUE)

rf_4 <- randomForest(Yield~., data=d4,
                     ntree = 100,
                     mtry = 8,
                     importance = TRUE,
                     proximity = TRUE)

rf_5 <- randomForest(Yield~., data=d5,
                     ntree = 100,
                     mtry = 8,
                     importance = TRUE,
                     proximity = TRUE)

rf_6 <- randomForest(Yield~., data=d6,
                     ntree = 100,
                     mtry = 8,
                     importance = TRUE,
                     proximity = TRUE)
rf2<- randomForest(Yield ~. , data=d2)


p1 <- predict(rf_1, myTesting1)
p2 <- predict(rf_2, myTesting2)
p3 <- predict(rf_3, myTesting3)
p4 <- predict(rf_4, myTesting4)
p5 <- predict(rf_5, myTesting5)
p6 <- predict(rf_6, myTesting6)



RMSE(p1,myTesting1[,2])
# [1] 16.97074
RMSE(p2,t2)

RMSE(p3,t3)
RMSE(p4,t4)
RMSE(p5,t5)
RMSE(p6,t6)


rmse(sim, obs)

RMSE <- function(x,y)
{
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}

# Prediction of Testing Dataset for each dataset Using Random Forest

predict(rf_1,t1)
predict(rf_2,t2)
predict(rf_3,t3)
predict(rf_4,t4)
predict(rf_5,t5)
predict(rf_6,t6)




inTrain <- createDataPartition(y=d1$Maturity_Group, p=0.8, list=FALSE)
myTraining1 <- d1[inTrain, ]
myTesting1 <- d1[-inTrain, ]

one <- randomForest(Yield~., data=myTraining1,
                    ntree = 100,
                    mtry = 8,
                    importance = TRUE,
                    proximity = TRUE)

p1 <- predict(one, myTesting1[,-2])
RMSE(p1,myTesting1[,2])
#  23.24695
RMSE(myTesting1[,2],p1)
#  23.24695
#-------------------

inTrain <- createDataPartition(y=d2$Maturity_Group, p=0.8, list=FALSE)
myTraining2 <- d2[inTrain, ]
myTesting2 <- d2[-inTrain, ]

two <- randomForest(Yield~., data=myTraining2,
                    ntree = 50,
                    mtry = 8,
                    importance = TRUE,
                    proximity = TRUE)

p2 <- predict(two, myTesting2[,-2])
RMSE(p2,myTesting2[,2])
# [1] 

#---------------

inTrain <- createDataPartition(y=d3$Maturity_Group, p=0.5, list=FALSE)
myTraining3 <- d3[inTrain, ]
myTesting3 <- d3[-inTrain, ]

three <- randomForest(Yield~., data=myTraining3,
                      ntree = 100,
                      mtry = 8,
                      importance = TRUE,
                      proximity = TRUE)

p3 <- predict(three, myTesting3[,-2])

#--------------------

inTrain <- createDataPartition(y=d4$Maturity_Group, p=0.8, list=FALSE)
myTraining4 <- d4[inTrain, ]
myTesting4 <- d4[-inTrain, ]

four <- randomForest(Yield~., data=myTraining4,
                     ntree = 100,
                     mtry = 8,
                     importance = TRUE,
                     proximity = TRUE)

p4 <- predict(four, myTesting4[,-2])

#----------------------

inTrain <- createDataPartition(y=d5$Maturity_Group, p=0.8, list=FALSE)
myTraining5 <- d1[inTrain, ]
myTesting5 <- d1[-inTrain, ]

five <- randomForest(Yield~., data=myTraining5,
                     ntree = 100,
                     mtry = 8,
                     importance = TRUE,
                     proximity = TRUE)

p5 <- predict(five, myTesting5[,-2])

#--------------

inTrain <- createDataPartition(y=d6$Maturity_Group, p=0.8, list=FALSE)
myTraining6 <- d6[inTrain, ]
myTesting6 <- d6[-inTrain, ]

six <- randomForest(Yield~., data=myTraining6,
                    ntree = 50,
                    mtry = 8,
                    importance = TRUE,
                    proximity = TRUE)

p6 <- predict(six, myTesting6[,-2])

##########################################################################################################################################
