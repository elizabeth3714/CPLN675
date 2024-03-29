# CPLN675 Environmental Modelling Midterm Project

rm(list=ls()) 
setwd("D:/675/Midterm/calgary_criteria")
dat <- read.csv("calgary_fincriteria.csv", header=TRUE) 
#dat <- read.csv("Cal_criteria.csv", header=TRUE) 

head(dat)
plot(dat$Inundation)
# backward selection
#install.packages("car")
library(car)

step(lm(Flow_Accum ~ Inundation + elevation + dist_water + tree_dens + Residentia + dev_sum 
        + basin + dist_slop + dist_park + flow_lengt + pop_Dens, data=dat), direction="backward")
mod_1<-lm (Flow_Accum ~ Inundation + dist_water + tree_dens + 
             dev_sum + basin + dist_slop + flow_lengt + pop_Dens, data=dat)
vif(mod_1)
sqrt(vif(mod_1))
summary(mod_1)


# Explore the data
#install.packages("tidyverse")
#install.packages("sf")
#install.packages("gridExtra")
#install.packages("viridis")
library(tidyverse)
library(sf)
library(gridExtra)
library(viridis)

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
     )
}

Calgary <- st_read("D:/675/Midterm/calgary_criteria/calgary_criteria.shp")
#plot(Calgary, max.plot = 14)
plot(Calgary[,2:10])
plot(Calgary[,11:14])

# Dependent var: flood accumulation
ggplot() +
  geom_sf(data=Calgary , aes(fill=Flow_Accum)) + 
  scale_fill_viridis() +
  labs(title="Flood Accumulation in Calgary") +
  mapTheme()
# Dependent var: inundation (dummy)
ggplot() +
  geom_sf(data=Calgary , aes(fill=Inundation)) + 
  scale_fill_viridis() +
  labs(title="Inundation in Calgary") +
  mapTheme()

#ggplot() +
#  geom_sf(data=Calgary, fill="black", colour = NA) +
#  geom_sf(data= filter(epa, training == 1) , aes(fill=ozoneHigh)) + 
#  scale_fill_viridis() +
#  labs(title="Flood Accumulation in Calgary") +
#  mapTheme()

# Interpolating with regression
#Calgary2 <-
#  Calgary %>% 
#  st_centroid() %>% st_coordinates() %>%
#  cbind(
#    Calgary %>% 
#      dplyr::select(Inundation, elevation, dist_water, tree_dens,
#                    Residentia, Flow_Accum, dev_sum, basin, dist_slop, dist_park,
#                    flow_lengt, pop_Dens))
#waterdistPlot <- 
#  ggplot() +
#  geom_sf(data = Calgary2, aes(fill=dist_water)) + 
#  scale_fill_viridis(name="Distance to Water") +
#  mapTheme()

#developPlot <- 
#  ggplot() +
#  geom_sf(data=Calgary2, aes(fill=dev_sum)) + 
#  scale_fill_viridis(name="Developed area") +
#  mapTheme()

#grid.arrange(waterdistPlot,developPlot, ncol=1)

# Build a regression
#training <- 
#  Calgary2 %>%
#  filter(training == 1)

#training %>% 
#  dplyr::select(-training) %>%
#  st_drop_geometry() %>%
#  gather(Variable, Value, -ozoneHigh) %>%
#  ggplot(aes(Value, ozoneHigh)) +
#  geom_point() +
#  geom_smooth(method = "lm", se=FALSE) +
#  facet_wrap(~Variable, scales="free", ncol=5)
#head(dat)

#Calgary2 %>% 
#  dplyr::select(-Calgary2) %>%
#  st_drop_geometry() %>%
#  gather(Variable, Value, -Flow_Accum) %>%
#  ggplot(aes(Value, Flow_Accum)) +
#  geom_point() +
#  geom_smooth(method = "lm", se=FALSE) +
#  facet_wrap(~Variable, scales="free", ncol=3)

# 


rm(list=ls()) 
setwd("D:/675/Midterm/calgary_criteria")
dat <- read.csv("calgary_fincriteria.csv", header=TRUE) 
#dat <- read.csv("Cal_criteria.csv", header=TRUE) 
## Predictive modelling
#install.packages("caret")
library(caret)
#install.packages("pscl")
library(pscl)
#install.packages("plotROC")
library(plotROC)
#install.packages("pROC")
library(pROC)
#install.packages("sf")
library(sf)
#install.packages("tidyverse")
library(tidyverse)

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

# Exploratory analysis
setwd("D:/675/Midterm/calgary_criteria")
Calgary <- st_read("calgary_criteria.shp")
#protected <- st_read("pa_protected_lands2.shp")

ggplot() + 
  geom_sf(data=Calgary)

# check out the variables
str(Calgary)
# build bar plots
head(Calgary)
# inundation
CalgaryPlotVariables <- 
  Calgary %>%
  as.data.frame() %>%
  select(Inundation, elevation, dist_water, tree_dens, 
          Flow_Accum, dev_sum, basin, dist_slop, dist_park,
         flow_lengt, pop_Dens) %>%
  gather(key, value, elevation:pop_Dens)

ggplot(CalgaryPlotVariables, aes(as.factor(Inundation), value, fill=as.factor(Inundation))) + 
  geom_bar(stat="identity") + 
  facet_wrap(~key) +
  scale_fill_manual(values = c("dodgerblue4", "darkgreen"),
                    labels = c("Not Inundation","Inundation"),
                    name = "") +
  labs(x="Inundation", y="Value")
##############
library(tidyverse)
library(sf)
library(gridExtra)
library(viridis)
ggplot() +
  geom_sf(data=Calgary , aes(fill=Flow_Accum)) + 
  scale_fill_viridis() +
  labs(title="Flood Accumulation in Calgary") +
  mapTheme()

ggplot() +
  geom_sf(data=Calgary , aes(fill=Flow_Accum)) + 
  scale_fill_viridis() +
  labs(title="Flood Accumulation in Calgary") +
  mapTheme()


################
# Do again to scale the bars
CalgaryPlotVariables <- 
  CalgaryPlotVariables %>%
  mutate(value = ifelse(key == "Inundation", value*100, value))

ggplot(CalgaryPlotVariables, aes(as.factor(Inundation), value, fill=as.factor(Inundation))) + 
  geom_bar(stat="identity") + 
  facet_wrap(~key) +
  scale_fill_manual(values = c("dodgerblue4", "darkgreen"),
                    labels = c("Not Inundation","Inundation"),
                    name = "") +
  labs(x="Inundation", y="Value")

ggplot(CalgaryPlotVariables, aes(as.factor(Inundation), value, fill=as.factor(Inundation))) + 
  geom_bar(stat="identity") + 
  facet_wrap(~key, scales="free") +
  scale_fill_manual(values = c("dodgerblue4", "darkgreen"),
                    labels = c("Not Inundation","Inundation"),
                    name = "") +
  labs(x="Inundation", y="Value")

# set Training vs Testing
rm(list=ls()) 
setwd("D:/675/Midterm/calgary_criteria")
Calgary <- st_read("calgary_criteria.shp")
library(caret)
library(pscl)
library(plotROC)
library(pROC)
library(sf)
library(tidyverse)
set.seed(3456)
trainIndex <- createDataPartition(Calgary$dev_sum, p = .70,
                                  list = FALSE,
                                  times = 1)
CalgaryTrain <- Calgary[ trainIndex,]
CalgaryTest  <- Calgary[-trainIndex,]

plot(CalgaryTrain[,1], main="Calgary Training Set", color="Green")
plot(CalgaryTest[,1], main="Calgary Training Set", color="Green")
#plot(CalgaryTrain)
#plot(CalgaryTest)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("sf")
library(sf)

library(gridExtra)
library(viridis)
#ggplot() +
#  geom_sf(data=Calgary, aes(fill = CalgaryTrain)) + 
#  scale_fill_viridis() +
#  labs(title="Calgary Training Set") +
#  mapTheme()



#dat$Trainig<-ifelse((dat$PRES_N_RES_REC_COLLCTN+dat$PRES_N_RES_REC_TRTM)>0, 1,0)
#summary(dat$grow_city)
#head(dat$grow_city,10)
#par(mfrow=c(1,1))
#plot(dat$STCOU, dat$grow_city, main="Whether or not a growing city", xlab = "City ID", ylab = "Growing City")



plot(CalgaryTrain, color="Grey")
plot(CalgaryTest, color="Grey")
#install.packages()
library(dpryl)
CalgaryModel <- glm(Inundation ~ ., 
                     family="binomial"(link="logit"), data = CalgaryTrain %>%
                       as.data.frame() %>%
                       select(-geometry))
summary(CalgaryModel)


# Model validation
classProbs <- predict(CalgaryModel, CalgaryTest, type="response")

hist(classProbs)

testProbs <- data.frame(obs = as.numeric(CalgaryTest$Inundation),
                        pred = classProbs)

ggplot(testProbs, aes(x = pred, fill=as.factor(obs))) + geom_density() +
  facet_grid(obs ~ .) + xlab("Probability") + geom_vline(xintercept = .5) +
  scale_fill_manual(values = c("dodgerblue4", "darkgreen"),
                    labels = c("Non-Inundation","Inundation"),
                    name = "")
ggplot(testProbs, aes(x = pred, fill=as.factor(obs))) + geom_density() +
  facet_grid(obs ~ ., scales = "free") + xlab("Probability") + geom_vline(xintercept = .5) +
  scale_fill_manual(values = c("dodgerblue4", "darkgreen"),
                    labels = c("Non-Inundation","Inundation"),
                    name = "")

testProbs$predClass  = ifelse(testProbs$pred > .5 ,1,0)
# if the prediction is greater than 0.5, give a value 1, otherwise give a value 0

caret::confusionMatrix(reference = as.factor(testProbs$obs), 
                       data = as.factor(testProbs$predClass), 
                       positive = "1")


testProbs <- testProbs %>%
  mutate(confusion = case_when(
    CalgaryTest$Inundation == 1 & predClass == 1 ~ "TruePositive",
    CalgaryTest$Inundation == 0 & predClass == 1 ~ "FalsePositive",
    CalgaryTest$Inundation == 1 & predClass == 0 ~ "FalseNegative",
    CalgaryTest$Inundation == 0 & predClass == 0 ~ "TrueNegative",
  ))


library(dplyr)
testProbs <- testProbs %>%
  mutate(geometry= CalgaryTest$geometry)

mutate(testProbs, geometry= CalgaryTest$geometry)

CalgaryTest$geometry
CalgaryTest <- CalgaryTest %>%
  mutate(confusion = case_when(
    CalgaryTest$Inundation == 1 & testProbs$predClass == 1 ~ "TruePositive",
    CalgaryTest$Inundation == 0 & testProbs$predClass == 1 ~ "FalsePositive",
    CalgaryTest$Inundation == 1 & testProbs$predClass == 0 ~ "FalseNegative",
    CalgaryTest$Inundation == 0 & testProbs$predClass == 0 ~ "TrueNegative",
  ))
ggplot() +
  geom_sf(data=CalgaryTest, aes(fill=confusion)) +
  scale_fill_manual(values = c("green", "yellow", "blue", "red"), name = "") +
  labs(title="Confusion Matrix Map - Inundation Prediction in Calgary Training Set") +
  mapTheme()



Calgary <- st_read("D:/675/Midterm/calgary_criteria/calgary_criteria.shp")
library(sf)
par(mfrow=c(1,1))
plot(Calgary[,1])
plot(testProbs[,4])
library(ggplot2)
library(ggplot2)
library(tibble)
library(sf)
library(maps)
install.packages("maps")


 #ggplot(CalgaryTest) +
#  geom_sf(data=testProbs, aes(fill=confusion)) +

ggplot()+geom_sf(data=testProbs)

# Accuracy: AUC Curve

#sessionInfo()

#install.packages("caret",
#                 repos = "http://cran.r-project.org", 
#                 dependencies = c("Depends", "Imports", "Suggests"))

#install.packages("caret")
#library(caret)
#load(caret)
#install.packages("pscl")
#library(pscl)
#install.packages("plotROC")
#library(plotROC)
#install.packages("pROC")
#library(pROC)
#install.packages("sf")
#library(sf)
#install.packages("tidyverse")
#library(tidyverse)

ggplot(testProbs, aes(d = obs, m = pred)) +
  geom_roc(n.cuts = 50, labels = FALSE) + 
  style_roc(theme = theme_grey) +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey') 

auc(testProbs$obs, testProbs$pred)

# 50 fold cross-validation
#install.packages("caret")
#library(caret)
#install.packages("pscl")
#library(pscl)
#install.packages("plotROC")
#library(plotROC)
#install.packages("pROC")
#library(pROC)
#install.packages("sf")
#library(sf)
#install.packages("tidyverse")
#library(tidyverse)

ctrl <- trainControl(method = "cv", 
                     number = 100, 
                     savePredictions = TRUE)
head(dat)
cvFit <- train(as.factor(Inundation) ~ .,  data = Calgary %>% 
                 as.data.frame() %>%
               select(-geometry,-dist_water,-tree_dens,-dev_sum, -basin, -flow_lengt, -pop_Dens), 
               method="glm", family="binomial",
               trControl = ctrl)

cvFit


ggplot(as.data.frame(cvFit$resample), aes(Accuracy)) + 
  geom_histogram() +
  scale_x_continuous(limits = c(0, 1)) +
  labs(x="Accuracy",
       y="Count")

#head(dat)
#install.packages("magrittr") # package installations are only needed the first time you use it
#install.packages("dplyr")    # alternative installation of the %>%
#library(magrittr) # needs to be run every time you start R and want to use %>%
#library(dplyr)    # alternatively, this also loads %>%


testProbs$predClass  = ifelse(testProbs$pred > .5 ,1,0)
# if the prediction is greater than 0.5, give a value 1, otherwise give a value 0

caret::confusionMatrix(reference = as.factor(testProbs$obs), 
                       data = as.factor(testProbs$predClass), 
                       positive = "1")

# map prediction

allPredictions <- 
  predict(cvFit, Calgary, type="prob")[,2]

Calgary <- 
  cbind(Calgary,allPredictions) %>%
  mutate(allPredictions = round(allPredictions * 100)) 

ggplot() + 
  geom_sf(data=Calgary, aes(fill=factor(ntile(allPredictions,5))), colour=NA) +
  scale_fill_manual(values = c("#edf8fb","#b3cde3","#8c96c6","#8856a7","#810f7c"),
                    labels=as.character(quantile(Calgary$allPredictions,
                                                 c(0.1,.2,.4,.6,.8),na.rm=T)),
                    name="Predicted\nProbabilities (%)") +
  mapTheme() +
  labs(title="Inundation Prediction in Calgary")
######
# Predict Denver using Calgary model
Denverver <- as.data.frame(Denver)

Calgary<-st_read("D:/675/Midterm/calgary_criteria/calgary_criteria.shp")

allPredictions2 <- 
  predict(cvFit, Denverver, type="prob")[,2]

Denver <- 
  cbind(Denver,allPredictions2) %>%
  mutate(allPredictions2 = round(allPredictions2 * 100)) 


ggplot() + 
  geom_sf(data=Denver, aes(fill=factor(ntile(allPredictions2,5))), colour=NA) +
  scale_fill_manual(values = c("#edf8fb","#b3cde3","#8c96c6","#8856a7","#810f7c"),
                    labels=as.character(quantile(Denver$allPredictions2,
                                                 c(0.1,.2,.4,.6,.8),na.rm=T)),
                    name="Predicted\nProbabilities (%)") +
  mapTheme() +
  labs(title="Inundation Prediction in Denver")
###########################
############################
# Do it again for Denver
###########################
###############################
setwd("D:/675/Midterm/denver_criteria")
Denver <- st_read("denver_criteria.shp")
library(caret)
library(pscl)
library(plotROC)
library(pROC)
library(sf)
library(tidyverse)
set.seed(3456)
trainIndex <- createDataPartition(Denver$dev_sum, p = .70,
                                  list = FALSE,
                                  times = 1)
DenverTrain <- Denver[ trainIndex,]
DenverTest  <- Denver[-trainIndex,]

plot(DenverTrain[,1], main="Denver Training Set", color="Green")
plot(DenverTrain)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("sf")
library(sf)

library(gridExtra)
library(viridis)
#ggplot() +
#  geom_sf(data=Calgary, aes(fill = CalgaryTrain)) + 
#  scale_fill_viridis() +
#  labs(title="Calgary Training Set") +
#  mapTheme()



#dat$Trainig<-ifelse((dat$PRES_N_RES_REC_COLLCTN+dat$PRES_N_RES_REC_TRTM)>0, 1,0)
#summary(dat$grow_city)
#head(dat$grow_city,10)
#par(mfrow=c(1,1))
#plot(dat$STCOU, dat$grow_city, main="Whether or not a growing city", xlab = "City ID", ylab = "Growing City")



plot(DenverTrain, color="Grey")
plot(DenversTest, color="Grey")
#install.packages()
library(dpryl)
DenverModel <- glm(Inundation ~ ., 
                    family="binomial"(link="logit"), data = DenverTrain %>%
                      as.data.frame() %>%
                      select(-geometry))
summary(DenverModel)


# Model validation
classProbs <- predict(DenverModel, DenverTest, type="response")

hist(classProbs)

testProbs <- data.frame(obs = as.numeric(DenverTest$Inundation),
                        pred = classProbs)

ggplot(testProbs, aes(x = pred, fill=as.factor(obs))) + geom_density() +
  facet_grid(obs ~ .) + xlab("Probability") + geom_vline(xintercept = .5) +
  scale_fill_manual(values = c("dodgerblue4", "darkgreen"),
                    labels = c("Non-Inundation","Inundation"),
                    name = "")
ggplot(testProbs, aes(x = pred, fill=as.factor(obs))) + geom_density() +
  facet_grid(obs ~ ., scales = "free") + xlab("Probability") + geom_vline(xintercept = .5) +
  scale_fill_manual(values = c("dodgerblue4", "darkgreen"),
                    labels = c("Non-Inundation","Inundation"),
                    name = "")
# Accuracy: AUC Curve

#sessionInfo()

#install.packages("caret",
#                 repos = "http://cran.r-project.org", 
#                 dependencies = c("Depends", "Imports", "Suggests"))

#install.packages("caret")
#library(caret)
#load(caret)
#install.packages("pscl")
#library(pscl)
#install.packages("plotROC")
#library(plotROC)
#install.packages("pROC")
#library(pROC)
#install.packages("sf")
#library(sf)
#install.packages("tidyverse")
#library(tidyverse)

ggplot(testProbs, aes(d = obs, m = pred)) +
  geom_roc(n.cuts = 50, labels = FALSE) + 
  style_roc(theme = theme_grey) +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey') 

auc(testProbs$obs, testProbs$pred)

# 50 fold cross-validation
#install.packages("caret")
#library(caret)
#install.packages("pscl")
#library(pscl)
#install.packages("plotROC")
#library(plotROC)
#install.packages("pROC")
#library(pROC)
#install.packages("sf")
#library(sf)
#install.packages("tidyverse")
#library(tidyverse)

ctrl <- trainControl(method = "cv", 
                     number = 100, 
                     savePredictions = TRUE)
head(dat)
cvFit <- train(as.factor(Inundation) ~ .,  data = Denver %>% 
                 as.data.frame() %>%
                 select(-geometry,-dist_water,-tree_dens,-dev_sum, -basin, -flow_lengt, -pop_Dens), 
               method="glm", family="binomial",
               trControl = ctrl)

cvFit


ggplot(as.data.frame(cvFit$resample), aes(Accuracy)) + 
  geom_histogram() +
  scale_x_continuous(limits = c(0, 1)) +
  labs(x="Accuracy",
       y="Count")

#head(dat)
#install.packages("magrittr") # package installations are only needed the first time you use it
#install.packages("dplyr")    # alternative installation of the %>%
#library(magrittr) # needs to be run every time you start R and want to use %>%
#library(dplyr)    # alternatively, this also loads %>%


testProbs$predClass  = ifelse(testProbs$pred > .5 ,1,0)
# if the prediction is greater than 0.5, give a value 1, otherwise give a value 0

caret::confusionMatrix(reference = as.factor(testProbs$obs), 
                       data = as.factor(testProbs$predClass), 
                       positive = "1")
# predict map
Denverver <- as.data.frame(Denver)

Calgary<-st_read("D:/675/Midterm/calgary_criteria/calgary_criteria.shp")

allPredictions2 <- 
  predict(cvFit, Denver, type="prob")[,2]

Denver <- 
  cbind(Denver,allPredictions2) %>%
  mutate(allPredictions2 = round(allPredictions2 * 100)) 


ggplot() + 
  geom_sf(data=Denver, aes(fill=factor(ntile(allPredictions2,5))), colour=NA) +
  scale_fill_manual(values = c("#edf8fb","#b3cde3","#8c96c6","#8856a7","#810f7c"),
                    labels=as.character(quantile(Denver$allPredictions2,
                                                 c(0.1,.2,.4,.6,.8),na.rm=T)),
                    name="Predicted\nProbabilities (%)") +
  mapTheme() +
  labs(title="Inundation Prediction in Denver")


###########################
# Predict Denver
Denverver <- as.data.frame(Denver)

Calgary<-st_read("D:/675/Midterm/calgary_criteria/calgary_criteria.shp")

allPredictions2 <- 
  predict(cvFit, Denverver, type="prob")[,2]

Denver <- 
  cbind(Denver,allPredictions2) %>%
  mutate(allPredictions2 = round(allPredictions2 * 100)) 


ggplot() + 
  geom_sf(data=Denver, aes(fill=factor(ntile(allPredictions2,5))), colour=NA) +
  scale_fill_manual(values = c("#edf8fb","#b3cde3","#8c96c6","#8856a7","#810f7c"),
                    labels=as.character(quantile(Denver$allPredictions2,
                                                 c(0.1,.2,.4,.6,.8),na.rm=T)),
                    name="Predicted\nProbabilities (%)") +
  mapTheme() +
  labs(title="Inundation Prediction in Denver")

######################################################################


# model prediction
rm(list=ls()) 
setwd("D:/675/Midterm/calgary_criteria")

#install.packages("caret")
library(caret)
#load(caret)
#install.packages("pscl")
library(pscl)
#install.packages("plotROC")
library(plotROC)
#install.packages("pROC")
library(pROC)
#install.packages("sf")
library(sf)
#install.packages("tidyverse")
library(tidyverse)

names(Calgary)
str(Calgary)
Calgary <- st_read("D:/675/Midterm/calgary_criteria/calgary_criteria.shp")
Denver <- st_read("D:/675/Midterm/denver_criteria/denver_criteria.shp")

# (preserveModel<=Calgary, preserveTest<=Denver)
classProbs <- predict(CalgaryModel, as.data.frame(Denver), type="response")

hist(classProbs)
par(mfrow=c(1,1))

Denver<-Denver%>%
mutate(Prediction=predict(CalgaryModel, as.data.frame(Denver), type="response"))

head(Denver)
head(Calgary)


# Predict Denver
install.packages("stats")
library(stats)

allPredictions <- 
  predict(CalgaryModel, as.data.frame(Denver), type="prob")[,2]

Inundation <- 
  cbind(Calgary,allPredictions) %>%
  mutate(allPredictions = round(allPredictions * 100)) 

ggplot() + 
  geom_sf(data=Denver, aes(fill=factor(ntile(allPredictions,5))), colour=NA) +
  scale_fill_manual(values = c("#edf8fb","#b3cde3","#8c96c6","#8856a7","#810f7c"),
                    labels=as.character(quantile(preserve$allPredictions,
                                                 c(0.1,.2,.4,.6,.8),na.rm=T)),
                    name="Predicted\nProbabilities (%)") +
  mapTheme() +
  labs(title="")
