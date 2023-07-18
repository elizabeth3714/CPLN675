library(tidyverse)
library(sf)
library(raster)
library(knitr)
library(kableExtra)
library(tidycensus)
library(tigris)
library(FNN)
library(QuantPsyc)
library(caret)
library(yardstick)
library(pscl)
library(plotROC) 
library(ggrepel)
library(pROC)
library(grid)
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

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

palette2 <- c("#41b6c4","#253494")
palette4 <- c("#a1dab4","#41b6c4","#2c7fb8","#253494")
palette5 <- c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494")
palette10 <- c("#f7fcf0","#e0f3db","#ccebc5","#a8ddb5","#7bccc4",
               "#4eb3d3","#2b8cbe","#0868ac","#084081","#f7fcf0")

#this function converts a column in to quintiles. It is used for mapping.
quintileBreaks <- function(df,variable) {
  as.character(quantile(df[[variable]],
                        c(.01,.2,.4,.6,.8),na.rm=T))
}

#This function can be used to convert a polygon sf to centroids xy coords.
xyC <- function(aPolygonSF) {
  as.data.frame(
    cbind(x=st_coordinates(st_centroid(aPolygonSF))[,1],
          y=st_coordinates(st_centroid(aPolygonSF))[,2]))
} 

#this function convert a raster to a data frame so it can be plotted in ggplot
rast <- function(inRaster) {
  data.frame(
    xyFromCell(inRaster, 1:ncell(inRaster)), 
    value = getValues(inRaster)) }



DVR <- 
  st_read("DVR_boundaryproj.shp") %>%
  st_transform(102729)

lc_change = raster("lc_chang_1kproj.tif")

reclassMatrix <- 
  matrix(c(
    0,0,0,
    1,1,1,
    1,Inf,0),
    ncol=3, byrow=T)

reclassMatrix


lc_change2 <- 
  reclassify(lc_change,reclassMatrix)

lc_change2[lc_change2 < 1] <- NA

names(lc_change2) <- "lc_change"

ggplot() +
  geom_sf(data=DVR) +
  geom_raster(data=rast(lc_change2) %>% na.omit, 
              aes(x,y,fill=as.factor(value))) +
  scale_fill_viridis(discrete=TRUE, name ="Land Cover\nChange") + 
  labs(title="Development land use change") +
  mapTheme()


ggplot() +
  geom_sf(data=DVR) +
  geom_raster(data=rast(lc_change) %>% na.omit, 
              aes(x,y,fill=as.factor(value))) +
  scale_fill_viridis(discrete=TRUE, name ="Land Cover\nChange") + 
  labs(title="Development land use change") +
  mapTheme()



DVR_fishnet <- 
  st_make_grid(DVR, 4000) %>%
  st_sf()

DVR_fishnet <-
  DVR_fishnet[DVR,]

ggplot() +
  geom_sf(data=DVR_fishnet) +
  labs(title="Fishnet, 3000 foot resolution") +
  mapTheme()

changePoints <-
  rasterToPoints(lc_change2) %>%
  as.data.frame() %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(DVR_fishnet))




ggplot() +
  geom_sf(data=changePoints) +
  labs(title="Fishnet, 3000 foot resolution") +
  mapTheme()


st_crs(changePoints) == st_crs(DVR_fishnet)

ggplot() +
  geom_sf(data=DVR_fishnet, fill="black", colour=NA) +
  geom_sf(data=filter(changePoints, resam1kproj == 1), colour = "white")

head(DVR_fishnet)


fishnet <- 
  aggregate(changePoints, DVR_fishnet, sum) %>%
  mutate(lc_change = ifelse(is.na(lc_chang),0,1),
         lc_change = replace_na("NA",0),
         lc_change = as.factor(lc_chang))

fishnet <- 
  aggregate(changePoints, DVR_fishnet, sum) %>%
  mutate(lc_change = ifelse(is.na(lc_change),0,1),
         lc_change = as.factor(lc_change))

plot(fishnet)
str(fishnet)


na.omit(fishnet, lc_change) %>% 
ggplot() +
  geom_sf(data=DVR) +
  geom_point(data=fishnet, 
             aes(x=xyC(fishnet)$x, y=xyC(fishnet)$y, colour=lc_change)) +
    scale_colour_manual(values = palette2,
                      labels=c("No Change","New Development"),
                      name = "") +
  labs(title = "Land cover development change", subtitle = "As fishnet centroids") +
  mapTheme()

rlang::last_error()

lc_2001 <- raster("lc_2001_1kproj.tif")

ggplot() +
  geom_sf(data=DVR) +
  geom_raster(data=rast(lc_2001) %>% na.omit %>% filter(value > 0), 
              aes(x,y,fill=as.factor(value))) +
  scale_fill_viridis(discrete=TRUE, name ="") +
  labs(title = "Land Cover, 2001") +
  mapTheme()

developed <- lc_2001 == 21 | lc_2001 == 22 | lc_2001 == 23 | lc_2001 == 24
forest <- lc_2001 == 41 | lc_2001 == 42 | lc_2001 == 43 
farm <- lc_2001 == 81 | lc_2001 == 82 
wetlands <- lc_2001 == 90 | lc_2001 == 95 
otherUndeveloped <- lc_2001 == 52 | lc_2001 == 71 | lc_2001 == 31 
water <- lc_2001 == 11

names(developed) <- "developed"
names(forest) <- "forest"
names(farm) <- "farm"
names(wetlands) <- "wetlands"
names(otherUndeveloped) <- "otherUndeveloped"
names(water) <- "water"



aggregateRaster <- function(inputRasterList, theFishnet) {
  #create an empty fishnet with the same dimensions as the input fishnet
  theseFishnets <- theFishnet %>% dplyr::select()
  #for each raster in the raster list
  for (i in inputRasterList) {
    #create a variable name corresponding to the ith raster
    varName <- names(i)
    #convert raster to points as an sf
    thesePoints <-
      rasterToPoints(i) %>%
      as.data.frame() %>%
      st_as_sf(coords = c("x", "y"), crs = st_crs(theFishnet)) %>%
      filter(.[[1]] == 1)
    #aggregate to the fishnet
    thisFishnet <-
      aggregate(thesePoints, theFishnet, length) %>%
      mutate(!!varName := ifelse(is.na(.[[1]]),0,1))
    #add to the larger fishnet
    theseFishnets <- cbind(theseFishnets,thisFishnet)
  }
  #output all aggregates as one large fishnet
  return(theseFishnets)
}





theRasterList <- c(developed,forest,farm,wetlands,otherUndeveloped,water)

aggregatedRasters <-
  aggregateRaster(theRasterList, DVR_fishnet) %>%
  dplyr::select(developed,forest,farm,wetlands,otherUndeveloped,water) %>%
  mutate_if(is.numeric,as.factor)


aggregatedRasters %>%
  gather(var,value,developed:water) %>%
  st_cast("POLYGON") %>%    #just to make sure no weird geometries slipped in
  mutate(X = xyC(.)$x,
         Y = xyC(.)$y) %>%
  ggplot() +
  geom_sf(data=DVR) +
  geom_point(aes(X,Y, colour=as.factor(value))) +
  facet_wrap(~var) +
  scale_colour_manual(values = palette2, na.translate = F,
                      labels=c("Other","Land Cover"),
                      name = "") +
  labs(title = "Land cover types, 2001",
       subtitle = "As fishnet centroids") +
  mapTheme()


str(aggregatedRasters)

census_api_key("95e1ba2797ea00d4a00a86bf8364df089c47a047", install = TRUE , overwrite=TRUE)


PAPop00 <- 
  get_decennial(geography = "tract", variables = "P001001", year = 2000,
                state =42, geometry = TRUE, 
                county=c("Bucks","Montgomery",
                         "Chester","Philadelphia","Delaware"))
NJPop00 <- 
  get_decennial(geography = "tract", variables = "P001001", year = 2000,
                state =34, geometry = TRUE, 
                county=c("Mercer","Camden","Gloucester","Burlington"))  
  
DVRPop00 <- rbind(PAPop00, NJPop00)%>%
  rename(pop_2000 = value) %>%
  st_transform(st_crs(DVR_fishnet)) 

 
PAPop10 <- 
  get_decennial(geography = "tract", variables = "P001001", year = 2010,
                state =42, geometry = TRUE, 
                county=c("Bucks","Montgomery",
                         "Chester","Philadelphia","Delaware"))

NJPop10 <- 
  get_decennial(geography = "tract", variables = "P001001", year = 2010,
                state =34, geometry = TRUE, 
                county=c("Mercer","Camden","Gloucester","Burlington"))  

DVRPop10 <- rbind(PAPop10, NJPop10)%>%
  rename(pop_2010 = value) %>%
  st_transform(st_crs(DVR_fishnet)) %>%
  st_buffer(-1)



grid.arrange(
  ggplot() +
    geom_sf(data = DVRPop00, aes(fill=factor(ntile(pop_2000,5))), colour=NA) +
    scale_fill_manual(values = palette5,
                      labels=quintileBreaks(DVRPop00,"pop_2000"),
                      name="Quintile\nBreaks") +
    labs(title="Population, DVR: 2000") +
    mapTheme(),
  
  ggplot() +
    geom_sf(data = DVRPop10, aes(fill=factor(ntile(pop_2010,5))), colour=NA) +
    scale_fill_manual(values = palette5,
                      labels=quintileBreaks(DVRPop10,"pop_2010"),
                      name="Quintile\nBreaks") +
    labs(title="Population, DVR: 2010") +
    mapTheme(), ncol=2)




DVR_fishnet <-
  DVR_fishnet %>%
  rownames_to_column("fishnetID") %>% 
  mutate(fishnetID = as.numeric(fishnetID)) %>%
  dplyr::select(fishnetID)

fishnetPopulation00 <-
  st_interpolate_aw(DVRPop00["pop_2000"],DVR_fishnet, extensive=TRUE) %>%
  as.data.frame(.) %>%
  left_join(DVR_fishnet, ., by=c("fishnetID"='Group.1')) %>% 
  mutate(pop_2000 = replace_na(pop_2000,0)) %>%
  dplyr::select(pop_2000)

fishnetPopulation10 <-
  st_interpolate_aw(DVRPop10["pop_2010"],DVR_fishnet, extensive=TRUE) %>%
  as.data.frame(.) %>%
  left_join(DVR_fishnet, ., by=c("fishnetID"='Group.1')) %>% 
  mutate(pop_2010 = replace_na(pop_2010,0)) %>%
  dplyr::select(pop_2010)

fishnetPopulation <- 
  cbind(fishnetPopulation00,fishnetPopulation10) %>%
  dplyr::select(pop_2000,pop_2010) %>%
  mutate(pop_Change = pop_2010 - pop_2000)





grid.arrange(
  ggplot() +
    geom_sf(data=DVRPop10, aes(fill=factor(ntile(pop_2010,5))),colour=NA) +
    scale_fill_manual(values = palette5,
                      labels=substr(quintileBreaks(DVRPop10,"pop_2010"),1,4),
                      name="Quintile\nBreaks") +
    labs(title="Population, DVR: 2010",
         subtitle="Represented as tracts; Boundaries omitted") +
    mapTheme(),
  
  ggplot() +
    geom_sf(data=fishnetPopulation, aes(fill=factor(ntile(pop_2010,5))),colour=NA) +
    scale_fill_manual(values = palette5,
                      labels=substr(quintileBreaks(fishnetPopulation,"pop_2010"),1,4),
                      name="Quintile\nBreaks") +
    labs(title="Population, DVR: 2010",
         subtitle="Represented as fishnet gridcells; Boundaries omitted") +
    mapTheme(), ncol=2)






# highway



DVRHighways <-
  read_sf("DVR_highway2proj.shp") %>%
  st_transform(st_crs(DVR)) %>%
  st_intersection(DVR)

ggplot() +
  geom_point(data=fishnet, 
             aes(x=xyC(fishnet)[,1], y=xyC(fishnet)[,2],colour=lc_change),size=1.5) +
  geom_sf(data=DVRHighways) +
  scale_colour_manual(values = palette2,na.translate = F,
                      labels=c("No Change","New Development")) +
  labs(title = "New Development and Highways",
       subtitle = "As fishnet centroids") +
  mapTheme()



emptyRaster <- lc_change
emptyRaster[] <- NA


highway_raster <- 
  as(DVRHighways,'Spatial') %>%
  rasterize(.,emptyRaster)

plot(highway_raster)

highway_raster_distance <- distance(highway_raster)
names(highway_raster_distance) <- "distance_highways"

plot(highway_raster_distance)

highwayPoints <-
  rasterToPoints(highway_raster_distance) %>%
  as.data.frame() %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(DVR_fishnet))

plot(highwayPoints)


ggplot() +
  geom_sf(data=highwayPoints_fishnet) +
  labs(title="Fishnet, 3000 foot resolution") +
  mapTheme()

highwayPoints_fishnet <- 
  aggregate(highwayPoints, DVR_fishnet, mean) %>%
  mutate(distance_highways = ifelse(is.na(distance_highways),0,distance_highways))

ggplot() +
  geom_sf(data=DVR) +
  geom_point(data=highwayPoints_fishnet, aes(x=xyC(highwayPoints_fishnet)[,1], 
                                             y=xyC(highwayPoints_fishnet)[,2], 
                                             colour=factor(ntile(distance_highways,5))),size=1.5) +
  scale_colour_manual(values = palette5,
                      labels=substr(quintileBreaks(highwayPoints_fishnet,"distance_highways"),1,8),
                      name="Quintile\nBreaks") +
  geom_sf(data=DVRHighways, colour = "red") +
  labs(title = "Distance to Highways",
       subtitle = "As fishnet centroids; Highways visualized in red") +
  mapTheme()







nn_function <- function(measureFrom,measureTo,k) {
  #convert the sf layers to matrices
  measureFrom_Matrix <-
    as.matrix(measureFrom)
  measureTo_Matrix <-
    as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()
  
  return(output)  
}



fishnet$lagDevelopment <-
  nn_function(xyC(fishnet),
              xyC(filter(aggregatedRasters,developed==1)),
              2)


reclassMatrix2 <- 
  matrix(c(
    0,0,0,
    1,1,1,
    1,Inf,0),
    ncol=3, byrow=T)

reclassMatrix2


fishnet$lagDevelopment[fishnet$lagDevelopment < 1] <- NA



ggplot() +
  geom_sf(data=DVR) +
  geom_point(data=fishnet%>% na.omit, 
             aes(x=xyC(fishnet)[,1], y=xyC(fishnet)[,2], 
                 colour=factor(ntile(lagDevelopment,5))), size=1.5) +
  scale_colour_manual(values = palette5,
                      labels=substr(quintileBreaks(fishnet,"lagDevelopment"),1,7),
                      name="Quintile\nBreaks") +
  labs(title = "Spatial lag to 2001 development",
       subtitle = "As fishnet centroids") +
  mapTheme()




options(tigris_class = "sf")

PACounties <- 
  counties("Pennsylvania") %>%
  st_transform(st_crs(DVR)) %>%
  dplyr::select(NAME) %>%
  .[st_buffer(DVR,-1000), , op=st_intersects]

NjCounties <- 
  counties("New Jersey") %>%
  st_transform(st_crs(DVR)) %>%
  dplyr::select(NAME) %>%
  .[st_buffer(DVR,-1000), , op=st_intersects]

studyAreaCounties <- rbind(PACounties, NjCounties)
  


ggplot() +
  geom_sf(data=studyAreaCounties) +
  labs(title = "Study area counties") +
  mapTheme()

head(fishnet)

dat <- 
  cbind(
    fishnet, highwayPoints_fishnet, fishnetPopulation, aggregatedRasters) %>%
  dplyr::select(lc_change, developed, forest, farm, wetlands, otherUndeveloped, water,
                pop_2000, pop_2010, pop_Change, distance_highways,lagDevelopment) %>%
  st_join(studyAreaCounties) %>%
  mutate(developed10 = ifelse(lc_change == 1 & developed == 1, 0, developed)) %>%
  filter(water == 0) 

dat %>%
  mutate(distance_highways = replace_na(distance_highways, 0)) 

dat %>%
  mutate(lc_change = replace_na(lc_change, 0)) 

dat %>%
  dplyr::select(distance_highways,lagDevelopment,lc_change) %>%
  gather(Variable, Value, -lc_change, -geometry) %>%
    ggplot(., aes(lc_change, Value, fill=lc_change,), Variable[!is.na(Variable$lc_change),]) + 
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable) +
  scale_fill_manual(values = palette2,na.translate = F,
                    labels=c("No Change","New Development"),
                    name="") +
  labs(title="New development as a function of the countinuous variables") +
  plotTheme() 



dat %>%
  dplyr::select(pop_2000,pop_2010,pop_Change,lc_change) %>%
  gather(Variable, Value, -lc_change, -geometry) %>%
  ggplot(., aes(lc_change, Value, fill=lc_change)) + 
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable) +
  scale_fill_manual(values = palette2,na.translate = F,
                    labels=c("No Change","New Development"),
                    name="") +
  labs(title="New development as a function of factor variables") +
  plotTheme()

dat %>%
  dplyr::select(lc_change:otherUndeveloped,developed) %>%
  gather(Land_Cover_Type, Value, -lc_change, -geometry) %>%
  st_set_geometry(NULL) %>%
  group_by(lc_change, Land_Cover_Type) %>%
  summarize(n = sum(as.numeric(Value))) %>%
  ungroup() %>%
  mutate(Conversion_Rate = paste0(round(100 * n/sum(n), 2), "%")) %>%
  filter(lc_change == 1) %>%
  dplyr::select(Land_Cover_Type,Conversion_Rate) %>%
  kable() %>% kable_styling(full_width = F)



set.seed(3456)
trainIndex <- 
  createDataPartition(dat$developed, p = .50,
                      list = FALSE,
                      times = 1)
datTrain <- dat[ trainIndex,]
datTest  <- dat[-trainIndex,]

nrow(dat)

Model1 <- glm(lc_change ~ wetlands + forest  + farm + otherUndeveloped, 
              family="binomial"(link="logit"), data = datTrain)

Model2 <- glm(lc_change ~ wetlands + forest  + farm + otherUndeveloped + lagDevelopment, 
              family="binomial"(link="logit"), data = datTrain)

Model3 <- glm(lc_change ~ wetlands + forest  + farm + otherUndeveloped + lagDevelopment + pop_2000, 
              family="binomial"(link="logit"), data = datTrain)          

Model4 <- glm(lc_change ~ wetlands + forest  + farm + otherUndeveloped + lagDevelopment + pop_2000 + 
                pop_2010, 
              family="binomial"(link="logit"), data = datTrain) 

Model5 <- glm(lc_change ~ wetlands + forest  + farm + otherUndeveloped + lagDevelopment + pop_Change, 
              family="binomial"(link="logit"), data = datTrain)              

Model6 <- glm(lc_change ~ wetlands + forest  + farm + otherUndeveloped + lagDevelopment + pop_Change + 
                distance_highways, 
              family="binomial"(link="logit"), data = datTrain) 


modelList <- paste0("Model", 1:6)
map_dfc(modelList, function(x)pR2(get(x)))[4,] %>%
  setNames(paste0("Model",1:6)) %>%
  gather(Model,McFadden) %>%
  ggplot(aes(Model,McFadden)) +
  geom_bar(stat="identity") +
  labs(title= "McFadden R-Squared by Model") +
  plotTheme()

testSetProbs <- 
  data.frame(class = datTest$lc_change,
             probs = predict(Model6, datTest, type="response")) 

ggplot(testSetProbs, aes(probs)) +
  geom_density(aes(fill=class), alpha=0.5) +
  scale_fill_manual(values = palette2,na.translate = F,
                    labels=c("No Change","New Development")) +
  labs(title = "Histogram of test set predicted probabilities",
       x="Predicted Probabilities",y="Density") +
  plotTheme()

head(testSetProbs)
options(yardstick.event_first = FALSE)

testSetProbs <- 
  testSetProbs %>% 
  mutate(predClass_05 = as.factor(ifelse(testSetProbs$probs >= 0.05 ,1,0)),
         predClass_17 = as.factor(ifelse(testSetProbs$probs >= 0.17 ,1,0))) 

testSetProbs %>%
  dplyr::select(-probs) %>%
  gather(Variable, Value, -class) %>%
  group_by(Variable) %>%
  summarize(Sensitivity = round(yardstick::sens_vec(class,factor(Value)),2),
            Specificity = round(yardstick::spec_vec(class,factor(Value)),2),
            Accuracy = round(yardstick::accuracy_vec(class,factor(Value)),2)) %>% 
  kable() %>%
  kable_styling(full_width = F)

predsForMap <-         
  dat %>%
  mutate(probs = predict(Model6, dat, type="response") ,
         Threshold_5_Pct = as.factor(ifelse(probs >= 0.05 ,1,0)),
         Threshold_17_Pct =  as.factor(ifelse(probs >= 0.17 ,1,0))) %>%
  dplyr::select(lc_change,Threshold_5_Pct,Threshold_17_Pct) %>%
  gather(Variable,Value, -geometry) %>%
  st_cast("POLYGON")

ggplot() +
  geom_point(data=predsForMap, aes(x=xyC(predsForMap)[,1], y=xyC(predsForMap)[,2], colour=Value)) +
  facet_wrap(~Variable) +
  scale_colour_manual(values = palette2, labels=c("No Change","New Development"),na.translate = F,
                      name="") +
  labs(title="Development predictions - Low threshold") + mapTheme()



ConfusionMatrix.metrics <-
  dat %>%
  mutate(probs = predict(Model6, dat, type="response") ,
         Threshold_5_Pct = as.factor(ifelse(probs >= 0.05 ,1,0)),
         Threshold_17_Pct =  as.factor(ifelse(probs >= 0.17 ,1,0))) %>%
  mutate(TrueP_05 = ifelse(lc_change  == 1 & Threshold_5_Pct == 1, 1,0),
         TrueN_05 = ifelse(lc_change  == 0 & Threshold_5_Pct == 0, 1,0),
         TrueP_17 = ifelse(lc_change  == 1 & Threshold_17_Pct == 1, 1,0),
         TrueN_17 = ifelse(lc_change  == 0 & Threshold_17_Pct == 0, 1,0)) %>%
  dplyr::select(., starts_with("True")) %>%
  gather(Variable, Value, -geometry) %>%
  st_cast("POLYGON") 

ggplot(data=ConfusionMatrix.metrics) +
  geom_point(aes(x=xyC(ConfusionMatrix.metrics)[,1], 
                 y=xyC(ConfusionMatrix.metrics)[,2], colour = as.factor(Value))) +
  facet_wrap(~Variable) +
  scale_colour_manual(values = palette2, labels=c("Correct","Incorrect"),na.translate = F,
                      name="") +
  labs(title="Development predictions - Low threshold") + mapTheme()







spatialCV <- function(dataFrame, uniqueID, dependentVariable, modelName) {
  
  #initialize a data frame 
  endList <- list()
  
  #create a list that is all the spatial group unqiue ids in the data frame (ie counties)    
  uniqueID_List <- unique(dataFrame[[uniqueID]])  
  x <- 1
  y <- length(uniqueID_List)
  
  #create a counter and while it is less than the number of counties...  
  while(x <= y) 
  {
    #call a current county    
    currentUniqueID <- uniqueID_List[x]
    #create a training set comprised of units not in that county and a test set of units
    #that are that county
    training <- dataFrame[ which(dataFrame[[uniqueID]] != uniqueID_List[x]),]
    testing <- dataFrame[ which(dataFrame[[uniqueID]] == uniqueID_List[x]),]
    #create seperate xy vectors
    trainingX <- training[ , -which(names(training) %in% c(dependentVariable))]
    testingX <- testing[ , -which(names(testing) %in% c(dependentVariable))]
    
    trainY <- training[[dependentVariable]]
    testY <- testing[[dependentVariable]]
    #Calculate predictions on the test county as part of a data frame including the observed
    #outcome and the unique county ID    
    thisPrediction <- 
      data.frame(class = testY,
                 probs = predict(modelName, testingX, type="response"),
                 county = currentUniqueID) 
    
    #Row bind the predictions to a data farme
    endList <- rbind(endList, thisPrediction)
    #iterate counter    
    x <- x + 1 
  } 
  #return the final list of counties and associated predictions  
  return (as.data.frame(endList))
}



spatialCV_counties <-
  spatialCV(dat,"NAME","lc_change", Model6) %>%
  mutate(predClass = as.factor(ifelse(probs >= 0.17 ,1,0)))


spatialCV_metrics <-
  spatialCV_counties %>% 
  group_by(county) %>% 
  summarize(Observed_Change = sum(as.numeric(as.character(class))),
            Sensitivity = round(yardstick::sens_vec(class,predClass),2),
            Specificity = round(yardstick::spec_vec(class,predClass),2),
            Accuracy = round(yardstick::accuracy_vec(class,predClass),2)) 

spatialCV_metrics %>%
  kable() %>%
  kable_styling(full_width = F)





# 2020 prediction
dat <-
  dat %>%
  mutate(lagDevelopment = nn_function(xyC(.), xyC(filter(.,developed10 == 1)),2))

countyPopulation_2020 <- 
  data.frame(
    NAME = 
      c("Bucks","Montgomery","Chester","Philadelphia","Delaware",
        "Mercer","Camden","Gloucester","Burlington"),
    county_projection_2020 = 
      c(640495, 840934, 543702, 1594787, 568337, 377328 , 514006, 307766, 459344)) %>%
  left_join(dat %>%
      st_set_geometry(NULL) %>%
      group_by(NAME) %>%
      summarize(county_population_2010 = round(sum(pop_2010))))

countyPopulation_2020 %>%
  gather(Variable,Value, -NAME) %>%
  ggplot(aes(reorder(NAME,-Value),Value)) +
  geom_bar(aes(fill=Variable), stat = "identity") +
  scale_fill_manual(values = palette2,
                    labels=c("2020","2010"),
                    name="Population") +
  labs(title="Population Change by county: 2010 - 2020",
       x="County", y="Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  plotTheme()



dat_infill <-
  dat %>%
  #calculate population change
  left_join(countyPopulation_2020) %>%
  mutate(proportion_of_county_pop = pop_2010 / county_population_2010,
         pop_2020.infill = proportion_of_county_pop * county_projection_2020,
         pop_Change = round(pop_2020.infill - pop_2010),2) %>%
  dplyr::select(-county_projection_2020, -county_population_2010, 
                -proportion_of_county_pop, -pop_2020.infill) %>%
  #predict for 2020
  mutate(predict_2020.infill = predict(Model6,. , type="response"))

dat_infill %>%
  ggplot() +  
  geom_point(aes(x=xyC(dat_infill)[,1], y=xyC(dat_infill)[,2], colour = factor(ntile(predict_2020.infill,5)))) +
  scale_colour_manual(values = palette5,
                      labels=substr(quintileBreaks(dat_infill,"predict_2020.infill"),1,4),
                      name="Quintile\nBreaks") +
  geom_sf(data=studyAreaCounties, fill=NA, colour="black", size=1.5) +
  labs(title= "Development Demand in 2020: Predicted Probabilities") +
  mapTheme()









lc_2011 <- raster("lc_2011_1kproj.tif")

developed11 <- lc_2011 == 21 | lc_2011 == 22 | lc_2011 == 23 | lc_2011 == 24
forest11 <- lc_2011 == 41 | lc_2011 == 42 | lc_2011 == 43 
farm11 <- lc_2011 == 81 | lc_2011 == 82 
wetlands11 <- lc_2011 == 90 | lc_2011 == 95 
otherUndeveloped11 <- lc_2011 == 52 | lc_2011 == 71 | lc_2011 == 31 
water11 <- lc_2011 == 11

names(developed11) <- "developed11"
names(forest11) <- "forest11"
names(farm11) <- "farm11"
names(wetlands11) <- "wetlands11"
names(otherUndeveloped11) <- "otherUndeveloped11"
names(water11) <- "water11"

ggplot() +
  geom_sf(data=DVR) +
  geom_raster(data = rbind(rast(lc_2001) %>% mutate(label = "2001"),
                           rast(lc_2011) %>% mutate(label = "2011")) %>% 
                na.omit %>% filter(value > 0), 
              aes(x,y,fill=as.factor(value))) +
  facet_wrap(~label) +
  scale_fill_viridis(discrete=TRUE, name ="") +
  labs(title = "Land Cover, 2001 & 2011") +
  mapTheme() + theme(legend.position = "none")


theRasterList11 <- c(developed11,forest11,farm11,wetlands11,otherUndeveloped11,water11)

dat2 <-
  aggregateRaster(theRasterList11, dat) %>%
  dplyr::select(developed11,forest11,farm11,wetlands11,otherUndeveloped11,water11) %>%
  st_set_geometry(NULL) %>%
  bind_cols(.,dat) %>%
  st_sf() %>%
  st_cast("POLYGON")

dat2 %>%
  gather(var,value,developed11:water11) %>%
  st_centroid() %>%
  mutate(X = st_coordinates(.)[,1],
         Y = st_coordinates(.)[,2]) %>%
  ggplot() +
  geom_sf(data=DVR) +
  geom_point(aes(X,Y, colour=as.factor(value))) +
  facet_wrap(~var) +
  scale_colour_manual(values = palette2,
                      labels=c("Other","Land Cover"),
                      name = "") +
  labs(title = "Land cover types, 2011",
       subtitle = "As fishnet centroids") +
  mapTheme()


dat2 <-
  dat2 %>%
  mutate(sensitive_lost11 = ifelse(forest == 1 & forest11 == 0 |
                                     wetlands == 1 & wetlands11 == 0,1,0))

ggplot() +
  geom_point(data=dat2, aes(x=xyC(dat2)[,1], y=xyC(dat2)[,2], colour=as.factor(sensitive_lost11))) +
  scale_colour_manual(values = palette2,
                      labels=c("No Change","Sensitive Lost"),
                      name = "") +
  labs(title = "Sensitive lands lost: 2001 - 2011",
       subtitle = "As fishnet centroids") +
  mapTheme()


sensitiveRegions <- 
  clump(wetlands11 + forest11) %>%
  rasterToPolygons() %>%
  st_as_sf() %>%
  group_by(clumps) %>% summarize() %>%
  mutate(Acres = as.numeric(st_area(.) * 0.0000229568)) %>%
  filter(Acres > 3954)  %>%
  dplyr::select() %>%
  rasterize(.,emptyRaster) 
sensitiveRegions[sensitiveRegions > 0] <- 1  
names(sensitiveRegions) <- "sensitiveRegions"

dat2 <-
  aggregateRaster(c(sensitiveRegions), dat2) %>%
  dplyr::select(sensitiveRegions) %>%
  st_set_geometry(NULL) %>%
  bind_cols(.,dat2) %>%
  st_sf()

ggplot() +
  geom_point(data=dat2, aes(x=xyC(dat2)[,1], y=xyC(dat2)[,2], colour=as.factor(sensitiveRegions))) +
  scale_colour_manual(values = palette2,
                      labels=c("Other","Sensitive Regions"),
                      name="") +
  labs(title = "Sensitive regions",
       subtitle = "Continous areas of either wetlands or forests\ngreater than 1 acre") +
  mapTheme()

########### starting from here 6.4
county_specific_metrics <- 
  dat2 %>%
  #predict development demand from our model
  mutate(Development_Demand = predict(Model6, dat2, type="response")) %>%
  #get a count count of grid cells by county which we can use to calculate rates below
  left_join(st_set_geometry(dat, NULL) %>% group_by(NAME) %>% summarize(count = n())) %>%
  #calculate summary statistics by county
  group_by(NAME) %>%
  summarize(Total_Farmland = sum(farm11) / max(count),
            Total_Forest = sum(forest11) / max(count),
            Total_Wetlands = sum(wetlands11) / max(count),
            Total_Undeveloped = sum(otherUndeveloped11) / max(count),
            Sensitive_Land_Lost = sum(sensitive_lost11) / max(count),
            Sensitive_Regions = sum(sensitiveRegions) / max(count),
            Mean_Development_Demand = mean(Development_Demand)) %>%
  #get population data by county
  left_join(countyPopulation_2020 %>% 
              mutate(Population_Change = county_projection_2020 - county_population_2010,
                     Population_Change_Rate = Population_Change / county_projection_2020) %>%
              dplyr::select(NAME,Population_Change_Rate))


county_specific_metrics %>%
  gather(Variable, Value, -NAME, -geometry) %>%
  mutate(Variable = factor(Variable, levels=c("Population_Change_Rate","Mean_Development_Demand",
                                              "Total_Farmland","Total_Undeveloped","Total_Forest",
                                              "Total_Wetlands","Sensitive_Land_Lost","Sensitive_Regions",
                                              ordered = TRUE))) %>%
  mutate(Planning_Designation = case_when(
    Variable == "Population_Change_Rate" | Variable == "Mean_Development_Demand" ~ "Demand-Side",
    Variable == "Total_Farmland" | Variable == "Total_Undeveloped"               ~ "Suitable",
    TRUE                                                                         ~ "Not Suitable")) %>%
  ggplot(aes(x=Variable, y=Value, fill=Planning_Designation)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  facet_wrap(~NAME, ncol=5) +
  coord_flip() +
  scale_y_continuous(breaks = seq(.25, 1, by = .25)) +
  geom_vline(xintercept = 2.5) + geom_vline(xintercept = 4.5) +
  scale_fill_manual(values=c("black","red","darkgreen")) +
  labs(title= "County Specific Allocation Metrics", subtitle= "As rates", x="Indicator", y="Rate") +
  plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="bottom")





Philadelphia <-
  dat2 %>%
  mutate(Development_Demand = predict(Model6, dat2, type="response")) %>%
  filter(NAME == "Philadelphia") 

Philadelphia_landUse <- rbind(
  filter(Philadelphia, forest11 == 1 | wetlands11 == 1 ) %>%
    dplyr::select() %>% mutate(Land_Use = "Not Suitable"),
  filter(Philadelphia, developed11 == 1) %>%
    dplyr::select() %>% mutate(Land_Use = "Developed"))

grid.arrange(
  ggplot() +
    geom_sf(data=Philadelphia, aes(fill=factor(ntile(Development_Demand,5))), colour=NA) +
    geom_point(data=Philadelphia_landUse, aes(x=xyC(Philadelphia_landUse)[,1], 
                                          y=xyC(Philadelphia_landUse)[,2], colour=Land_Use),
               shape = 15, size = 2) +
    geom_sf(data=st_intersection(DVRHighways,filter(studyAreaCounties, NAME=="Philadelphia")), size=2) +
    scale_fill_manual(values = palette5, name="Development\nDemand",
                      labels=substr(quintileBreaks(Philadelphia,"Development_Demand"),1,5)) +
    scale_colour_manual(values = c("black","red")) + 
    labs(title = "Development Potential, 2020: Philadelphia") + mapTheme() +
    guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2)),
  
  ggplot() +
    geom_sf(data=Philadelphia, aes(fill=factor(ntile(pop_Change,5))), colour=NA) +
    geom_point(data=Philadelphia_landUse, aes(x=xyC(Philadelphia_landUse)[,1], 
                                          y=xyC(Philadelphia_landUse)[,2], colour=Land_Use),
               shape = 15, size = 2) +
    geom_sf(data=st_intersection(DVRHighways,filter(studyAreaCounties, NAME=="Philadelphia")), size=2) +
    scale_fill_manual(values = palette5, name="Population\nChange",
                      labels=substr(quintileBreaks(Philadelphia,"pop_Change"),1,5)) +
    scale_colour_manual(values = c("black","red")) + 
    labs(title = "Projected Population, 2020: Philadelphia") + mapTheme() +
    guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2)), ncol=2)





















#2020demand
# new highway
NEWHighways <-
  read_sf("new_highways.shp") %>%
  st_transform(st_crs(DVR)) %>%
  st_intersection(DVR)
plot(NEWHighways)
ggplot() +
  geom_point(data=fishnet, 
             aes(x=xyC(fishnet)[,1], y=xyC(fishnet)[,2],colour=lc_change),size=1.5) +
  geom_sf(data=NEWHighways) +
  scale_colour_manual(values = palette2,na.translate = F,
                      labels=c("No Change","New Development")) +
  labs(title = "New Development and Highways",
       subtitle = "As fishnet centroids") +
  mapTheme()



emptyRaster2 <- lc_change
emptyRaster2[] <- NA


NEWhighway_raster <- 
  as(NEWHighways,'Spatial') %>%
  rasterize(.,emptyRaster)

NEWhighway_raster_distance <- distance(NEWhighway_raster)
names(NEWhighway_raster_distance) <- "new_distance_highways"

NEWhighwayPoints <-
  rasterToPoints(NEWhighway_raster_distance) %>%
  as.data.frame() %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(DVR_fishnet))


NEWhighwayPoints_fishnet <- 
  aggregate(NEWhighwayPoints, DVR_fishnet, mean) %>%
  mutate(new_distance_highways = ifelse(is.na(new_distance_highways),0,new_distance_highways))

ggplot() +
  geom_sf(data=DVR) +
  geom_point(data=NEWhighwayPoints_fishnet, aes(x=xyC(NEWhighwayPoints_fishnet)[,1], 
                                             y=xyC(NEWhighwayPoints_fishnet)[,2], 
                                             colour=factor(ntile(new_distance_highways,5))),size=1.5) +
  scale_colour_manual(values = palette5,
                      labels=substr(quintileBreaks(NEWhighwayPoints_fishnet,"new_distance_highways"),1,8),
                      name="Quintile\nBreaks") +
  geom_sf(data=NEWHighways, colour = "red") +
  labs(title = "Distance to NEW Highways",
       subtitle = "As fishnet centroids; Highways visualized in red") +
  mapTheme()








# 2020 with new highway

dat2 <- 
  cbind(
    fishnet, NEWhighwayPoints_fishnet, fishnetPopulation, aggregatedRasters) %>%
  dplyr::select(lc_change, developed, forest, farm, wetlands, otherUndeveloped, water,
                pop_2000, pop_2010, pop_Change, new_distance_highways,lagDevelopment) %>%
  st_join(studyAreaCounties) %>%
  mutate(developed10 = ifelse(lc_change == 1 & developed == 1, 0, developed)) %>%
  filter(water == 0) 

dat2 %>%
  mutate(new_distance_highways = replace_na(new_distance_highways, 0)) 

dat2 %>%
  mutate(lc_change = replace_na(lc_change, 0)) 

dat2 %>%
  dplyr::select(new_distance_highways,lagDevelopment,lc_change) %>%
  gather(Variable, Value, -lc_change, -geometry) %>%
  ggplot(., aes(lc_change, Value, fill=lc_change,), Variable[!is.na(Variable$lc_change),]) + 
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable) +
  scale_fill_manual(values = palette2,na.translate = F,
                    labels=c("No Change","New Development"),
                    name="") +
  labs(title="New development as a function of the countinuous variables") +
  plotTheme() 



dat2 %>%
  dplyr::select(pop_2000,pop_2010,pop_Change,lc_change) %>%
  gather(Variable, Value, -lc_change, -geometry) %>%
  ggplot(., aes(lc_change, Value, fill=lc_change)) + 
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable) +
  scale_fill_manual(values = palette2,na.translate = F,
                    labels=c("No Change","New Development"),
                    name="") +
  labs(title="New development as a function of factor variables") +
  plotTheme()

dat2 %>%
  dplyr::select(lc_change:otherUndeveloped,developed) %>%
  gather(Land_Cover_Type, Value, -lc_change, -geometry) %>%
  st_set_geometry(NULL) %>%
  group_by(lc_change, Land_Cover_Type) %>%
  summarize(n = sum(as.numeric(Value))) %>%
  ungroup() %>%
  mutate(Conversion_Rate = paste0(round(100 * n/sum(n), 2), "%")) %>%
  filter(lc_change == 1) %>%
  dplyr::select(Land_Cover_Type,Conversion_Rate) %>%
  kable() %>% kable_styling(full_width = F)

set.seed(3456)
trainIndex2 <- 
  createDataPartition(dat2$developed, p = .50,
                      list = FALSE,
                      times = 1)
datTrain2 <- dat2[ trainIndex2,]
datTest2  <- dat2[-trainIndex2,]

nrow(dat2)

Model11 <- glm(lc_change ~ wetlands + forest  + farm + otherUndeveloped, 
              family="binomial"(link="logit"), data = datTrain2)

Model12 <- glm(lc_change ~ wetlands + forest  + farm + otherUndeveloped + lagDevelopment, 
              family="binomial"(link="logit"), data = datTrain2)

Model13 <- glm(lc_change ~ wetlands + forest  + farm + otherUndeveloped + lagDevelopment + pop_2000, 
              family="binomial"(link="logit"), data = datTrain2)          

Model14 <- glm(lc_change ~ wetlands + forest  + farm + otherUndeveloped + lagDevelopment + pop_2000 + 
                pop_2010, 
              family="binomial"(link="logit"), data = datTrain2) 

Model15 <- glm(lc_change ~ wetlands + forest  + farm + otherUndeveloped + lagDevelopment + pop_Change, 
              family="binomial"(link="logit"), data = datTrain2)              

 
Model16 <- glm(lc_change ~ wetlands + forest  + farm + otherUndeveloped + lagDevelopment + pop_Change + 
                new_distance_highways, 
              family="binomial"(link="logit"), data = datTrain2) 


modelList <- paste0("Model", 11:16)
map_dfc(modelList, function(x)pR2(get(x)))[4,] %>%
  setNames(paste0("Model",11:16)) %>%
  gather(Model,McFadden) %>%
  ggplot(aes(Model,McFadden)) +
  geom_bar(stat="identity") +
  labs(title= "McFadden R-Squared by Model") +
  plotTheme()

testSetProbs2 <- 
  data.frame(class2 = datTest2$lc_change,
             probs2 = predict(Model16, datTest2, type="response")) 

ggplot(testSetProbs2, aes(probs2)) +
  geom_density(aes(fill=class2), alpha=0.5) +
  scale_fill_manual(values = palette2,na.translate = F,
                    labels=c("No Change","New Development")) +
  labs(title = "Histogram of test set predicted probabilities",
       x="Predicted Probabilities",y="Density") +
  plotTheme()


options(yardstick.event_first = FALSE)

testSetProbs2 <- 
  testSetProbs2 %>% 
  mutate(predClass2_05 = as.factor(ifelse(testSetProbs2$probs2 >= 0.05 ,1,0)),
         predClass2_17 = as.factor(ifelse(testSetProbs2$probs2 >= 0.17 ,1,0))) 

testSetProbs2 %>%
  dplyr::select(-probs2) %>%
  gather(Variable, Value, -class2) %>%
  group_by(Variable) %>%
  summarize(Sensitivity = round(yardstick::sens_vec(class2,factor(Value)),2),
            Specificity = round(yardstick::spec_vec(class2,factor(Value)),2),
            Accuracy = round(yardstick::accuracy_vec(class2,factor(Value)),2)) %>% 
  kable() %>%
  kable_styling(full_width = F)

predsForMap2 <-         
  dat2 %>%
  mutate(probs2 = predict(Model16, dat2, type="response") ,
         Threshold_5_Pct = as.factor(ifelse(probs2 >= 0.05 ,1,0)),
         Threshold_17_Pct =  as.factor(ifelse(probs2 >= 0.17 ,1,0))) %>%
  dplyr::select(lc_change,Threshold_5_Pct,Threshold_17_Pct) %>%
  gather(Variable,Value, -geometry) %>%
  st_cast("POLYGON")

ggplot() +
  geom_point(data=predsForMap2, aes(x=xyC(predsForMap2)[,1], y=xyC(predsForMap2)[,2], colour=Value)) +
  facet_wrap(~Variable) +
  scale_colour_manual(values = palette2, labels=c("No Change","New Development"),na.translate = F,
                      name="") +
  labs(title="New Development predictions - Low threshold") + mapTheme()




ConfusionMatrix.metrics2 <-
  dat2 %>%
  mutate(probs2 = predict(Model16, dat2, type="response") ,
         Threshold_5_Pct = as.factor(ifelse(probs2 >= 0.05 ,1,0)),
         Threshold_17_Pct =  as.factor(ifelse(probs2 >= 0.17 ,1,0))) %>%
  mutate(TrueP_05 = ifelse(lc_change  == 1 & Threshold_5_Pct == 1, 1,0),
         TrueN_05 = ifelse(lc_change  == 0 & Threshold_5_Pct == 0, 1,0),
         TrueP_17 = ifelse(lc_change  == 1 & Threshold_17_Pct == 1, 1,0),
         TrueN_17 = ifelse(lc_change  == 0 & Threshold_17_Pct == 0, 1,0)) %>%
  dplyr::select(., starts_with("True")) %>%
  gather(Variable, Value, -geometry) %>%
  st_cast("POLYGON") 

ggplot(data=ConfusionMatrix.metrics2) +
  geom_point(aes(x=xyC(ConfusionMatrix.metrics2)[,1], 
                 y=xyC(ConfusionMatrix.metrics2)[,2], colour = as.factor(Value))) +
  facet_wrap(~Variable) +
  scale_colour_manual(values = palette2, labels=c("Correct","Incorrect"),na.translate = F,
                      name="") +
  labs(title="Development predictions - Low threshold") + mapTheme()

spatialCV <- function(dataFrame, uniqueID, dependentVariable, modelName) {
  
  #initialize a data frame 
  endList <- list()
  
  #create a list that is all the spatial group unqiue ids in the data frame (ie counties)    
  uniqueID_List <- unique(dataFrame[[uniqueID]])  
  x <- 1
  y <- length(uniqueID_List)
  
  #create a counter and while it is less than the number of counties...  
  while(x <= y) 
  {
    #call a current county    
    currentUniqueID <- uniqueID_List[x]
    #create a training set comprised of units not in that county and a test set of units
    #that are that county
    training2 <- dataFrame[ which(dataFrame[[uniqueID]] != uniqueID_List[x]),]
    testing2 <- dataFrame[ which(dataFrame[[uniqueID]] == uniqueID_List[x]),]
    #create seperate xy vectors
    trainingX2 <- training2[ , -which(names(training) %in% c(dependentVariable))]
    testingX2 <- testing2[ , -which(names(testing) %in% c(dependentVariable))]
    
    trainY2 <- training2[[dependentVariable]]
    testY2 <- testing2[[dependentVariable]]
    #Calculate predictions on the test county as part of a data frame including the observed
    #outcome and the unique county ID    
    thisPrediction2 <- 
      data.frame(class = testY,
                 probs = predict(modelName, testingX2, type="response"),
                 county = currentUniqueID) 
    
    #Row bind the predictions to a data farme
    endList2 <- rbind(endList2, thisPrediction2)
    #iterate counter    
    x <- x + 1 
  } 
  #return the final list of counties and associated predictions  
  return (as.data.frame(endList))
}



spatialCV_counties2 <-
  spatialCV(dat2,"NAME","lc_change", Model16) %>%
  mutate(predClass2 = as.factor(ifelse(probs2 >= 0.17 ,1,0)))


spatialCV_metrics2 <-
  spatialCV_counties2 %>% 
  group_by(county) %>% 
  summarize(Observed_Change = sum(as.numeric(as.character(class))),
            Sensitivity = round(yardstick::sens_vec(class,predClass2),2),
            Specificity = round(yardstick::spec_vec(class,predClass2),2),
            Accuracy = round(yardstick::accuracy_vec(class,predClass2),2)) 

spatialCV_metrics2 %>%
  kable() %>%
  kable_styling(full_width = F)






dat2 <-
  dat2 %>%
  mutate(lagDevelopment = nn_function(xyC(.), xyC(filter(.,developed10 == 1)),2))

countyPopulation_2020 <- 
  data.frame(
    NAME = 
      c("Bucks","Montgomery","Chester","Philadelphia","Delaware",
        "Mercer","Camden","Gloucester","Burlington"),
    county_projection_2020 = 
      c(640495, 840934, 543702, 1594787, 568337, 377328 , 514006, 307766, 459344)) %>%
  left_join(
    dat %>%
      st_set_geometry(NULL) %>%
      group_by(NAME) %>%
      summarize(county_population_2010 = round(sum(pop_2010))))

countyPopulation_2020 %>%
  gather(Variable,Value, -NAME) %>%
  ggplot(aes(reorder(NAME,-Value),Value)) +
  geom_bar(aes(fill=Variable), stat = "identity") +
  scale_fill_manual(values = palette2,
                    labels=c("2020","2010"),
                    name="Population") +
  labs(title="Population Change by county: 2010 - 2020",
       x="County", y="Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  plotTheme()



dat_infill2 <-
  dat2 %>%
  #calculate population change
  left_join(countyPopulation_2020) %>%
  mutate(proportion_of_county_pop = pop_2010 / county_population_2010,
         pop_2020.infill = proportion_of_county_pop * county_projection_2020,
         pop_Change = round(pop_2020.infill - pop_2010),2) %>%
  dplyr::select(-county_projection_2020, -county_population_2010, 
                -proportion_of_county_pop, -pop_2020.infill) %>%
  #predict for 2020
  mutate(predict_2020.infill = predict(Model16,. , type="response"))
#model6 not model16
dat_infill2 %>%
  ggplot() +  
  geom_point(aes(x=xyC(dat_infill2)[,1], y=xyC(dat_infill2)[,2], colour = factor(ntile(predict_2020.infill,5)))) +
  scale_colour_manual(values = palette5,
                      labels=substr(quintileBreaks(dat_infill2,"predict_2020.infill"),1,4),
                      name="Quintile\nBreaks") +
  geom_sf(data=studyAreaCounties, fill=NA, colour="black", size=1.5) +
  labs(title= "Development Demand in 2020: Predicted Probabilities") +
  mapTheme()

