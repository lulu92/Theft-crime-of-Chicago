# case study 3

source("CrimeUtil.R")
library(ks)
library(car)

# get crime samples
theft.jan.to.mar = sample.crime("2014_THEFT.csv", -1, 1, 3)
theft.jan = theft.jan.to.mar[theft.jan.to.mar$month == 1,]
theft.feb = theft.jan.to.mar[theft.jan.to.mar$month == 2,]
theft.mar = theft.jan.to.mar[theft.jan.to.mar$month == 3,]

# read chicago boundary
city.boundary = read.shapefile("City_Boundary", "poly", "+init=epsg:3435", "+init=epsg:26971")

# set prediction resolution
prediction.resolution.meters = 200

##### train model on responses from feb data, using predictors from jan. #####

# get negative observations within chicago
non.crime.points = cbind(0, get.grid.points(city.boundary, prediction.resolution.meters, TRUE))
names(non.crime.points)[1] = "response"

# get positive observations from february within chicago
training.crime.points = cbind(1, theft.feb[,c("x","y")])
names(training.crime.points)[1] = "response"

# combine positive and negative points
training.data = rbind(non.crime.points, training.crime.points)

# calculate crime density for each training observation based on january records
theft.density = run.spatial.kde(theft.jan[,c("x","y")], training.data[,c("x","y")], 1000)

#########################################################
####### calculate distances to police stations #########
police.points = read.shapefile("PoliceStationsDec2012", "points", "+init=epsg:3435", "+init=epsg:26971")@coords
police.min.distance = get.min.distances(training.data[,c("x","y")], police.points)


# add predictor columns (theft density and police station distances) to training data
training.data1 = cbind(training.data, theft.density, police.min.distance)

# fit GLM -- don't use x- and y-coordinates as predictors.
glm.fit = glm(response ~ . -x -y, data = training.data1, family=binomial)

summary(glm.fit)
vif(glm.fit)

##### predict responses on mar data, using predictors from feb. #####

# build dataframe to predict, based on february's data
prediction.points = get.grid.points(city.boundary, prediction.resolution.meters, TRUE)
theft.density = run.spatial.kde(theft.feb[,c("x","y")], prediction.points, max.sample.size=1000)
police.min.distance = get.min.distances(prediction.points, police.points)
prediction.data = as.data.frame(cbind(prediction.points, theft.density, police.min.distance))

# run prediction
threats = predict(glm.fit, prediction.data, type="response")

# build prediction dataframe
theft.prediction = cbind(prediction.points, threats)
names(theft.prediction) = c("x", "y", "threat")

# evaluate prediction on march crime records
plot.surveillance.curve(theft.prediction, theft.mar[,c("x","y")], prediction.resolution.meters)

###########################################################


# calculate distances to bus stop
bus.points = read.shapefile("CTA_BusTurnarounds2", "points", "+init=epsg:3435", "+init=epsg:26971")@coords

bus.min.distance = get.min.distances(training.data[,c("x","y")], bus.points)

# calculate crime density for each training observation based on january records
theft.density = run.spatial.kde(theft.jan[,c("x","y")], training.data[,c("x","y")], 1000)

# add predictor columns (theft density and bus distances) to training data
training.data2 = cbind(training.data, theft.density, bus.min.distance)

# fit GLM -- don't use x- and y-coordinates as predictors.
glm.fit2 = glm(response ~ . -x -y, data = training.data2, family=binomial)

summary(glm.fit2)
vif(glm.fit2)
##### predict responses on mar data, using predictors from feb. #####

# build dataframe to predict, based on february's data
prediction.points = get.grid.points(city.boundary, prediction.resolution.meters, TRUE)
theft.density = run.spatial.kde(theft.feb[,c("x","y")], prediction.points, max.sample.size=1000)
bus.min.distance = get.min.distances(prediction.points, bus.points)
prediction.data = as.data.frame(cbind(prediction.points, theft.density, bus.min.distance))

# run prediction
threats = predict(glm.fit2, prediction.data, type="response")

# build prediction dataframe
theft.prediction = cbind(prediction.points, threats)
names(theft.prediction) = c("x", "y", "threat")

# evaluate prediction on march crime records
plot.surveillance.curve(theft.prediction, theft.mar[,c("x","y")], prediction.resolution.meters)

########################################################

# calculate distances to mall and plaza
plaza = read.shapefile("DATA_ADMIN_OPNSP_MALL_PLAZA", "poly", "+init=epsg:3435", "+init=epsg:26971")

plot(plaza)
points(coordinates(plaza))
plaza.points = coordinates(plaza)

plaza.min.distance = get.min.distances(training.data[,c("x","y")], plaza.points)

# calculate crime density for each training observation based on january records
theft.density = run.spatial.kde(theft.jan[,c("x","y")], training.data[,c("x","y")], 1000)


# add predictor columns (theft density and plaza distances) to training data
training.data3 = cbind(training.data, theft.density, plaza.min.distance)

# fit GLM -- don't use x- and y-coordinates as predictors.
glm.fit3 = glm(response ~ . -x -y, data = training.data3, family=binomial)
summary(glm.fit3)
vif(glm.fit3)
##### predict responses on mar data, using predictors from feb. #####

# build dataframe to predict, based on february's data
prediction.points = get.grid.points(city.boundary, prediction.resolution.meters, TRUE)
theft.density = run.spatial.kde(theft.feb[,c("x","y")], prediction.points, max.sample.size=1000)
plaza.min.distance = get.min.distances(prediction.points, plaza.points)
prediction.data = as.data.frame(cbind(prediction.points, theft.density, plaza.min.distance))

# run prediction
threats = predict(glm.fit3, prediction.data, type="response")

# build prediction dataframe
theft.prediction = cbind(prediction.points, threats)
names(theft.prediction) = c("x", "y", "threat")

# evaluate prediction on march crime records
plot.surveillance.curve(theft.prediction, theft.mar[,c("x","y")], prediction.resolution.meters)

################################################################
police.min.distance = get.min.distances(training.data[,c("x","y")], police.points)
bus.min.distance = get.min.distances(training.data[,c("x","y")], bus.points)
plaza.min.distance = get.min.distances(training.data[,c("x","y")], plaza.points)

# calculate crime density for each training observation based on january records
theft.density = run.spatial.kde(theft.jan[,c("x","y")], training.data[,c("x","y")], 1000)



# add predictor columns (theft density and plaza distances) to training data
training.data4 = cbind(training.data, theft.density, police.min.distance, bus.min.distance, plaza.min.distance)

# fit GLM -- don't use x- and y-coordinates as predictors.
glm.fit4 = glm(response ~ . -x -y, data = training.data4, family=binomial)

summary(glm.fit4)
vif(glm.fit4)

##### predict responses on mar data, using predictors from feb. #####

# build dataframe to predict, based on february's data
prediction.points = get.grid.points(city.boundary, prediction.resolution.meters, TRUE)
theft.density = run.spatial.kde(theft.feb[,c("x","y")], prediction.points, max.sample.size=1000)
police.min.distance = get.min.distances(prediction.points, police.points)
bus.min.distance = get.min.distances(prediction.points, bus.points)
plaza.min.distance = get.min.distances(prediction.points, plaza.points)

prediction.data = as.data.frame(cbind(prediction.points, theft.density, police.min.distance, bus.min.distance, plaza.min.distance))

# run prediction
threats = predict(glm.fit4, prediction.data, type="response")

# build prediction dataframe
theft.prediction = cbind(prediction.points, threats)
names(theft.prediction) = c("x", "y", "threat")

# evaluate prediction on march crime records
plot.surveillance.curve(theft.prediction, theft.mar[,c("x","y")], prediction.resolution.meters)



