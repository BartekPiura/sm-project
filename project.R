# LIBRARIES #

library(ggplot2)

library(summarytools)

#converting '?' to NA
auto.mpg$horsepower[auto.mpg$horsepower=='?']<- NA

#checking null values
sapply(auto.mpg, function(x) sum(is.na(x)))

#dropping rows with missing values
cars <- na.omit(auto.mpg)

#check the structure of the dataset
str(cars)

#horsepower should be numeric so we have to convert that
cars$horsepower <- as.numeric(cars$horsepower)

#in order to obtain colors in our plot we have to change the type to factor 
#tu na razie to zakomentowalem bo wtedy summarytools nie robi statystyk dla wszystkich kolumn 
cars$cylinders <- as.factor(cars$cylinders)

cars$model_year <- as.factor(cars$model_year)

cars$origin <- as.factor(cars$origin)


#create country column and fill with values based on origin country code
cars["country"]
cars$country[cars$origin == 1] <- 'USA'
cars$country[cars$origin == 2] <- 'EUROPE'
cars$country[cars$origin == 3] <- 'JAPAN'

#rearrange the table
cars<-cars[c(1,2,3,4,5,6,7,8,10,9)]

#using summarytools package create table with summary statistics for our data
cars_statistics<-summarytools::descr(cars)

#mpg histogram with density curve
ggplot(data=cars, aes(mpg)) + geom_histogram(aes(y =..density..), 
                                             breaks=seq(0, 50, by =3), 
                                             col="green", 
                                             fill="green") + geom_density(col=2)

# histogram for mpg, color filled for cylinders
ggplot(cars, aes(mpg,fill=cylinders))+geom_histogram(binwidth=1)


# scatter plot b/w mpg and weight for different cylinders
ggplot(cars, aes(x=mpg, y= weight))+geom_point()+facet_grid(cylinders~.)

# car mpg shows negative correlation with its weight
# car weight is increasing with number of cylinders



