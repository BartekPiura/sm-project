# LIBRARIES #

library(ggplot2)

library(ggcorrplot)

library(summarytools)

auto.mpg<-read.table("data/auto-mpg.data",header=TRUE)

#converting '?' to NA
auto.mpg$horsepower[auto.mpg$horsepower=='?']<- NA

#checking null values
sapply(auto.mpg, function(x) sum(is.na(x)))

#dropping rows with missing values
cars <- na.omit(auto.mpg)

#dropping records with 3 and 5 cylinders
cars<-cars[!(cars$cylinders==3 | cars$cylinders==5),]


#check the structure of the dataset
str(cars)

#horsepower should be numeric so we have to convert that
cars$horsepower <- as.numeric(cars$horsepower)

#in order to obtain colors in our plot we have to change the types to factor 
#to na razie tak zostawilem bo w zaleznosci jakie wykresy bedziemy chcieli miec to trzeba bedzie to zmienic albo nie 
cars$cylinders <- as.factor(cars$cylinders)

cars$model_year <- as.factor(cars$model_year)

cars$origin <- as.factor(cars$origin)


#create country column and fill with values based on origin country code
cars["country"]
cars$country[cars$origin == 1] <- 'USA'
cars$country[cars$origin == 2] <- 'EUROPE'
cars$country[cars$origin == 3] <- 'JAPAN'

#drop origin column because we have country column
cars <- subset(cars, select = -c(origin))

#rearrange the table
cars<-cars[c(1,2,3,4,5,6,7,9,8)]


#mpg histogram with density curve
#it is not a normal distribution
ggplot(data=cars, aes(mpg)) + geom_histogram(aes(y =..density..), 
                                             breaks=seq(0, 50, by =3), 
                                             col="green", 
                                             fill="green") + geom_density(col=2)

# histogram for mpg, color filled for cylinders
ggplot(cars, aes(mpg,fill=cylinders))+geom_histogram(binwidth=1)

# plot mpg and cylinders
ggplot(cars , aes(x=cylinders, y= mpg, color=cylinders))+geom_boxplot()

# scatter plot b/w mpg and weight for different cylinders
# car mpg shows negative correlation with its weight
# car weight is increasing with number of cylinders
ggplot(cars, aes(x=mpg, y= weight,color=cylinders))+geom_point()



# negative correlation in displacement and mpg
# displacement increases with number of cylinders (obvious)
# very high correlation b/w cylinders and displacement

ggplot(cars, aes(x=displacement,y=mpg,color=cylinders))+geom_point()


# no apparent descriptive relation between horsepower and mpg
ggplot(cars, aes(x=horsepower,y=mpg,color=cylinders)) + geom_point()


# acceleration variable is not adding considerable strenght to predictive model if included.
ggplot(cars,aes(x=acceleration,y=mpg,color=cylinders))+geom_point()



ggplot(cars, aes(x=model_year,fill=country))+geom_bar()+facet_grid(cylinders~.)


# cars with 8 cylinders had come from USA, after 1980 USA manufacture turned to manufacture 4 cylinder cars
# cars from EUROPE and JAPAN are more fuel efficient


mean_mpg_year_country <- aggregate(x=cars$mpg,by = list(country=cars$country,model_year=cars$model_year),FUN = mean)
#average mpg for every year group by country
ggplot(mean_mpg_year_country , aes(x=model_year, y= x, color=country))+geom_point()+geom_smooth(aes(group=country),method=lm,se=FALSE)+labs(y="mean mpg")

#mpg and country
#The red line marks the average of the set. From the above plot we can observe:
#Majority of the cars from USA (almost 75%) have MPG below global average.
#Majority of the cars from Japan and Europe have MPG above global average.

ggplot(cars , aes(x=country, y= mpg, color=country))+geom_boxplot()+ geom_hline(yintercept = mean(cars$mpg), color="red",linetype="dotted",size=1.5)


#mpg/weight and country BARTLOMIEJ PIURA DESIGN
ggplot(cars , aes(x=country, y= mpg/weight, color=country))+geom_boxplot()+ geom_hline(yintercept = mean(cars$mpg/cars$weigh), color="red",linetype="dotted",size=1.5)


ggplot(cars, aes(x=mpg,y=horsepower))+geom_point() + geom_smooth(se=FALSE)





#using summarytools package create table with summary statistics for our data
cars_statistics<-summarytools::descr(cars)

cars_statistics

cars_statistics<-as.numeric(cars_statistics)
x<-c(48,49,50,51,52)

print("MIN MPG: ")
print(cars[cars$mpg==cars_statistics[48],])
print(paste0("Q1 MPG: ",cars_statistics[49]))
print(paste0("MEDIAN MPG: ",cars_statistics[50]))
print(paste0("Q3 MPG: ",cars_statistics[51]))
print("MAX MPG: ")
print(cars[cars$mpg==cars_statistics[52],])

#CORRELATION MATRIX
correlation_data <- cars[, c(1,3,4,5,6)]
correlation_matrix <- cor(correlation_data)
round(correlation_matrix, 2)

#CORRELATION PLOT
ggcorrplot(correlation_matrix,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)
