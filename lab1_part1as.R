EPI_data <- read.csv(file.choose(), header = T)

#or
#EPI_data <- read.xlsx(”<path>/2010EPI_data.xlsx")
# Note: replace default data frame name – cannot start with numbers!
View(EPI_data)
#
attach(EPI_data) 	# sets the ‘default’ object
fix(EPI_data)	# launches a simple data editor
EPI <- EPI_data$EPI			# prints out values EPI_data$EPI
tf <- is.na(EPI) # records True values if the value is NA
E <- EPI[!tf] # filters out NA values, new array

summary(EPI)
fivenum(EPI,na.rm=TRUE)
help(stem)#help
stem(EPI) #help
hist(EPI)
hist(EPI, seq(90 30.,95.,1.0),prob=TRUE)
help(lines)
lines(density(EPI,na.rm=TRUE,bw=1.))
rug(EPI)

plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI);qqline(EPI)

qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)


boxplot(EPI,DALY)
qqplot(EPI,DALY)

boxplot(EPI,ENVHEALTH,ECOSYSTEM,DALY,AIR_H,WATER_H,BIODIVERSITY)
qqplot(EPI,ENVHEALTH)
qqplot(EPI,ECOSYSTEM)
qqplot(EPI,AIR_H)
qqplot(EPI,WATER_H)
qqplot(EPI,BIODIVERSITY)

help("distributions")

EPILand <- EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)
lines(density(Eland,na.rm=TRUE,bw=1.))
plot(ecdf(Eland), do.points=FALSE, verticals=TRUE)
qqnorm(Eland);qqline(Eland)
boxplot(Eland,No_surface_water,Desert,High_Population_Density)
qqplot(Eland, No_surface_water)
qqplot(Eland,Desert)
qqplot(Eland,High_Population_Density)

#filtering

#EPI regions
South_Asia <- EPI_data[EPI_data$EPI_regions == "South Asia",]
Europe <- EPI_data[EPI_data$EPI_regions == "Europe",]

#GEO subregion
Western <- Europe[Europe$GEO_subregion == "Western Europe",]
Central <- Europe[Europe$GEO_subregion == "Central Europe",]

#other data
GRUMP_data <- read.csv(file.choose(), header = T)
View(GRUMP_data)
Res<- GRUMP_data$Resolution
RESO <- is.na(Res)
R <- Res[!RESO]

summary(Res)
fivenum(Res,na.rm=TRUE) 
r <- hist(Res)
print(r)
hist(Res, seq(0,500,100),prob=TRUE)
lines(density(Res,na.rm=TRUE,bw=1.5))

hist(Res, breaks = 100, xlim = c(0, 600))
hist(Res, probability = TRUE)
hist(Res, breaks = 100)
hist(Res, seq(0,500,100),prob=TRUE)

plot(ecdf(Res), do.points=FALSE, verticals=TRUE)
qqnorm(Res);qqline(Res)

Pop <-GRUMP_data$PopulationPerUnit


boxplot(Res,Pop)
qqplot(Res,Pop)

Asia <- GRUMP_data[GRUMP_data$ContinentName == "Asia",]
Africa <- GRUMP_data[GRUMP_data$ContinentName == "Africa",]

#part 2

plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 
plot(ecdf(EPI_data$EPI),do.points=TRUE,verticals = TRUE) # points are visible on the plot.
par(pty="s")
qqnorm(EPI);qqline(EPI)

x <- rt(250, df = 5)
qqnorm(x); qqline(x)

qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t dsn")
qqline(x)
help("qqnorm") # read the RStudio documentation for qqnorm 
help("qqplot") # read the RStudio documentation for qqplot 
qqnorm(EPI_data$EPI)
qqline(EPI_data$EPI) # adding the line on the Q-Q plot
x <- seq(30,95,1)
x
x2 <-seq(30,95,2)
x2
x2 <-seq(30,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)
 

#done in part 1

multivariate <- read.csv(file.choose(), header = T)
head(multivariate)
attach(multivariate)
help(lm)
mm <-lm(Homeowners~Immigrants) #HELP; shorten data 
#got different intercept: 96496.08, coefficient: -7045.249
#estimated regression equaton: Homeowner = 96496.08 +(-7045.249)*Immigrants
summary(mm)$coef

plot(Homeowners~Immigrants)
help(abline)
abline(mm,col=2,lwd=3)

#immigrant value =0: Homeowners = 96496.08 - 7045.249*0= 96496.08
#immigrant value = 20: Homeowners = 9649.6 -7045.249*20 = -44408.98

newimmigrantdata <- data.frame(Immigrant=c(0,20))
mm %>% predict(newimmigrantdata)

abline(mm)
abline(mm,col=3,lwd=3)
attributes(mm)
mm$coefficients

#Creating Plots 
#Chapter 2 - R Graphics Cookbook
plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data=mtcars)
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()
plot(pressure$temperature,pressure$pressure, type ="l")
points(pressure$temperature, pressure$pressure)

lines(pressure$temperature, pressure$pressure/2, col="red")
points(pressure$temperature, pressure$pressure/2, col = "blue")
qplot(pressure$temperature, pressure$pressure, geom = "line")
qplot(temperature, pressure, data = pressure, geom = "line")
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line() + geom_point()

#creating bar graphs 
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl)) #generate a table of counts
qplot(mtcars$cyl) #treat cyl as discrete
#bar graph of counts 
qplot(factor(cyl),data=mtcars)
ggplot(mtcars,aes(x=factor(cyl)))+geom_bar()

#Creating Histogram 
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10)
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)
qplot(mpg, data=mtcars, binwidth=4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth=4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth=5)

#Creating Box-plot
plot(ToothGrowth$supp, ToothGrowth$len)

#formula syntax
boxplot(len~supp, data = ToothGrowth)
#can combine two variables on x axis 
boxplot(len~supp+dose, data = ToothGrowth)
qplot(ToothGrowth$supp, ToothGrowth$len, geom="boxplot")
qplot(supp,len,data=ToothGrowth, geom = "boxplot")
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom ="boxplot")       
qplot(interaction(supp,dose), len, data=ToothGrowth, geom = "boxplot")       
ggplot(ToothGrowth, aes(x=interaction(supp,dose), y=len))+ geom_boxplot()
