
setwd("~/Desktop")
x <- read.csv(file.choose() , header = T)

y <- lm (formula = x$Alumni.Giving.Rate ~ x$Graduation.Rate , data = x)
summary(y)

#normality assumption
#normality plot
model1<-lm(x$Alumni.Giving.Rate~x$Graduation.Rate,data=x)
plot(model1)

#SW-test
shapiro.test(x$Alumni.Giving.Rate)


y <- lm (formula = x$Alumni.Giving.Rate ~ x$Student.Faculty.Ratio 
         + x$Percentage.of.Classes.U20 
         + x$Graduation.Rate , data = x)
summary(y)

#assumption check
#normality plot
model2<-lm(x$Alumni.Giving.Rate~x$Graduation.Rate
           * x$Percentage.of.Classes.U20 
           * x$Student.Faculty.Ratio,data=x)
plot(model2)


#SW-test
shapiro.test(x$Alumni.Giving.Rate)

#Correlation matrix
library("PerformanceAnalytics")
my_data <- x[, c(3:5)]
chart.Correlation(my_data , histogram = TRUE , pch = 19)
cor(my_data)

#VIF value
library("mctest")
my_data <- x[, c(3:5)]
imcdiag(x = my_data, y = x$Alumni.Giving.Rate)

