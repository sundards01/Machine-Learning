# Exploratory Data Analysis

mydata <- wbcd[,3:32] # remove forst 2 columns for EDA
View(mydata)
View(wbcd)

# First Business moment

mean(mydata$radius_mean) # calculate mean
median(mydata$radius_mean) # calculate median
temp<-table(as.vector(mydata$radius_mean)) # Calculate mode
temp
names(temp)[temp == max(temp)]

# Second Business Moment

var(mydata$radius_mean) # calculate Variance
sd(mydata$radius_mean)  #calculate Standard deviation
x<- max(mydata$radius_mean) # calcualte range
x
y<-min(mydata$radius_mean)
y
r<- x-y
r

# Third Business Moment

library(e1071) # for skewness & Kurtosis

skewness(mydata$radius_mean) # Skewness

# Fourth Business Moment

kurtosis(mydata) # kurtosis

summary(mydata)

# Various Graphical representation

plot(mydata$radius_mean)
boxplot(mydata$radius_mean , main = "radius_mean") # identified outliers
boxplot(mydata$texture_mean ,main = "texture_mean") # identified outliers
boxplot(mydata$perimeter_mean , main = "perimeter_mean") # identified outliers
boxplot(mydata$area_mean , main = "area_mean") # identified outliers
boxplot(mydata$smoothness_mean ,main = "smoothness_mean") # identified outliers
boxplot(mydata$compactness_mean , main = "compactness_mean") # identified outliers
boxplot(mydata$concavity_mean ,main = "concavity_mean") # identified outliers
hist(mydata$radius_mean)
barplot(mydata$radius_mean)


# Calcualte Outliers
summary(mydata)
mydata1<- scale(mydata)
View(mydata1)
attach(mydata1)
boxplot(mydata1) # After scaling boxplot
boxplot(mydata1)$out # outliers
qqnorm(mydata1) #normalize the data
skewness(mydata1) # skewness after scaling
kurtosis(mydata1) # kurtosis after scaling
install.packages("fBasics") # install the fbasics package
library(fBasics)
basicStats(mydata1)

#----------Outliers removed using function---------

  outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}

#source("https://goo.gl/4mthoF")
out1<-outlierKD(mydata, perimeter_mean)
View(out1)
outlierKD(mydata,area_mean)
outlierKD(mydata,compactness_mean)
outlierKD(mydata , compactness_mean)
