######libraries######

library(ggplot2)
library(plotly)


######data import######

df <- Dataset_ISYS3446_1

######EDA######

#view data
head(df)
#view headers
colnames(df)
#number of coloumnes
ncol(df)
#number of rows
nrow(df)
#summary 
summary(df$price)
#
str(df)

######data prepossessing######

#missing values
colSums(is.na(df))#identification
sum(is.na(df))#93 missing values

df$normalized_losses <- NULL#remove normalised_losses col

mode <-names(sort(-table(df$num_of_doors)))[1]
mode
df$num_of_doors[is.na(df$num_of_doors)] <- mode

mean_bore <- mean(df$bore, na.rm = TRUE)
df$bore[is.na(df$bore)] <- mean_bore

mean_stroke <- mean(df$stroke, na.rm = TRUE)
df$stroke[is.na(df$stroke)] <- mean_stroke

median_horsepower <- median(df$horsepower, na.rm = TRUE)
df$horsepower[is.na(df$horsepower)] <- median_horsepower

mean_peak_rpm <- mean(df$peak_rpm, na.rm = TRUE)
df$peak_rpm[is.na(df$peak_rpm)] <- mean_peak_rpm

colSums(is.na(df))#check above have missing values been replaced with mean

df <- na.omit(df)

colSums(is.na(df))# check no missing values
str(df)#dataset size now 401-25, was 405-26 (only 4 in price were removed)

#outliers
numeric <- df[, sapply(df, is.numeric)] #group numeric coloumns
boxplot(numeric, las = 2)#use boxplots to show which have outliers

ggplot(data=df , aes(x=wheel_base)) + geom_boxplot(outlier.colour = "red")
ggplot(data=df , aes(x=length)) + geom_boxplot(outlier.colour = "red")
ggplot(data=df , aes(x=width)) + geom_boxplot(outlier.colour = "red")
ggplot(data=df , aes(x=engine_size)) + geom_boxplot(outlier.colour = "red")
ggplot(data=df , aes(x=stroke)) + geom_boxplot(outlier.colour = "red")
ggplot(data=df , aes(x=compression_ratio)) + geom_boxplot(outlier.colour = "red")
ggplot(data=df , aes(x=horsepower)) + geom_boxplot(outlier.colour = "red")
ggplot(data=df , aes(x=peak_rpm)) + geom_boxplot(outlier.colour = "red")
ggplot(data=df , aes(x=city_mpg)) + geom_boxplot(outlier.colour = "red")
ggplot(data=df , aes(x=highway_mpg)) + geom_boxplot(outlier.colour = "red")
ggplot(data=df , aes(x=price)) + geom_boxplot(outlier.colour = "red")

#outlier detection function
detect_outlier <- function(x){
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  x > Q3 + 1.5 * IQR | x < Q1 - 1.5 * IQR
}
#outlier removal function
remove_outlier <- function(dataframe,columns=names(dataframe)){
  for(col in columns){
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]),]
  }
  dataframe
}
#use function outlier
df <-remove_outlier(df,c("length", "width","engine_size","horsepower","price"))
#check results
numeric1 <- df[, sapply(df, is.numeric)] #checking
boxplot(numeric1, las = 2)
str(df)


#data errors
characters <- df[, sapply(df, is.character)] #group char coloumns
lapply(characters, unique)#shows each unique 

#replace spelling mistakes
df$make <- gsub("alfa-romero", "alfa-romeo", df$make)
df$make <- gsub("peugot", "peugeot", df$make)


#final check of size
str(df)



######visualisations######

#visualisation1
p1 <- ggplot(data=df , aes(x=price, y=make, fill = make))
p2 <- geom_boxplot(outlier.colour = "red", outlier.shape= 5)
p3 <- labs(x = "car price", y = "car make", title = "distribution of price per car brand" )
p1+p2+p3
#visualisation2
p1 <- ggplot(df, aes(x = horsepower, y = price))
p2 <- geom_point(aes(colour = num_of_cylinders))
p3 <- geom_smooth(method = "lm")
p4 <- facet_wrap(~ make)
p5 <- labs(x = "Horsepower", y = "Price", title = "Horsepower vs Price by Car Make",colour = "Number of Cylinders")
ggplotly(p1+p2+p3+p4+p5)
#visualisation3
p1 <- ggplot(data=df, aes(x=height, y= length, colour = price))
p2 <- geom_point()
p3 <- facet_wrap(~body_style)
p4 <- labs(x= "height of car", y= "length of car", title = "relationship with car size on price")
ggplotly(p1+p2+p3+p4)
#visualisation4
p1 <- ggplot(data=df, aes(x=engine_size, y= curb_weight, colour = price))
p2 <- geom_point()
p3 <- facet_wrap(~body_style)
p4 <- labs(x= "engine size", y= "curbweight", title = "relationship with engine size and curb weight on price")
ggplotly(p1+p2+p3+p4)

######machine learning model######

#encoding
df$make <- as.factor(df$make)

#scatterplots
p1 <- ggplot(data=df , aes(x=horsepower, y=price))
p2 <- geom_point()
p3 <- geom_smooth(method="lm")
p4 <- labs(x = "horsepower", y = "price", title = "distribution of horsepower and price" )
p1+p2+p3+p4

p1 <- ggplot(data=df , aes(x=curb_weight, y=price))
p4 <- labs(x = "curb weight", y = "price", title = "distribution of curbweight and price" )
p1+p2+p3+p4

p1 <- ggplot(data=df , aes(x=horsepower, y=price, colour =make))
p4 <- labs(x = "horsepower", y = "price", title = "distribution of horsepower and price with make" )
p1+p2+p4

#test regression models
reg <- lm(price ~ curb_weight + horsepower, data = df)
reg$coefficients
summary(reg)# r squared 0.7374

reg <- lm(price ~ horsepower, data = df)
reg$coefficients
summary(reg)# r squared 0.514

reg <- lm(price ~ city_mpg + highway_mpg, data = df)
reg$coefficients
summary(reg)# r squared 0.5164

reg <- lm(price ~ curb_weight + horsepower + body_style, data = df)
reg$coefficients
summary(reg)#0.7666

reg <- lm(price ~ curb_weight + horsepower + length + height, data = df)
reg$coefficients
summary(reg)#0.7427

reg<- lm(log(price) ~ curb_weight + horsepower + make, data = df)
reg$coefficients
summary(reg)#0.7427

#final multi regression model
reg <- lm(price ~ curb_weight + horsepower + make, data = df)
reg$coefficients
summary(reg)#0.8611

#r squared = 0.8555
#p value = 2.2e-16
#p value of each attribute = Pr(>|t|)
#y = intercept + curbwieght * x1 + horsepower * x2 + car make * x3
#y = -4353.4447 + 5.2496 * x1 + 44.5614 * x2 + car make * x3 
#
df$residuals <- residuals(reg)
df$predicted <- predict(reg, df)
df1 <- df[, c("price", "predicted", "residuals")]

df1

#residual plot
p1 <- ggplot(data = df, aes(x = predicted, y = residuals))
p2 <- geom_point(colour = "red")
p3 <- geom_hline(yintercept = 0, colour = "blue")
p4 <- labs(x = "Predicted Price",
           y = "Residuals",
           title = "Residuals vs Predicted Price")
p1+p2+p3+p4


#export df1
write.csv(df1, "~/Downloads/df1.csv", row.names = FALSE)

  