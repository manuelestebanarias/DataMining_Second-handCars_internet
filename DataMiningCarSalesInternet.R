################################################################################
###########################  D a t a    M i n i n g  ###########################
################################################################################
########### By Manuel Esteban ARIAS ############################################
################################################################################


##################### Requisites ############################################
library(dplyr)
library(tidyverse)
library(ggpubr) 
library(corrplot)
library(grid)
library(vcd)
library(rcompanion)
library(FactoMineR)
library(factoextra)
### Set the work space ###
#getwd()#where are we working
#setwd("")#set the working directory
rm(list=ls())
setwd('C:/Users/manue/Downloads')

### Load the data ###
data <- read.csv("C:/Users/manue/Downloads/archive (1)/vehicles.csv")

columnas <- colnames(data)
###Counting NaNs
for (i in columnas) {
  j <- sum(is.na(data[, i]))
  print(paste("column ", i, "'s NANs : ", j)) 
}

###################     Data Cleaning    #######################################

# Supressing the identified variables, odometer, county, lat and long, and 
##variables that are not suceptible to be analysed
data<-subset(data,select = -(county))# empty
data<-subset(data,select = -(lat))#Useless
data<-subset(data,select = -(long))#Useless
data<-subset(data,select = -(posting_date))#Useless
data<-subset(data,select = -(description))#Useless
data<-subset(data,select = -(image_url))#Useless
data<-subset(data,select = -(url))#Useless
data<-subset(data,select = -(region_url))#Useless
data<-subset(data,select = -(VIN))#Useless

columnas <- colnames(data)

for (i in columnas) {
  j <- sum(is.na(data[, i]))
  print(paste("column ", i, "'s NANs : ", j)) 
}
#Analyising missing values observations
dataNaN=data[is.na(c(data$odometer,data$year)),]
summary(dataNaN$price)
summary(dataNaN$odometer)
summary(dataNaN$year)

#supressing all the observations with NANs
data0=na.omit(data)

summary(data0$price)
summary(data0$odometer)
summary(data0$year)



# Removing the 0 values of odometer: 1943 rows(0,4% of the data)
df0<-data0[data0$price < 750000, ]
df0<-df0[df0$odometer <999999, ]

df0[df0==""] <- "Unknown"

NumOdometerHigh <- nrow(data0[data0$odometer >999999, ])
Price0<-nrow(data0[data0$price == 0, ])
ggplot(aes(x=year,y=odometer),data=df0)+
  geom_point()
################### Exploratory analysis #######################################
#Creating Quanty and quali databases
## Quanti
num=df0[sapply(df0,is.numeric)==T]
num=subset(num,select = -(id))

##histogram
for(x in seq(length(num),1)) 
  hist(num[,x],xlab=names(num[x]),col=rainbow(10), main=names(num[x]))

#Boxplot
for(x in seq(length(num),1)) 
  boxplot(num[,x],xlab=names(num[x]),col="cyan", main=names(num[x]))
#Descriptive statistics:
summary(num$price)
upper_whisker = min(max(num$price), quantile(num$price,c(.75)) + 1.5 * (quantile(num$price,c(.75))-quantile(num$price,c(.25))))
upper_whisker
summary(num$year)
lower_whisker = max(min(num$year), quantile(num$year,c(.25)) - 1.5 * (quantile(num$year,c(.75))-quantile(num$year,c(.25))))
lower_whisker
summary(num$odometer)
upper_whisker = min(max(num$odometer), quantile(num$odometer,c(.75)) + 1.5 * (quantile(num$odometer,c(.75))-quantile(num$odometer,c(.25))))
upper_whisker

#correlation
M<-cor(num)
corrplot(M, method="ellipse",sig.level=0.05,  tl.cex=1)


##quali
quali=df0[sapply(df0,is.numeric)!=T]



#we are particularily interested in condition, cylinders, fuel, manufacturer
Condition_Manufact=table(quali$condition, quali$manufacturer)
Condition_Manufact
assocstats(Condition_Manufact)
cylinders_fuel=table(quali$cylinders,quali$fuel)
cylinders_fuel
assocstats(cylinders_fuel)


################### unsupervised analysis ######################################
### Principal Component Analysis
#Eigen values
res.pca<-PCA(num, graph=FALSE)
eig.val<-get_eigenvalue(res.pca)
eig.val
#Dimension loadings
var<-get_pca_var(res.pca)
fviz_pca_var(res.pca,col.var="blue")

var$cor
#correlation to dimensions
corrplot(var$cor)
#contribution to dimensions
corrplot(var$contrib,is.corr=FALSE)

###Multiple correspondence analysis (MCA)
#####Given that running the full database requieres a big memory space, we will
#####use a simple of the database
set.seed(142)
quali2<-quali[sample(nrow(quali),1000),]
#MCA
res.mca<-MCA(quali2,graph=FALSE,level.ventil=0.05)
###The dimensions are nor representatives, lets stry to make the same excesice
#with less variables
quali3<-subset(quali2,select = -(region))
quali3<-subset(quali3,select = -(state))
quali3<-subset(quali3,select = -(model))

res.mca<-MCA(quali3,graph=FALSE,level.ventil=0.05)
var<-get_mca_var(res.mca)
#visualisation
fviz_screeplot(res.mca,addlabels=TRUE)

fviz_mca_var(res.mca,chois="mca.cor",repel=TRUE)

corrplot(var$contrib,is.corr = FALSE)

fviz_contrib(res.mca,choice = "var",axes=1,top=10)
fviz_contrib(res.mca,choice = "var",axes=2,top=10)
fviz_contrib(res.mca,choice = "var",axes=3:4,top=10)
fviz_mca_var(res.mca)
##Quality of representaiton
corrplot(var$cos2,is.corr=FALSE,main="Cos2 par modality")###It does not run for me
fviz_cos2(res.mca,choice="ind",axes=1:2,top=20)
##Indiidual Contribution
fviz_contrib(res.mca,choice="ind",axes=1:2,top=20)
#Biplot
fviz_mca_biplot(res.mca, 
    repel = TRUE, # Avoid text overlapping (slow if many point)
    ggtheme = theme_minimal())

fviz_mca_var(res.mca, col.var = "cos2",
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
    repel = TRUE, # Avoid text overlapping
    ggtheme = theme_minimal())
fviz_mca_ind(res.mca, col.ind = "cos2", 
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE, # Avoid text overlapping (slow if many points)
    ggtheme = theme_minimal())
## MCA factor Maps
fviz_ellipses(res.mca, c("manufacturer", "cylinders"),
                           geom = "point") 
fviz_ellipses(res.mca, c("paint_color", "fuel"),
              geom = "point") 

#############    Supervised Analysis    #######################################

predictor_variables <- c("year", "manufacturer", "condition", "type", "odometer")

set.seed(123) # For reproducibility
train_index <- createDataPartition(df0$price, p = 0.7, list = FALSE)
train_data <- df0[train_index, ]
test_data <- df0[-train_index, ]

ols_model <- lm(price ~ ., data = train_data[, c("price", predictor_variables)])

summary(ols_model)


ols_preds <- predict(ols_model, newdata = test_data[, predictor_variables])
ols_rmse <- RMSE(ols_preds, test_data$target_variable)
ols_rmse

