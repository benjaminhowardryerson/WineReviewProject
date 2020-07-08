
library(dyplr)
library(purrrr)
library(tidyr)
library(stringr)
library(GGally)

wine1 = read.csv("C:/Users/benjamin.howard/Desktop/winemag-data-130k-v2.csv")
Wine2 = as.data.frame(wine1)
wine3 = wine2[,-c(1,4,9,11)]
colnames(wine3)
wine4 = wine3[!duplicated(wine3[c(7,8)]),]
str(wine4)

rowcnt = length(wine4$title)
Vintage_Year = rep(0,rowcnt)
head(Vintage_Year, 10)
wine5 = cbind(wine4, Vintage_Year)

library(stringr)
yearextract <- function(string){str_extract(string, "\\-*\\d+\\.*\\d*")}
year <- yearextract(wne5$title)
wine6 <-cbind(wine5, year)
head(wine6,10)

price_range <- ifelse(wine7$price <= 10, "Value", 
                      ifelse(wine7$price <= 20, "Premium", 
                             ifelse(wine7$price <= 40, "Ultra Premium", 
                                    ifelse(wine7$price <= 100, "Luxury", 
                                           ifelse(wine7$price <= 300, "Super Luxury", 
                                                  ifelse(wine7$price > 300 , "Iconic", NA))))))

wine8 <- cbind(wine7 , price_range)
wine9 <- wine8 %>% drop_na(price, points)
wine10 <- wine9[!(wine9$taster_name ==""),]
wine11 <- wine10[!(wine10$price > 500),]
wineclean <- wine11

hist(wineclean$price)
hist(wineclean$points)

price_points <-wineclean[, 3:4]
points_pp <- price_points$points
price_pp <- price_points$price
corspeartest1 <- cor.test(price_pp, points_pp, method = "spearman", exact = FALSE)
ggpairs(price_points)
