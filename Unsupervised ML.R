library(quantmod)
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(tidyquant)
library(tidyverse)
library(factoextra)
#UNSUPERVISED 
#K-Means Clustering

#To gather data using tidyquant package install 
stocks=function(x){getSymbols(x,src="yahoo", from="2016-01-01", to="2020-01-01", auto.assign=FALSE)} #to get the data for a specific ticker/stock
comp=c("AMZN","WMT","NFLX", "GOOG", "CRM","UPS","MSFT","AAPL", "COST")
price_close=map(comp, stocks)%>%map(Cl)%>%reduce(merge.xts)
View(price_close) #to view the table from the created dataset
summary(price_close) #to see the descriptive statistics for the closing price data of each company

#create data frame with gathered data to begin clustering
stock_data=as.data.frame(price_close)
View(stock_data)

#To switch the columns to roq position and rows to column position
#rownames_to_column(stock_data, var="Date") %>% head
comp_col<- data.frame(t(stock_data[,]))
comp_col %>% head
colnames(comp_col)<-rownames(stock_data)
view(comp_col)


#to determine best k/centers value to use
#using silhouette
fviz_nbclust(stock_data, kmeans, method="silhouette")+
  labs(subtitle = "Silhouette Method")
kmeans(stock_data, centers = 2, nstart = 100)

#plot the clusters
fviz_cluster(kmeans(comp_col,centers=3, nstart = 100),xlim=c(-40,60), ylim=c(-20,25),data=comp_col)


