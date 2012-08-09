####################################
# AUTHOR: Reto Schmucki
# DATE: 8 August 2012
# GIT Repository: https://github.com/RetoSchmucki/Trillium-Demography.git
#------------------------------
# This R-Script compute the the Drought index (SPI, Standardized Precipitation Index)
# And the temperature anomaly, using the period 1971-2000 as base
# The data where averaged from 4 weather stations in the area (La Chute, Oka, Ste-Therese and Mirabel)
#####################################

library(SPEI)
library(zoo)

setwd('/Users/retoschmucki/Desktop/Dropbox/Reto Schmucki/Analysis and graphs/')

# Load data locally on my MAC
## qc_temp_prec <- read.csv("climat quebec/montly_prec_temp.csv", header=T)

qc_temp_prec <- read.csv('http://dl.dropbox.com/s/udcaegy0wqk4bqj/montly_prec_temp.csv',header=T)

str(qc_temp_prec)

# set the limit of the data between 1965 adn 2011
qc_temp_prec <- qc_temp_prec[qc_temp_prec$Year > 1964 & qc_temp_prec$Year < 2012,]

# extract the dataset to determine the average temperature (1971-2000)
base_temp <- qc_temp_prec[ qc_temp_prec$Year > 1970 & qc_temp_prec$Year < 2001 ,]

# compute the average per month across the reference period
month_norm <- aggregate(base_temp$Temp,by=list(base_temp$Month),FUN=mean)
qc_temp_prec$month_norm <- rep(month_norm$x,dim(qc_temp_prec)[1]/12)

# compute temperature anomaly
qc_temp_prec$anomalie71_01 <- qc_temp_prec$Temp - qc_temp_prec$month_norm

qc_temp_prec$Season <- rep(rep(c(1,2,3,4,1),c(2,2,4,2,2)),dim(qc_temp_prec)[1]/12) #this is according to Trillium cycle


#This is a graph for the average temperature anomalies observed during Trillium growth (e.g. May-August) using year 1971-2000 as base 

x <- c(2003,2003,2005,2005)
y <- c(-10,10,10,-10)

summer_anomaly <- aggregate(qc_temp_prec$anomalie71_01,by=list(qc_temp_prec$Year,qc_temp_prec$Season),FUN=mean)
plot(summer_anomaly[which(summer_anomaly[,2]==3),1],summer_anomaly$x[which(summer_anomaly[,2]==3)],type='n',ylab="Temperature Anomaly (oC)",xlab='Year')
polygon(x,y,col=gray.colors(1, start = 0.95, end = 0.9, gamma = 2.2))
abline(h=0,col="red",lw=0.5,lty=2)
points(summer_anomaly[which(summer_anomaly[,2]==3),1],summer_anomaly$x[which(summer_anomaly[,2]==3)],type='l')

#moving average...?
#points(summer_anomaly[which(summer_anomaly[,2]==3),1],rollmean(summer_anomaly$x[which(summer_anomaly[,2]==3)],3,na.pad=T),type='l',col='blue')

#########################
# temperature graph!
#########################

pdf(file='R_scripts/temp_prec.pdf')

par(mfrow=c(3,1))
m <- c(1:12)
x <- c(462,462,488,488) #From June 2003 to August 2005
y <- c(-11,10,10,-11)
plot(qc_temp_prec$anomalie71_0[which(qc_temp_prec$Month%in%m)],type='n',xlab="",ylab="Temperature Anomaly (C)",main='Monthly temp vs. base 1971-2000',xaxt='n')
polygon(x,y,col=gray.colors(1, start = 0.9, end = 0.9, gamma = 2.2))
points(qc_temp_prec$anomalie71_0[which(qc_temp_prec$Month%in%m)],type='l')
abline(h=0,col="red",lw=0.5,lty=2)
# abline(v=2003,col="blue",lw=0.5,lty=1)
# abline(v=2005,col="blue",lw=0.5,lty=1)
points(rollmean(qc_temp_prec$anomalie71_0[which(qc_temp_prec$Month%in%m)],3,na.pad=T),col='red',type="l")
axis(1,at=seq(from=0,to=540,by=60),label=c(seq(from=1965,to=2011,by=5)))


m <- c(1:12)
x <- c(42,42,45,45) #From June 2003 to August 2005
y <- c(-11,10,10,-11)
plot(qc_temp_prec$anomalie71_0[which(qc_temp_prec$Month%in%m)%in%c(421:552)],type='n',ylab="Temperature Anomaly (C)", xlab="Year",xaxt='n')
polygon(x,y,col=gray.colors(1, start = 0.9, end = 0.9, gamma = 2.2))
polygon(x+12,y,col=gray.colors(1, start = 0.9, end = 0.9, gamma = 2.2))
polygon(x+24,y,col=gray.colors(1, start = 0.9, end = 0.9, gamma = 2.2))
polygon(x-12,y,col=gray.colors(1, start = 0.97, end = 0.97, gamma = 2.2))
points(qc_temp_prec$anomalie71_0[which(qc_temp_prec$Month%in%m)%in%c(421:552)],type='l')
abline(h=0,col="red",lw=0.5,lty=2)
# abline(v=2003,col="blue",lw=0.5,lty=1)
# abline(v=2005,col="blue",lw=0.5,lty=1)
points(rollmean(qc_temp_prec$anomalie71_0[which(qc_temp_prec$Month%in%m)%in%c(421:552)],3,na.pad=T),col='red',type="l")
axis(1,at=seq(from=0,to=length(c(421:552)),by=12),label=c(seq(from=2000,to=2011,by=1)))

##################################

# compute SPI

spi_3 <- spi(qc_temp_prec$Prec,3)

spi_3_extract <- matrix(spi_3$fitted)

qc_temp_prec$spi3 <- spi_3_extract

m <- c(1:12)
x <- c(42,42,45,45) #From June 2003 to August 2005
y <- c(-11,10,10,-11)
plot(qc_temp_prec$spi3[which(qc_temp_prec$Month%in%m)%in%c(421:552)],type='n',xlab='Year',ylab='Standardized Precipitation Index (3 months)',xaxt='n',main='Standardized Precipitation Index (SPI)')
polygon(x,y,col=gray.colors(1, start = 0.9, end = 0.9, gamma = 2.2))
polygon(x+12,y,col=gray.colors(1, start = 0.9, end = 0.9, gamma = 2.2))
polygon(x+24,y,col=gray.colors(1, start = 0.9, end = 0.9, gamma = 2.2))
polygon(x-12,y,col=gray.colors(1, start = 0.97, end = 0.97, gamma = 2.2))
points(qc_temp_prec$spi3[which(qc_temp_prec$Month%in%m)%in%c(421:552)],type='l')
abline(h=0,col="red",lw=0.5,lty=2)
axis(1,at=seq(from=0,to=length(c(421:552)),by=12),label=c(seq(from=2000,to=2011,by=1)))

# NOW shade in red and blue!

v_rep <-data.frame()
for (i in c(1:length(c(421:552))-1))
{
  x=c(i,i+1)
  mod <- lm(qc_temp_prec$spi3[which(qc_temp_prec$Month%in%m)%in%c((420+i):(421+i))]~x)
  v <- cbind(seq(i,i+1,0.01),predict(mod,data.frame(x=seq(i,i+1,0.01))))
  v_rep <- rbind(v_rep,v)
}

low <-c(0,v_rep[,2],0)
low[low > 0] <- 0
year <- c(0,v_rep[,1],v_rep[length(v_rep[,1]),1])
polygon(year,low,col='red')

up <-c(0,v_rep[,2],0)
up[up < 0] <- 0
year <- c(0,v_rep[,1],v_rep[length(v_rep[,1]),1])
polygon(year,up,col='blue')

dev.off()

#test for text within the graph for each transition
text(x = 48, y = -2, '{', srt = 90, cex = 8, family = 'Helvetica Neue UltraLight')
text(x = 48, y = -2.5, '(2003 - 2004)', cex= 1, family = 'Helvetica Neue UltraLight')


