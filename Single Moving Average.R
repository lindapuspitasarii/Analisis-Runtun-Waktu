library(forecast)
library(TTR)
library(graphics)

hujan<-read.csv("D:/Semester 6/Analisis Runtun Waktu/hujan.CSV", header=TRUE, sep=";")
hujan
hujan.ts(hujan, start=c(1813))

#SMA
hujan.sma<-SMA(hujan.ts,n=3)
cbind(hujan.ts,hujan.sma)

#Plot
plot(hujan.ts,xlab="Tahun",ylab="Curah Hujan", lty=1,col="black")
points(hujan.ts)
lines(hujan.sma, col="read")

#Prediksi
phujan.sma<-lag(hujan.sma,-1)
phujan.sma
sma<-cbind(hujan.ts,hujan.sma,phujan.sma)
sma

#Evaluasi
SSE<-sum((phujan.sma-hujan.ts)^2,na.rm=T)
MSE<-mean((phujan.sma-hujan.ts)^2,na.rm=T)
MAPE<-mean(abs((hujan.ts-phujan.sma)/hujan.ts),na.rm=T)