setwd("C:/Users/jhkjhk/Desktop/New folder/R Project/Stock")

library("tidyverse")
library("ggthemes")
library("stats")

fed <- read_csv("FEDFUNDS.csv")
head(fed)

nasdaq <- read_csv("^IXIC.csv")
head(nasdaq)


colors <- c("Close" =="blue", "FEDFUNDS" == "red")

ggplot()+
  geom_line(data=nasdaq[which(nasdaq$Date>="1998-01-01" & nasdaq$Date<= "2002-12-31"),],mapping=aes(x=Date,y=Close, col="NASDAQ"))+
  geom_line(data=fed[which(fed$DATE>="1998-01-01" & fed$DATE <="2002-12-31"),], mapping=aes(x=DATE,y=FEDFUNDS*600, col="Fed Rate"))+
  scale_y_continuous("NASDAQ", sec.axis = sec_axis(~ .*1/600,name = "Fed Funds Rate"))+
  labs(title="NASDAQ VS Fed Fund Rate in 1995-2002")+
  scale_color_manual(name="", values=c("Red","Blue"))+
  theme_economist()

  
ggplot()+
  geom_line(data=nasdaq[which(nasdaq$Date>="2003-01-01" & nasdaq$Date<= "2008-12-31"),],mapping=aes(x=Date,y=Close, col="NASDAQ"))+
  geom_line(data=fed[which(fed$DATE>="2003-01-01" & fed$DATE <="2008-12-31"),], mapping=aes(x=DATE,y=FEDFUNDS*600, col="Fed Rate"))+
  scale_y_continuous("NASDAQ", sec.axis = sec_axis(~ .*1/600,name = "Fed Funds Rate"))+
  labs(title="NASDAQ VS Fed Fund Rate in 2003-2008")+
  scale_color_manual(name="", values=c("Red","Blue"))+
  theme_excel_new()



bond10yr <- read_csv("10yearbond.csv")
bond10yr <- bond10yr[-apply(bond10yr,2,function(x) which(x =="null"))[[2]],] # removed null
bond10yr[,-1] <- sapply(bond10yr[,-1],as.numeric) # columns to numeric
str(bond10yr)

install.packages("anytime")
library("anytime")
bond2yr <- read_csv("2yearbond.csv")
str(bond2yr)
bond2yr$Date <- gsub(", +", ",", bond2yr$Date)
bond2yr$Date <- as.Date(anytime(bond2yr$Date))



ggplot()+
  geom_line(data=nasdaq[which(nasdaq$Date>="1998-01-01" & nasdaq$Date<= "2002-12-31"),],mapping=aes(x=Date,y=Close, col="NASDAQ"))+
  geom_line(data=bond10yr[which(bond10yr$Date>="1998-01-01" & bond10yr$Date <="2002-12-31"),], mapping=aes(x=Date,y=Close*600, col="US 10yr Bond"))+
  geom_line(data=bond2yr[which(bond2yr$Date>="1998-01-01" & bond2yr$Date <="2002-12-31"),], mapping=aes(x=Date,y=Price*600, col="US 2yr Bond"))+
  scale_y_continuous("NASDAQ", sec.axis = sec_axis(~ .*1/600,name = "10yr Bond Rate"))+
  labs(title="NASDAQ VS US 10yr Bond in 1995-2002")+
  scale_color_manual(name="", values=c("Blue","Red", "Black"))+
  theme_economist()




nasdaq$ma200 <- stats::filter(nasdaq$Close, method="convolution", sides=1, filter=rep(1/200,200))
head(nasdaq)

nasdaq$ma200.osi <- (nasdaq$Close - nasdaq$ma200)/ nasdaq$ma200*100

ggplot(nasdaq,aes(x=ma200.osi))+geom_histogram(binwidth=3.5*sd(na.omit(nasdaq$ma200.osi))*(nrow(nasdaq)-199)^-(1/3))


length(na.omit(nasdaq$ma200.osi))
((7878-197)-197)/7878

sort(na.omit(nasdaq$ma20.osi),decreasing=F)[197]
sort(na.omit(nasdaq$ma20.osi),decreasing=F)[(7878-197)]

extremefear <- which(nasdaq$ma200.osi <= -26.10132)
View(nasdaq[extremefear,])

extremegreed <- which(nasdaq$ma20.osi >=23.94748)
View(nasdaq[extremegreed,])

nasdaq$highlight <- 0
nasdaq[extremegreed,"highlight"] <-1
datemark <- diff(c(0,nasdaq$highlight))

start <- nasdaq$Date[datemark==1]
end <- nasdaq$Date[datemark == -1]

time <- data.frame(start=start,end=end,group=seq_along(start))

ggplot(nasdaq,aes(x=Date))+geom_line(aes(y=Close))+geom_line(aes(y=ma200,col="200SMA"),)+
  labs(title="NASDAQ Historical Chart", y="NASDAQ")+
  scale_color_manual(name='', values=("200SMA"="red"))+
  scale_x_date(date_breaks="3 year", date_labels="%Y", limits=as.Date(c("1990-01-01","2022-01-01")))+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_rect(data=time, inherit.aes=FALSE, 
            aes(xmin=start, xmax=end, ymin=min(nasdaq$Close),
            ymax=max(nasdaq$Close), group=group), color="transparent", fill="orange", alpha=0.3)



#Dot-com era

dotcom <- nasdaq[which(nasdaq$Date >="1999-01-01" & nasdaq$Date <= "2000-12-31"),]
datemark.dotcom <- diff(c(0,dotcom$highlight))

start.dotcom <- dotcom$Date[datemark.dotcom== 1]
end.dotcom <- dotcom$Date[datemark.dotcom == -1]

time.dotcom <- data.frame(start=start.dotcom,end=end.dotcom,group=seq_along(start.dotcom))


ggplot(dotcom,aes(x=Date))+geom_line(aes(y=Close))+geom_line(aes(y=ma200,col="200SMA"),)+
  labs(title="NASDAQ Historical Chart(1999-2000)", y="NASDAQ")+
  scale_color_manual(name='', values=("200SMA"="red"))+
  scale_x_date(date_breaks="3 months", date_labels="%m-%Y", limits=as.Date(c("1999-01-01","2000-12-31")))+
  scale_y_continuous(limits=c(min(dotcom$ma200), max(dotcom$Close)), breaks= seq(2000,5000,by=500) )+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_rect(data=time.dotcom, inherit.aes=FALSE, 
            aes(xmin=start, xmax=end, ymin=min(dotcom$ma200),
                ymax=max(dotcom$Close), group=group), color="transparent", fill="orange", alpha=0.3)
# covid era
covid<- nasdaq[which(nasdaq$Date >="2018-01-01"),]
datemark.covid <- diff(c(0,covid$highlight))
start.covid <- covid$Date[datemark.covid == 1]
end.covid <- covid$Date[datemark.covid == -1]
time.covid <- data.frame(start=start.covid,end=end.covid,group=seq_along(start.covid))

ggplot(covid,aes(x=Date))+geom_line(aes(y=Close))+geom_line(aes(y=ma200,col="200SMA"),)+
  labs(title="NASDAQ Historical Chart(2018-Present)", y="NASDAQ")+
  scale_color_manual(name='', values=("200SMA"="red"))+
  scale_x_date(date_breaks="6 months", date_labels="%m-%Y", limits=as.Date(c("2018-01-01","2022-01-31")))+
  scale_y_continuous(limits=c(min(covid$Close), max(covid$Close)), breaks= seq(6000,16000,by=2000) )+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  geom_rect(data=time.covid, inherit.aes=FALSE, 
            aes(xmin=start, xmax=end, ymin=min(covid$Close),
                ymax=max(covid$Close), group=group), color="transparent", fill="orange", alpha=0.3)


