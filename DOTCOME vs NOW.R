setwd("E:/STOCK")

library("tidyverse")
library("ggthemes")

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
  theme_economist()






