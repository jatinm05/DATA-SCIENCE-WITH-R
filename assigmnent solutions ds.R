library(rvest)
library(tidyverse)
#part (a)

html1<- read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")
table=html1%>%html_table()
data=data.frame(table[1])
data=data[,-c(1,14,15)]

#part (b)

html21=read_html("https://www.moneyworks4me.com/indianstocks/large-cap/telecom/telecommunication-service-provider/bharti-airtel/company-info")
b=html21%>%html_table()
b1=data.frame(b[1])  
b1<-b1[-c(1,2,3,4,5),]
b1<-b1[,-c(12,13,14)]
b2=data.frame(b[3])
b2<-b2[,-c(12,13)]
b1 <- b1 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
b2 <- b2 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
b=rbind(b1,b2)
b<-b[-c(9),]
row.names(b)=c(1:14)
html22=read_html("https://www.moneyworks4me.com/indianstocks/large-cap/chemicals-fertilizers/paints/asian-paints/company-info")
z=html22%>%html_table()
z1=data.frame(z[1])  
z1<-z1[-c(1,2,3,4,5),]
z1<-z1[,-c(12,13,14)]
z2=data.frame(z[3])
z2<-z2[,-c(12,13)]
z1 <- z1 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
z2 <- z2 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
z=rbind(z1,z2)
z<-z[-c(9),]
row.names(z)=c(1:14)
html23=read_html("https://www.moneyworks4me.com/indianstocks/large-cap/automobiles/automobiles-trucks-lcv/tata-motors/company-info")
y=html23%>%html_table()
y1=data.frame(y[1])  
y1<-y1[-c(1,2,3,4,5),]
y1<-y1[,-c(12,13,14)]
y2=data.frame(y[3])
y2<-y2[,-c(12,13)]
y1 <- y1 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
y2 <- y2 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
y=rbind(y1,y2)
y<-y[-c(9),]
row.names(y)=c(1:14)
html24=read_html("https://www.moneyworks4me.com/indianstocks/large-cap/fmcg/consumer-food/britannia-inds/company-info")
x=html24%>%html_table()
x1=data.frame(x[1])  
x1<-x1[-c(1,2,3,4,5),]
x1<-x1[,-c(12,13,14)]
x2=data.frame(x[3])
x2<-x2[,-c(12,13)]
x1 <- x1 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
x2 <- x2 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
x=rbind(x1,x2)
x<-x[-c(9),]
row.names(x)=c(1:14)
html25=read_html("https://www.moneyworks4me.com/indianstocks/large-cap/fmcg/consumer-food/nestle/company-info")
w=html25%>%html_table()
w1=data.frame(w[1])  
w1<-w1[-c(1,2,3,4,5),]
w1<-w1[,-c(12,13,14)]
w2=data.frame(w[3])
w2<-w2[,-c(12,13)]
w1 <- w1 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11)
w2 <- w2 %>%rename(DATA=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
w=rbind(w1,w2)
w<-w[-c(9),]
row.names(w)=c(1:14)

#part (c1)

tennis <- function(p)
{
  a=rbinom(n=5,size=1,prob=p)
  Matches=0
  win=0
  loss=0
  for(i in a)
    {
    if(win>=3)
    {
      return(Matches)
      }
    if(loss>=3)
    {
      return(Matches)
      }
    if(i==1)  # i is either 1 or 0
      {
      win = win+1
    }
    else
      {
      loss = loss+1
    }
    Matches = Matches+1
    
  }
  return(Matches)
}

#part (c2)
matches = {}
for(i in 1:1000) 
{
  matches[i] <- tennis(0.7)
}
ans1 <- mean(matches)

#part (d)

MontyHall = function()
  {
  door=sample(x=1:3,size = 1)
  if(door==1)  #let us assume that the car is behind door 3
    {
    return(1)
  }
  if(door==2)
    {
    return(1)
  }
  else
    {
    return(0)
  }
}

results = {}
for(i in 1:1000) 
{
  results[i] <- MontyHall()
}
ans2 <- sum(results)/1000

#part (e)

html5 = read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")
j=html5%>%html_table()
Ranking=html5%>%html_elements(".countdown-index")%>%html_text()
Title=html5%>%html_elements(".article_movie_title a")%>%html_text()
Score=html5%>%html_elements(".tMeterScore")%>%html_text()
Year=html5%>%html_elements(".start-year")%>%html_text()