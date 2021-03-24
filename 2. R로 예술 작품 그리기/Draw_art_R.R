# 0. 라이브러리 호출
library(plyr)
library(dplyr)
library(tibble)
library(viridis)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(hrbrthemes)
library(RColorBrewer)

# 1. 데이터 셋 생성
# 1.1 포아송 분포 (좌측 산 그리는 용도)
lambda = 1
x = seq(-1,5,by=0.01)
y = 20*(lambda^x)*(exp(-lambda))/(factorial(x))
data_1 = data.frame(x,y)
data_1 = na.omit(data_1)

# 1.2 도넛 데이터 (달 그리는 용도)
n  <- 10000;   s  <- 10
xr <- 2;   yr <- 2
sx <- xr*s;   sy <- yr*s
a <- sx*runif(n);   b <- sy*runif(n)
sp <- rep("black",n)
sp[(a-sx/2)^2/xr +(b-sy/2)^2/yr <= (0.6*s)^2/xr] <- "yellow"
sp[(a-(sx-1.5)/2)^2/xr +(b-(sy-0.5)/2)^2/yr <= (0.4*s)^2/xr] <- "darkgoldenrod1"
sp[(a-(sx+0.3)/2)^2/xr +(b-(sy+0.5)/2)^2/yr <= (0.29*s)^2/xr] <- "yellow1"
sp <- factor(sp, levels=c("black","yellow","darkgoldenrod1","yellow1"))
a <- 7+(a)/7; b <- 7+(b)/7
dnut <- data.frame(sp, a, b)
dnut1 <- dnut %>% filter(sp == "yellow")
dnut2 <- dnut %>% filter(sp == "darkgoldenrod1")
dnut3 <- dnut %>% filter(sp == "yellow1")

# 1.3 도넛 데이터 (별 그리는 용도)
n  <- 10000;   s  <- 10
xr <- 2;   yr <- 2
sx <- xr*s;   sy <- yr*s
a <- sx*runif(n);   b <- sy*runif(n)
sp <- rep("black",n)
sp[(a-sx/2)^2/xr +(b-sy/2)^2/yr <= (0.45*s)^2/xr] <- "yellow"
sp[(a-sx/2)^2/xr +(b-sy/2)^2/yr <= (0.4*s)^2/xr] <- "yellow1"
sp[(a-sx/2)^2/xr +(b-sy/2)^2/yr <= (0.2*s)^2/xr] <- "yellow2"
sp[(a-sx/2)^2/xr +(b-sy/2)^2/yr <= (0.05*s)^2/xr] <- "yellow3"
sp <- factor(sp, levels=c("black","yellow","yellow1","yellow2","yellow3"))
a <- 3+(a)/12; b <- 6+(b)/12
star <- data.frame(sp, a, b)
star1 <- star %>% filter(sp == "yellow")
star2 <- star %>% filter(sp == "yellow1")
star3 <- star %>% filter(sp == "yellow2")
star4 <- star %>% filter(sp == "yellow3")

n  <- 10000;   s  <- 10
xr <- 2;   yr <- 2
sx <- xr*s;   sy <- yr*s
a <- sx*runif(n);   b <- sy*runif(n)
sp <- rep("black",n)
sp[(a-sx/2)^2/xr +(b-sy/2)^2/yr <= (0.45*s)^2/xr] <- "yellow"
sp[(a-sx/2)^2/xr +(b-sy/2)^2/yr <= (0.4*s)^2/xr] <- "yellow1"
sp[(a-sx/2)^2/xr +(b-sy/2)^2/yr <= (0.2*s)^2/xr] <- "yellow2"
sp[(a-sx/2)^2/xr +(b-sy/2)^2/yr <= (0.05*s)^2/xr] <- "yellow3"
sp <- factor(sp, levels=c("black","yellow","yellow1","yellow2","yellow3"))
a <- 5+(a)/24; b <- 9+(b)/24
star <- data.frame(sp, a, b)
star5 <- star %>% filter(sp == "yellow")
star6 <- star %>% filter(sp == "yellow1")
star7 <- star %>% filter(sp == "yellow2")
star8 <- star %>% filter(sp == "yellow3")

n  <- 10000;   s  <- 10
xr <- 2;   yr <- 2
sx <- xr*s;   sy <- yr*s
a <- sx*runif(n);   b <- sy*runif(n)
sp <- rep("black",n)
sp[(a-sx/2)^2/xr +(b-sy/2)^2/yr <= (0.45*s)^2/xr] <- "yellow"
sp[(a-sx/2)^2/xr +(b-sy/2)^2/yr <= (0.4*s)^2/xr] <- "yellow1"
sp[(a-sx/2)^2/xr +(b-sy/2)^2/yr <= (0.2*s)^2/xr] <- "yellow2"
sp[(a-sx/2)^2/xr +(b-sy/2)^2/yr <= (0.05*s)^2/xr] <- "yellow3"
sp <- factor(sp, levels=c("black","yellow","yellow1","yellow2","yellow3"))
a <- -1+(a)/35; b <- 5.5+(b)/35
star <- data.frame(sp, a, b)
star9 <- star %>% filter(sp == "yellow")
star10 <- star %>% filter(sp == "yellow1")
star11 <- star %>% filter(sp == "yellow2")
star12 <- star %>% filter(sp == "yellow3")

# 1.4 마을의 산맥 그리는 용도1
x <- seq(-1,10,length=11)
y <- seq(2,5,length=11)
for (i in 1:11){
  if (rbinom(n=1, size=1, prob=0.5) == 1) {y[i] <- y[i]+0.3}
  else {y[i] <- y[i]-0.3}
}
data_4 = data.frame(x,y)

x <- seq(-1,10,length=11)
y <- seq(1.5,4.4,length=11)
for (i in 1:11){
  if (rbinom(n=1, size=1, prob=0.5) == 1) {y[i] <- y[i]+0.3}
  else {y[i] <- y[i]-0.3}
}
data_5 = data.frame(x,y)

x <- seq(-1,10,length=11)
y <- seq(1,3.8,length=11)
for (i in 1:11){
  if (rbinom(n=1, size=1, prob=0.5) == 1) {y[i] <- y[i]+0.3}
  else {y[i] <- y[i]-0.3}
}
data_6 = data.frame(x,y)

x <- seq(-1,10,length=11)
y <- seq(0.5,3,length=11)
for (i in 1:11){
  if (rbinom(n=1, size=1, prob=0.5) == 1) {y[i] <- y[i]+0.3}
  else {y[i] <- y[i]-0.3}
}
data_7 = data.frame(x,y)

# 1.5 배경 밑 채색 용도
coul = brewer.pal(5, "YlGn") 
coul = colorRampPalette(coul)(5)
coul=coul[sample(c(1:length(coul)) , size=length(coul))]
time <- as.numeric(rep(seq(-1,10,length=8),each=5))
value <- runif(40, 0.8, 1.0)*2.4
group <- rep(LETTERS[1:5],times=8)
data_background <- data.frame(time, value, group)
data_background_2 <- data_background %>% filter(group != 'A')

# 1.6 마을의 산맥 그리는 용도2
normalize <- function(x,a,b){
  x <- a + ((x-min(x))*(b-a))/(max(x)-min(x))
}

mx1=seq(10,100)
my1=(seq(10,100)/2)+rnorm(91,mean=3000)
mx2=seq(10,100)
my2=(seq(10,100)/2)+rnorm(91,mean=2000)
mx3=seq(10,100)
my3=(seq(10,100)/2)+rnorm(91,mean=1000)

mx1 = normalize(mx1, -1,10)
my1 = normalize(my1, 2,5)
mx2 = normalize(mx2, -1,10)
my2 = normalize(my2, 1.5,3.7)
mx3 = normalize(mx3, -1,10)
my3 = normalize(my3, 1,2)
data_mount_1 <- data.frame(mx1,my1)
data_mount_2 <- data.frame(mx2,my2)
data_mount_3 <- data.frame(mx3,my3)

# 1.7 오로라
normalize <- function(x,a,b){
  x <- a + ((x-min(x))*(b-a))/(max(x)-min(x))
}

x <- rnorm(200, 0.8, 1.2)
e <- rnorm(200, 0, 3)*(abs(x)^1.5 + .5) + rnorm(200, 0, 4)
e2 <- rnorm(200, 0, 5)*(abs(x)^1.5 + .8) + rnorm(200, 0, 5)
y <- 8*x - x^3 + e
y2 <- 20 + 3*x + 0.6*x^3 + e2

x = normalize(x, -1,10)
y = normalize(y, 3,10)
y2 = normalize(y, 3,10)

data_aurora <- data.frame(x, y, y2)

data = data_aurora
formula = y~x
slices=200
method=loess
B=1000

palette=colorRampPalette(c("#FFEDA0", "#DD0000"), bias=2)(20)

IV <- all.vars(formula)[2]
DV <- all.vars(formula)[1]
data <- na.omit(data[order(data[, IV]), c(IV, DV)])

newx <- data.frame(seq(min(data[, IV]), max(data[, IV]), length=slices))
colnames(newx) <- IV
l0.boot <- matrix(NA, nrow=nrow(newx), ncol=B)

l0 <- method(formula, data)
for (i in 1:B) {
  data2 <- data[sample(nrow(data), replace=TRUE), ]
  data2 <- data2[order(data2[, IV]), ]
  if (class(l0)=="loess") {
    m1 <- method(formula, data2, control = loess.control(surface = "i", statistics="a", trace.hat="a"))
  } else {
    m1 <- method(formula, data2, ...)
  }
  l0.boot[, i] <- predict(m1, newdata=newx)
}

CI.boot <- adply(l0.boot, 1, function(x) quantile(x, prob=c(.025, .5, .975, pnorm(c(-3, -2, -1, 0, 1, 2, 3))), na.rm=TRUE))[, -1]
colnames(CI.boot)[1:10] <- c("LL", "M", "UL", paste0("SD", 1:7))
CI.boot$x <- newx[, 1]
CI.boot$width <- CI.boot$UL - CI.boot$LL
CI.boot$w2 <- (CI.boot$width - min(CI.boot$width))
CI.boot$w3 <- 1-(CI.boot$w2/max(CI.boot$w2))

b2 <- melt(l0.boot)
b2$x <- newx[,1]
colnames(b2) <- c("index", "B", "value", "x")
b2 <- na.omit(b2)


# 2. 그림 그리기
ggplot(data=data_1,aes(x,y),color="darkolivegreen")+
  geom_area(data=data_background_2, aes(x=time,y=value,fill=group),
          alpha=0.13, colour='white')+
  scale_fill_manual(values=coul)+
  geom_area(data=data_7,aes(x,y),alpha=0.4,fill="navy")+
  geom_area(data=data_6,aes(x,y),alpha=0.4,fill="navy")+
  geom_area(data=data_5,aes(x,y),alpha=0.4,fill="navy")+
  geom_area(data=data_4,aes(x,y),alpha=0.4,fill="navy")+
  geom_area(data=data_mount_1, aes(mx1,my1), alpha=.3, fill='black')+
  geom_area(data=data_mount_2, aes(mx2,my2), alpha=.3, fill='gray')+
  geom_area(data=data_mount_3, aes(mx3,my3), alpha=.2, fill='black')+
  geom_area(aes(x,y),alpha=0.9,fill="darkolivegreen")+
  geom_area(aes(x,y/1.1),alpha=0.5,color="darkolivegreen")+
  geom_area(aes(x,y/1.2),alpha=0.9,fill="darkolivegreen")+
  geom_area(aes(x,y/1.3),alpha=0.9,color="darkolivegreen")+
  geom_area(aes(x,y/1.4),alpha=0.5,fill="darkolivegreen")+
  geom_area(aes(x,y/1.5),alpha=0.9,color="darkolivegreen")+
  geom_area(aes(x,y/1.5),alpha=0.5,fill="darkolivegreen")+
  geom_area(aes(x,y/1.7),alpha=0.9,color="darkolivegreen")+
  geom_area(aes(x,y/1.8),alpha=0.5,fill="darkolivegreen")+
  geom_area(aes(x,y/1.9),alpha=0.9,color="darkolivegreen")+
  geom_area(aes(x,y/2.0),alpha=0.5,fill="darkolivegreen")+
  geom_area(aes(x,y/2.1),alpha=0.9,color="darkolivegreen")+
  geom_area(aes(x,y/2.2),alpha=0.5,fill="darkolivegreen")+
  geom_area(aes(x,y/2.3),alpha=0.9,color="darkolivegreen")+
  geom_area(aes(x,y/2.7),alpha=0.5,fill="darkolivegreen")+
  geom_area(aes(x,y/3.0),alpha=0.9,fill="darkolivegreen")+
  geom_area(aes(x,y/4.0),alpha=0.9,color="darkolivegreen")+
  geom_area(aes(x,y/6.0),alpha=0.9,fill="darkolivegreen")+
  geom_area(aes(x,y/10.2),alpha=0.5,color="darkolivegreen")+
  geom_area(aes(x,y/100.6),alpha=0.9,fill="darkolivegreen")+
  geom_area(aes(x,y/300.6),alpha=0.9,color="darkolivegreen")+
  geom_area(aes(x,y/700.6),alpha=0.9,fill="darkolivegreen")+
  geom_area(aes(x,y/1000.0),alpha=0.5,fill="darkolivegreen")+
  geom_area(aes(x,y/10000.0),alpha=0.9,fill="darkolivegreen")+
  geom_point(data=dnut1,aes(a,b),color='yellow',size=4)+
  geom_point(data=dnut2,aes(a,b),color='darkgoldenrod1',size=4)+
  geom_point(data=dnut3,aes(a,b),color='yellow1',size=4)+
  geom_point(data=star1,aes(a,b),color='yellow',size=4)+
  geom_point(data=star2,aes(a,b),color='yellow1',size=4)+
  geom_point(data=star3,aes(a,b),color='yellow2',size=4)+
  geom_point(data=star4,aes(a,b),color='yellow3',size=4)+
  geom_point(data=star5,aes(a,b),color='yellow',size=4)+
  geom_point(data=star6,aes(a,b),color='yellow1',size=4)+
  geom_point(data=star7,aes(a,b),color='yellow2',size=4)+
  geom_point(data=star8,aes(a,b),color='yellow3',size=4)+
  geom_point(data=star9,aes(a,b),color='yellow',size=4)+
  geom_point(data=star10,aes(a,b),color='yellow1',size=4)+
  geom_point(data=star11,aes(a,b),color='yellow2',size=4)+
  geom_point(data=star12,aes(a,b),color='yellow3',size=4)+
  geom_path(data=b2, aes(x=x, y=value, group=B),size=0.7,alpha=10/B, color="lawngreen")+
  xlim(-1,10)+ylim(0,10)+
  theme(
    text = element_blank(),
    line = element_blank(),
    title = element_blank(),
    legend.position="none",
    panel.border = element_blank(),
    panel.background = element_blank())

library(plotly)
ggplotly()