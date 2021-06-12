########Applied Statistic HW1###########
library(ggpubr)
library(ggplot2)
########### Q1 ############
########### 1 ############
#pdf
normal <-seq(0,200,1)

f1 <- function(n){
  z <- 0
  for (i in 1:n){
    x <- sample(c(0,1), replace=TRUE, size=200)
    y <- sum(x)
    z[i] <- y
  }
  df <- as.data.frame(z, stringsAsFactors=FALSE)
  a <- 0:200
  b <- dbinom(a, size = 200, prob = 0.5)
  c <- data.frame(bx=a,by=b)
  ggplot(df,aes(x=z)) +
    geom_line(data=c, aes(x=bx, y=by), col = 'orange', size=1,linetype = 4) +  
    stat_function(fun = dnorm, n = 200, args = list(mean = 100, sqrt(50)), col = 'skyblue', size=1
                  , linetype = 2) +
    geom_histogram(aes(y = stat(density)),alpha = 0.2, fill = 'pink', col = 'grey') +
    ggtitle(paste('PDF Plot of Binomial & Normal Distribution of N = ', n,
                  sep='')) +
    xlab("Numbers of Head") + ylab("Probability") + 
    theme(
      plot.title = element_text(size=14, face="bold"),
      axis.title.x = element_text(size=12),
      axis.title.y = element_text(size=12)
    ) + 
    xlim(60,140) +
    annotate("text", x = 120, y = 0.05, 
             label = paste("Binomial Experiment"), size = 4) +
    annotate("segment", x = 132, xend = 137, y = 0.05, yend = 0.05,
             colour = "pink", size = 1) +
    annotate("text", x = 120, y = 0.04, 
             label = paste("Binomial Distribution"), size = 4) +
    annotate("segment", x = 132, xend = 137, y = 0.04, yend = 0.04,
             colour = "orange", size = 1,linetype=4) +
    annotate("text", x = 120, y = 0.03, 
             label = paste("Normal Distribution"), size = 4) +
    annotate("segment", x = 132, xend = 137, y = 0.03, yend = 0.03,
             colour = "skyblue", size = 1, linetype=2)
  
}

ggarrange(f1(100),f1(1000),f1(10000),ncol=2,nrow=2)



#cdf

R<-function(x,y){
  z<-0
  z<-sum((x-y)^2)/length(x)
  return(z)
}#构造求R的函数

f1c<-function(n){
  x<-seq(0,200,1)#取值范围为【0，200】
  data<-matrix(0,nrow=n,ncol=200)
  x1<-rep(0,n)
  for(i in 1:n){
    data[i,]<-sample(c(0,1),size=200,replace=T)
    x1[i]<-sum(data[i,])
  }
  x1<-sort(x1)
  y<-rep(0,length(x))
  for(j in 1:length(x)){
    for(i in 1:n){
      if(x[j]<=x1[i]){y[j]<-((i-1)/n)
      break}
      if(x[j]>x1[n]){y[j]<-(1)
      break}
    }
  }
  df <- data.frame(aesx=c(0:200),aesy=y)
  test <-   R(y,pnorm(x,mean=100,sd=sqrt(50)))#求R
  ggplot(df,aes(x=aesx,y=aesy)) +
    geom_line(col = 'orange',size = 2, alpha = 0.8) + 
    stat_function(fun = pnorm, n = 200, args = list(mean = 100, sqrt(50)), col = 'skyblue'
                  , size=2, alpha = 0.8) + 
    ggtitle(paste('CDF Plot of Binomial & Normal Distribution of N = ', n,
                  sep='')) +
    xlab("Numbers of Head") + ylab("Cumulative Probability") + 
    theme(
      plot.title = element_text(size=14, face="bold"),
      axis.title.x = element_text(size=12),
      axis.title.y = element_text(size=12)
    ) + 
    annotate("text", x = 25, y = 0.3, 
             label = paste('R= ',round(test,6),sep=''), size = 4) +
    annotate("text", x = 25, y = 0.75, 
             label = paste("Binomial Experiment"), size = 4) +
    annotate("segment", x = 60, xend = 75, y = 0.75, yend = 0.75,
             colour = "orange", size = 2) +
    annotate("text", x = 25, y = 0.5, 
             label = paste("Normal Distribution"), size = 4) +
    annotate("segment", x = 60, xend = 75, y = 0.5, yend = 0.5,
             colour = "skyblue", size = 2)
}

ggarrange(f1c(100),f1c(1000),f1c(10000),ncol=2,nrow=2)

########### 2 ############
sim <- 10000
data2<-matrix(0, nrow=sim, ncol=200)
for (i in 1:sim){
  for (j in 1:200){
    data2[i, j] = sample(c(0,1),replace = TRUE, size = 1)
  }
}
f2<-function(x){
  max<-0
  temp<-0
  for(i in 1:length(x)){
    if(x[i]==0){temp=0}
    if(x[i]==1){temp<-temp+1}
    if(max<=temp){max=temp}
  }
  return(max)
}

t <- numeric(sim)
for (i in 1:sim){
  t[i] <- f2(data2[i,])
}

df2 <- as.data.frame(t)

p2_1 <- ggplot(df2, aes(t, colour = 'Blue')) + 
  scale_color_manual(values=c('#00BBFF')) +
  stat_ecdf(geom = "step",size=4) + 
  ggtitle('CDF Plot of the Longest Run by Experiment') +
  xlab("Numbers of the Longest Runs") + ylab("Cumulative Probability") +
  theme(legend.position="none") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12)
  )

###
data2_2 <- matrix(0, nrow = 201, ncol = 200)
for (x in 1:201){
  data2_2[x,1] = 0
  for (n in 1:200){
    if (n <= (x-1))
      data2_2[x,n] <- (2^n)
    else
      data2_2[x,n]<-sum(data2_2[(x),(n-x):(n-1)])
  }
}
t2 <- data2_2[,200]/(2^200)

df2_2 <- data.frame(aesx=c(1:30),aesy=t2[1:30])
p2_2 <- ggplot(df2_2,aes(x=aesx,y=aesy)) +
  geom_line(col = 'orange',size = 1, alpha = 0.8) + 
  ggtitle('CDF Plot of the Longest by Probability') +
  xlab("Numbers of the Longest Run") + ylab("Cumulative Probability") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12)
  )

ggarrange(p2_1,p2_2, ncol = 2)

########### 3 ############

sim <- 10000
data<-matrix(0, nrow=sim, ncol=200)
for (i in 1:sim){
  for (j in 1:200){
    data[i, j] = sample(c(0,1),replace = TRUE, size = 1)
  }
}
f<-function(x){
  max<-0
  temp<-0
  for(i in 1:length(x)){
    if(x[i]==0){temp=0}
    if(x[i]==1){temp<-temp+1}
    if(max<=temp){max=temp}
  }
  return(max)
}
t <- numeric(sim)
for (i in 1:sim){
  t[i] <- f(data[i,])
}

ff<-function(x){
  leter<-c(0,0,0)
  temp<-0
  for(i in 1:length(x)){
    if(x[i]==0){temp=0}
    if(x[i]==1){temp<-temp+1}
    if(temp==5){leter[1]<-leter[1]+1}
    if(temp==6){leter[2]<-leter[2]+1}
    if(temp==7){leter[3]<-leter[3]+1}
  }
  leter[1]<-leter[1]-leter[2]
  leter[2]<-leter[2]-leter[3]
  return(leter)
}

count5<-c()
count6<-c()
count7<-c()

for(i in 1:sim){
  count5[i]<-ff(data[i,])[1]
  count6[i]<-ff(data[i,])[2]
  count7[i]<-ff(data[i,])[3]
}

count5<-sort(count5)
count6<-sort(count6)
count7<-sort(count7)



p1 <- ggplot(as.data.frame(count5), aes(count5, colour = 'Blue')) + 
  scale_color_manual(values=c('#00BBFF')) +
  scale_size_manual(values=c(2)) +
  stat_ecdf(geom = "step") + 
  ggtitle('CDF Plot of the Length = 5') +
  xlab("Numbers of the Longest Runs") + ylab("Cumulative Probability") +
  theme(legend.position="none") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12)
  )

p2 <- ggplot(as.data.frame(count6), aes(count6, colour = 'Purple')) + 
  scale_color_manual(values=c('#B088FF')) +
  scale_size_manual(values=c(2)) +
  stat_ecdf(geom = "step") + 
  ggtitle('CDF Plot of the Length = 6') +
  xlab("Numbers of the Longest Runs") + ylab("Cumulative Probability") +
  theme(legend.position="none") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12)
  )

p3 <- ggplot(as.data.frame(count7), aes(count7, colour = 'Green')) + 
  scale_color_manual(values=c('#00AA55')) +
  scale_size_manual(values=c(2)) +
  stat_ecdf(geom = "step") + 
  ggtitle('CDF Plot of the Length >= 7') +
  xlab("Numbers of the Longest Runs") + ylab("Cumulative Probability") +
  theme(legend.position="none") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12)
  )

ggarrange(p1,p2,p3,ncol=2,nrow=2)

########### 4 ############

f4<-function(n){
  data<-matrix(0,nrow=n,ncol=200)
  n0<-rep(0,n)
  n1<-rep(0,n)
  n2<-rep(0,n)
  n3<-rep(0,n)
  X<-rep(0,n)
  for(i in 1:n){
    data[i,]<-sample(c(0,1),size=200,replace=T)
    for(j in 1:100){
      if(data[i,2*j-1]==0 && data[i,2*j]==0){
        n0[i]=n0[i]+1
      }
      else if(data[i,2*j-1]==0 && data[i,2*j]==1){
        n1[i]=n1[i]+1
      }
      else if(data[i,2*j-1]==1 && data[i,2*j]==0){
        n2[i]=n2[i]+1
      }
      else if(data[i,2*j-1]==1 && data[i,2*j]==1){
        n3[i]=n3[i]+1
      }
      X[i]=sum((n0[i]-25)^2/25,(n1[i]-25)^2/25,(n2[i]-25)^2/25,(n3[i]-25)^2/25)}
  }
  X<-sort(X)
  x<-seq(0,20,0.1)
  y<-rep(0,length(x))
  for(j in 1:length(x)){
    for(i in 1:n){
      if(x[j]<=X[i]){y[j]<-((i-1)/n)
      break}
      if(x[j]>X[n]){y[j]<-(1)
      break}
    }
  }
  df <- data.frame(col1=c(1:201),col2=y, col3 = pchisq(x,3))
  test <-   R(y,pchisq(x,3))#求R
  ggplot(df) +
    geom_line(aes(col1,col2),col = 'orange',size = 1, alpha = 0.8) + 
    geom_line(aes(col1,col3),col = 'skyblue',size = 1, alpha = 0.8) + 
    ggtitle(paste('CDF Plot of Experiment and Qui-square', n,
                  sep='')) +
    xlab("Numbers of X") + ylab("Cumulative Probability") + 
    theme(
      plot.title = element_text(size=14, face="bold"),
      axis.title.x = element_text(size=12),
      axis.title.y = element_text(size=12)
    )  + 
    annotate("text", x = 130, y = 0.3, 
             label = paste('R= ',round(test,6),sep=''), size = 4) +
    annotate("text", x = 130, y = 0.5, 
             label = paste("Experiment"), size = 4) +
    annotate("segment", x = 160, xend = 165, y = 0.5, yend = 0.5,
             colour = "orange", size = 1) +
    annotate("text", x = 130, y = 0.4, 
             label = paste("Qui-square"), size = 4) +
    annotate("segment", x = 160, xend = 165, y = 0.4, yend = 0.4,
             colour = "skyblue", size = 1)
}


ggarrange(f4(100),f4(1000),f4(10000), ncol = 2, nrow = 2)




########### Q2 ############

sim <- 100000
f5 <- function(n,col){
data <- matrix(0, nrow = sim, ncol = 100)
for (i in 1:sim){
  x <-sort(rnorm(100, 0, 1), decreasing = T)
  for (j in 1:100){
    data[i, j]<-x[j]
  }
}
df <- as.data.frame(data)
ggplot(df, aes(data[,n], colour = 'Blue')) + 
  scale_color_manual(values=col) +
  scale_size_manual(values=c(2)) +
  stat_ecdf(geom = "step") + 
  ggtitle(paste('CDF Plot of ', n,'th X',
                sep='')) +
  xlab("Numbers of the Longest Runs") + ylab("Cumulative Probability") +
  theme(legend.position="none") + 
  theme(
    plot.title = element_text(size=14, face="bold"),
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12)
  )
}

ggarrange(f5(1,'#00BBFF'),f5(2,'#B088FF'),f5(3,'#EE7700'),f5(10,'#00AAAA'),
          f5(20,'#FF88C2'),f5(50,'#227700'),f5(80,'#C63300'),f5(98,'#FFB3FF'),
          f5(99,'#FFCC22'),f5(100,'#880000'),ncol=3,nrow=4)

data <- matrix(0, nrow = sim, ncol = 100)
for (i in 1:sim){
  x <-sort(rnorm(100, 0, 1), decreasing = T)
  for (j in 1:100){
    data[i, j]<-x[j]
  }
}
table <- data.frame(OrderStatistic=c('X1','X2','X3','X10','X20','X50','X80','X98','X99','X100'),
                    Expectation=c(mean(data[,1]),mean(data[,2]),mean(data[,3]),mean(data[,10]),
                                  mean(data[,20]),mean(data[,50]),mean(data[,80]),mean(data[,98]),
                                  mean(data[,99]),mean(data[,100])),
                    Variation=c(var(data[,1]),var(data[,2]),var(data[,3]),var(data[,10]),
                                var(data[,20]),var(data[,50]),var(data[,80]),var(data[,98]),
                                var(data[,99]),var(data[,100])))

cat("X1的期望约为",mean(data[,1]),"，方差约为：",var(data[,1]),"\n")
cat("X2的期望约为",mean(data[,2]),"，方差约为：",var(data[,2]),"\n")
cat("X3的期望约为",mean(data[,3]),"，方差约为：",var(data[,3]),"\n")
cat("X10的期望约为",mean(data[,10]),"，方差约为：",var(data[,10]),"\n")
cat("X20的期望约为",mean(data[,20]),"，方差约为：",var(data[,20]),"\n")
cat("X50的期望约为",mean(data[,50]),"，方差约为：",var(data[,50]),"\n")
cat("X80的期望约为",mean(data[,80]),"，方差约为：",var(data[,80]),"\n")
cat("X98的期望约为",mean(data[,98]),"，方差约为：",var(data[,98]),"\n")
cat("X99的期望约为",mean(data[,99]),"，方差约为：",var(data[,99]),"\n")
cat("X100的期望约为",mean(data[,100]),"，方差约为：",var(data[,100]),"\n")

