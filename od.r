#import dataset
data<-read.csv(file.choose(),header=T)


# Scaling data for the Neural network model [0,1] i.e x'=(x-a)/(b-a)
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

#Split data into 2 subset(train and test) test=>2013-2017 train=>1986-2012
train<-scaled[1:130,]
test<-scaled[131:155,]

#loading neural network library
library(neuralnet)

#creating formula that connect input and target 
n <- names(train)
f <- as.formula(paste("Rain ~", paste(n[!n %in% "Rain"], collapse = " + ")))

#training the neural network
nn <- neuralnet (f, data=train, hidden=c(3,2),linear.output=T)

# Visual plot of the model
plot(nn)

# Predict the test data (2013-2017)
pr.nn <- compute(nn, test[,2:21])
pr.nn$net.result

# Results from NN are normalized (scaled)
# Descaling for comparison
pr.nn_ <- pr.nn$net.result*(max(dat$Rain)-min(dat$Rain))+min(dat$Rain)
test.r <- (test$Rain)*(max(dat$Rain)-min(dat$Rain))+min(dat$Rain)

# correlation between predicted nn result and observed
actual_pred<-data.frame(pred<-pr.nn_,actual<-test.r)
cor(actual_pred)

# Calculating Mean square error (MSE)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test)
RMSE<-sqrt(MSE.nn)
MAE<-sum((test.r - pr.nn_))/nrow(test)

write.csv(actual_pred,file="SOKOTOANN.csv",row.names=FALSE)
#......plotting rainfall amount
require(ggplot2)
p<-ggplot(data=data21,aes(x=year,y=Rain,group=1))+
  geom_line(color="green")+
  geom_point(size=1)+
  xlab("(June to October, 1986-2016)")+
  ylab("Rainfall amount (mm)")

p+theme(axis.title.x=element_text(size = 12,face="bold"),
        axis.title.y=element_text(size = 12,face="bold"),
        axis.text=element_text(size=12),
        axis.line =element_line(colour = "white"))
        


#...plotting barplot of observed against pedicted
months<-rep(c("june","july","august","september","october"))
observed<-c(124.9,	115.6,	81.5,	301.1,	343.9)

predicted<-c(120.3,	120.5,	72.6,	286.1,	312.4)

values<-c(observed,predicted)
legend<-c(rep("observed",5),rep("predicted",5))
mydata<-data.frame(months,values)

mydata$months<-factor(mydata$months,levels=c("june","july","august","september","october"))


p<-ggplot(mydata,aes(months,values))+
  geom_bar(stat = "identity",aes(fill=legend),position="dodge")+
  xlab("Months")+
  ylab("Rainfall amount (mm)")+ylim(0,400)

p+theme(axis.title.x=element_text(size = 12,face="bold"),
        axis.title.y=element_text(size = 12,face="bold"),
        axis.text=element_text(size=12))
        
coeff=coefficients(reg)
eq=paste0("y=",round(coeff[2],1),"*x + ",round(coeff[1],1))
sp+geom_abline(intercept=-46.63498,slope=1.37521)+
  ggtitle(eq)
#..........................scatter plot
g<-read.csv(file.choose(),header=T)
require(stats)
reg<-lm(obs~pred,data=g)
reg


sp<-ggplot(data=g,aes(x=obs,y=pred))+geom_point()+
  xlab("Observed Rainfall")+
  ylab("Predicted Rainfall")

g1=sp+geom_abline(intercept=11.193391,slope=1.092719,color="blue",
               linetype="solid",
               size=0.8)+xlim(0,500)+ylim(0,400)+
  theme(axis.title.x=element_text(size = 10,face="bold"),
        axis.title.y=element_text(size = 10,face="bold"),
        axis.text=element_text(size=10,face="bold"))
#.................................................................
h<-read.csv(file.choose(),header = T)
#..........................scatter plot
require(stats)
reg<-lm(obs~pred,data=h)
reg

coeff=coefficients(reg)
eq=paste0("y=",round(coeff[2],1),"*x + ",round(coeff[1],1))

s<-ggplot(data=h,aes(x=obs,y=pred))+geom_point()+
  xlab("Observed Rainfall")+
  ylab("Predicted Rainfall")

labs(caption="(scatter plot of ANN Model result)")

s+geom_abline(intercept=-0.305299,slope=1.269471)+
  ggtitle(eq)

g2=s+geom_abline(intercept=-0.305299,slope=1.269471,color="blue",
                  linetype="solid",
                  size=0.8)+ylim(0,500)+
  theme(axis.title.x=element_text(size = 10,face="bold"),
        axis.title.y=element_text(size = 10,face="bold"),
        axis.text=element_text(size=10,face="bold"))

grid.arrange(g1, g2, ncol=2, nrow =1)        




#..........................plotting scaled training data from ANN model
df<-read.csv(file.choose(),header=T)
ggplot(df,aes(x,y=value,color=variable))+geom_line(aes(y=y1,col="target output"))+ geom_line(aes(y=y2,col="MLMA"))+
  xlab("NUMBER  OF SCALED TRAINING DATA SAMPLESS")+
  ylab("SCALED BAUCHI RAINFALL")+xlim(0,150)+
  theme(axis.title.x=element_text(size = 10,face="bold"),
        axis.title.y=element_text(size = 10,face="bold"),
        axis.text=element_text(size=10),
        axis.line =element_line(colour = "black"))
data<-read.csv(file.choose(),header=True)

