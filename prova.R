
setwd("~/Desktop/BGSE/14D001_Statistical_Modeling_and_Inference/EXAM")

#Load data
data <- read.csv("data.csv")

#Create plot
p1<-ggplot(data=data,aes(x=score.1,y=score.2))+
  geom_point(aes(colour=as.factor(label)))+
  scale_color_manual(values=c("red","green"))+
  labs(colour="Admitted")+
  xlab("Score 1")+
  ylab("Score 2")+
  ggtitle("Admission vs score 1 and score 2")

#Predictor variables
X <- as.matrix(data[,c(1,2)])

#Add ones to X
X <- cbind(rep(1,nrow(X)),X)

#Response variable
Y <- as.matrix(data$label)

#define the sigmoid function
sigmoid <- function(z)
{
  g <- 1/(1+exp(-z))
  return(g)
}


#Cost Function
cost <- function(theta)
{
  m <- nrow(X)
  g <- sigmoid(X%*%theta)
  J <- (1/m)*sum((-Y*log(g)) - ((1-Y)*log(1-g)))
  return(J)
}

#Intial theta
initial_theta <- rep(0,ncol(X))

#Cost at inital theta
cost(initial_theta)

# Derive theta using gradient descent using optim function
theta_optim <- optim(par=initial_theta,fn=cost)

#set theta
theta <- theta_optim$par

#cost at optimal value of the theta
theta_optim$value

# probability of admission for student
prob <- sigmoid(t(c(1,45,85))%*%theta)

library(ggplot2)

#Create plot
ggplot(data$score.1,data$score.2,col=as.factor(data$label),xlab="Score-1",ylab="Score-2")

fy<-function(x,w0,w1,w2){
  return((-w0-w1*x)/w2)
}

x<-seq(25,101,by=1)
y<-fy(x,theta[1],theta[2],theta[3])

l<-as.data.frame(cbind(x,y))



ggplot()+
  geom_point(data=data,aes(x=score.1,y=score.2,colour=as.factor(label)))+
  scale_color_manual(values=c("red","green"))+
  geom_line(data=l,aes(x=x,y=y))+
  labs(colour="Admitted")+
  xlab("Score 1")+
  ylab("Score 2")+
  ggtitle("Admission vs score 1 and score 2")








#LASSO

# use crossvalidation to find the best lambda
library(glmnet)
cv <- cv.glmnet(x,y,alpha=1,nfolds=10)
l <- cv$lambda.min
alpha=1

plot(cv, xvar='lambda') #log Lambda against the coefficients

library(plotmo) # labels the coef
plot_glmnet(cv)

plot(model$glmnet.fit, "norm",   label=TRUE)
plot(model$glmnet.fit, "lambda", label=TRUE)

# fit the model
fits <- glmnet( x, y, family="gaussian", alpha=alpha, nlambda=100)
res <- predict(fits, s=l, type="coefficients")

# Stepwise regression
model.ls <- lm(y~ ., data=data)
rss.ls <- sum(model.ls$resid^2)/model.ls$df.residual

model.backward <- step(model.ls, direction="backward")
rss.backward <- sum(model.backward$resid^2)/model.backward$df.residual
