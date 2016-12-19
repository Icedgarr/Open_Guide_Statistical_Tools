library(MASS)

datafile <- read.table("synthetic_regression.txt", header=TRUE, nrow=300)[,1:31]


#USE LM

linear<-lm(t~.,data=datafile)

H_diag<-lm.influence(linear)$hat #leverage
change_coef<-lm.influence(linear)$coefficients


#PLOT ERROR BARS

#Define t and Phi.
t<-datafile[,1]
Phi<-data.matrix(datafile[,2:31])
Phi<-cbind(rep(1,nrow(Phi)),Phi)


#mle_estimation<-function(Phi, t){

#Compute the AUX variable Aphi as solve(t(Phi)*Phi)*t(Phi).
inverse<-solve(t(Phi)%*%Phi)
Aphi<-inverse%*%t(Phi)
#Define the estimator of w, the matrix H and the vector of errors e.
N<-nrow(Phi)
w<-Aphi%*%t
H<-Phi%*%Aphi
e<-(diag(N)-H)%*%t
#Get the standard errors of w.
qMLE<-as.numeric((1/N*t(e)%*%e)^(-1))
w_var<-solve((qMLE*t(Phi)%*%Phi))
w_se<-sqrt(diag(w_var))


#}

#Get the error bars.
library(ggplot2)

errorbars<-aes(ymin=w-w_se*1.96,ymax=w+w_se*1.96)

#Plot the coefficients.
plotwMLE<-as.data.frame(cbind(c(1:length(w)),w))
colnames(plotwMLE)<-c("Coeff_ID","Coeff_Value")

ggplot(data=plotwMLE,aes(x=Coeff_ID,y=Coeff_Value))+
  geom_point()+
  geom_errorbar(errorbars)+
  ggtitle("Estimated coefficients")



#PLOT STANDARIZED RESIDUALS

#Compute leverage and standarized errors.
Leverage<-diag(H)  

std_residuals<-sqrt(qMLE/(1-diag(H)))*e

fitted<-H%*%t

plot2 <- as.data.frame(cbind(fitted, std_residuals, Leverage))

ggplot(data=plot2, 
       aes(x=fitted, y=std_residuals))+
  geom_point(aes(colour=Leverage>3*nrow(w)/N))+
  scale_color_manual(values=c("black","red"))+
  labs(colour="High leverage")+
  xlab("Fitted values")+
  ylab("Standarised residuals")+
  ggtitle("Standarised residuals VS fitted values")

ggplot(data=as.data.frame(cbind(Leverage,abs(std_residuals))),aes(x=Leverage,y=abs(std_residuals)))+
  geom_point(aes(colour=Leverage>3*31/300))+
  scale_color_manual(values=c("black","red"))+
  labs(colour="High leverage")+
  xlab("Leverage")+
  ylab("abs(Standarised residuals)")+
  ggtitle("Abs(Standarised residuals) VS Leverage")


#PLOT QUANTILES OF STANDARD NORMAL VS RESIDUALS

qqnorm(std_residuals)
qqline(std_residuals)

