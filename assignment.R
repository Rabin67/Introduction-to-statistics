
library(rsample)
library(matlib)
library(ggplot2)

#Here we set working directory which is C:\MSC\Statistics\Assignment
setwd("C:/MSC/Statistics/Assignment")
getwd()

#reading data from X.csv in matrix form and setting column names
X_value = read.csv(file = "X.csv",header = FALSE)
X_value
X_matrix = as.matrix(X_value)
colnames(X_matrix) = c("X1","X2","X3","X4")
X_matrix 


#reading data from Y.csv in matrix form and setting column name
Y_value = read.csv(file ="Y.csv" , header = FALSE)
Y_value
Y_matrix = as.matrix(Y_value)
colnames(Y_matrix) = c("Y")
Y_matrix

#reading data from Time.csv in matrix form and setting column name
Time_value = read.csv(file = "Time.csv", header = FALSE)
Time_value
Time_matrix = as.matrix(Time_value)
colnames(Time_matrix) = c("Time")
Time_matrix

#Plotting time Series of input EEG signal
X.ts = ts(data = X_matrix, start =c( min(Time_matrix), max(Time_matrix)), frequency = 1)
plot(X.ts, main="Time series plot of input  EEG signal", xlab="Time", ylab=" Input EEG signal", col="blue")

X1.ts = ts(data = X_matrix[,"X1"], start =c( min(Time_matrix), max(Time_matrix)), frequency = 1)
plot(X1.ts, main="Time series plot of input  EEG signal X1", xlab="Time", ylab=" Input EEG signal X1", col="blue")

X2.ts = ts(data = X_matrix[,"X2"], start =c( min(Time_matrix), max(Time_matrix)), frequency = 1)
plot(X2.ts, main="Time series plot of input  EEG signal X2", xlab="Time", ylab=" Input EEG signal X2", col="blue")

X3.ts = ts(data = X_matrix[,"X3"], start =c( min(Time_matrix), max(Time_matrix)), frequency = 1)
plot(X3.ts, main="Time series plot of input  EEG signal X3", xlab="Time", ylab=" Input EEG signal X3", col="blue")

X4.ts = ts(data = X_matrix[,"X4"], start =c( min(Time_matrix), max(Time_matrix)), frequency = 1)
plot(X4.ts, main="Time series plot of input  EEG signal X4", xlab="Time", ylab=" Input EEG signal X4", col="blue")

#Plotting time Series of output EEG signal
Y.ts = ts (data = Y_matrix, start=c(min(Time_matrix),max(Time_matrix)), frequency = 1)
plot(Y.ts, main="Time series plot of output  EEG signal", xlab="Time", ylab=" Output EEG signal", col="blue")

#distribution for EEG signal
#Plotting distribution of entire input EEG signal
dist_X = density(X_matrix)
plot(dist_X,main = "Density Plot of Input signal", col="blue", lwd="2", ylab="input EEG")
hist(X_matrix, freq = FALSE, main = " Histogram and Density plot of input EEG signal",xlab="input EEG Signal" )
lines(dist_X,col="blue",lwd="2")
rug(jitter(X_matrix))

#Plotting distribution of  input EEG signal X1               
dist_X1 = density(X_matrix[,"X1"])
plot(dist_X1, main="Density plot of input signal X1", col="blue", lwd="2")
hist(X_matrix[,"X1"], xlab="Input EEG signal X1", freq = FALSE, main = " Histogram and Density plot of input EEG signal X1")
lines(dist_X1,col="blue",lwd="2")
rug(jitter(X_matrix[,"X1"]))

#Plotting distribution of  input EEG signal X2               
dist_X2 = density(X_matrix[,"X2"])
plot(dist_X2, main="Density plot of input signal X2", col="blue", lwd="2")
hist(X_matrix[,"X2"], xlab="Input EEG signal X2", freq = FALSE, main = " Histogram and Density plot of input EEG signal X2")
lines(dist_X2,col="blue",lwd="2")
rug(jitter(X_matrix[,"X2"]))

#Plotting distribution of  input EEG signal X3               
dist_X3 = density(X_matrix[,"X3"])
plot(dist_X3, main="Density plot of input signal X3", col="blue", lwd="2")
hist(X_matrix[,"X3"], xlab="Input EEG signal X3", freq = FALSE, main = " Histogram and Density plot of input EEG signal X3")
lines(dist_X3,col="blue",lwd="2")
rug(jitter(X_matrix[,"X3"]))

#Plotting distribution of  input EEG signal X4               
dist_X4 = density(X_matrix[,"X4"])
plot(dist_X4, main="Density plot of input signal X4", col="blue", lwd="2")
hist(X_matrix[,"X4"], xlab="Input EEG signal X4", freq = FALSE, main = " Histogram and Density plot of input EEG signal X4")
lines(dist_X4,col="blue", lwd ="2")
rug(jitter(X_matrix[,"X4"]))

#plotting distribution of output EEG signal
dist_Y = density(Y_matrix)
plot(dist_Y, main="Density plot of Output signal", col="blue", lwd="2")
hist(Y_matrix, xlab="Output EEG signal", freq = FALSE, main = " Histogram and Density plot of Output EEG signal")
lines(dist_Y,col="blue",lwd="2")
rug(jitter(Y_matrix))

#correlation and scatter plot of input signal X1 and output signal Y
plot(X_matrix[,"X1"],Y_matrix, pch=16, col="blue",main = "scatter plot of input signal X1 and output signal",
     xlab = "Input EEG signal X1", ylab = "Output EEg signal")
cor(X_matrix[,"X1"],Y_matrix)

#correlation and scatter plot of input signal X2 and output signal Y
plot(X_matrix[,"X2"],Y_matrix, col="blue",pch=16, main = "scatter plot of input signal X2 and output signal", 
     xlab = "Input EEG signal X2", ylab = "Output EEg signal")
cor(X_matrix[,"X2"],Y_matrix)

#correlation and scatter plot of input signal X3 and output signal Y
plot(X_matrix[,"X3"],Y_matrix, col="blue", pch=16, main = "scatter plot of input signal X3 and output signal", 
     xlab = "Input EEG signal X3", ylab = "Output EEg signal")
cor(X_matrix[,"X3"],Y_matrix)

#correlation and scatter plot of input signal X4 and output signal Y
plot(X_matrix[,"X4"],Y_matrix, col="blue",pch=16, main = "scatter plot of input signal X4 and output EEG signal", 
     xlab = "Input EEG signal X4", ylab = "Output EEg signal")
cor(X_matrix[,"X4"],Y_matrix)

#Task2 Regression: modelling the relationship between EEG signals
#creating a matrix of ones to be added in matrix representation of models to calculate thetahat
ap = matrix(1, length(X_matrix)/4,1)

#matrix representation of model1
X_mod1 = cbind(ap,(X_matrix[,"X4"]),(X_matrix[,"X1"])^2,(X_matrix[,"X1"])^3,(X_matrix[,"X2"])^4,(X_matrix[,"X1"])^4) 
X_mod1
#thetahat calculation of Model1
X_mod1_thetahat = solve(t(X_mod1)%*%X_mod1)%*%t(X_mod1)%*%Y_matrix
X_mod1_thetahat

#matrix representation of model2
X_mod2 = cbind(ap,(X_matrix[,"X4"]),(X_matrix[,"X1"])^3,(X_matrix[,"X3"])^4) 
X_mod2
#thetahat calculation of Model2
X_mod2_thetahat = solve(t(X_mod2)%*%X_mod2)%*%t(X_mod2)%*%Y_matrix
X_mod2_thetahat

#matrix representation of model3
X_mod3 = cbind(ap,(X_matrix[,"X3"])^3,(X_matrix[,"X3"])^4)
X_mod3
#thetahat calculation of Model3
X_mod3_thetahat = solve(t(X_mod3)%*%X_mod3)%*%t(X_mod3)%*%Y_matrix
X_mod3_thetahat

#matrix representation of model4
X_mod4 = cbind(ap,(X_matrix[,"X2"]),(X_matrix[,"X1"])^3,(X_matrix[,"X3"])^4) 
X_mod4
#thetahat calculation of Model4
X_mod4_thetahat = solve(t(X_mod4)%*%X_mod4)%*%t(X_mod4)%*%Y_matrix
X_mod4_thetahat

#matrix representation of model5
X_mod5 = cbind(ap,(X_matrix[,"X4"]),(X_matrix[,"X1"])^2,(X_matrix[,"X1"])^3,(X_matrix[,"X3"])^4) 
X_mod5

#thetahat calculation of Model5
X_mod5_thetahat = solve(t(X_mod5)%*%X_mod5)%*%t(X_mod5)%*%Y_matrix
X_mod5_thetahat

#Residual Sum of Squares Calculation
# Model1 yhat and RSS calculation
yhat_mod1 = X_mod1%*%X_mod1_thetahat
yhat_mod1
mod1_RSS = sum((Y_matrix-yhat_mod1)^2)
mod1_RSS

# Model2 yhat and RSS calculation
yhat_mod2 = X_mod2%*%X_mod2_thetahat
yhat_mod2
mod2_RSS = sum((Y_matrix-yhat_mod2)^2)
mod2_RSS

# Model3 yhat and RSS calculation
yhat_mod3 = X_mod3%*%X_mod3_thetahat
yhat_mod3
mod3_RSS = sum((Y_matrix-yhat_mod3)^2)
mod3_RSS

# Model4 yhat and RSS calculation
yhat_mod4 = X_mod4%*%X_mod4_thetahat
yhat_mod4
mod4_RSS = sum((Y_matrix-yhat_mod4)^2)
mod4_RSS

# Model5 yhat and RSS calculation
yhat_mod5 = X_mod5%*%X_mod5_thetahat
yhat_mod5
mod5_RSS = sum((Y_matrix-yhat_mod5)^2)
mod5_RSS

#Log likelihood function for candidate models
#variance calculation of model 1
n = length(Y_matrix)
n
var_mod1 = mod1_RSS/(n-1)
var_mod1
#log likelihood calculation of model1
likehd_mod1 = -((n/2)*log(2*pi))-((n/2)*log(var_mod1))-((1/(2*var_mod1))*mod1_RSS)
likehd_mod1
#variance calculation of model 2
var_mod2 = mod2_RSS/(n-1)
var_mod2
#log likelihood calculation of model2
likehd_mod2 = -((n/2)*log(2*pi))-((n/2)*log(var_mod2))-((1/(2*var_mod2))*mod2_RSS)
likehd_mod2
#variance calculation of model 3
var_mod3 = mod3_RSS/(n-1)
var_mod3
#log likelihood calculation of model2
likehd_mod3 = -((n/2)*log(2*pi))-((n/2)*log(var_mod3))-((1/(2*var_mod3))*mod3_RSS)
likehd_mod3
#variance calculation of model 4
var_mod4 = mod4_RSS/(n-1)
var_mod4
#log likelihood calculation of model2
likehd_mod4 = -((n/2)*log(2*pi))-((n/2)*log(var_mod4))-((1/(2*var_mod4))*mod4_RSS)
likehd_mod4
#variance calculation of model 4
var_mod5 = mod5_RSS/(n-1)
var_mod5
#log likelihood calculation of model2
likehd_mod5 = -((n/2)*log(2*pi))-((n/2)*log(var_mod5))-((1/(2*var_mod5))*mod5_RSS)
likehd_mod5


#Akaike information criterion (AIC) and Bayesian  Information Criterion(BIC)
#Akaike information criterion (AIC) calculation of model1
K_mod1 = length(X_mod1_thetahat) 
K_mod1
AIC_mod1 = (2*K_mod1)-(2*likehd_mod1)
AIC_mod1
#Bayesian  Information Criterion(BIC) calculation of model1
BIC_mod1 = (log(n)*K_mod1)-(2*likehd_mod1)
BIC_mod1
#Akaike information criterion (AIC) calculation of model2
K_mod2 = length(X_mod2_thetahat) 
K_mod2
AIC_mod2 = (2*K_mod2)-(2*likehd_mod2)
AIC_mod2
#Bayesian  Information Criterion(BIC) calculation of model3
BIC_mod2 = (log(n)*K_mod2)-(2*likehd_mod2)
BIC_mod2
#Akaike information criterion (AIC) calculation of model3
K_mod3 = length(X_mod3_thetahat) 
K_mod3
AIC_mod3 = (2*K_mod3)-(2*likehd_mod3)
AIC_mod3
#Bayesian  Information Criterion(BIC) calculation of model3
BIC_mod3 = (log(n)*K_mod3)-(2*likehd_mod3)
BIC_mod3
#Akaike information criterion (AIC) calculation of model4
K_mod4 = length(X_mod4_thetahat) 
K_mod4
AIC_mod4 = (2*K_mod4)-(2*likehd_mod4)
AIC_mod4
#Bayesian  Information Criterion(BIC) calculation of model4
BIC_mod4 = (log(n)*K_mod4)-(2*likehd_mod4)
BIC_mod4

#Akaike information criterion (AIC) calculation of model5
K_mod5 = length(X_mod5_thetahat) 
K_mod4
AIC_mod5 = (2*K_mod5)-(2*likehd_mod5)
AIC_mod5

#Bayesian  Information Criterion(BIC) calculation of model5
BIC_mod5 = (log(n)*K_mod5)-(2*likehd_mod5)
BIC_mod5

#model prediction errors (Residuals) for candidate models
# error in model 1
err_mod1 = Y_matrix-yhat_mod1
err_mod1
#QQplot and qqline for model 1

qqnorm(err_mod1,main="QQ plot for model 1 error",col="blue")
qqline(err_mod1,col="red",lwd="2")
# error in model 2
err_mod2 = Y_matrix-yhat_mod2
err_mod2
#QQplot and qqline for model 2
qqnorm(err_mod2,main="QQ plot for model 2 error",col="blue")
qqline(err_mod2,col="red",lwd="2")
# error in model 3
err_mod3 = Y_matrix-yhat_mod3
err_mod3
#QQplot and qqline for model 3
qqnorm(err_mod3,main="QQ plot for model 3 error",col="blue")
qqline(err_mod3,col="red",lwd="2")
# error in model 4
err_mod4 = Y_matrix-yhat_mod4
err_mod4
#QQplot and qqline for model 4
qqnorm(err_mod4,main="QQ plot for model 4 error",col="blue")
qqline(err_mod4,col="red",lwd="2")

# error in model 5
err_mod5 = Y_matrix-yhat_mod5
err_mod5
#QQplot and qqline for model 5
qqnorm(err_mod5,main="QQ plot for model 5 error",col="blue")
qqline(err_mod5,col="red",lwd="2")

#Task 2.7 model validation using training and testing
# Binding input matrix and output matrix 
X_Y_matrix = cbind(X_matrix,Y_matrix)
set.seed(1353)
#splitting dataset into testing and training 
r_split_obj_XY = initial_split(data = as.data.frame(X_Y_matrix),prop=.7)
r_split_obj_XY
#calculating training dataset of input and output signals
XY_train = as.matrix(training(r_split_obj_XY))
XY_train
#calculating testing dataset of input and output signals
XY_test = as.matrix(testing(r_split_obj_XY))
XY_test
# Estimating model parameters using Training set 
train_one = matrix(1 , nrow(XY_train),1) 
train_one
training_thetahat=solve(t(X_training_model) %*% X_training_model) %*% t(X_training_model) %*% XY_training_set[,"Y"]
XY_train_mod = cbind(train_one,XY_train[,"X4"], XY_train[,"X1"]^3,XY_train[,"X3"]^4)
XY_train_mod
# estimated model parameters from selected model 2
train_thetahat = solve(t(XY_train_mod)%*%XY_train_mod)%*%t(XY_train_mod)%*%XY_train[,"Y"]
train_thetahat 
# Model output
#creating X_matrix testing model 
test_one = matrix(1, length(XY_test[,"X1"]), 1)
test_one
#selecting X_test variables needed as per model 2
X_test_mod = cbind(test_one,XY_test[,"X4"],(XY_test[,"X1"])^3,(XY_test[ ,"X3"])^4) 
X_test_mod
#model prediction/output calculation
Y_test_hat = X_test_mod%*% train_thetahat 
Y_test_hat 
#residual error calculation on model output
RSS_test=sum((XY_test[,"Y"]-Y_test_hat)^2) 
RSS_test

#calculation of 95% confidence intervals of predicted model
smp_std = sd(Y_test_hat)
smp_size = nrow(Y_test_hat)
smp_mean = mean(Y_test_hat)

#T-test
# for n-1, 61-1=60 degree of freedom t=2, from t-table
t = 2     
ci_L2 = smp_mean + t * (smp_std/sqrt(smp_size))
ci_L2
ci_L1 = smp_mean - t * (smp_std/sqrt(smp_size))
ci_L1
#table with confidence intervals calculated for Y-testing-hat for errorplot
XY_dataset = as.data.frame.matrix(cbind(XY_test[,"Y"],  Y_test_hat, Y_test_hat 
                                        + ci_L1, Y_test_hat + ci_L2))
colnames(XY_dataset) = c("Y","Yhat","limit_low","limit_up")
XY_dataset

ggplot(XY_dataset, aes(x=row.names(XY_dataset)))+
  geom_point(aes(y = Yhat))+
  geom_errorbar(aes(ymin = limit_low, ymax=limit_up))+
  theme(axis.text.x = element_text(angle = 90))

plot(density(XY_train[,"Y"]), main="Distribution of Training data of Output EEg signal")
abline(v=mean(XY_train[,"Y"]) , lty=2, lwd=1)
abline(v=ci_L1, col="red", lty=2)
abline(v=ci_L2, col="red", lty=2)




#Task 3: Approximate Bayesian Computation (ABC)
#parameters and constant selection for carrying out Rejection ABC
arry_1=0
arry_2=0
rdev_mat1=0
rdev_mat2=0
count_ = 0
#model 2 theta hat values
theta_bias = 0.483065688 #selected parameter1 (p1)
theta_1 = 0.143578928 #selected parameter2 (p2)
theta_2 = 0.010038614
theta_3 = -0.001912836
the_Epsilon = mod2_RSS*2
#y hat calculation
for (j in 1:100) {
  p1_deviation = runif(1,-0.483065688,0.483065688)
  p2_deviation = runif(1,-0.143578928,0.143578928)
  mod2_new_thetahat = matrix(c(p1_deviation,p2_deviation,theta_2,theta_3))
  mod2_new_Y_hat = X_mod2%*%mod2_new_thetahat
  mod2_new_RSS = sum((Y_matrix-mod2_new_Y_hat)^2)
  mod2_new_RSS
  if(mod2_new_RSS>the_Epsilon){
    arry_1[j]=p1_deviation
    arry_2[j]=p2_deviation
    count_ = count_+1
    rdev_mat1=matrix(arry_1)
    rdev_mat2=matrix(arry_2)
  }
}
rdev_mat1
rdev_mat2
plot (rdev_mat1,rdev_mat2,pch=16, col = c("red","blue"), 
      main="Joint and Marginal Posterior Distribution", 
      xlab="posterior distribution parameter1", 
      ylab="posterior distribution parameter 2")
