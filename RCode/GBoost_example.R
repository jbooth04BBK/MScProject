library(xgboost)
library(ggplot2)
library(reshape2)
library(Ecdat)

set.seed(1)
N = 1000
k = 10
x = matrix(rnorm(N*k),N,k)
b = (-1)^(1:k)
yaux=(x%*%b)^2
e = rnorm(N)
y=yaux+e

# = select train and test indexes = #
train=sample(1:N,800)
test=setdiff(1:N,train)

# = parameters = #
# = eta candidates = #
eta=c(0.05,0.1,0.2,0.5,1)
# = colsample_bylevel candidates = #
cs=c(1/3,2/3,1)
# = max_depth candidates = #
md=c(2,4,6,10)
# = sub_sample candidates = #
ss=c(0.25,0.5,0.75,1)

# = standard model is the second/third value of each vector above = #
standard=c(2,2,3,2)

# = train and test data = #
xtrain = x[train,]
ytrain = y[train]
xtest = x[test,]
ytest = y[test]

set.seed(1)
conv_eta = matrix(NA,500,length(eta))
pred_eta = matrix(NA,length(test), length(eta))
colnames(conv_eta) = colnames(pred_eta) = eta
for(i in 1:length(eta)){
  params=list(eta = eta[i], colsample_bylevel=cs[standard[2]],
              subsample = ss[standard[4]], max_depth = md[standard[3]],
              min_child_weigth = 1)
  xgb=xgboost(xtrain, label = ytrain, nrounds = 500, params = params)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = predict(xgb, xtest)
}

conv_eta = data.frame(iter=1:500, conv_eta)
conv_eta = melt(conv_eta, id.vars = "iter")
ggplot(data = conv_eta) + geom_line(aes(x = iter, y = value, color = variable))
