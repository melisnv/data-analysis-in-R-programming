library(glmnet)

mtcars # mpg is the response

x = as.matrix(mtcars[,-1]) # remove the response variable
y = mtcars[,1] # response variables

train = sample(1:nrow(x),0.67*nrow(x)) # train by using 2/3 of the x rows
x.train = x[train,] ; y.train = y[train] # data to train
x.test = x[-train,] ; y.test = y[-train] # data to test the prediction quality

# Prediction by using the linear model
# First fit linear model on the train data
attach(mtcars)
lm.model = lm(mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb, data = mtcars, subset = train)
y.predict.lm = predict(lm.model, newdata = mtcars[-train,]) # predict for the test rows
mse.lm = mean((y.test - y.predict.lm)^2) ; # prediction quality by the linear model
mse.lm

# Now applying lasso for selecting the variables and prediction
lasso.model = glmnet(x.train, y.train, alpha = 1) # alpha = 1 for lasso
# more options standardize = TRUE, intercept = FALSE, nlambda = 1000

lasso.cv = cv.glmnet(x.train, y.train, alpha = 1, type.measure = "mse", nfolds = 5)

plot(lasso.model, label = T, xvar = "lambda")
# standardize = T, type.coef = "2norm", xvar = "norm"
# plot(lasso.cv$glmnet.fit, xvar ="lambda", label = T) # The same plot

plot(lasso.cv) # smallest lambda
plot(lasso.cv$glmnet.fit, xvar ="lambda", label = T)

lambda.min = lasso.cv$lambda.min 
lambda.1se = lasso.cv$lambda.1se
lambda.min; lambda.1se # best lambda by cross validation 
coef(lasso.model, s= lasso.cv$lambda.min)
coef(lasso.model, s= lasso.cv$lambda.1se)

# lambda.min is the value of lambda that gives minimum mean cross-validated 
# error. The other lambda saved is lambda.1se, which gives the most regularized 
# model such that error is within one standard error of the minimum. 

lasso.pred1=predict(lasso.model,s=lambda.min,newx=x.test) 
lasso.pred2=predict(lasso.model,s=lambda.1se,newx=as.matrix(x.test))
mse1.lasso=mean((y.test-lasso.pred1)^2); mse1.lasso
mse2.lasso=mean((y.test-lasso.pred2)^2); mse2.lasso

# By default, the glmnet function standardizes all the independent 
# variables, but here the dependent variable can also be standardized 
# by the function standardize=function(x){(x-mean(x))/sd(x)}).
# Then one may want not to include an intercept in lm mand glmnet models, 
# because all the variables have already been standardized to a mean of zero. 




