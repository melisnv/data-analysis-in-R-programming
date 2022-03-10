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






