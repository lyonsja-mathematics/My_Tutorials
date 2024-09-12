library(reshape2)
library(ggplot2)
library(dplyr)
library(mclust)
library(pwr)
library(tidyr)

# Import data
session_times=read.csv("https://github.com/gedeck/practical-statistics-for-data-scientists/blob/master/data/web_page_data.csv?raw=TRUE") %>% mutate(Time=60*Time); head(session_times)

lung=read.csv("https://github.com/gedeck/practical-statistics-for-data-scientists/blob/master/data/LungDisease.csv?raw=TRUE") %>% sample_n(1e4)

house=read.csv("https://github.com/gedeck/practical-statistics-for-data-scientists/blob/master/data/house_sales.csv?raw=TRUE", sep = "\t") %>% sample_n(1e4)

loan_data=data.table::fread("https://github.com/gedeck/practical-statistics-for-data-scientists/blob/master/data/loan_data.csv.gz?raw=TRUE") %>% sample_n(1e4) %>% select(-1) %>% mutate(outcome=factor(outcome)) %>% as.data.frame()

sp500=data.table::fread("https://github.com/gedeck/practical-statistics-for-data-scientists/blob/master/data/sp500_data.csv.gz?raw=TRUE") %>% as.data.frame(); View(head(sp500,10))

sp500_sectors=read.csv("https://github.com/gedeck/practical-statistics-for-data-scientists/blob/master/data/sp500_sectors.csv?raw=TRUE"); View(head(sp500_sectors))

dbinom(x=0, size=200, p=0.02)
pbinom(q = 5, size = 2e2, prob = 2e-2)
rpois(n = 100, lambda = 2)

# Student's t-test
t.test(Time~Page, data=session_times, alternative="less")

effectSize=ES.h(p1=1.21e-2, p2=1.1e-2)
pwr.2p.test(h=effectSize, sig.level=5e-2, power=0.5, alternative = "greater")

head(lung)
model.1=lm(PEFR~Exposure, data=lung); summary(model.1)

lung %>% mutate(Predicted.PEFR=predict(model.1), Residual=residuals(model.1)) %>% head

head(house)

house_lm = lm(AdjSalePrice~SqFtTotLiving+SqFtLot+Bathrooms+Bedrooms+BldgGrade, data=house, na.action = na.omit); summary(house_lm)

update(object=house_lm, .~. -SqFtLot) -> house_lm; summary(house_lm)

house_full = lm(AdjSalePrice~SqFtTotLiving+SqFtLot+Bathrooms+Bedrooms+BldgGrade+PropertyType+NbrLivingUnits+SqFtFinBasement+YrBuilt+YrRenovated+NewConstruction, data=house, na.action = na.omit); summary(house_full)

step=MASS::stepAIC(house_full, direction="both"); step

predictionInterval=predict(object=step, interval="prediction", level=0.95); head(predictionInterval)
house=cbind(house, predictionInterval)

house %>% mutate(Year = lubridate::year(DocumentDate), Weight=Year-2005) -> house

house_wt = lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms + BldgGrade, data=house, weight=Weight); summary(house_wt)

round(cbind(house_lm=house_lm$coefficients,house_wt=house_wt$coefficients), digits=3)

predict(object=step, newdata=house, interval="prediction", level=0.95) %>% head

prop_type_dummies=model.matrix(~PropertyType-1, data=house); head(prop_type_dummies)

lm(AdjSalePrice~SqFtTotLiving+SqFtLot+Bathrooms+Bedrooms+BldgGrade+PropertyType, data=house, na.action = na.omit)

house %>% mutate(resid=residuals(house_lm)) %>% group_by(ZipCode) %>% summarize(med_resid=median(resid), cnt=n()) %>% arrange(med_resid) %>% mutate(cum_cnt=cumsum(cnt), ZipGroup=ntile(cum_cnt, 5)) -> zip_groups

house %>% left_join(select(zip_groups, ZipCode, ZipGroup), by="ZipCode") -> house

update(step, .~. + SqFtLot - SqFtFinBasement - YrBuilt + ZipGroup)

lm(formula = AdjSalePrice ~ SqFtTotLiving*ZipGroup + SqFtLot + Bathrooms + Bedrooms + BldgGrade + PropertyType, data = house %>% mutate(ZipGroup=factor(ZipGroup)), na.action = na.omit)

house_98105=subset(house, ZipCode==98105)

lm_98105 = lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + Bedrooms + BldgGrade, data=house_98105); summary(lm_98105)

sresid=rstandard(lm_98105)
idx=order(sresid)
sresid[idx[1]]

house_98105[idx[1], ] %>% select(AdjSalePrice,SqFtTotLiving,SqFtLot,Bathrooms,Bedrooms,BldgGrade)

std_resid = rstandard(lm_98105)
Cooks_D = cooks.distance(lm_98105)
hat_values = hatvalues(lm_98105)

data.frame(hat_values, std_resid, Cooks_D) -> DF
  
  ggplot(mapping=aes(x=hat_values, y=std_resid)) + 
  
  geom_point(data=DF, size=1) + 
  
  geom_point(data=subset(DF, Cooks_D > 8e-2), size=10*sqrt(subset(Cooks_D, Cooks_D > 8e-2))) +
  
  labs(size="") + 
  
  theme(legend.position="none")

ggplot(data=data.frame(resid=residuals(lm_98105), pred=predict(lm_98105)), mapping=aes(x=pred, y=abs(resid))) + geom_point(size=4, color="Red3") + geom_smooth()

terms=predict(lm_98105, type="terms")
partial_resid=resid(lm_98105) + terms

data.frame(SqFtTotLiving=house_98105$SqFtTotLiving, Terms=terms[,"SqFtTotLiving"], PartialResid=partial_resid[,"SqFtTotLiving"]) %>% 
  
  ggplot(aes(x=SqFtTotLiving, y=PartialResid)) + 
  
  geom_point(size=3) + geom_smooth(linetype=2) + scale_shape(solid=FALSE) + 
  
  geom_line(aes(y=Terms)) + 
  
  theme(panel.background = element_blank(), axis.line = element_line(linewidth=2), text = element_text(size=16, family = "Modern"))

lm_poly2=lm(AdjSalePrice ~ poly(SqFtTotLiving, degree = 2) + SqFtLot + BldgGrade + Bathrooms + Bedrooms, data=house_98105); summary(lm_poly2)

knots=quantile(house_98105$SqFtTotLiving, p=c(0.25,0.5,0.75))
lm_spline=lm(AdjSalePrice ~ splines::bs(SqFtTotLiving, knots=knots, degree=3) + SqFtLot + Bathrooms + Bedrooms + BldgGrade, data=house_98105); summary(lm_spline)

lm_spline %>% update(.~. -Bedrooms -Bathrooms) %>% summary()

mgcv::gam(AdjSalePrice~s(SqFtTotLiving) + SqFtLot + Bathrooms + Bedrooms + BldgGrade, data=house_98105) -> lm_gam; summary(lm_gam)

klaR::NaiveBayes(outcome ~ purpose_ + home_ + emp_len_, data=loan_data %>% subset(complete.cases(outcome))) -> naive_model
naive_model$table

loan_data[147, c("purpose_","home_","emp_len_")] -> new_loan; row.names(new_loan) = NULL
predict(naive_model, new_loan)
loan_data[147,"outcome"]

loan_lda = MASS::lda(outcome ~ borrower_score + payment_inc_ratio, data=subset(loan_data,loan_amnt > 3e3)); loan_lda
head(predict(loan_lda)$posterior)

glm(outcome ~ purpose_ + payment_inc_ratio + home_ + emp_len_ + borrower_score, data=loan_data, family="binomial") -> logistic_model; summary(logistic_model)

logistic_model %>% predict -> pred; summary(pred)
prob=1/(1 + exp(-pred)); summary(prob)

logistic_gam = mgcv::gam(outcome ~ s(payment_inc_ratio) + purpose_ + home_ + emp_len_ + s(borrower_score), data=loan_data, family="binomial")
summary(logistic_gam)
predict(logistic_gam, type="terms") -> terms
partial_resid = resid(logistic_model) + terms
df = data.frame(payment_inc_ratio = loan_data$payment_inc_ratio, terms=terms[,"s(payment_inc_ratio)"], partial_resid=partial_resid[,"s(payment_inc_ratio)"])

ggplot(subset(df, payment_inc_ratio <= 25), aes(x=payment_inc_ratio, y=partial_resid, solid=FALSE)) + geom_point(shape=46, alpha=0.4) + geom_line(aes(y=terms), color="Red3", alpha=0.5, linewidth=2)

pred = predict(logistic_gam, newdata = loan_data)
as.numeric(pred[pred > 0]) -> pred_y
as.numeric(loan_data$outcome=="default") -> true_y
(true_y==1) & (pred_y==1)

i=sample(1:nrow(loan_data),1)

class::knn(
  train = loan_data[-i, c("dti","payment_inc_ratio")], 
  test = loan_data[i, c("dti","payment_inc_ratio")], 
  cl = loan_data[-i, "outcome"], 
  k=20
) -> pred; as.character(pred)

loan_data[i, "outcome"]

model.matrix(~ -1 + payment_inc_ratio + dti + revol_bal + revol_util, data=loan_data) -> loan_df
loan_df[1, , drop=FALSE] -> new_loan
loan_df[-1, ] -> loan_df
loan_data[-1, "outcome"] -> outcome
knn_pred=FNN::knn(train=loan_df, test=new_loan, cl=outcome, k=5); as.character(knn_pred)
loan_df[attr(x=knn_pred,which = "nn.index"),]

model.matrix(~ -1 + payment_inc_ratio + dti + revol_bal + revol_util, data=loan_data) -> loan_df
loan_std=as.data.frame(scale(loan_df))

FNN::knn(train=loan_std[-1, ], test=loan_std[1, , drop=FALSE], cl=loan_data[-1, "outcome"], k=5) -> knn_std_pred
loan_std[attr(knn_std_pred,"nn.index"),]

borrow_df <- model.matrix(~ -1 + dti + revol_bal + revol_util + open_acc + delinq_2yrs_zero + pub_rec_zero, data=loan_data)
borrow_knn <- FNN::knn(train=borrow_df, test=borrow_df, cl=loan_data[, 'outcome'], prob=TRUE, k=20)
prob <- attr(borrow_knn, "prob")
borrow_feature <- ifelse(borrow_knn == 'default', prob, 1 - prob); summary(borrow_feature)

sample_n(loan_data, 3e3) -> loan3000
library(rpart)
loan_tree <- rpart(outcome ~ borrower_score + payment_inc_ratio, data=loan3000, control=rpart.control(cp=0.005))
plot(loan_tree, uniform=TRUE, margin=0.05)
text(loan_tree)

library(randomForest)
rf <- randomForest(outcome ~ borrower_score + payment_inc_ratio, data=loan3000); rf

data.frame(error.rate=rf$err.rate[,"OOB"], num_trees=1:rf$ntree) %>% ggplot(aes(x=num_trees, y=1e2*error.rate)) + geom_line(size=2)

loan3000 %>% mutate(pred=predict(rf, prob=TRUE)) %>% ggplot(aes(x=borrower_score, y=payment_inc_ratio, shape=pred, color=pred, size=pred)) + geom_point()

rf_all=randomForest(outcome ~ ., data=loan_data, importance=TRUE); rf_all

varImpPlot(rf_all %>% update(.~. - status), type=1)
varImpPlot(rf_all, type=2)

# xgboost does not support the formula syntax, predictors need to be in matrix format.
predictors = data.matrix(loan_data[, c('borrower_score', 'payment_inc_ratio')])

# The response needs to be 0 or 1 variable.
label = ifelse(loan_data$outcome=="default", 1, 0)

# The objective tells xgboost what kind of function it is.
xgb <- xgboost::xgboost(data=predictors, label=label, objective="binary:logistic", params=list(subsample=0.63, eta=0.1), nrounds=100)

pred = predict(xgb, newdata=predictors)

xgb_df = loan_data %>% select(borrower_score,payment_inc_ratio,outcome) %>% cbind(pred_default = ifelse(pred > 0.5, "default", "paid off"), prob_default = pred) %>% mutate(outcome=factor(outcome), pred_default=factor(pred_default)); head(xgb_df)

confusion.matrix=data.frame(default=c(0,0), paid.off=c(0,0), row.names = c("default","paid.off"))

for(i in 1:nrow(xgb_df)){
  if(xgb_df[i, "outcome"]=="default" & xgb_df[i, "pred_default"]=="default"){
    confusion.matrix["default","default"] = confusion.matrix["default","default"] + 1 
  } else if(xgb_df[i, "outcome"]=="default" & xgb_df[i, "pred_default"]=="paid off"){
    confusion.matrix["paid.off","default"] = confusion.matrix["paid.off","default"] + 1 
  } else if(xgb_df[i, "outcome"]=="paid off" & xgb_df[i, "pred_default"]=="default"){
    confusion.matrix["default","paid.off"] = confusion.matrix["default","paid.off"] + 1 
  } else {
    confusion.matrix["paid.off","paid.off"] = confusion.matrix["paid.off","paid.off"] + 1 
  } 
}

# Accuracy
confusion.matrix[1,1]/nrow(xgb_df) + confusion.matrix[2,2]/nrow(xgb_df)

caret::confusionMatrix(data=xgb_df$outcome, reference=xgb_df$pred_default)

# Bookmark page 274

seed <- 400820

predictors = loan_data %>% select(-outcome) %>% data.matrix()

label = ifelse(loan_data$outcome=="default", 1, 0)

test_idx = sample(nrow(loan_data), 1000)

xgb_default = xgboost::xgboost(data=predictors[-test_idx,], label=label[-test_idx], objective='binary:logistic', nrounds=250, verbose=0)

# Error on the training set
as.numeric(xgb_default$evaluation_log[250,"train_logloss"])

pred_default <- predict(object=xgb_default, predictors[test_idx,])
error_default = ifelse(abs(label[test_idx] - pred_default) > 0.5, 1, 0)

# Error on the test set
mean(error_default)

xgb_penalty = xgboost::xgboost(data=predictors[-test_idx,], label=label[-test_idx], params=list(eta=.1, subsample=.63, lambda=1000), objective='binary:logistic', nrounds=250, verbose=0)

pred_penalty <- predict(xgb_penalty, predictors[test_idx,])

error_penalty <- ifelse(abs(label[test_idx] - pred_penalty) > 0.5,1,0)

# Training penalty
as.numeric(xgb_penalty$evaluation_log[250,"train_logloss"])

# Test penalty
mean(error_penalty)

error_default <- rep(0, 250)
error_penalty <- rep(0, 250)
for(i in 1:250){
  pred_def <- predict(xgb_default, predictors[test_idx,], ntreelimit=i)
  error_default[i] <- mean(abs(label[test_idx] - pred_def) >= 0.5)
  pred_pen <- predict(xgb_penalty, predictors[test_idx,], ntreelimit=i)
  error_penalty[i] <- mean(abs(label[test_idx] - pred_pen) >= 0.5)
}

errors <- rbind(
  
  data.frame(xgb_default$evaluation_log), 
  data.frame(xgb_penalty$evaluation_log),
  data.frame(iter=1:250, train_logloss=error_default), 
  data.frame(iter=1:250, train_logloss=error_penalty)
  
  )

errors$type <- rep(c('default train', 'penalty train', 'default test', 'penalty test'), rep(250, 4))

ggplot(
  
  data=errors, 
  aes(
    x=iter, 
    y=train_logloss, 
    group=type,
    linetype=type, 
    color=type
    
    )
  
  ) + 
  
  geom_line(size=2)

fold_number <- sample(1:5, nrow(loan_data), replace=TRUE)
params <- data.frame(eta = rep(c(0.1, 0.5, 0.9), 3), 
                     max_depth = rep(c(3, 6, 12), rep(3,3)))

error <- matrix(0, nrow=9, ncol=5)
for(i in 1:nrow(params)){
  for(k in 1:5){
    fold_idx <- (1:nrow(loan_data))[fold_number == k]
    xgb = xgboost::xgboost(data=predictors[-fold_idx,], 
                           label=label[-fold_idx],
                           params=list(eta=params[i, 'eta'],
                                       max_depth=params[i, 'max_depth']),
                           objective='binary:logistic', 
                           nrounds=100, 
                           verbose=0)
    
    pred <- predict(xgb, predictors[fold_idx,])
    error[i, k] <- mean(ifelse(abs(label[fold_idx] - pred) >= 0.5,1,0))
  }
}; rm(i,k)

error

# What is cross-validation?
# Split the data into K different groups, called folds. For each fold, a model is trained on the data not in the fold then tested on the data in the fold.

# What is the difference between supervised and unsupervised learning?
# Statistical methods that extract meaning from data without training a model on labelled data is called unsupervised learning. Supervised learning is when a model is trained on data to predict an outcome from a set of predictor variables.

# How does Principal Component Analysis work?
# The idea of PCA is to reduce the number of numerical predictor variables to a smaller set of predictors, called principal components, which are a weighted linear combination of the predictors. The principal components explain most of the variability of the full set of predictors and thereby reducing the dimension of the data.

oil_px <- sp500[, c('CVX', 'XOM')]
pca <- princomp(oil_px)
loadings=pca$loadings
ggplot(data=oil_px, aes(x=CVX, y=XOM)) +
  geom_point(alpha=.3) +
  stat_ellipse(type='norm', level=.99) +
  geom_abline(intercept = 0, slope = loadings[2,1]/loadings[1,1]) +
  geom_abline(intercept = 0, slope = loadings[2,2]/loadings[1,2])

# Chapter 7: Unsupervised Learning

oil_px <- sp500[, c('CVX', 'XOM')]
pca <- princomp(oil_px)
loadings=pca$loadings; loadings

ggplot(data=oil_px, aes(x=CVX, y=XOM)) +
  geom_point(alpha=.3) +
  stat_ellipse(type='norm', level=.99) +
  geom_abline(intercept = 0, slope = loadings[2,1]/loadings[1,1]) +
  geom_abline(intercept = 0, slope = loadings[2,2]/loadings[1,2])
# The dashed lines show the direction of the principal components.

syms = c( 'AAPL', 'MSFT', 'CSCO', 'INTC', 'CVX', 'XOM', 'SLB', 'COP', 'JPM', 'WFC', 'USB', 'AXP', 'WMT', 'TGT', 'HD', 'COST')
top_sp = sp500[row.names(sp500)>='2005-01-01', syms]
sp_pca = princomp(top_sp)
screeplot(sp_pca)

sp_pca$loadings[,1:3] %>% 
  as.data.frame() %>% 
  mutate(Symbol=row.names(.)) %>% 
  gather('Component', 'Weight', -Symbol) %>% 
  ggplot(aes(x=Symbol, y=Weight, fill=Weight)) +
  geom_bar(stat='identity') + theme(legend.position="none") +
  facet_grid(Component ~ ., scales='free_y')
# The loadings for the first principal component are all positive, this is because all columns in the data share a common factor, in this case the overall market trend.

df=sp500[row.names(sp500)>='2011-01-01', c('XOM', 'CVX')]
km=kmeans(df, centers=4)
df %>% mutate(cluster=factor(km$cluster)) -> df
df %>% sample_n(6)

centers=data.frame(cluster=factor(1:4), km$centers)
centers

ggplot(data=df, aes(x=XOM, y=CVX, color=cluster, shape=cluster)) +
  geom_point(alpha=.3) +
  geom_point(data=centers, aes(x=XOM, y=CVX), size=3, stroke=2)

data.frame(cluster=1:4, size=km$size)

df = sp500[row.names(sp500) >= '2011-01-01', c('XOM', 'CVX')]
mcl=Mclust(df)
summary(mcl)

cluster = factor(predict(mcl)$classification)
ggplot(data=df, aes(x=XOM, y=CVX, color=cluster, shape=cluster)) + geom_point(alpha=.8)

