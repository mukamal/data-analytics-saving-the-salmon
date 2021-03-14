library(C50)
library(gmodels)
library(partykit)
library(RColorBrewer)
library(dplyr)
library(rpart)
library(rpart.plot)
#install.packages("rlang")
library(rlang)
library(readr)
library(e1071)                    # load e1071 
library(caret)
library(tibble)
library(tidyr)
library(readxl)


###################################Exploring Data ########################################
data <- read_excel("C:/Users/19294/OneDrive/S/Uni/DA/P/Final/trees_Final.xls")
str(data)
summary(data)
data<- as.data.frame(data)


# prints out indeces with NA and num of NA.
checkNa <- function(df){
  df<-as.data.frame(df)
  c<-0
  for(i in 1:length(df)) {
    
    if(length(which(is.na(df[,i])))>0){
      print(paste(colnames(df)[i],"has",length(which(is.na(df[,i]))),"NAs. That is",round(100*length(which(is.na(df[,i])))/length(df[,i]),2),"%" ))
      print(summary(df[,i]))
      c<-c+1
    }
  }
  if(c==0)
    print("There are No NA's")
  
}

checkNa(data)



# check for outliers:
checkOut <- function(df){
  for(i in 1:length(df)) {
    if (mode(df[,i])=="numeric") {
      print(i)
      if(abs(skewness(df[,i])>.5)){  
        print(paste(colnames(df)[i],"May have an outlier" ))
        plot(df[,i],main=colnames(df)[i])    
      }
    }
  }
}


checkOut(data)
hist(data$lengthinmonths)
hist(data$public)

#factorize all char attributes

dataF<-data
summary(factor(dataF$watercouncils ))
dataF$StartYear<-factor(data$StartYear)
dataF$public<-factor(data$public)
dataF$watercouncils<-factor(data$watercouncils)

str(dataF)

#######################################Data Munging######################################

#labelling dependent variable
min<- min(dataF$lcostperoutput)
max<-max(dataF$lcostperoutput)
avg<- mean(dataF$lcostperoutput)
(c1<-(min+avg)/2)
(c2<-(max+avg)/2)

#dataF$lcostperoutput = cut(data$lcostperoutput, breaks = c(-Inf,c1,c2,Inf), 
#                           labels = c("LowC", "MidC", "HighC"))

dataF$lcostperoutput = cut(data$lcostperoutput, breaks = c(-Inf,7.53,8.7,Inf), 
                           labels = c("LowC", "MidC", "HighC"))


summary(dataF$lcostperoutput)

#Remove and adjust attributes as necessary.
dataR<- dataF %>%
  select(-c(PROJNUM,goalflag,exp,sq,ppsq, StartYear))


str(dataR)



########################################## Exploring ML Models ###########################

#Build a decision tree model 


# Inspecting our classifications
table(dataR$lcostperoutput)/nrow(dataR)*100


# Splitting our data into train and test sets
train_len<- as.integer(nrow(dataR)*.8)
test_len<-nrow(dataR)- train_len


set.seed(3)

train_sample <- sample(nrow(dataR), train_len)

train_set <- dataR[train_sample, ]
test_set <- dataR[-train_sample, ]


# Inspect the classifications of our train and test sets
table(train_set$lcostperoutput)/train_len*100
table(test_set$lcostperoutput)/test_len*100



# Train our model

set.seed(6)
tree_1 <- rpart(lcostperoutput ~ ., 
                data = train_set,method = 'class')

summary(tree_1)

prp(tree_1, 
    faclen = 0, 
    cex = 0.5, 
    extra = 3)  

rpart.plot(tree_1, extra = 106)





cred_conf_matrix_1 <- table(test_set$lcostperoutput,
                            predict(tree_1,
                                    test_set,
                                    type="class"))

rownames(cred_conf_matrix_1) <- paste("Actual", rownames(cred_conf_matrix_1), sep = ":")
colnames(cred_conf_matrix_1) <- paste("Pred", colnames(cred_conf_matrix_1), sep = ":")


print(paste('Accuracy for test',(accuracy_DT_1 <- sum(diag(cred_conf_matrix_1)) / sum(cred_conf_matrix_1))))

# Precision (Positive Predictive Value = TP / (TP + FP))
# when a model predicts the positive class, how often is it right?

(precision_1 <- diag(cred_conf_matrix_1) / rowSums(cred_conf_matrix_1))

# A search engine with a high recall returns a large number of documents pertinent to the search
(recall_1 <- diag(cred_conf_matrix_1) / colSums(cred_conf_matrix_1))


# F-Measure
F_measure_1 <- (2*precision_1*recall_1)/(precision_1 + recall_1)
F_measure_1


printcp(tree_1)

min_xerror <- tree_1$cptable[,"xerror"] %>% 
  which.min()

bestcp <- tree_1$cptable[min_xerror,"CP"]


# Step 3: Prune the tree using the best cp.
tree.pruned <- prune(tree_1, cp = bestcp)

prp(tree.pruned, 
    faclen = 0, 
    cex = 0.8, 
    extra = 1)  
rpart.plot(tree.pruned, extra = 106)

cred_conf_matrix_2 <- table(test_set$lcostperoutput,
                            predict(tree.pruned,
                                    test_set,
                                    type="class"))


rownames(cred_conf_matrix_2) <- paste("Actual", rownames(cred_conf_matrix_2), sep = ":")
colnames(cred_conf_matrix_2) <- paste("Pred", colnames(cred_conf_matrix_2), sep = ":")


print(paste('Accuracy for test',(accuracy_DT_2 <- sum(diag(cred_conf_matrix_2)) / sum(cred_conf_matrix_2))))

# Precision (Positive Predictive Value = TP / (TP + FP))
# when a model predicts the positive class, how often is it right?

(Precision_2 <- diag(cred_conf_matrix_2) / rowSums(cred_conf_matrix_2))

# A search engine with a high recall returns a large number of documents pertinent to the search
(Recall_2 <- diag(cred_conf_matrix_2) / colSums(cred_conf_matrix_2))



# F-Measure
F_measure_2 <- (2*Precision_2*Recall_2)/(Precision_2 + Recall_2)
F_measure_2


#################################################Naive Bayes##################################

sms_classifier <- naiveBayes(train_set, train_set$lcostperoutput)

sms_test_pred <- predict(sms_classifier, test_set)


cred_conf_matrix_4 <- table(test_set$lcostperoutput,
                            sms_test_pred)


rownames(cred_conf_matrix_4) <- paste("Actual", rownames(cred_conf_matrix_4), sep = ":")
colnames(cred_conf_matrix_4) <- paste("Pred", colnames(cred_conf_matrix_4), sep = ":")

print(paste('Accuracy for test',(accuracy_4 <- sum(diag(cred_conf_matrix_4)) / sum(cred_conf_matrix_4))))

# Precision (Positive Predictive Value = TP / (TP + FP))
# when a model predicts the positive class, how often is it right?

(Precision_4 <- diag(cred_conf_matrix_4) / rowSums(cred_conf_matrix_4))

# A search engine with a high recall returns a large number of documents pertinent to the search
(Recall_4 <- diag(cred_conf_matrix_4) / colSums(cred_conf_matrix_4))



# F-Measure
F_measure_4 <- (2*Precision_4*Recall_4)/(Precision_4 + Recall_4)
F_measure_4


#Tune the parameters, such as the discretization options, to compare results. 
str(dataR)

hist(dataR$lengthinmonths)
mean(dataR$inkindperc )

m<-mean(dataR$lengthinmonths)
m2<-mean(dataR$inkindperc)


dataDezc <- dataR %>%
  mutate(lengthinmonths = cut(lengthinmonths, breaks = c(-Inf,mean(c(min(dataR$lengthinmonths),m)),mean(c(m,max(dataR$lengthinmonths))),Inf), labels = c("Low", "Mid", "High")),
         inkindperc   = cut(inkindperc  , breaks = c(-Inf,mean(c(min(dataR$inkindperc),m2)),mean(c(m2,max(dataR$inkindperc))),Inf), labels = c("Low", "Mid", "High"))
        )
str(dataDezc)         


train_set_D <- dataDezc[train_sample, ]
test_set_D <- dataDezc[-train_sample, ]



sms_classifier_D <- naiveBayes(train_set_D, train_set_D$lcostperoutput)

sms_test_pred_D <- predict(sms_classifier_D, test_set_D)


cred_conf_matrix_5 <- table(test_set_D$lcostperoutput,
                            sms_test_pred_D)


rownames(cred_conf_matrix_5) <- paste("Actual", rownames(cred_conf_matrix_5), sep = ":")
colnames(cred_conf_matrix_5) <- paste("Pred", colnames(cred_conf_matrix_5), sep = ":")
print(paste('Accuracy for test',(accuracy_5 <- sum(diag(cred_conf_matrix_5)) / sum(cred_conf_matrix_5))))

# Precision (Positive Predictive Value = TP / (TP + FP))
# when a model predicts the positive class, how often is it right?

(Precision_5 <- diag(cred_conf_matrix_5) / rowSums(cred_conf_matrix_5))

# A search engine with a high recall returns a large number of documents pertinent to the search
(Recall_5 <- diag(cred_conf_matrix_5) / colSums(cred_conf_matrix_5))



# F-Measure
F_measure_5 <- (2*Precision_5*Recall_5)/(Precision_5 + Recall_5)
F_measure_5



####################################################3 SVM ##########################################################33

#install.packages("fastDummies")
library(fastDummies)
norm_min_max <-function(x) { 
  ( x - min(x) ) / ( max(x) - min(x) )
}


dataF$ppcount<-factor(data$ppcount)
dataF$SuperTypecount<-factor(data$SuperTypecount)
#Remove and adjust attributes as necessary.
dataR<- dataF %>%
  select(-c(PROJNUM,goalflag,exp,sq,ppsq))

str(dataR)
summary(dataR$ppcount)


results <- fastDummies::dummy_cols(dataR)
summary(results)
colnames(dataR)
colnames(results)

data8<- results %>%
    select(StartYear_1996,StartYear_1998,StartYear_1999,StartYear_2000,StartYear_2002, StartYear_2007, StartYear_2009,
#    select(
           lengthinmonths, public,watercouncils,inkindperc,
          ppcount_8, ppcount_13,
           SuperTypecount_3,SuperTypecount_4, SuperTypecount_5, SuperTypecount_6,
           lcostperoutput)

data8 <- data8 %>%
  mutate(lengthinmonths = norm_min_max(lengthinmonths),
         inkindperc = norm_min_max(inkindperc))
  

str(data8)


train_set_8 <- data8[train_sample, ]
test_set_8 <- data8[-train_sample, ]


test_labels <- data8[-train_sample,] %>% select(lcostperoutput) # Extracting test labels
test_set_final  <- data8[-train_sample,] %>% select(-lcostperoutput) 
dim(test_labels)

# Fitting SVM to the Training set 
classifier <- svm(formula = lcostperoutput ~ .,
                  data = train_set_8,
                  type = 'C-classification',
                  kernel = 'linear') 

# Predicting the Test set results 
churn_pred <- predict(classifier, 
                    newdata = test_set_final)










cred_conf_matrix_svm <- table(test_set_8$lcostperoutput,
                            churn_pred)



rownames(cred_conf_matrix_svm) <- paste("Actual", rownames(cred_conf_matrix_svm), sep = ":")
colnames(cred_conf_matrix_svm) <- paste("Pred", colnames(cred_conf_matrix_svm), sep = ":")

print(paste('Accuracy for test',(aaccuracy_Test_svm <- sum(diag(cred_conf_matrix_svm)) / sum(cred_conf_matrix_svm))))

# Precision (Positive Predictive Value = TP / (TP + FP))
# when a model predicts the positive class, how often is it right?

(Precision_svm <- diag(cred_conf_matrix_svm) / rowSums(cred_conf_matrix_svm))

# A search engine with a high recall returns a large number of documents pertinent to the search
(Recall_svm <- diag(cred_conf_matrix_svm) / colSums(cred_conf_matrix_svm))

# F-Measure
F_measure_svm <- (2*Precision_svm*Recall_svm)/(Precision_svm + Recall_svm)
F_measure_svm





#################################################### KNN ##########################################################33

# Setting our model parameters and creating our model
ctrl <- trainControl(method="repeatedcv",
                     repeats = 3)

churn_knn <- train(lcostperoutput ~ ., 
                    data = train_set_8, 
                    method = "knn", 
                    trControl = ctrl, 
                    tuneLength = 20)


churn_knn


plot(churn_knn)


# Generating predictions on the test data        
knnPredict <- predict(churn_knn,
                      newdata = test_set_final )




cred_conf_matrix_knn <- table(test_set_8$lcostperoutput,
                              knnPredict)



rownames(cred_conf_matrix_knn) <- paste("Actual", rownames(cred_conf_matrix_knn), sep = ":")
colnames(cred_conf_matrix_knn) <- paste("Pred", colnames(cred_conf_matrix_knn), sep = ":")

accuracy_Test_knn <- sum(diag(cred_conf_matrix_knn)) / sum(cred_conf_matrix_knn)

print(paste('Accuracy for test',(aaccuracy_Test_knn <- sum(diag(cred_conf_matrix_knn)) / sum(cred_conf_matrix_knn))))

# Precision (Positive Predictive Value = TP / (TP + FP))
# when a model predicts the positive class, how often is it right?

(Precision_knn <- diag(cred_conf_matrix_knn) / rowSums(cred_conf_matrix_knn))

# A search engine with a high recall returns a large number of documents pertinent to the search
(Recall_knn <- diag(cred_conf_matrix_knn) / colSums(cred_conf_matrix_knn))



# F-Measure
F_measure_knn <- (2*Precision_knn*Recall_knn)/(Precision_knn + Recall_knn)
F_measure_knn

################################################## PAM Clustering ################################ 
library(dplyr) # for data cleaning
library(ISLR) # for college dataset
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) 


dataR<- dataF %>%
  select(-c(PROJNUM,goalflag,exp,sq,ppsq, StartYear))

glimpse(dataR)
t_12<- dataR
# Remove label before clustering

gower_dist <- daisy(t_12[, -6],
                    metric = "gower",
                    type = list(asymm = c("public","watercouncils"),
                                ordratio= c("ppcount","SuperTypecount"),
                                logratio= c("lengthinmonths","inkindperc")
                                
                    ))


summary(gower_dist)



gower_mat <- as.matrix(gower_dist)

# Output most similar pair

t_12[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]




# Output most dissimilar pair

t_12[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]





#pam_fit <- pam(gower_dist, diss = TRUE, k = 3)
pam_fit <- pam(gower_dist, diss = TRUE, k = 3,cluster.only=TRUE)

pam_fit

pam_results <- t_12 %>%
  dplyr::select(-lcostperoutput) %>%
  mutate(cluster = pam_fit) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary






tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  #  mutate(cluster = factor(pam_fit$clustering),
  mutate(cluster = factor(pam_fit),
         
         lcostperoutput = t_12$lcostperoutput)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))




cred_conf_matrix_1<- table(pam_fit,t_12$lcostperoutput)
cred_conf_matrix_1

accuracy_Test_1 <- sum(diag(cred_conf_matrix_1)) / sum(cred_conf_matrix_1)
accuracy_Test_1*100

(Precision <- diag(cred_conf_matrix_1) / rowSums(cred_conf_matrix_1))

(Recall <- diag(cred_conf_matrix_1) / colSums(cred_conf_matrix_1))

F_measure <- (2*Precision*Recall)/(Precision + Recall)
F_measure

