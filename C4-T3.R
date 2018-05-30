setwd("C:/Users/user/Google Drive/WD")
getwd()

library(dplyr)
library(tidyr)
library(corrplot) # Visualization of a Correlation Matrix 
library(ggplot2)
library(arules)# discretize function
library(lattice)
library(caret)
library(C50)

#dataset
iphone_matrix <- read.csv("iPhoneLargeMatrix.csv")
samsung_matrix <- read.csv("GalaxyLargeMatrix.csv")
iphone_matrix$id <- NULL
samsung_matrix$id <- NULL
iphone_matrix <- tbl_df(iphone_matrix) #tibble 
samsung_matrix <- tbl_df(samsung_matrix)
class(iphone_matrix)
class(samsung_matrix)
glimpse(iphone_matrix)
glimpse(samsung_matrix)
summary(iphone_matrix)
summary(samsung_matrix)
anyNA(iphone_matrix)
anyNA(samsung_matrix)
dim(iphone_matrix)
dim(samsung_matrix)
#attributes()
#head(iphone_matrix)
#tail(iphone_matrix)
#names(iphone_matrix)


#Extracting columns realted to iPhone and Samsung without dependend variable (iPhoneSentiment)
iphone_and_samsung <- iphone_matrix %>% select(contains("iphone"), contains("samsung"), contains("ios"), contains("google")) 
iphone_and_samsung$iphoneSentiment <- NULL
glimpse(iphone_and_samsung)
summary(iphone_and_samsung)
iphone_samsung_pos_neg <- iphone_and_samsung %>% select(ends_with("pos"), ends_with("neg"))
glimpse(iphone_samsung_pos_neg)

means <- colMeans(iphone_samsung_pos_neg)
means

names <-  c("iphonecampos", "iphonedispos", "iphoneperpos", "samsungcampos", "samsungdispos", "samsungperpos", "osperpos", "googleperpos", "iphonecamneg", "iphonedisneg", "phoneperneg", "samsungcamneg", "samsungdisneg", "samsungperneg", "iosperneg", "googleperneg") 
their_means <- c(0.31042224, 0.45769370, 0.35116041, 0.15103496, 0.18437820, 0.12386517, 0.14027269, 0.05222673, 0.13825889, 0.24489122, 0.22158397, 0.07807600, 0.11316893, 0.08411739, 0.08626325, 0.04149747)
means2 <- data.frame(Feature = names(means), Mean = means, row.names = NULL)
means2
class(means2)
means2$Feature <- as.factor(means2$Feature)
str(means2)
means2

cam_pos <- means2[c(1,4),]
cam_pos$Phones <- c("iPhone", "Samsung")
cam_pos

cam_pos_plot <- ggplot(cam_pos, aes(x = Feature, y = Mean, fill = Phones)) + 
  geom_bar(stat = "identity") +
  ggtitle("Camera: mean of positive reviews") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(),  axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), panel.background = element_blank(), panel.grid = element_blank(), plot.background = element_blank(), strip.background = element_blank())
cam_pos_plot

cam_neg <- means2[c(9, 12),]
cam_neg
Phones <- c("iPhone", "Samsung")
cam_neg <- cbind(Phones, cam_neg)
cam_neg

cam_neg_plot <- ggplot(cam_neg, aes(x = Feature, y = Mean, fill = Phones)) + 
  geom_bar(stat = "identity") +
  ggtitle("Camera: mean of negative reviews") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(),  axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_blank(), panel.background = element_blank(), panel.grid = element_blank(), plot.background = element_blank(), strip.background = element_blank())
cam_neg_plot

#Correlation Matrix visualisation 
corrplot(cor(iphone_matrix), order = "hclust") 
corrplot(cor(samsung_matrix), order = "hclust" )

#Feature selection
#
#1 Method: running Random Forest or other decision tree 
#and check what varables have been used: predictors()
#Data has to be discretized: (arules package)
#Sample: 4000
#RF removes only 1 variable
#C5.0 left 38 predictors
iphone_disfixed7 <- discretize(iphone_matrix$iphoneSentiment, "fixed", categories = c(-Inf, -50, -10, -1, 1, 10, 50, Inf))
summary(iphone_disfixed7)
iphone_matrix_disc <- data.frame(iphone_matrix)
#tracemem(iphone_matrix_disc) == tracemem(iphone_matrix)
#untracemem(iphone_matrix_disc)
iphone_matrix_disc$iphoneSentiment <- iphone_disfixed7
glimpse(iphone_matrix_disc)

samsung_disfixed7 <- discretize(samsung_matrix$galaxySentiment, "fixed", categories = c(-Inf, -50, -10, -1, 1, 10, 50, Inf))
summary(samsung_disfixed7)
samsung_matrix_disc <- data.frame(samsung_matrix)
samsung_matrix_disc$galaxySentiment <- samsung_disfixed7
glimpse(samsung_matrix_disc)

#Feature Selection: The first method (RF)

iphone_sample_4000 <- iphone_matrix_disc[sample(1:nrow(iphone_matrix_disc), 4000, replace = FALSE),]
glimpse(iphone_sample_4000)

samsung_sample_4000 <- samsung_matrix_disc[sample(1:nrow(samsung_matrix_disc), 4000, replace = FALSE),]
glimpse(samsung_sample_4000)

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = T)
iphone_sample_RF <- train(iphoneSentiment~., data = iphone_sample_4000, method = "rf", trControl = ctrl)
iphone_sample_RF

predictors1 <- predictors(iphone_sample_RF) #list of the variables used in the RF model 
predictors1
length(predictors1)
#both times (with sample 1500, 3000 and 4000)
#usually didn't removes any predictorss variables and
#the result suggest overfiting ()
#mtry  Accuracy   Kappa    
#2    0.8258373  0.3672479
#30    0.9773360  0.9396931
#58    0.9779210  0.9411703


#The second try with C5.0
iphone_sample_C5.0 <- train(iphoneSentiment~., data = iphone_sample_4000, method = "C5.0", trControl = ctrl)
iphone_sample_C5.0
predictors2 <- predictors(iphone_sample_C5.0) #list of the variables used in the model 
predictors2
length(predictors2)
#leaves 20 predictors and it has been chosen for a further analysis

#Dataset based on C5.0 feature selection
iphone_matrix_1_method <- iphone_matrix_disc[, predictors2]
glimpse(iphone_matrix_1_method)
iphone_matrix_1_method$iphoneSentiment <- iphone_matrix_disc$iphoneSentiment
dim(iphone_matrix_1_method)
glimpse(iphone_matrix_1_method)

#Dataset sampled: 4000 observarions, feature selection based on C5.0
iphone_matrix_1_method_4000 <- iphone_sample_4000[, predictors2]
glimpse(iphone_matrix_1_method_4000)
iphone_matrix_1_method_4000$iphoneSentiment <- iphone_sample_4000$iphoneSentiment
dim(iphone_matrix_1_method_4000)
glimpse(iphone_matrix_1_method_4000)

#data partition: first method of the feature selection  
set.seed(222)
TrainSize1 <- createDataPartition(y = iphone_matrix_1_method$iphoneSentiment, p = .70, list = FALSE)
training1 <- iphone_matrix_1_method[TrainSize1,] 
testing1 <- iphone_matrix_1_method[- TrainSize1,] 
dim(training1)
dim(testing1)

#data partition: sample 4000, firs method of the feature selection
set.seed(4000)
TrainSize4000 <- createDataPartition(y = iphone_matrix_1_method_4000$iphoneSentiment, p = .70, list = FALSE)
training4000 <- iphone_matrix_1_method_4000[TrainSize4000,] 
testing4000 <- iphone_matrix_1_method_4000[- TrainSize4000,] 
dim(training4000)
dim(testing4000)

#train control
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = T)

#SVM: 1 method, 21 variables. First try: OVERFITTING on a full set
#The second try: cost: 1.00;  accuracy: 0.9513747; kappa: 0.8629406
iphone_matrix_1_method_4000_svm <- train(iphoneSentiment~., data = training4000, method = 'svmLinear2', trControl = ctrl)
iphone_matrix_1_method_4000_svm

#kNN - FAIL "toom many ties in knn" on a full set  
#sample: 4000 - k: 5;  accuracy: 0.9106082;  kappa: 0.7384565
iphone_matrix_1_method_4000_knn <- train(iphoneSentiment~., data = training4000, method = 'knn', trControl = ctrl)
iphone_matrix_1_method_4000_knn

#C5.0   tree:  FALSE 20; accuracy:  0.9696905, kappa: 0.9177308
iphone_matrix_1_method_C5_4000 <- train(iphoneSentiment~., data = training4000, method = 'C5.0', trControl = ctrl)
iphone_matrix_1_method_C5_4000

#C5.0 on the whole dataset 

#Naive Bayes:   usekernel: TRUE; accuracy: 0.8279985; kappa: 0.3981375
iphone_matrix_1_method_NB_4000 <- train(iphoneSentiment~., data = training4000, method = 'naive_bayes', trControl = ctrl)
iphone_matrix_1_method_NB_4000

#RF  mtry: 20;  accuracy: 0.9664798, kappa: 0.9087623
iphone_matrix_1_method_RF_4000 <- train(iphoneSentiment~., data = training4000, method = 'rf', trControl = ctrl)
iphone_matrix_1_method_RF_4000



#2 Method of the feature selection
#The second method: findCorrelation() evaluates the relationships within the dataset and creates a list of highly correlated predictors for elimination 
#Data can't be discretized

#First corelations identified in the whole dataset and than removed from sample 

glimpse(iphone_matrix)
glimpse(samsung_matrix)

descrCor1 <- cor(iphone_matrix)
descrCor1
descrCor2 <- cor(samsung_matrix)
descrCor2
summary(descrCor1[upper.tri(descrCor1)])
summary(descrCor2[upper.tri(descrCor2)])

highlyCorDescr1 <- findCorrelation(descrCor1, cutoff = .95)
highlyCorDescr1
summary(highlyCorDescr1)

highlyCorDescr2 <- findCorrelation(descrCor2, cutoff = .95)
highlyCorDescr2
summary(highlyCorDescr2)

iphone_matrix_2_method <- iphone_matrix[, -highlyCorDescr1]
glimpse(iphone_matrix_2_method)
dim(iphone_matrix_2_method)

samsung_matrix_2_method <- samsung_matrix[, -highlyCorDescr2]
glimpse(samsung_matrix_2_method)
dim(samsung_matrix_2_method)

#sample 4000
iphone_Sample_2_method <- sample_n(iphone_matrix_2_method, 4000, replace = FALSE)
glimpse(iphone_Sample_2_method)

samsung_Sample_2_method <- sample_n(samsung_matrix_2_method, 1000, replace = FALSE)
glimpse(samsung_Sample_2_method)

#discretized dependent variable (iphoneSentiment)
iphone_Sample_2_method$iphoneSentiment <- discretize(iphone_Sample_2_method$iphoneSentiment, "fixed", categories = c(-Inf, -50, -10, -1, 1, 10, 50, Inf))
glimpse(iphone_Sample_2_method)

samsung_Sample_2_method$galaxySentiment <- discretize(samsung_Sample_2_method$galaxySentiment, "fixed", categories = c(-Inf, -50, -10, -1, 1, 10, 50, Inf))
glimpse(samsung_Sample_2_method)

#Data partition 

set.seed(222)
TrainSize2 <- createDataPartition(y = iphone_Sample_2_method$iphoneSentiment, p = .70, list = FALSE)
training2 <- iphone_Sample_2_method[TrainSize2,] 
testing2 <- iphone_Sample_2_method[- TrainSize2,] 
dim(training2)
dim(testing2)
glimpse(training2)
glimpse(testing2)

set.seed(333)
TrainSize3 <- createDataPartition(y = samsung_Sample_2_method$galaxySentiment, p = .70, list = FALSE)
training3 <- samsung_Sample_2_method[TrainSize3,] 
testing3 <- samsung_Sample_2_method[- TrainSize3,] 
dim(training3)
dim(testing3)
glimpse(training3)
glimpse(testing3)





#train control
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = T)

#models were run for iphone and samsung (the best result RF)

#SVM: 31 variables
# with 4000 sample:
#cost  Accuracy   Kappa    
#0.25  0.8393887  0.4920572
#0.50  0.8433135  0.5167889
#1.00  0.8452170  0.5286814

iphone_Sample_2_method_svm <- train(galaxySentiment~., data = training3, method = 'svmLinear2', trControl = ctrl)
iphone_Sample_2_method_svm

#kNN 4000:
#k  Accuracy   Kappa    
#5  0.8472617  0.5381389
#7  0.8402577  0.5068982
#9  0.8322851  0.4718292

iphone_Sample_2_method_knn <- train(galaxySentiment~., data = training3, method = 'knn', trControl = ctrl, tuneLength = 15)
iphone_Sample_2_method_knn

#Not available    
#iphone_Sample_2_method_C5 <- train(iphoneSentiment~., data = training2, method = 'C5.0', trControl = ctrl)
#iphone_Sample_2_method_C5

#NB sample 4000: 
#usekernel  Accuracy    Kappa     
#FALSE     0.02912765  0.00160244
#TRUE      0.80719595  0.31792461
#iphone_Sample_2_method_NB <- train(iphoneSentiment~., data = training2, method = 'naive_bayes', trControl = ctrl)
#iphone_Sample_2_method_NB

# iphone 4000         mtry: 16   accuracy: 0.8873862  kappa: 0.6747500
# galaxy overfitting? mtry: 29   accuracy: 0.9790803  kappa: 0.8117344 with tuneLength
# galaxy is it overfitting? 26    0.9638688  0.6870846  without tuneLength
iphone_Sample_2_method_RF <- train(galaxySentiment~., data = training3, method = 'rf', trControl = ctrl) #tuneLength = 15)
iphone_Sample_2_method_RF

#rpart, gmb, glm, kknn, parRF, 
#iphone_Sample_2_method_rpart <- train(iphoneSentiment~., data = training2, method = 'parRF', trControl = ctrl, tuneLength = 15)
#iphone_Sample_2_method_rpart


#Prediction RF
predRF_iPhone <- predict(iphone_Sample_2_method_RF, newdata = testing3)
predRF_iPhone
summary(predRF_iPhone)
postRF_iPhone <- postResample(predRF_iPhone, testing3$galaxySentiment)
postRF_iPhone
#Accuracy     Kappa 
#0.8897243    0.6771680

predRF_samsung <- predict(iphone_Sample_2_method_RF, newdata = testing3)
predRF_samsung
summary(predRF_samsung)
postRF_samsung <- postResample(predRF_samsung, testing2$iphoneSentiment)
postRF_samsung


#1 plot for sentiments  
#iphone_matrix_disc %>% ggplot(aes(iphoneSentiment)) +
#geom_bar(fill = "darkturquoise")+
#ggtitle("IPhone Sentiment") +
#theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.background = element_blank(), panel.grid = element_blank(), plot.background = element_blank(), strip.background = element_blank())

#samsung_matrix_disc %>% ggplot(aes(galaxySentiment)) +
#geom_bar(fill = "darkviolet")+
#ggtitle("Samsung Galaxy Sentiment") +
#theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.background = element_blank(), panel.grid = element_blank(), plot.background = element_blank(), strip.background = element_blank())

Sentiments <- data.frame(iphone_matrix_disc$iphoneSentiment, samsung_matrix_disc$galaxySentiment)
head(Sentiments)
colnames(Sentiments) <- c("iPhone", "Samsung Galaxy")
glimpse(Sentiments)
summary(Sentiments)

levels(Sentiments$iPhone)== levels(Sentiments$'Samsung Galaxy')

general_sentiment <- gather(Sentiments, Phone, Sentiment) %>% group_by(Phone, Sentiment) %>% summarise(sum = n())
general_sentiment

gen_sen_plot <-  ggplot(general_sentiment, aes(Sentiment, sum, fill = Phone)) + 
  geom_bar(position = "dodge", stat="identity") + 
  ggtitle("iPhone and Samsung Galaxy: Comparition of Sentiments") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  scale_x_discrete(limits=c("[-Inf,-50)", "[-50,-10)", "[-10,-1)", "[-1,1)", "[1,10)", "[10,50)", "[50, Inf]" ))
gen_sen_plot






#iphone_df <- df %>% select(contains("iphone"), contains("ios"))
#names(iphone_df)                           
#Changing the order of the attributes (iphoneSentiment as the last one) 
#iphone_df <- iphone_df[c(1:10, 12:15, 11)]

save.image()

