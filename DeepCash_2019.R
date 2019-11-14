# 1.1 Read in data: (From Scratch)

DC.Orig <- as.data.frame(read_csv("~/R/DeepCash/TruCompTrans_2019.csv"))

for (i in 1:12) {
  print(class(DC.Orig[,i]))
}

# Don't do as.factor yet
DC.Orig$label <- "regular"

for (i in 7:13){
  DC.Orig[,i]<-as.factor(DC.Orig[,i])
}

### Verifying correctness
#head(DC.Orig %>% filter(label=="local"))
#DC.Local.Anon<-DC.Orig %>% filter(label=="local")
#write.csv(DC.Local.Anon,file="DC_Local_Anon.csv",row.names=FALSE)

DC.Orig.factor<-DC.Orig[,7:12]
DC.Orig.numeric<-DC.Orig[,2:6]

summary(DC.Orig.factor)
summary(DC.Orig.numeric)

# Noise Generation

get_factor_noise<-function(factor.vec,noise_row){
  set.seed(666)
  noise.vec <- unique(factor.vec)[ceiling(runif(noise_row, 0, length(unique(factor.vec))))]
  print(noise.vec)
}

noise_size=10000

DC.Orig.factor.noise<-as.data.frame(sapply(DC.Orig.factor,get_factor_noise,noise_row=noise_size))

#sapply(DC.Orig.factor.noise,levels)
#sapply(sapply(DC.Orig.factor.noise,unique),length)

get_numeric_noise<-function(numeric.vec,noise_row){
  set.seed(666)
  noise.vec <- runif(noise_row, quantile(numeric.vec, .75, na.rm = TRUE),quantile(numeric.vec, .99, na.rm = TRUE))
  print(noise.vec)
}

DC.Orig.numeric.noise<-as.data.frame(sapply(DC.Orig.numeric,get_numeric_noise,noise_row=noise_size))

head(DC.Orig.numeric.noise)

# Noise Integration

DC.Noise<-cbind(cbind(cbind((1:noise_size),DC.Orig.numeric.noise),DC.Orig.factor.noise), rep("noise",noise_size))
names(DC.Noise)<-names(DC.Orig)
summary(DC.Noise)

for (i in 7:13){
  DC.Noise[,i]<-as.factor(DC.Noise[,i])
}

DC.All<-rbind(DC.Orig,DC.Noise)

tail(DC.All)
head(DC.All)
#summary(MLR.All)

# Bring in Synthesized Global error where all the categorical variables do not appear in the data set
# DC.Global.Anon<-as.data.frame(read_csv("~/R/DeepCash/Global_Anomalies.csv"))
# DC.Global.Anon$label <- "global"

# Bring in Synthesized Noise error where all the categorical variables are random draws from existing data set
# This increases 13 levels of cfo_cd, because of the 004, 4 difference

# DC.Noise.Anon<-as.data.frame(read_csv("~/R/DeepCash/Noise_Anomalies.csv"))
# DC.Noise.Anon$label <- "noise"

# for (i in 7:13){
#  DC.Global.Anon[,i]<-as.factor(DC.Global.Anon[,i])
# }

# for (i in 7:13){
#  DC.Noise.Anon[,i]<-as.factor(DC.Noise.Anon[,i])
# }

# DC.All<-rbind(DC.Orig,DC.Noise.Anon)

# tail(DC.All)

# summary(DC.All)

#####################################Old#############################################
###############################train-test split########################################
###################Use 100,000 as test set because data is big##########################

train_old<-function(data){
  set.seed(777)
  sample <- sample.int(n = nrow(data), size = (nrow(data)-100000), replace = F)
  print(data[sample, ])
}

test_old<-function(data){
  set.seed(777)
  sample <- sample.int(n = nrow(data), size = (nrow(data)-100000), replace = F)
  print(data[-sample, ])
}


#DC.All.train_old <-train_old(DC.All)
#DC.All.test_old <-test_old(DC.All)

#####################################Old#############################################
###############################train-test split########################################
###################Use 100,000 as test set because data is big##########################

#################################### Start 2019 #########################################
############################### Train/ DEV/ Test split################################
############################98% Train/ 1% DEV/ 1% Test################################
############################### Because data is big###################################

train_size <- floor(0.98 * nrow(DC.All))
set.seed(777)
train_ind <- sample(seq_len(nrow(DC.All)), size = train_size)


DC.All.train <-DC.All[train_ind,]
DC.All.test_dev <- DC.All[-train_ind,]

dev_size <- floor(0.5 * nrow(DC.All.test_dev))

dev_ind <- sample(seq_len(nrow(DC.All.test_dev)), size = dev_size)

DC.All.dev <-DC.All.test_dev[dev_ind,]
DC.All.test <- DC.All.test_dev[-dev_ind,]
?sample
head(DC.All.dev)
head(DC.dev.Final)
head(DC.All.test)
head(DC.test.Final)
rm("DC.All.test_dev")
#################################### End 2019 #########################################
############################### Train/ DEV/ Test split################################
############################98% Train/ 1% DEV/ 1% Test################################
############################### Because data is big###################################

#########################################################################################################

#summary(as.integer(row.names(DC.All.train)))
#summary(as.integer(row.names(DC.All.test)))

#row.train<-as.integer(row.names(DC.All.train))
#row.test<-as.integer(row.names(DC.All.test))

# Verifying training set and test set do not overlap

#sum(table(row.train[row.train %in% row.test]))

#min(row.train)
#min(row.test)

DC<-DC.All[,2:12]
DC.train<-DC.All.train[,2:12]
DC.dev<-DC.All.dev[,2:12]
DC.test<-DC.All.test[,2:12]

ID_Positive_Train<-(DC.All.train %>% filter(BonusAmount>=0))[,1]
ID_Positive_Test<-(DC.All.test %>% filter(BonusAmount>=0))[,1]


ID_New_Positive_Train<-(DC.All.train %>% filter(BonusAmount>=0))[,1]
ID_New_Positive_Test<-(DC.All.test %>% filter(BonusAmount>=0))[,1]
ID_New_Positive_Dev<-(DC.All.dev %>% filter(BonusAmount>=0))[,1]

summary(DC.train)
summary(DC.test)

# 1.2 Filter out Negative Value of Bonus (Adjustment of Bonus cannot be fraudulent)

DC.1.train<-DC.train %>% filter(BonusAmount>=0)
DC.1.test<-DC.test %>% filter(BonusAmount>=0)
DC.1.dev<-DC.dev %>% filter(BonusAmount>=0)
# DC.Negative.Bonus<-DC %>% filter(BonusAmount<0)

head(DC.1.train)

# 2.1 Seperate Numerical and Factorial Variables

# 2, 4, 1 correspond to "Count_prdcr", "Count_custunq", "BonusAmount" after careful choice; Can refer to later sections to find out why

DC.2.train.numeric<-DC.1.train[,c(2,4,1)]
head(DC.2.train.numeric)
DC.2.test.numeric<-DC.1.test[,c(2,4,1)]
head(DC.2.test.numeric)
DC.2.dev.numeric<-DC.1.dev[,c(2,4,1)]
head(DC.2.dev.numeric)


DC.2.train.factor<-DC.1.train[,6:11]
#summary(DC.2.train.factor)

DC.2.test.factor<-DC.1.test[,6:11]
summary(DC.2.test.factor)

DC.2.dev.factor<-DC.1.dev[,6:11]
summary(DC.2.dev.factor)

names(DC.1.train)

DC.2.train.factor$share <- as.factor(ifelse(DC.1.train$PYESHR_PCT==100,"Whole","Partial"))
DC.2.dev.factor$share <- as.factor(ifelse(DC.1.dev$PYESHR_PCT==100,"Whole","Partial"))
DC.2.test.factor$share <- as.factor(ifelse(DC.1.test$PYESHR_PCT==100,"Whole","Partial"))

#head(DC.2.dev.factor)
sapply(DC.2.train.factor,levels)

#sapply(sapply(DC.2.train.factor,unique),length)

# names(DC.2.1.factor)

# 2.1.1 Plotting

#plot1 <-qplot(x=Count_prdcr, data=DC.2.numeric)
#plot2 <-qplot(x=Count_custunq, data=DC.2.numeric)
#plot3 <-qplot(x=BonusAmount, data=DC.2.numeric)

# Doing this will crash R studio.
# grid.arrange(plot1, plot2, plot3, ncol=3)

# pairs(sample(DC.numeric,10000))

## 2.1.2 Exploring pyeshr_pct: Reason why not include this as a nuerical but in the model

#qplot(x=pyeshr_pct, data=DC.All)

#DC.partial<-DC %>% filter(pyeshr_pct!=100)
#DC.whole<-DC %>% filter(pyeshr_pct==100)

#class(summary(DC.partial))
#summary(DC.whole)

#box1=boxplot(DC.2.numeric$Count_prdcr~DC.2.factor$share, main="Exploring pyeshr_pct", 
#        xlab="pyeshr_pct", ylab="Count_prdcr")

# box1

# 3.1 Log transform the numerical except Bonus Amount

DC.3.train.numeric<-log(DC.2.train.numeric+1e-7)
DC.3.dev.numeric<-log(DC.2.dev.numeric+1e-7)
DC.3.test.numeric<-log(DC.2.test.numeric+1e-7)


#https://stats.stackexchange.com/questions/70801/how-to-normalize-data-to-0-1-range
min.max<-function(x){
  print((x-min(x))/(max(x)-min(x)))
}

for (i in 1:length(names(DC.3.train.numeric))){
  DC.3.train.numeric[,i]<-min.max(DC.3.train.numeric[,i])
}

for (i in 1:length(names(DC.3.dev.numeric))){
  DC.3.dev.numeric[,i]<-min.max(DC.3.dev.numeric[,i])
}

for (i in 1:length(names(DC.3.test.numeric))){
  DC.3.test.numeric[,i]<-min.max(DC.3.test.numeric[,i])
}


#### Checking the Min-Max function ####
#### Checking the Min-Max function ####
#### Checking the Min-Max function ####

# Example Data
#x = sample(-100:100, 50)
#summary(DC.3.dev.numeric)

#Normalized Data
#normalized = (x-min(x))/(max(x)-min(x))
#normalized = (DC.3.dev.numeric-min(DC.3.dev.numeric))/(max(DC.3.dev.numeric)-min(DC.3.dev.numeric))

#summary(normalized)

# Histogram of example data and normalized data
#par(mfrow=c(1,2))
#hist(DC.3.dev.numeric[,3],breaks=10, xlab="Data",col="lightblue", main="")
#hist(normalized[,3], breaks=10, xlab="Normalized Data", col="lightblue", main="")

summary(DC.dev.Final[,1:3])
#### Checking the Min-Max function ####
#### Checking the Min-Max function ####
#### Checking the Min-Max function ####



#pairs(DC.3.train.numeric)

#plot4 <-qplot(x=Count_prdcr, data=DC.3.test.numeric)
#plot5 <-qplot(x=Count_custunq, data=DC.3.test.numeric)
#plot6 <-qplot(x=BonusAmount, data=DC.3.test.numeric)

# Doing this will crash R studio.
# grid.arrange(plot4, plot5, plot6, ncol=3)


## Dummy Coding

options(na.action='na.pass')
DC.3.train.factor = as.data.frame(model.matrix(~., data=DC.2.train.factor)[,-1])
DC.3.dev.factor = as.data.frame(model.matrix(~., data=DC.2.dev.factor)[,-1])
DC.3.test.factor = as.data.frame(model.matrix(~., data=DC.2.test.factor)[,-1])

dim(DC.2.train.factor)
dim(DC.3.train.factor)

# model_matirx returns fewer than orignial rows; 
# after troubleshooting, identified that it was because of NA values

DC.2.train.factor[is.na(DC.2.train.factor$CFO_CD),]

## dim(DC.3.factor)

## Combine

## 2019/10/8 note: the numeric data did not go through the min-max nornalization line 258
## Leave it for now because the numerical range is acceptable
## ??

##   Count_prdcr     Count_custunq    BonusAmount    
## Min.   : 0.000   Min.   :0.000   Min.   :-4.605  
## 1st Qu.: 8.118   1st Qu.:2.485   1st Qu.: 1.072  
## Median :10.938   Median :3.091   Median : 1.792  
## Mean   :10.477   Mean   :2.955   Mean   : 2.023  
## 3rd Qu.:13.367   3rd Qu.:3.497   3rd Qu.: 3.117  
## Max.   :14.192   Max.   :8.780   Max.   :11.990  


DC.train.Final<-cbind(DC.3.train.numeric,DC.3.train.factor)
DC.dev.Final<-cbind(DC.3.dev.numeric,DC.3.dev.factor)
DC.test.Final<-cbind(DC.3.test.numeric,DC.3.test.factor)

DC.all.Final<-rbind(DC.train.Final,DC.dev.Final,DC.test.Final)

summary(DC.test.Final[,1:3])
summary(DC.dev.Final[,1:3])
summary(DC.train.Final[,1:3])
# write.csv(DC.train.Final, "DC.train.Final.csv")
# write.csv(DC.dev.Final, "DC.dev.Final.csv")
# write.csv(DC.test.Final, "DC.test.Final.csv")

#DC.Final.1M<-DC.Final[sample(nrow(DC.train.Final), 1000000), ]

toc()

###############################################################################################
################ H2O

#library(h2o)
#h2o.init(nthreads = -1)
#h2o.init()
#h2o.init(port = 9999)


h2o.connect(config = conf$connect_params)
##

tic("H2O data generation")
DC_all.10.11.h2o <- as.h2o(DC.all.Final)
DC_train.10.11.h2o <- as.h2o(DC.train.Final)
DC_dev.10.11.h2o <- as.h2o(DC.dev.Final)
DC_test.10.11.h2o <- as.h2o(DC.test.Final)

rm("DC.all.Final")
toc()
##################################################################
#####################Start: Saving to HDFS########################
##################################################################

tic("Saving to HDFS")

h2o.exportFile(DC_train.10.11.h2o, path="hdfs://prodmid/user/a311228/DC_train.10.11.h2o")
h2o.exportFile(DC_dev.10.11.h2o, path="hdfs://prodmid/user/a311228/DC_dev.10.11.h2o")
h2o.exportFile(DC_test.10.11.h2o, path="hdfs://prodmid/user/a311228/DC_test.10.11.h2o")
h2o.exportFile(DC_all.10.11.h2o, path="hdfs://prodmid/user/a311228/DC_all.10.11.h2o")
toc()

# h2o.exportFile(DC_all_test.h2o, path="hdfs://prodmid/user/a311228/DC_all_test_10_6.h2o")

##################################################################

DC_all_test.10.9.h2o<- h2o.importFile(path="hdfs://prodmid/user/a311228/DC_all_train.h2o.10.9")
DC_all_dev.10.9.h2o<- h2o.importFile(path="hdfs://prodmid/user/a311228/DC_all_dev.h2o.10.9")
DC_all_train.10.9.h2o<- h2o.importFile(path="hdfs://prodmid/user/a311228/DC_all_test.h2o.10.9")



##################################################################
#####################End: Saving to HDFS##########################
##################################################################

##################################################################
#####################DC_Tune_2019.R###############################
##################################################################

summary(DC_all_train.h2o)

#DC_all_1M.h2o<- as.h2o(DC.Final.1M)

#DC.test.1000.h2o<-as.h2o(DC.1000)

toc()

#summary(DC.all.h2o)

#summary(DC_all_1M.h2o)

features <-colnames(DC_all_train.h2o)

# Disable the following code to avoid losing results

#Auto=list()
#Anon=list()
#Err=list()
#plot=list()

################################################################################################################################
# 2019: First Run a Deep NN structure with intuitive 
################################################################################################################################
# Run 1:  (Full change epochs and max_w2 to see robustness) epochs = 400 max_w2=50 "TanhWithDropout"
################################################################################################################################

tic("Total")

tic("Model Fitting")

features <-colnames(DC_all_train.h2o)

Auto[[1]] = h2o.deeplearning(x = features, training_frame = DC_all_train.h2o,
                             autoencoder = TRUE,
                             reproducible = F, #slow - turn off for real problems
                             #seed = 1234,
                             hidden = c(311,256,128,64,128,256,311), epochs = 400,
                             input_dropout_ratio = 0.2,
                             max_w2=50,
                             #loss = "CrossEntropy",
                             activation = "TanhWithDropout")

toc()

tic("Error Recording")

summary(Auto[[1]])

Anon[[1]] = h2o.anomaly(Auto[[1]], DC_all_train.h2o, per_feature=FALSE)
# per_feature=FALSE gives out MSE for entrire row.

head(Anon[[1]])
Err[[1]]<-as.data.frame(Anon[[1]])
toc()

tic("Error Plotting")
plot[[1]]=plot(sort(Err[[1]]$Reconstruction.MSE), main='Full (304,256,128,64,32,16,8,4,3,4,8,16,32,64,128,256,304) epochs = 400 "TanhWithDropout" max_w2=500')
toc()

toc()

Auto1=Auto[[1]]
#h2o.saveModel(Auto1,path="/home/n263890/R/DeepCash",force = TRUE)
####################################################################################################################################


####################################################################################################################################
# Run 2:  (Full change epochs and max_w2 to see robustness) (317,...,3,4,...,317) epochs = 5000 max_w2=50 "TanhWithDropout"
####################################################################################################################################

tic("Total")

tic("Model Fitting")

features <-colnames(DC_all_train.h2o)

Auto[[2]] = h2o.deeplearning(x = features, training_frame = DC_all_train.h2o,
                             autoencoder = TRUE,
                             reproducible = F, #slow - turn off for real problems
                             #seed = 1234,
                             hidden = c(311,256,128,64,128,256,311), epochs = 5000,
                             input_dropout_ratio = 0.2,
                             max_w2=50,
                             #loss = "CrossEntropy",
                             activation = "TanhWithDropout")
toc()


tic("Error Recording")
summary(Auto[[2]])

Anon[[2]] = h2o.anomaly(Auto[[2]], DC_all_train.h2o, per_feature=FALSE)
# per_feature=FALSE gives out MSE for entrire row.

head(Anon[[2]])
Err[[2]]<-as.data.frame(Anon[[2]])
toc()

tic("Error Plotting")
plot[[2]]=plot(sort(Err[[2]]$Reconstruction.MSE), main='Full (317,...,3,...,317) epochs = 5000 "TanhWithDropout" max_w2=500')
toc()

toc()

Auto2=Auto[[2]]
#h2o.saveModel(Auto1,path="/home/n263890/R/DeepCash",force = TRUE) 
####################################################################################################################################

####################################################################################################################################
# Run 3:  Shallow Network
# hidden = c(311,256,311)
####################################################################################################################################
tic("Total")

tic("Model Fitting")

features <-colnames(DC_all_train.h2o)

Model_1014[[3]] = h2o.deeplearning(x = features, training_frame = DC_all_train.h2o,
                                   autoencoder = TRUE,
                                   reproducible = F, #slow - turn off for real problems
                                   #seed = 1234,
                                   hidden = c(311,256,311), epochs = 40,
                                   input_dropout_ratio = 0.2,
                                   max_w2=50,
                                   #loss = "CrossEntropy",
                                   activation = "TanhWithDropout")

toc()

tic("Error Recording")

summary(Model_1014[[3]])

# Returns an H2OFrame object containing the reconstruction MSE or the per-feature squared error.

# Anon[[1]] = h2o.anomaly(Auto[[1]], DC_all_train.h2o, per_feature=FALSE)

#Train_Rec_PerFeat[[1]]=h2o.anomaly(Model[[1]], DC_all_train.h2o, per_feature=TRUE)
#Dev_Rec_PerFeat[[1]]=h2o.anomaly(Model[[1]], DC_all_dev.h2o, per_feature=TRUE)
#Test_Rec_PerFeat[[1]]=h2o.anomaly(Model[[1]], DC_all_test.h2o, per_feature=TRUE)

Train_MSE_All_1014[[3]]=h2o.anomaly(Model_1014[[3]], DC_all_train.h2o, per_feature=FALSE)
Dev_MSE_All_1014[[3]]=h2o.anomaly(Model_1014[[3]], DC_all_dev.h2o, per_feature=FALSE)
Test_MSE_All_1014[[3]]=h2o.anomaly(Model_1014[[3]], DC_all_test.h2o, per_feature=FALSE)

sum(Train_MSE_All_1014[[3]])
sum(Dev_MSE_All_1014[[3]])
sum(Test_MSE_All_1014[[3]])

# per_feature=FALSE gives out MSE for entrire row.

#Err[[1]]<-as.data.frame(Anon[[1]])
toc()

#tic("Error Plotting")
# plot[[1]]=plot(sort(Err[[1]]$Reconstruction.MSE), main='Full (304,256,128,64,32,16,8,4,3,4,8,16,32,64,128,256,304) epochs = 400 "TanhWithDropout" max_w2=500')
# toc()

toc()

#Model1_1014=Model_1014[[3]]
#h2o.saveModel(Auto1,path="/home/n263890/R/DeepCash",force = TRUE)
####################################################################################################################################

####################################################################################################################################
# Run 4:  Deep and Narrow Network
# hidden = c(311,256,128,64,32,16,8,4,3,4,8,16,32,64,128,256,311)
####################################################################################################################################
tic("Total")

tic("Model Fitting")

features <-colnames(DC_all_train.h2o)

Model_1014[[4]] = h2o.deeplearning(x = features, training_frame = DC_all_train.h2o,
                                   autoencoder = TRUE,
                                   reproducible = F, #slow - turn off for real problems
                                   #seed = 1234,
                                   hidden = c(311,256,128,64,32,16,8,4,3,4,8,16,32,64,128,256,311), epochs = 40,
                                   input_dropout_ratio = 0.2,
                                   max_w2=50,
                                   #loss = "CrossEntropy",
                                   activation = "TanhWithDropout")

toc()

tic("Error Recording")

summary(Model_1014[[4]])

# Returns an H2OFrame object containing the reconstruction MSE or the per-feature squared error.

# Anon[[1]] = h2o.anomaly(Auto[[1]], DC_all_train.h2o, per_feature=FALSE)

#Train_Rec_PerFeat[[1]]=h2o.anomaly(Model[[1]], DC_all_train.h2o, per_feature=TRUE)
#Dev_Rec_PerFeat[[1]]=h2o.anomaly(Model[[1]], DC_all_dev.h2o, per_feature=TRUE)
#Test_Rec_PerFeat[[1]]=h2o.anomaly(Model[[1]], DC_all_test.h2o, per_feature=TRUE)

Train_MSE_All_1014[[4]]=h2o.anomaly(Model_1014[[4]], DC_all_train.h2o, per_feature=FALSE)
Dev_MSE_All_1014[[4]]=h2o.anomaly(Model_1014[[4]], DC_all_dev.h2o, per_feature=FALSE)
Test_MSE_All_1014[[4]]=h2o.anomaly(Model_1014[[4]], DC_all_test.h2o, per_feature=FALSE)

sum(Train_MSE_All_1014[[4]])
sum(Dev_MSE_All_1014[[4]])
sum(Test_MSE_All_1014[[4]])

# per_feature=FALSE gives out MSE for entrire row.

#Err[[1]]<-as.data.frame(Anon[[1]])
toc()

#tic("Error Plotting")
# plot[[1]]=plot(sort(Err[[1]]$Reconstruction.MSE), main='Full (304,256,128,64,32,16,8,4,3,4,8,16,32,64,128,256,304) epochs = 400 "TanhWithDropout" max_w2=500')
# toc()

toc()

#Model1_1014=Model_1014[[3]]
#h2o.saveModel(Auto1,path="/home/n263890/R/DeepCash",force = TRUE)

####################################################################################################################################
# Run 5:  Non-symmetric Autoencoder
# hidden = c(311,36,99,222,311)
####################################################################################################################################

tic("Total")

tic("Model Fitting")

#features <-colnames(DC_all_train.h2o)


#Trying to predict with an unstable model.
#Job was aborted due to observed numerical instability (exponential growth).
tic("Total")

tic("Model Fitting")

features <-colnames(DC_all_train.h2o)

Model_1014[[5]] = h2o.deeplearning(x = features, training_frame = DC_all_train.h2o,
                                   autoencoder = TRUE,
                                   reproducible = F, #slow - turn off for real problems
                                   #seed = 1234,
                                   hidden = c(311,36,99,222,311), epochs = 40,
                                   input_dropout_ratio = 0.2,
                                   max_w2=50,
                                   #loss = "CrossEntropy",
                                   activation = "TanhWithDropout")

toc()

tic("Error Recording")

summary(Model_1014[[5]])

# Returns an H2OFrame object containing the reconstruction MSE or the per-feature squared error.

# Anon[[1]] = h2o.anomaly(Auto[[1]], DC_all_train.h2o, per_feature=FALSE)

#Train_Rec_PerFeat[[1]]=h2o.anomaly(Model[[1]], DC_all_train.h2o, per_feature=TRUE)
#Dev_Rec_PerFeat[[1]]=h2o.anomaly(Model[[1]], DC_all_dev.h2o, per_feature=TRUE)
#Test_Rec_PerFeat[[1]]=h2o.anomaly(Model[[1]], DC_all_test.h2o, per_feature=TRUE)

Train_MSE_All_1014[[5]]=h2o.anomaly(Model_1014[[5]], DC_all_train.h2o, per_feature=FALSE)
Dev_MSE_All_1014[[5]]=h2o.anomaly(Model_1014[[5]], DC_all_dev.h2o, per_feature=FALSE)
Test_MSE_All_1014[[5]]=h2o.anomaly(Model_1014[[5]], DC_all_test.h2o, per_feature=FALSE)

sum(Train_MSE_All_1014[[5]])
sum(Dev_MSE_All_1014[[5]])
sum(Test_MSE_All_1014[[5]])

# per_feature=FALSE gives out MSE for entrire row.

#Err[[1]]<-as.data.frame(Anon[[1]])
toc()

#tic("Error Plotting")
# plot[[1]]=plot(sort(Err[[1]]$Reconstruction.MSE), main='Full (304,256,128,64,32,16,8,4,3,4,8,16,32,64,128,256,304) epochs = 400 "TanhWithDropout" max_w2=500')
# toc()

toc()

#Model1_1014=Model_1014[[3]]
#h2o.saveModel(Auto1,path="/home/n263890/R/DeepCash",force = TRUE)


####################################################################################################################################
# Run 6:   Non-symmetric Autoencoder
# hidden = c(311,222,99,36,99,222,311)
####################################################################################################################################

tic("Total")

tic("Model Fitting")

features <-colnames(DC_all_train.h2o)

Model_1014[[6]] = h2o.deeplearning(x = features, training_frame = DC_all_train.h2o,
                                   autoencoder = TRUE,
                                   reproducible = F, #slow - turn off for real problems
                                   #seed = 1234,
                                   hidden = c(311,222,99,36,99,222,311), epochs = 40,
                                   input_dropout_ratio = 0.2,
                                   max_w2=50,
                                   #loss = "CrossEntropy",
                                   activation = "TanhWithDropout")

toc()

tic("Error Recording")

summary(Model_1014[[6]])

Train_MSE_All_1014[[6]]=h2o.anomaly(Model_1014[[6]], DC_all_train.h2o, per_feature=FALSE)
Dev_MSE_All_1014[[6]]=h2o.anomaly(Model_1014[[6]], DC_all_dev.h2o, per_feature=FALSE)
Test_MSE_All_1014[[6]]=h2o.anomaly(Model_1014[[6]], DC_all_test.h2o, per_feature=FALSE)

sum(Train_MSE_All_1014[[6]])
sum(Dev_MSE_All_1014[[6]])
sum(Test_MSE_All_1014[[6]])

# per_feature=FALSE gives out MSE for entrire row.

#Err[[1]]<-as.data.frame(Anon[[1]])
toc()

toc()


####################################################################################################################################
# Run 7:  Non-symmetric Autoencoder
# hidden = c(311,36,18,99,222,311)
####################################################################################################################################

tic("Total")

tic("Model Fitting")

features <-colnames(DC_all_train.h2o)

Model_1014[[7]] = h2o.deeplearning(x = features, training_frame = DC_all_train.h2o,
                                   autoencoder = TRUE,
                                   reproducible = F, #slow - turn off for real problems
                                   #seed = 1234,
                                   hidden = c(311,36,18,99,222,311), epochs = 40,
                                   input_dropout_ratio = 0.2,
                                   max_w2=50,
                                   #loss = "CrossEntropy",
                                   activation = "TanhWithDropout")

toc()

tic("Error Recording")

summary(Model_1014[[7]])

Train_MSE_All_1014[[7]]=h2o.anomaly(Model_1014[[7]], DC_all_train.h2o, per_feature=FALSE)
Dev_MSE_All_1014[[7]]=h2o.anomaly(Model_1014[[7]], DC_all_dev.h2o, per_feature=FALSE)
Test_MSE_All_1014[[7]]=h2o.anomaly(Model_1014[[7]], DC_all_test.h2o, per_feature=FALSE)

sum(Train_MSE_All_1014[[7]])
sum(Dev_MSE_All_1014[[7]])
sum(Test_MSE_All_1014[[7]])

# per_feature=FALSE gives out MSE for entrire row.

#Err[[1]]<-as.data.frame(Anon[[1]])
toc()

toc()

####################################################################################################################################

####################################################################################################################################
# Run 8:  Non-symmetric Autoencoder
# hidden = c(311,18,99,222,311)
####################################################################################################################################

tic("Total")

tic("Model Fitting")

features <-colnames(DC_all_train.h2o)

Model_1014[[8]] = h2o.deeplearning(x = features, training_frame = DC_all_train.h2o,
                                   autoencoder = TRUE,
                                   reproducible = F, #slow - turn off for real problems
                                   #seed = 1234,
                                   hidden = c(311,18,99,222,311), epochs = 40,
                                   input_dropout_ratio = 0.2,
                                   max_w2=50,
                                   #loss = "CrossEntropy",
                                   activation = "TanhWithDropout")

toc()

tic("Error Recording")

summary(Model_1014[[8]])

Train_MSE_All_1014[[8]]=h2o.anomaly(Model_1014[[8]], DC_all_train.h2o, per_feature=FALSE)
Dev_MSE_All_1014[[8]]=h2o.anomaly(Model_1014[[8]], DC_all_dev.h2o, per_feature=FALSE)
Test_MSE_All_1014[[8]]=h2o.anomaly(Model_1014[[8]], DC_all_test.h2o, per_feature=FALSE)

sum(Train_MSE_All_1014[[8]])
sum(Dev_MSE_All_1014[[8]])
sum(Test_MSE_All_1014[[8]])

# per_feature=FALSE gives out MSE for entrire row.

#Err[[1]]<-as.data.frame(Anon[[1]])
toc()

toc()

####################################################################################################################################
# Run 9:  Non-symmetric Autoencoder
# hidden = c(311,222,36,99,311)
####################################################################################################################################

tic("Total")

tic("Model Fitting")

features <-colnames(DC_all_train.h2o)

Model_1014[[9]] = h2o.deeplearning(x = features, training_frame = DC_all_train.h2o,
                                   autoencoder = TRUE,
                                   reproducible = F, #slow - turn off for real problems
                                   #seed = 1234,
                                   hidden = c(311,222,36,99,311), epochs = 40,
                                   input_dropout_ratio = 0.2,
                                   max_w2=50,
                                   #loss = "CrossEntropy",
                                   activation = "TanhWithDropout")

toc()

tic("Error Recording")

summary(Model_1014[[9]])

Train_MSE_All_1014[[9]]=h2o.anomaly(Model_1014[[9]], DC_all_train.h2o, per_feature=FALSE)
Dev_MSE_All_1014[[9]]=h2o.anomaly(Model_1014[[9]], DC_all_dev.h2o, per_feature=FALSE)
Test_MSE_All_1014[[9]]=h2o.anomaly(Model_1014[[9]], DC_all_test.h2o, per_feature=FALSE)

sum(Train_MSE_All_1014[[9]])
sum(Dev_MSE_All_1014[[9]])
sum(Test_MSE_All_1014[[9]])

# per_feature=FALSE gives out MSE for entrire row.

#Err[[1]]<-as.data.frame(Anon[[1]])
toc()

toc()


####################################################################################################################################

####################################################################################################################################
# Run 10:  Middle layer wider
# hidden = c(311,777,311)
####################################################################################################################################

tic("Total")

tic("Model Fitting")

features <-colnames(DC_all_train.h2o)

Model_1014[[10]] = h2o.deeplearning(x = features, training_frame = DC_all_train.h2o,
                                   autoencoder = TRUE,
                                   reproducible = F, #slow - turn off for real problems
                                   #seed = 1234,
                                   hidden = c(311,777,311), epochs = 40,
                                   input_dropout_ratio = 0.2,
                                   max_w2=50,
                                   #loss = "CrossEntropy",
                                   activation = "TanhWithDropout")

toc()

tic("Error Recording")

summary(Model_1014[[10]])

Train_MSE_All_1014[[10]]=h2o.anomaly(Model_1014[[10]], DC_all_train.h2o, per_feature=FALSE)
Dev_MSE_All_1014[[10]]=h2o.anomaly(Model_1014[[10]], DC_all_dev.h2o, per_feature=FALSE)
Test_MSE_All_1014[[10]]=h2o.anomaly(Model_1014[[10]], DC_all_test.h2o, per_feature=FALSE)

sum(Train_MSE_All_1014[[10]])
sum(Dev_MSE_All_1014[[10]])
sum(Test_MSE_All_1014[[10]])

# per_feature=FALSE gives out MSE for entrire row.

#Err[[1]]<-as.data.frame(Anon[[1]])
toc()

toc()

####################################################################################################################################
# Run 11:  Non-symmetric Autoencoder Deep but Narrow
# hidden = c(311,222,36,18,9,4,3,4,9,18,36,99,311)
####################################################################################################################################

tic("Total")

tic("Model Fitting")

features <-colnames(DC_all_train.h2o)

Model_1014[[11]] = h2o.deeplearning(x = features, training_frame = DC_all_train.h2o,
                                   autoencoder = TRUE,
                                   reproducible = F, #slow - turn off for real problems
                                   #seed = 1234,
                                   hidden = c(311,222,36,18,9,4,3,4,9,18,36,99,311), epochs = 40,
                                   input_dropout_ratio = 0.2,
                                   max_w2=50,
                                   #loss = "CrossEntropy",
                                   activation = "TanhWithDropout")

toc()

tic("Error Recording")

summary(Model_1014[[11]])

Train_MSE_All_1014[[11]]=h2o.anomaly(Model_1014[[11]], DC_all_train.h2o, per_feature=FALSE)
Dev_MSE_All_1014[[11]]=h2o.anomaly(Model_1014[[11]], DC_all_dev.h2o, per_feature=FALSE)
Test_MSE_All_1014[[11]]=h2o.anomaly(Model_1014[[11]], DC_all_test.h2o, per_feature=FALSE)

sum(Train_MSE_All_1014[[11]])
sum(Dev_MSE_All_1014[[11]])
sum(Test_MSE_All_1014[[11]])

# per_feature=FALSE gives out MSE for entrire row.

#Err[[1]]<-as.data.frame(Anon[[1]])
toc()

toc()


####################################################################################################################################
####################################################################################################################################
# Run 12:  Non-symmetric Autoencoder
# hidden = c(311,222,36,99,311)
####################################################################################################################################

tic("Total")

tic("Model Fitting")

features <-colnames(DC_all_train.h2o)

Model_1014[[12]] = h2o.deeplearning(x = features, training_frame = DC_all_train.h2o,
                                   autoencoder = TRUE,
                                   reproducible = F, #slow - turn off for real problems
                                   #seed = 1234,
                                   hidden = c(311,222,99,36,99,222,311), epochs = 40,
                                   input_dropout_ratio = 0.2,
                                   max_w2=50,
                                   #loss = "CrossEntropy",
                                   activation = "TanhWithDropout")

toc()

tic("Error Recording")

summary(Model_1014[[12]])

Train_MSE_All_1014[[12]]=h2o.anomaly(Model_1014[[12]], DC_all_train.h2o, per_feature=FALSE)
Dev_MSE_All_1014[[12]]=h2o.anomaly(Model_1014[[12]], DC_all_dev.h2o, per_feature=FALSE)
Test_MSE_All_1014[[12]]=h2o.anomaly(Model_1014[[12]], DC_all_test.h2o, per_feature=FALSE)

sum(Train_MSE_All_1014[[12]])
sum(Dev_MSE_All_1014[[12]])
sum(Test_MSE_All_1014[[12]])

# per_feature=FALSE gives out MSE for entrire row.

#Err[[1]]<-as.data.frame(Anon[[1]])
toc()

toc()

####################################################################################################################################
# Run 9:  Non-symmetric Autoencoder
# hidden = c(311,222,36,99,311)
####################################################################################################################################

tic("Total")

tic("Model Fitting")

features <-colnames(DC_all_train.h2o)

Model_1014[[13]] = h2o.deeplearning(x = features, training_frame = DC_all_train.h2o,
                                   autoencoder = TRUE,
                                   reproducible = F, #slow - turn off for real problems
                                   #seed = 1234,
                                   hidden = c(311,222,18,99,311), epochs = 40,
                                   input_dropout_ratio = 0.2,
                                   max_w2=50,
                                   #loss = "CrossEntropy",
                                   activation = "TanhWithDropout")

toc()

tic("Error Recording")

summary(Model_1014[[13]])

Train_MSE_All_1014[[13]]=h2o.anomaly(Model_1014[[13]], DC_all_train.h2o, per_feature=FALSE)
Dev_MSE_All_1014[[13]]=h2o.anomaly(Model_1014[[13]], DC_all_dev.h2o, per_feature=FALSE)
Test_MSE_All_1014[[13]]=h2o.anomaly(Model_1014[[13]], DC_all_test.h2o, per_feature=FALSE)

sum(Train_MSE_All_1014[[13]])
sum(Dev_MSE_All_1014[[13]])
sum(Test_MSE_All_1014[[13]])

# per_feature=FALSE gives out MSE for entrire row.

#Err[[1]]<-as.data.frame(Anon[[1]])
toc()

toc()


####################################################################################################################################

####################################################################################################################################

lapply(Train_MSE_All_1014,mean)
lapply(Dev_MSE_All_1014,mean)
lapply(Test_MSE_All_1014,mean)

lapply(Train_MSE_All_1014,sum)
lapply(Dev_MSE_All_1014,sum)
lapply(Test_MSE_All_1014,sum)

lapply(Train_MSE_All_1014,head)
lapply(Dev_MSE_All_1014,head)
lapply(Test_MSE_All_1014,head)

################################################################################################################################
################################################################################################################################
## Result investigation
################################################################################################################################
################################################################################################################################

dim(Err[[1]])

anomaly.suspects=list()
anomaly.top50=list()
a2.10.all.inv=list()
top50.anomaly=list()
top150.anomaly=list()

for (i in 1:6) {
  a2.10.all.inv[[i]]<-cbind(Err[[i]],a2.9.all.inv)
}

class(a2.04)

a2.04.err.6<-cbind(as.data.frame(Err[[6]]),as.data.frame(a2.04))


head(a2.10.all.inv[[1]])

anomaly.suspects[[1]] <- a2.9.all.inv[Err[[1]]$Reconstruction.MSE > 0.1,]
anomaly.suspects[[2]] <- a2.9.all.inv[Err[[2]]$Reconstruction.MSE > 0.1,]
anomaly.suspects[[3]]<- a2.9.all.inv[Err[[3]]$Reconstruction.MSE > 0.12,]


for (i in 3) {
  top50.anomaly[[i]] <- a2.10.all.inv[[i]] %>%
    filter(rank(desc(Err[[i]]))<=50) 
}


for (i in 3) {
  top150.anomaly[[i]] <- a2.10.all.inv[[i]] %>%
    filter(rank(desc(Err[[i]]))<=150) 
}


for (i in 3) {
  write.csv(top150.anomaly[[i]],file=paste0("top150.anomaly",i, "th Run.csv"),row.names=FALSE)
}



for (i in 3) {
  write.csv(anomaly.top50[[i]],file=paste0("anomaly.top50",i, "th Run.csv"),row.names=FALSE)
}

### Warning 1,2,4 yeild idenical reconstruction error.

Err[[1]]-Err[[4]]


write.csv(cbind(inv.id,a2.7.all)[sample(nrow(a2.7.all), 20), ],file=paste0("Fields.csv"),row.names=FALSE)

write.csv(d[sample(nrow(d), 20), ],file=paste0("Full20.csv"),row.names=FALSE)


#############################################################################
###############################Result1#####################################
#############################################################################
Result1<-cbind(cbind(Err[[1]],ID_Positive_Train),DC.1.train)
Result1<-Result1 %>% arrange(desc(Result1[,1]))

DeepCashOne1M<-Result1[1:1000000,]

write.csv(DeepCashOne,file=paste0("DeepCashOne1M.csv"),row.names=FALSE)

head(Result1)
