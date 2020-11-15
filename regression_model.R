rm(list = ls())


library(nnet)
library(rpart)
library(neuralnet)
library(ISLR)
library(caret)
library(xts)
library(zoo)



data<-read.csv('dataset_Facebook_1.csv')

# model=lm(data$Page.total.likes ~ data$Lifetime.Post.Total.Reach + data$Total.Interactions + data$Lifetime.Post.Total.Impressions + data$Lifetime.Engaged.Users+ 
#            data$Lifetime.Post.Consumers + data$Lifetime.Post.Consumptions + data$Lifetime.Post.Impressions.by.people.who.have.liked.your.Page + 
#            data$Lifetime.Post.reach.by.people.who.like.your.Page + data$comment + data$like + data$share)
# 
# summary(model)

#data<-read.csv('Total_Data.csv')
#data<-read.csv('Total_Data_Reduced_Features_include_CART.csv')
#data<-read.csv('Total_Data_Reduced_Features.csv')


#data_1<-read.csv('Total_data_with_label.csv')
#placement_array=data_1$Placement
#placement_numeric_array=ifelse(placement_array %in% "Placed", 1, 0)
#write.csv(placement_numeric_array,"total_label.csv")


################################## sample training and testing row nuber of the total data ##########################

# set_of_row=1:nrow(data)
# train_row=sample(set_of_row,200)
# test_row=setdiff(set_of_row,train_row)

 # train_row=read.csv('Training_Data_Row_Number.csv')
 # train_row_number=train_row$Row_Number
 # 
 # test_row=read.csv('Testing_Data_Row_Number.csv')
 # test_row_number=test_row$Row_Number
 # 
 # 
 # training_data=data[train_row_number,]
 # testing_data=data[test_row_number,]


#######################################

# 
# train_row=read.csv('Training_Data_Row_Number.csv')
# test_row=read.csv('Testing_Data_Row_Number.csv')
# 
# training_data=read.csv('Training_Data.csv')
# testing_data=read.csv('Testing_Data.csv')

############## Only NN ###################

maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

scaled_data=scaled

nnet.fit <- nnet(Lifetime.Post.Consumers ~ ., data=scaled_data, size=2)


# nnet.fit <- nnet("data$Page.total.likes ~ data$Lifetime.Post.Total.Reach + data$Total.Interactions + data$Lifetime.Post.Total.Impressions + data$Lifetime.Engaged.Users+
#              data$Lifetime.Post.Consumers + data$Lifetime.Post.Consumptions + data$Lifetime.Post.Impressions.by.people.who.have.liked.your.Page +
#               data$Lifetime.Post.reach.by.people.who.like.your.Page + data$comment + data$like + data$share", data=scaled_data, size=2)



# nn1 <- neuralnet::neuralnet("data$Page.total.likes ~ data$Lifetime.Post.Total.Reach + data$Total.Interactions + data$Lifetime.Post.Total.Impressions + data$Lifetime.Engaged.Users+ 
#                             data$Lifetime.Post.Consumers + data$Lifetime.Post.Consumptions + data$Lifetime.Post.Impressions.by.people.who.have.liked.your.Page + 
#                             data$Lifetime.Post.reach.by.people.who.like.your.Page + data$comment + data$like + data$share ~ ",data=scaled_data,hidden=c(3),startweights=seq(1,100,by=0.2),linear.output=T)

    # maxs <- apply(data, 2, max)
    # mins <- apply(data, 2, min)
    # scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
    # 
    # #######  scaled trained and test_data for NN ###########
    # train_data <- scaled[train_row_number,]
    # test_data <- scaled[test_row_number,]
    # ###########
    # 
    # 
    # ###actual class label of test data  ###
    # test.r= (test_data$Placement)*(max(data$Placement)-min(data$Placement))+min(data$Placement)
    # ##############
    # 
    # 
    # n <- names(train_data)
    # f <- as.formula(paste("Placement ~", paste(n[!n %in% "Placement"], collapse = " + ")))
    # 
    # nn1 <- neuralnet::neuralnet(f,data=train_data,hidden=c(3),startweights=seq(1,100,by=0.2),linear.output=T)
    # plot(nn1)
    # 
    # n1=(names(test_data))
    # 
    # #pr.nn1 <- compute(nn1,test_data[,n1[1:30]])
    # pr.nn1 <- compute(nn1,test_data[,n1[1:5]])
    # 
    # ########  predicted class label of test data after nn #########
    # pr.nn1_1=pr.nn1$net.result*(max(data$Placement)-min(data$Placement))+min(data$Placement)
    # ##########
    # 
    # MSE.nn1 <- sum((test.r - pr.nn1_1)^2)/nrow(test_data)
    # print(paste(MSE.nn1))
    # MAPE.nn1 <- (sum(abs(test.r - pr.nn1_1))/nrow(test_data))*100
    # MAPE.nn1
    # print(paste(MAPE.nn1))
    # 
    # 
    # ### confusion matrix ########
    # 
    # #table(data[201:236,]$Placement, as.integer(round(pr.nn1$net.resul[,1])))
    # confusion_matrix_nn<-table(test.r, as.integer(round(pr.nn1$net.resul[,1])))
    # 
    # print('confusion_matrix_nn')
    # print(confusion_matrix_nn)

    ############
    # 
    # 
    # ######### Only CART  over total data ########################

    #actual_cl_total_data_CART<-data$Placement
   #  n <- names(data)
   # 
   #  f1 <- as.formula(paste("data$Page.total.likes ~ data$Lifetime.Post.Total.Reach + data$Total.Interactions + data$Lifetime.Post.Total.Impressions + data$Lifetime.Engaged.Users+ 
   #  data$Lifetime.Post.Consumers + data$Lifetime.Post.Consumptions + data$Lifetime.Post.Impressions.by.people.who.have.liked.your.Page + 
   # data$Lifetime.Post.reach.by.people.who.like.your.Page + data$comment + data$like + data$share"))
   #  #tree_1 <-  rpart(f1, data = data, method = "anova", control = rpart.control(cp =0.04))
   #  
   #  tree_1 <-  rpart(f1, data = data, method = "anova",control = rpart.control(minsplit = 25))
   # 
   #  plot(tree_1)
   #  text(tree_1, use.n=TRUE, all=TRUE, cex=.8)
   #   
   #  predicted_cl_total_data_CART<-as.integer(round(predict(tree_1,data)))
     
    # confusion_matrix_total_data_cart<-table(actual_cl_total_data_CART,predicted_cl_total_data_CART)
    # 
    # print('confusion_matrix_total_data_cart')
    # print(confusion_matrix_total_data_cart)


    ###############################################################################################################


    # f2 <- as.formula(paste("Placement ~", paste(n[!n %in% c("Placement","Percent_SSC")], collapse = " + ")))
    #
    # tree_2 <-  rpart(f2, data = data, control = rpart.control(cp =   0.0001))
    #
    # plot(tree_2)
    # text(tree_2, use.n=TRUE, all=TRUE, cex=.8)
    #
    #
    # f3 <- as.formula(paste("Placement ~", paste(n[!n %in% c("Placement","Percent_SSC","Percent_HSC")], collapse = " + ")))
    # #f4 <- as.formula(paste("Placement ~", paste(n[!n %in% c("Placement","Percent_SSC","Percent_HSC")], collapse = " + "),"+Percent_HSC^2"))
    #
    # tree_3 <-  rpart(f4, data = data, control = rpart.control(cp =   0.0001))
    #
    # plot(tree_3)
    # text(tree_3, use.n=TRUE, all=TRUE, cex=.8)


    ##########

    ##################### CART trained over train data and applied/predicted over test data #################

    # actual_cl_train_data_CART<-training_data$Placement
    # actual_cl_test_data_CART<-testing_data$Placement
    # n <- names(data)
    # 
    # f11 <- as.formula(paste("Placement ~", paste(n[!n %in% "Placement"], collapse = " + ")))
    # tree_11 <-  rpart(f11, data = training_data, control = rpart.control(cp =0.01))
    # #tree_11 <-  rpart(f11, data = data[201:236,], control = rpart.control(cp =   0.010))
    # 
    # plot(tree_11)
    # text(tree_11, use.n=TRUE, all=TRUE, cex=.8)
    # 
    # #predicted_cl_train_data_CART<-as.integer(round(predict(tree_11,data[1:200,])))
    # predicted_cl_test_data_CART<-as.integer(round(predict(tree_11,testing_data)))
    # 
    # #confusion_matrix_train_data_cart<-table(actual_cl_train_data_CART,predicted_cl_train_data_CART)
    # confusion_matrix_test_data_cart<-table(actual_cl_test_data_CART,predicted_cl_test_data_CART)
    # 
    # print('confusion_matrix_test_data_cart')
    # print(confusion_matrix_test_data_cart)

    ####################### #################  CART + NN ##############################


# 
# 
#     maxs <- apply(data, 2, max)
#     mins <- apply(data, 2, min)
#     scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
# 
#     #######  scaled trained and test_data for NN ###########
#     train_data <- scaled[train_row_number,]
#     test_data <- scaled[test_row_number,]
#     ###########
# 
#     ############  scaled individual training and testing data sets ##################
# 
#     # maxs <- apply(training_data, 2, max)
#     # mins <- apply(training_data, 2, min)
#     # training_scaled <- as.data.frame(scale(training_data, center = mins, scale = maxs - mins))
#     #
#     # maxs <- apply(testing_data, 2, max)
#     # mins <- apply(testing_data, 2, min)
#     # testing_scaled <- as.data.frame(scale(testing_data, center = mins, scale = maxs - mins))
#     #
#     # train_data <- training_scaled
#     # test_data <- testing_scaled
# 
#     ###################################################
# 
#     ####  Included CART output here to NN ##########
#     train_data$Placement=predicted_cl_total_data_CART[train_row_number]
#     #train_data$Placement=predicted_cl_train_data_CART
#     #######
# 
#     ###actual class label of test data  ###
#     test.r= (test_data$Placement)*(max(data$Placement)-min(data$Placement))+min(data$Placement)
# 
#     ############ predicted class label form cart output ########
#     test.r_cart=predicted_cl_total_data_CART[test_row_number]
#     ##############
# 
# 
#     n <- names(train_data)
#     f <- as.formula(paste("Placement ~", paste(n[!n %in% "Placement"], collapse = " + ")))
# 
#     nn1 <- neuralnet::neuralnet(f,data=train_data,hidden=c(3),startweights=seq(1,100,by=0.1),linear.output=T)
#     #plot(nn1)
# 
#     n1=(names(test_data))
# 
#     #pr.nn1 <- compute(nn1,test_data[,n1[1:30]])
#     pr.nn1 <- compute(nn1,test_data[,n1[1:5]])
# 
# 
#     ########  predicted class label of test data after nn #########
#     pr.nn1_1=pr.nn1$net.result*(max(data$Placement)-min(data$Placement))+min(data$Placement)
#     ##########
# 
#     MSE.nn1 <- sum((test.r - pr.nn1_1)^2)/nrow(test_data)
#     print(paste(MSE.nn1))
#     MAPE.nn1 <- (sum(abs(test.r - pr.nn1_1))/nrow(test_data))*100
#     MAPE.nn1
#     print(paste(MAPE.nn1))
# 
# 
#     ### confusion matrix ########
# 
#     #table(data[201:236,]$Placement, as.integer(round(pr.nn1$net.resul[,1])))
#     confusion_matrix_cart_nn<-table(test.r, as.integer(round(pr.nn1$net.resul[,1])))
#     print(confusion_matrix_cart_nn)
# 

    ############

    
    ########################## KNN ######################
    
    # set.seed(400)
    # ctrl <- trainControl(method="repeatedcv",repeats = 3)
    # knnFit <- train(Placement ~ ., data = train_data, method = "knn", trControl = ctrl, preProcess = c("center","scale"),tuneLength = 20)
    # 

