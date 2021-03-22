library(caret)
library(elmNNRcpp)
#Define type of problem reached by the implementation
ELMmodell <- list(label = "Extreme Learning Machine",
  type  ="Regression",
             library="elmNNRcpp",
             loop   =NULL,
             prob = NULL)

  ELMmodell$parameters = data.frame(parameter = c('nhid','actfun','init_weights'),
                          class = c("numeric","character","character"),
                          label = c('#Hidden Units',"#Activation Function", "#Weights Initialization"))
  
  ELMmodell$grid = function(x, y, len = NULL, search = "grid") 
  {
    funs           <-c('sig','sin','radbas','hardlim','hardlims','satlins','tansig','tribas','relu','purelin')
    weights        <- c('normal_gaussian','uniform_positive','uniform_negative')

    
    if(search == "grid")     {
      out <- data.frame(expand.grid(nhid = floor(runif(len, min = 1, max = 20)),
                                    actfun=as.character(funs),
                                    init_weights=as.character(weights)))
    } 
    else 
    {
      out <- data.frame(nhid = sample(1:20, replace = TRUE, size = len),
                        actfun = sample(fun, replace = TRUE, size = len),
                        init_weights = sample(weights, replace = TRUE, size = len))
    }
  }
  
  
  ELMmodell$fit = function(x, y, wts, param, lev, last, classProbs, ...) {
      elmNNRcpp::elm_train(x = as.matrix(x), y = as.matrix(y),
        nhid         = param$nhid,
        actfun       = as.character(param$actfun),
        init_weights = as.character(param$init_weights),
        ...
      )
    }
  
  ELMmodell$predict = function(modelFit, newdata, submodels = NULL)
   elmNNRcpp::elm_predict(modelFit, as.matrix(newdata))
  
  ELMmodell$sort = function(x) x[order(x$nhid),]
  
  ##########################################################################
  # modelInfo <- list(label = "Extreme Learning Machine",
  #                   library = "brnn",
  #                   type = "Regression",
  #                   parameters = data.frame(parameter = 'neurons',
  #                                           class = "numeric",
  #                                           label = '# Neurons'),
  #                   grid = function(x, y, len = NULL, search = "grid") {
  #                     if(search == "grid") {
  #                       out <- expand.grid(neurons = 1:len)
  #                     } else {
  #                       out <- data.frame(neurons = sample(1:20, replace = TRUE, size = len))
  #                     }
  #                     out
  #                   },
  #                   loop = NULL,
  #                   fit = function(x, y, wts, param, lev, last, classProbs, ...)
  #                     brnn::brnn(as.matrix(x), y, neurons = param$neurons, ...),
  #                   predict = function(modelFit, newdata, submodels = NULL)
  #                     predict(modelFit,as.matrix(newdata)),
  #                   prob = NULL,
  #                   predictors = function(x, s = NULL, ...) 
  #                     names(x$x_spread),
  #                   tags = c("Bayesian Model", "Neural Network", "Regularization"),
  #                   prob = NULL,
  #                   sort = function(x) x[order(x[,1]),])
  #########################################################################################
#test com mtcars
# data(mtcars)
# 
#   setwd("C:/Users/Usuario/Dropbox/Paper-Wind")
#   
#   #Suprime warning mensages
#   options(warn=-1)
#   set.seed(1234)
#   
#   #Run this step to load initial informations
#   #Save several directories
#   baseDir       <- getwd()
#   dataDir       <- paste(baseDir, "Data", sep="/")
#   librariesDir  <- paste(baseDir, "Functions", sep="/")
#   ResultsDir    <- paste(baseDir, "Results", sep="/")
#   
#   
#   setwd(dataDir)
#   dados <- read.csv("dataset_wind_.txt", header = T, sep = "\t", dec = ",")
#   
#   input <- dados %>% select(-Power) %>% as.matrix()
#   output <- dados %>% select(Power) %>% as.matrix()
#   
#  
#   input1 <- data.frame(input[2:4439,],output[2:4439,1])
#   output1 <- data.frame(output[1:4438,1])
#   
#   cut <- floor(dim(input1)[1]*0.7)
#   n <- dim(input1)[1]
#   
#   x_treino <- input1[1:cut,]
#   x_test <- tail(input1,n-cut)
#   
#   y_treino <- as.matrix(output1[1:cut,])
#   y_test <- data.frame(tail(output1,n-cut))
#   
# trControl <- trainControl(method = "cv",number = 5)
# 
 # ELM<-train(X_trainm[1:cut,],Y_train,
 #       method=ELMmodell,
 #      preProc = process,
 #       tuneLength = 5,
 #       trControl = trControl,
 #      metric="Rsquared")
# 
# ELM<-train(x_treino,as.numeric(y_treino),
#            method="rvmLinear",
#            preProc = c("center", "scale"),
#            tuneLength = 5,
#            trControl = trControl,
#            metric="Rsquared")
# 
# ggplot(ELM) + scale_x_log10()
# 
# y_pred<-predict(ELM,x_test)
# cor(y_test,y_pred)^2
