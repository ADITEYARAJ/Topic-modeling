update.packages("tidyverse")
update.packages("rlang")
install.packages("quanteda")
install.packages("readtext")
install.packages("irlba")
install.packages("rlang")
install.packages("devtools")
install.packages("quanteda.textmodels")
install.packages("topicmodels")
devtools::install_github("quanteda/quanteda.corpora")
install.packages("syuzhet")
library("syuzhet")
library(rlang)
library(caret)
library(quanteda)
library(readtext)
library(irlba)
library(wordcloud)
library(irlba)
library(topicmodels)
library(rvest)
library(XML)
library(magrittr)
df=read.csv(file.choose(),stringsAsFactors = F)
#View(df)
df1=df
df=df1[,-1]
#tx=apply(df,1,cat)
dim(df)
View(df)
df0=df[,1:2]
colnames(df0)
library(dplyr)
library(tidyverse)
df01=read.csv(file.choose(),stringsAsFactors = F)
df02=df01[,-1]
dim(df02)
df=rbind(df[,1:2],df02)
dim(df)
df=df%>%unite(txt,TITLE,ABSTRACT)
df_=df
dim(df)
colnames(df)
View(df)
(df[2,1])
summary(df[1])

dim(df)
#df=df[,1]
dff=df
df=as.character(df)
dim(df)
View(df)
df=dff
df=apply(df,1,as.character)
View(df)
df=as.data.frame(df,stringsAsFactors = F)
#df=str_replace_all(df, "[[:punct:]]", " ")
df[1]
df=str_replace_all(df, "[[:punct:]]", "")
df[1]
df=gsub('[[:digit:]]+', '', df)
df[1]
df=str_replace_all(df, "[^[:alnum:]]", " ")
View(df[3])
#txt=apply(df0,1,paste)
#dim(txt)
#View(txt)
library(dplyr)
library(tidyr)
#df=unite(df,Merged, df$ABSTRACT,df$TITLE, sep= "")
library(stringr)
#df$p="Null"
#df %>%
 # transmute(p, p = str_c(df$ABSTRACT,df$TITLE, sep=" "))
#df[1,]
#View(df)
library(quanteda)
train.tokens=tokens(df,what="word",remove_punct = T,remove_numbers = T,remove_symbols = T,split_hyphens=T)
View(train.tokens)
dim(train.tokens)

train.tokens=tokens_select(train.tokens,stopwords(),selection = "remove")
train.tokens <- tokens_select(train.tokens,c("A","and","for","the","of","in","a","from","to","An","predict"),selection = "remove")
train.tokens=tokens_tolower(train.tokens)
View(train.tokens)
train.tokens=tokens_wordstem(train.tokens,language = "english")
View(train.tokens)

train.tokens.dfm=dfm(train.tokens,tolower=T)
dim(train.tokens.dfm)
dfmtrimmed <- dfm_trim(train.tokens.dfm, min_docfreq = 10, min_termfreq = 100, verbose = TRUE)
#View(dfmtrimmed)

ncol(dfmtrimmed)
dim(dfmtrimmed)
train.token.matrix=as.matrix(dfmtrimmed)
dim(train.token.matrix)
colnames((train.token.matrix))
#train.tokens1=tokens(df$ABSTRACT,what="word",remove_punct = T,remove_numbers = T,remove_symbols = T,split_hyphens=T)
#View(train.tokens)

#train.tokens1=tokens_select(train.tokens1,stopwords(),selection = "remove")
#train.tokens1 <- tokens_select(train.tokens1,c("A","and","for","the","of","in","a","from","to","An","predict"),selection = "remove")
#train.tokens1=tokens_tolower(train.tokens1)
#View(train.tokens)
#train.tokens1=tokens_wordstem(train.tokens1,language = "english")
#View(train.tokens)

#train.tokens.dfm1=dfm(train.tokens1,tolower=T)
#dfmtrimmed1<- dfm_trim(train.tokens.dfm1, min_docfreq = 10, min_termfreq = 800, verbose = TRUE)
#View(dfmtrimmed)

#ncol(dfmtrimmed1)
#train.token.matrix1=as.matrix(dfmtrimmed1)
dim(train.token.matrix)
#colnames((train.token.matrix1))
#attach((df))
#data=cbind(as.data.frame(train.token.matrix),as.data.frame(train.token.matrix1))
data=train.token.matrix
data=data[,!duplicated(colnames(data))]
dim(data)
sum(duplicated(colnames(data)))
dim(data)

inter.raw.matrix <- as.matrix(data)                           

inter.raw.matrix1 <- as.data.frame(inter.raw.matrix)                           
#View(inter.raw.matrix1)
dim(inter.raw.matrix1)
s <- 0
#for (i in 1:nrow(inter.raw.matrix)){
 # if(sum(inter.raw.matrix1[i,])==0){
  #  inter.raw.matrix2 <- inter.raw.matrix1[-i,]
   # s <- s+1
  #}
  
#}
#print(s)
#inter.raw.matrix3<- inter.raw.matrix2[which(rowSums(inter.raw.matrix2) > 0),]
#function for calculating tje tem frequency
term.frequency <- function(row){
  row/sum(row)  
}
#function for calculating inverse document frequency
inverse.doc.freq <- function(col){
  cor.size <- length(col)
  doc.size <- length(which(col>0))
  log10(cor.size/doc.size)
}  
#fun for calculating TF-IDF
tf.idf <- function(tf,idf){
  tf*idf
}
#first normalize all document via tf
inter.raw.dfm <- apply(inter.raw.matrix1,1,term.frequency)
dim(inter.raw.dfm)
#second,calculate IDF
inter.raw.idf <- apply(inter.raw.matrix1, 2,inverse.doc.freq)
#lastly calculate TF-IDF
inter.raw.tfidf <- apply(inter.raw.dfm, 2,tf.idf,idf=inter.raw.idf)
dim(inter.raw.tfidf)
View(inter.raw.tfidf)
str(inter.raw.tfidf)
train.tokens.tfidf <- t(inter.raw.tfidf)
#View(train.tokens.tfidf)
#incomplete.cases <- which(!complete.cases(train.tokens.tfidf))
train.tokens.tfidf <- as.data.frame(train.tokens.tfidf)
View(df)
y=df[,3:8]
y <- sapply(y, as.logical)
View(y)
#View(y)
dim(data)
#View(data)
#colnames(c(df$Computer.Science,df$Physics,df$Mathematics,df$Statistics,df$Quantitative.Biology,df$Quantitative.Finance))[,6] <- "y"
data1=cbind(y,train.tokens.tfidf)
View(data1)
#normalize = function(x){return((x-min(x))/(max(x)-min(x)))
#}
data1=data1[,-c(52,90,134)]

colnames(data1)
#concrete_norm=as.data.frame(lapply(data1, normalize))
#View(concrete_norm)
#View(y)
index=createDataPartition((data1[,1]),time=1,p=0.7,list=F)
train=data1[index,]
test=data1[-index,]
#train=train[-which(!complete.cases(train)),]
length(which(!complete.cases(train)))
train=train[-which(!complete.cases(train)),]
#View(train)

length(which(!complete.cases(test)))
test=test[-which(!complete.cases(test)),]
View(train)
trainX=train[,7:628]
testX=(test[,7:628])
trainY=train[,1:6]
testY=test[,1:6]
View(trainY)
labels <- colnames(trainY)
library(mlr)
colnames(train[132])
data.task <- makeMultilabelTask(id='multi', data = train, target = labels)
str(data1)

learn.rfsrc <- makeLearner("multilabel.randomForestSRC", ntree = 100, importance = TRUE)
learn.rfsrc
data.task
modelRFSRC <- train(learn.rfsrc, data.task, subset = 1:10000)
modelRFSRC
predRFSRC <- predict(modelRFSRC, task = data.task, subset = 10001:14644)

performance <- performance(predRFSRC, measures = list(multilabel.hamloss, multilabel.subset01, multilabel.f1, multilabel.acc))
accuracy_rfsrc <- performance[4]
accuracy_rfsrc <- round(accuracy_rfsrc*100, 2)

cat('Accuracy:',accuracy_rfsrc)
```

#Predicting test data
```{r}
predictions <- predict(modelRFSRC, newdata = testPC)
predictions <- as.data.frame(predictions)
colnames(predictions) <- labels
```

#Submitting results
```{r}
submission_select <- data.frame(id = testId, predictions)
write.csv(submission_select, file="multilabel_classification_submission.csv", row.names = FALSE)
```



















library(randomForestSRC)
train<-as.data.frame(train)
#View(train)
library(tree)
#install.packages("makeMultilabelTask")
#library(makeMultilabelTask)
#label=colnames(train)[1:6]
#colnames(train)[52]
#train=train[,-52]
#yeast.task = makeMultilabelTask(id = "multi", data = train, target = label)
#View(train)
library(class)
length(train)
length(trainY)
dim(train)
dim(trainY)
length(which(!complete.cases(trainY)))
sum(duplicated(colnames(train)))
#install.packages("neuralnet")
library(neuralnet)
attach(train)
View(df)
View(train)
cor(train$Computer.Science,train$Physics)
cor(y)
library(MultivariateRandomForest)
n_tree=1
m_feature=1
min_leaf=1
trainX=as.matrix(trainX)
trainY=as.matrix(trainY)
testX=as.matrix(testX)
testY=as.matrix(testY)
dim(testX)


























memory.size()
memory.size(TRUE)
memory.limit()
sessionInfo()

Prediction=build_forest_predict(trainX, trainY,n_tree=1,m_feature=1,min_leaf=1, testX)

install.packages("utiml")
library(utiml)
library(mldr)
write.csv(data1,"data1.xml")
corel5k <- mldr("data1")
library(mlr)
install.packages("OpenML")
library(OpenML)
target=C(Computer.Science,Mathematics,Physics,Quantitative.Biology,Quantitative.Finance,Statistics)
scene.task = makeMultilabelTask(data = train, target = trainY)
ds <- create_holdout_partition(data1, c(train=0.65, test=0.35), "iterative")
model=br(train,"RF")














concrete_model = neuralnet(Computer.Science+Mathematics+Physics+Quantitative.Biology+Quantitative.Finance+Statistics~train,data=train,hidden =c(5,3)) 
concrete_model
saveRDS(concrete_model, "concrete_model1.rds")
getwd()
concrete_model2 <- readRDS("concrete_model1.rds")
test=as.matrix(test)
View(test)
test=as.matrix(test)
dim(test)
model_results=compute(concrete_model2,testX)
View(concrete_model1)
num_hidden_layers
weights[[7 + 1]]
cbind(1, test) %*% weights[[6 + 1]]
View(weights)
?neuralnet
plot(concrete_model)
?compute


test_pred <- knn(train = train, test = test,
                      cl = trainY[,2], k=21)
library(gmodels)
CrossTable(x = testY[,1], y = test_pred,
           prop.chisq=FALSE)

sum(test_pred==testY[,1])
length(test_pred)
length(test[,1])

table(test_pred,testY[,2])
test__ <- read.csv(file.choose())
test_pred1 <- knn(train = train, test = test__,
                 cl = trainY[,1], k=21)
test_pred <- knn(train=train,test=)


####

fr=function(i){
  ifelse(i==TRUE,1,0)
}





















multivar_fit<-rfsrc(cbind(train$Computer.Science,train$Physics,train$Mathematics,train$Statistics,train$Quantitative.Biology,train$Quantitative.Finance,data)~.,data=train)





print(multivar_fit)
save(multivar_fit, "multivar_fit.RData")
saveRDS(multivar_fit, "multivar_fit.rds")
getwd()
my_model <- readRDS("multivar_fit.rds")
View(my_model)
my_model
#model <- readRDS("multivar_fit.rds")
model=my_model$regrOutput
View(model)
model=as.matrix(model)
model$Computer.Science
model1=cbind(model$Computer.Science$predicted,model$Physics$predicted,model$Mathematics$predicted,model$Statistics$predicted,model$Quantitative.Biology$predicted,model$Quantitative.Finance$predicted)
model2=as.data.frame(model1)
View(model2)
dim(model2)
dim(trainY)
mt <- function(j){
ifelse(j<=0.5,0,1)
}
  
model2=apply(model2,1,mt)
View(model2)
model2=t(model2)
View(model2)
dim(model2)
model2=as.data.frame(model2)
table(trainY[1:14679,],model2)
confusionMatrix(trainY[1:14679,],model2)
install.packages("MLmetrics")
library(MLmetrics)
str(model2)
trainY=apply(trainY,1,as.numeric)
str(trainY)
View(trainY)
trainY=as.data.frame(trainY)
trainY=t(trainY)

F1_Score(trainY[1:14679,1],model2[,1], positive = NULL)
F1_Score(trainY[1:14679,2],model2[,2], positive = NULL)
F1_Score(trainY[1:14679,3],model2[,3], positive = NULL)
F1_Score(trainY[1:14679,4],model2[,4], positive = NULL)
F1_Score(trainY[1:14679,5],model2[,5], positive = NULL)
F1_Score(trainY[1:14679,6],model2[,6], positive = NULL)
(0.99+0.97+0.77+0.76+0.74+0.62)/6
preds<-predict.rfsrc(my_model,data=test,importance = T)


length(trainY)
length(model2$V2)

View(multivar_fit)
a=multivar_fit$regrOutput$Computer.Science$predicted
b=multivar_fit$regrOutput$Physics$predicted
c=multivar_fit$regrOutput$Mathematics$predicted
d=multivar_fit$regrOutput$Statistics$predicted
e=multivar_fit$regrOutput$Quantitative.Biology$predicted
f=multivar_fit$regrOutput$Quantitative.Finance$predicted
ou <- function(t)
{
  for (i in t)
  {
    if(i<0.5)
    {
      i=0
    }
    if(i>=0.5){
      i=1}
  }
}
c=ou(c)
for(j in c(a,b,c,d,e,f))
{
  print(j)
}
b

for (i in 1:length(j)){
  if(j[i]<0.5){
    j[i]=0
  }
  if(j[i]>=0.5){
    j[i]=1
    
  }
}
b=apply(b,1,ou)
b=ou(b)
View()
View(testY)
confusionMatrix(a,trainY$Computer.Science)
table(a)
table(trainY$Computer.Science)
length(a)









library(randomForest)
library(caret)
View(df)
y=df[,5:10]
View(y)
dim(data)
#colnames(c(df$Computer.Science,df$Physics,df$Mathematics,df$Statistics,df$Quantitative.Biology,df$Quantitative.Finance))[,6] <- "y"
data1=cbind(y,data)
View(data1)
index=createDataPartition((data1$Computer.Science),time=1,p=0.7,list=F)
train=data1[index,]
test=data1[-index,]
View(test)
trainX=matrix(train[,7:633])
testX=matrix(test[,7:633])
trainY=matrix(train[,1:6])
testY=matrix(test[,1:6])
#library(MultivariateRandomForest)
library(plyr)
install.packages("mlr")
library(mlr)
install.packages("randomForestSRC")
library(randomForestSRC)
install.packages("MultivariateRandomForest")
library(MultivariateRandomForest)
Prediction=MultivariateRandomForest(trainX, trainY, n_tree=5, mtree=6, min_leaf=5, testX)
mod=rfsrc(Multivar(Computer.Science,Physics,Mathematics,Statistics,Quantitative.Biology,Quantitative.Finance)~.,my.data=train)
rfs
bays_data<-as.data.frame(cbind(train$Computer.Science,train$Physics,train$Mathematics,train$Statistics,train$Quantitative.Biology,train$Quantitative.Finance,trainX))
bays_data<-as.data.frame((train))
multivar_fit<-rfsrc(cbind(train$Computer.Science,train$Physics,train$Mathematics,train$Statistics,train$Quantitative.Biology,train$Quantitative.Finance,data)~.,data=bays_data)
#multivar_fit<-rfsrc(y~.,data=bays_data)
preds<-predict.rfsrc(my_model,data=test,importance = T)
















model=tree(~.,data=train)
multivar_fit
y_pred = predict(multivar_fit, newdata = test)
confusionMatrix(class_prediction, test$Computer.Science)
View(y_pred)
o=y_pred$regrOutput
View(o)
table(y_pred)
y_pred
prediction_y=y_pred$yvar[,2:7]
dim(prediction_y)
confusionMatrix(prediction_y[,1],testY[,1])
table(prediction_y[,6])
table(testY[,6])
View(prediction_y[,1])
View(testY[,1])
dim(testY)
View(test)
testY=test[,1:6]
dim(testY)
View(y_pred$yvar)
testY=as.matrix(as.factor(testY))
prediction_y=as.matrix(as.factor(prediction_y))
confusionMatrix(prediction_y[,1],testY[,1])
table(prediction_y[,1])
prediction_y
View(class_prediction)
table(class_prediction)
table(test$Computer.Science)

train.token_df <- cbind(Computer.Science,Physics,Mathematics,Statistics,Quantitative.Biology,Quantitative.Finance,as.data.frame(train.token.matrix),as.data.frame(train.token.matrix1))
train.token_df=train.token_df[,!duplicated(colnames(train.token_df))]

View(train.token_df)
dim(train.token_df)
model1=train(c(Computer.Science,Physics,Mathematics,Statistics,Quantitative.Biology,Quantitative.Finance)~.,data=train.token_df,method="rpart")
library(tree)
model=tree(Computer.Science~.,data=train.token_df)
summary(train.token_df)
sum(duplicated(colnames(train.token_df)))
unique(colnames(train.token_df)) 
dim(train.token_df)
dim(train.token_df$LEN_TITLE)
length(which(!complete.cases(train.token_df)))
