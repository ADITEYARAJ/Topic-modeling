df0=read.csv("../input/janatahack-independence-day-2020-ml-hackathon/train.csv",stringsAsFactors = F)
df21=read.csv("../input/janatahack-independence-day-2020-ml-hackathon/test.csv",stringsAsFactors = F)


df1=df0
df01=df1[,-1]

colnames(df01)
df2=df21[,-1]
df=rbind(df01[,1:2],df2)

dim(df)
colnames(df)
library(dplyr)
library(tidyr)
#df=unite(df,Merged, df$ABSTRACT,df$TITLE, sep= "")
library(stringr)
#df$p="Null"
#df %>%
# transmute(p, p = str_c(df$ABSTRACT,df$TITLE, sep=" "))

#View(df)
df=df%>%unite(txt,TITLE,ABSTRACT)
length(df)
colnames(df)
dim(df)
#df_y=df[,2:7]
#df_x=df[1]
#dim(df_y)
#df
#colnames(df_x)
#dim(df_x)
#df[1,1]
#df_x
#df_x=apply(df_x,1,str_replace_all(df_x, "[^[a-z,A-Z]]", ""))

#df_x=str_replace_all(df_x, "[^[a-z,A-Z]]", "")
#(df_x)


#fruits <- c("Aon12e app^le two3//'2 pea1\\Prs", "t2LO0@#/hr45ee bananas")
#d=strsplit(fruits," ")
#str_replace_all(fruits, "[^[:alnum:]]", " ")

#str_replace_all(fruits, "[^[a-z,A-Z]]", "")
#dim(df9)
#df=gsub('[[:digit:]]+', '', df)
#df_x=str_replace_all(df_x, "[^[:alnum:]]", " ")
#dim(df_x)

#df
#class(txt)
#dim(dff)
#dff
dff=df
df=apply(df,1,as.character)
str(df)
dim(df)
length(df)
library(quanteda)
#df$TITLE=str_replace_all(df$TITLE, "[[:punct:]]", "")
#df$TITLE=gsub('[[:digit:]]+', '', df$TITLE)
train.tokens=tokens(df,what="word",remove_punct = T,remove_numbers = T,remove_symbols = T,split_hyphens=T)
#View(train.tokens)
dim(train.tokens)

train.tokens=tokens_select(train.tokens,stopwords(),selection = "remove")
train.tokens <- tokens_select(train.tokens,c("A","and","for","the","of","in","a","from","to","An","predict"),selection = "remove")
train.tokens=tokens_tolower(train.tokens)
#View(train.tokens)
train.tokens=tokens_wordstem(train.tokens,language = "english")
#View(train.tokens)
dim(train.tokens)
train.tokens.dfm=dfm(train.tokens,tolower=T)
dim(train.tokens.dfm)
colnames(train.tokens.dfm)[1:100]

dfmtrimmed <- dfm_trim(train.tokens.dfm, min_docfreq = 10, min_termfreq = 50, verbose = TRUE)
colnames(dfmtrimmed)
train.tokens.dfm
#(dfmtrimmed)
colnames(dfmtrimmed)=str_replace_all(colnames(dfmtrimmed), "[^[:alnum:]]", "")
colnames(dfmtrimmed)=gsub("[^[:alnum:]]", "", colnames(dfmtrimmed))
colnames(dfmtrimmed)=gsub("[[:digit:]]", "", colnames(dfmtrimmed))

train.token.matrix=as.matrix(dfmtrimmed)
dim(train.token.matrix)
train.token.matrix
colnames((train.token.matrix))
#colnames(train.token.matrix)
#df$ABSTRACT=str_replace_all(df$ABSTRACT, "[[:punct:]]", "")
#df$ABSTRACT=gsub('[[:digit:]]+', '', df$ABSTRACT)
#train.tokens1=tokens(df$ABSTRACT,what="word",remove_punct = T,remove_numbers = T,remove_symbols = T,split_hyphens=T)
#View(train.tokens)

#train.tokens1=tokens_select(train.tokens1,stopwords(),selection = "remove")
#train.tokens1 <- tokens_select(train.tokens1,c("A","and","cifar10","u1","2dimension","2n","1x","ell1","1d","t2","t1","o1","su2","for","l1","n1","d1","the","of","in","a","5g","from","f_1","4d","times10","_1","l_1","d2d","mos2","_6","to","An","predict","_","a_1","co2","_0","_4","ell_2","2k","t_1","t_2","h2","u.","l_2","_x","x_1","2d","w.r.t","t_c","k2","x_n","x_i","_3","l2","3d","_2","i.e","e.g"),selection = "remove")
#train.tokens1=tokens_tolower(train.tokens1)
#View(train.tokens)
#train.tokens1=tokens_wordstem(train.tokens1,language = "english")
#View(train.tokens)

#train.tokens.dfm1=dfm(train.tokens1,tolower=T)
#dfmtrimmed1<- dfm_trim(train.tokens.dfm1, min_docfreq = 10, min_termfreq = 50, verbose = TRUE)
#View(dfmtrimmed)
#x="a1~!@#$%6543217890"
#ncol(dfmtrimmed1)
#train.token.matrix1=as.matrix(dfmtrimmed1)
#dim(train.token.matrix1)
#colnames((train.token.matrix1))
#attach((df))
#data=cbind(as.data.frame(train.token.matrix),as.data.frame(train.token.matrix1))
data=train.token.matrix
sum(duplicated(colnames(data)))
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
#View(inter.raw.tfidf)
str(inter.raw.tfidf)
train.tokens.tfidf <- t(inter.raw.tfidf)
#View(train.tokens.tfidf)
#incomplete.cases <- which(!complete.cases(train.tokens.tfidf))
train.tokens.tfidf <- as.data.frame(train.tokens.tfidf)
dim(df1)
colnames(df1)
y=df1[,4:9]
y <- sapply(y, as.logical)
#View(y)
#View(y)
dim(train.tokens.tfidf)

#View(data)
#colnames(c(df$Computer.Science,df$Physics,df$Mathematics,df$Statistics,df$Quantitative.Biology,df$Quantitative.Finance))[,6] <- "y"
data1=train.tokens.tfidf
dim(data1)
colnames(data1)
#normalize = function(x){return((x-min(x))/(max(x)-min(x)))
#}
#cols <- trimws(colnames(data1))
#cols
#cols[make.names(cols)==cols]
#cols
#data1=data1[,-c(3757,2992,2780,1705)]
dim(data1)
#nam=c(1:4060)
#nam
#data1[make.names(colnames(data1))=nam]
colnames(data1)
colnames(data1)
#names=colnames(data1)
#colnames(data1)=make.names(names, unique = FALSE, allow_ = TRUE)
#names


colnames(data1)
#new_data=data1[20973:29961,]
#data2=data1[1:20972,]
#dim(data2)
library(caret)
length(which(!complete.cases(data1)))
#dim(df2)
#dim(df0)
(which(!complete.cases(data1)))
#y=y[-c(20867),]
dim(y)

#data2=data2[-which(!complete.cases(data2)),]
dim(data1)


#index=createDataPartition((data1[,1]),time=1,p=0.7,list=F)
#train=data1[index,]
#test=data1[-index,]
#train=train[-which(!complete.cases(train)),]
#length(which(!complete.cases(train)))
#dim(train)
#train=train[-which(!complete.cases(train)),]
#dim(train)
#View(train)

#length(which(!complete.cases(test)))
#test=test[-which(!complete.cases(test)),]
#dim(test)
#trainX=train[,7:626]
#testX=(test[,7:626])
#trainY=train[,1:6]
#testY=test[,1:6]
#View(trainY)
library(irlba)
train.irlba=irlba(t(data1),nv=100,Maxit=400)
colnames(train.irlba$v)
dim(train.irlba$v)
train.svd=as.data.frame(train.irlba$v)
colnames(train.svd)
dim(train.svd)
new_data=train.svd[20973:29961,]
dim(new_data)
data2=train.svd[1:20972,]
data2[1,1:20]
new_data[1,1:20]

dim(y)
dim(df2)
dim(data1)
dim(df0)
labels <- colnames(y)
labels
dim(y)
dim(data2)


#train.irlba$v[1:10,1:50]
train_svd=data.frame(y,data2)
str(train_svd)
str(new_data)
#data3=cbind(y,data2)
#dim(data3)
#str(data3)
library(mlr)
names(train_svd)=make.names(names(train_svd))
colnames(train_svd)
#str(train)
#length(which(!complete.cases(data1)))
#data1=data1[-which(!complete.cases(data1)),]
#dim(data3)
install.packages("rFerns")
library(rFerns)
data.task <- makeMultilabelTask(id='multi', data = train_svd, target = labels)
data.task

learn.rfsrc <- makeLearner("multilabel.randomForestSRC",ntree = 100, importance = TRUE)
learn.rfsrc
modelRFSRC <- train(learn.rfsrc, data.task, subset = 1:17000)
modelRFSRC
predRFSRC <- predict(modelRFSRC, task = data.task, subset = 17001:20972)
performance <- performance(predRFSRC, measures = list(multilabel.hamloss, multilabel.subset01, multilabel.f1, multilabel.acc))
accuracy_rfsrc <- performance[4]
accuracy_rfsrc <- round(accuracy_rfsrc*100, 2)
cat('Accuracy:',accuracy_rfsrc)
performance[3]
dim(new_data)

dim(test.svd$v)
test.svd=irlba(t(new_data),nv=100,Maxit=400)
test.new=as.data.frame(test.svd$v)
prd=predict(modelRFSRC,newdata=test.new)

prd=predict(modelRFSRC,newdata=new_data)
dim(prd[1:6])
summary(prd)
dim(prd$data)
length(prd)
colnames(df21)
dim(df21)
dim(prd)
prd1=apply(prd$data,2,as.numeric)
submission_select1 <- data.frame(ID =df21[1] , prd1)
dim(submission_select1)
#freq.col =function(col){
#ifelse(col==TRUE,1,0)}
#apply(prd)
write.csv(submission_select1, file="multilabel_classification_submission3.csv", row.names = FALSE)



dim(data1)
labels
data.pred=makeMultilabelTask(id="multi",data=data1,target=labels)
prd=predict(modelRFSRC,newdata=new_data)
dim(prd)
colnames(df21[1])
dim(df21[1])
dim(prd[1:6])
prd0=prd
#fr=function(i){
#   if(i==TRUE)
#      {i=1}
# else{
#    i=0
#    }
#}
str(prd1)
prd1=apply(prd0$data,2,as.numeric)
submission_select1 <- data.frame(ID =df21[1] , prd1)
dim(submission_select1)
#freq.col =function(col){
#ifelse(col==TRUE,1,0)}
#apply(prd)
write.csv(submission_select1, file="multilabel_classification_submission1.csv", row.names = FALSE)




