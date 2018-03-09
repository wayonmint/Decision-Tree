#read data 
Mydata <- read.csv("Mushrooms.csv")

#view dataset information
str(Mydata)

#views rows and colums
dim(Mydata)

#view head of dataset
head(Mydata)

#view tail of dataset
tail(Mydata)

#information gain function 
InformationGain <- function( tble ) {
  tble <- as.data.frame.matrix(tble)
  entropyBefore <- Entropy(colSums(tble))
  s <- rowSums(tble)
  entropyAfter <- sum (s / sum(s) * apply(tble, MARGIN = 1, FUN = Entropy ))
  informationGain <- entropyBefore - entropyAfter
  return (informationGain)
}

#information gain of attribute ex.odor
tble <- table(Mydata[,c('odor', 'class')])
InformationGain(tble)

# split dataset into trainset and testset 
set.seed(142)
dt = sort(sample(nrow(Mydata), nrow(Mydata)*.8))
trainset <- na.omit(Mydata[dt,])
testset <- na.omit(Mydata[-dt,])

#show rows and colums of trainset
dim(trainset)

#show rows and colums of testset
dim(testset)

#Train Model
tree_train <- rpart(class~.,data=trainset,control = rpart.control(cp = .0005))

#plot tree model
rpart.plot(tree_train)

#print splitting rules of tree
print(tree_train)

#predict with testset
pred <- predict(tree_train,testset,type = "class")

#view Cross-Validation dataset
table(pred,testset$class)

#view  Confusion Matrix and Statistics
confusionMatrix(pred, testset$class)
