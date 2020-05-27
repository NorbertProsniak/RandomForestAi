library("party")
library(partykit)
require(MASS)
library(rpart.plot)
library(rpart)
library('randomForest')

cf2<-read.csv('C:/Users/npros/Desktop/mojezadanie/balance-scale.data',header=FALSE)


names(cf2)[1] <- "class"
names(cf2)[2] <- "Left.Weight"
names(cf2)[3] <- "Left.Distance"
names(cf2)[4] <- "Right.Weight"
names(cf2)[5] <- "Right.Distance"


cf2_ctree <- ctree(class ~ 
                     Left.Weight + 
                     Left.Distance + 
                     Right.Weight + 
                     Right.Distance,
                   data=cf2)



print(cf2_ctree)
plot(cf2_ctree)
st <- as.simpleparty(cf2_ctree)
myfun <- function(i) c(
  as.character(i$prediction),
  paste("n =", i$n),
  format(round(i$distribution/i$n, digits = 3), nsmall = 3)
)
plot(st, tp_args = list(FUN = myfun), ep_args = list(justmin = 20))





covtype2<-cf2[,c(1:5)]
covtype2$class <- as.factor(covtype2$class)
table(covtype2$class)

train<-sample(1:nrow(covtype2),300)  
covtype.train<-covtype2[train,]  
covtype.test<-covtype2[-train,] 

rf <- randomForest(class ~ . , data = covtype.train)
cm <- table(covtype.test$class, predict(rf, covtype.test))
cm

cm2 <- cm
err <- rep(0, nrow(cm2))
for (a in 1:nrow(cm2))
{
  for (b in 1:ncol(cm2))
  {
    if (a != b)
    {
      err[a] <- err[a] + cm2[a,b]
    }
    cm2[a,b] <- cm2[a,b] / sum(cm[a,])
  }
  err[a] <- err[a] / sum(cm[a,])
}
round(cbind(cm2, err),2)
#recognition rate
rr <- 0
for (a in 1:nrow(cm))
{
  rr <- rr + cm[a,a]
}
round(rr / sum(cm),2)
