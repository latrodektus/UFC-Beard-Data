

set_up_btm<-function(predictors,winner,loser){
  winner$ID<-as.factor(winner$ID)
  loser$ID<-as.factor(loser$ID)
  names(predictors)[1]<-"ID"
  predictors<-arrange(predictors,ID)
  row.names(predictors)<-predictors$ID
  names(winner)[1]<-"ID"
  names(loser)[1]<-"ID"
  all<-rbind(winner,loser)
  all$winning<-c(rep("yes",length(winner$ID)),rep("no",length(winner$ID)))
  all$ID<-as.factor(all$ID)
  winner2<-filter(all,winning=="yes")
  loser2<-filter(all,winning=="no")
  beards<-list(winner=winner2,loser=loser2,predictors=predictors)
  return(beards)
}