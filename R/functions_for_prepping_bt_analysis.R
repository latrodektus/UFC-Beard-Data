

set_up_btm<-function(predictors,winner,loser){
  
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
  
  
#   names(predictors)[1]<-"ID"
#   predictors<-arrange(predictors,ID)
#   row.names(predictors)<-predictors$ID
#   names(winner)[1]<-"ID"
#   names(loser)[1]<-"ID"
#   winner$ID<-as.factor(winner$ID)
#   loser$ID<-as.factor(loser$ID)
#   
#   all<-rbind(winner,loser)
#   all$winning<-c(rep("yes",length(winner$ID)),rep("no",length(winner$ID)))
#   all$ID<-as.factor(all$ID)
#   winner2<-filter(all,winning=="yes")
#   loser2<-filter(all,winning=="no")
#   beards<-list(winner=winner2,loser=loser2,predictors=predictors)
#   return(beards)
# }

get_bt_abilities<-function(model,predictors=predictors){
  nm <- grep("[ID]", names(coef(model)),fixed = TRUE, value = TRUE)
  IDvar <- gsub("[ID]", "", nm, fixed = TRUE)
  cf <- coef(model)[nm]
  X <- as.matrix(predictors[, IDvar])
  abilities <- X %*% cf
  colnames(abilities) <- "abilities"
  V <- vcov(model)[c(3,4), c(3,4)] # hack!
  res <- cbind(abilities = abilities,se = sqrt(diag(X %*% V %*% t(X))))
  return(data.frame(res))
}


