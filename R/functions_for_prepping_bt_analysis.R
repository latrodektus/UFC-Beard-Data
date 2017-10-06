

randomization_of_beards<-function(winner,loser){
  
  all<-rbind(winner,loser)
  all$winning<-c(rep("yes",length(winner$ID)),rep("no",length(winner$ID)))
  reps<-1000
  prop_beard_wins<-NULL
  list_beard_winners<-list()
  j<-1
  p.value<-NULL
  prop_beard_wins_list<-NULL
  for (i in 1:reps){
    all$facehair <- sample(all$facehair)  
    winner_ran<-filter(all,winning=="yes")
    loser_ran<-filter(all,winning=="no")
    combination<-paste0(winner_ran$facehair,loser_ran$facehair)
    prop_beard_wins<-sum(combination=="31")/(sum(combination=="31")+sum(combination=="13"))
    if(prop_beard_wins>0.55){
      list_beard_winners[[j]]<-all
      prop_beard_wins_list[j]<-prop_beard_wins
      j<-j+1
    } 
  }
  
  for (k in 1:(j-1)){
    w2<-filter(list_beard_winners[[k]],winning=="yes")
    l2<-filter(list_beard_winners[[k]],winning=="no")
    test<-set_up_btm(predictors,w2,l2)
    model_rand<-BTm(player1=winner,player2=loser,
                formula = ~ prev + facehair + ht[ID] + reach[ID] + stance[ID] +
                  (1|ID), id="ID",data=test)
    p.value[k]<-summary(model_rand)$fixef[2,4]
  }
   power<-data_frame(prop_beard_wins=prop_beard_wins_list,p.value=p.value) 
   ggplot(power,aes(x=prop_beard_wins,y=p.value))+geom_point(alpha=0.5)+geom_hline(yintercept = 0.05,linetype="dashed")+geom_smooth(se=FALSE)+theme_bw()+labs(x="Proportion of fights won by the bearded competitor",y="P-value for the facial hair term in the model")
  ggsave("figuers/power_analysis.pdf")
   }




set_up_btm<-function(predictors,winner,loser){
  
  names(predictors)[1]<-"ID"
  predictors<-arrange(predictors,ID)
 row.names(predictors)<-predictors$ID
  names(winner)[1]<-"ID"
  names(loser)[1]<-"ID"
  all<-rbind(winner,loser)
  all$winning<-c(rep("yes",length(winner$ID)),rep("no",length(winner$ID)))
  all$ID<-as.factor(all$ID)
  all$facehair<-as.factor(all$facehair)
  winner2<-subset(all,all$winning=="yes")
  loser2<-subset(all,winning=="no")
  
  #<sad>
  #winner2$ID<-as.factor(winner2$ID)
  #loser2$ID<-as.factor(loser2$ID)
  #winner2$facehair<-as.factor(winner2$facehair)
  #loser2$facehair<-as.factor(loser2$facehair)
  #predictors$ID<-as.factor(predictors$ID)
  # </sad>
  
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

