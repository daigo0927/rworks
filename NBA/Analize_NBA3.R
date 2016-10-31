# Analize NBA data 2\
# point_score_r, point_score_p : time-sorted match list, regular season, playoffs
# point_by_team[[i]] : match list integurated by i-th team
# head_to_head[[i]][[j]] : match list integurated by team coupling i vs j
# score_sum_matrix[[i]] : 30*30 score sum matrix, FGM, TPM, FTM
# match_num : match number of team i vs j
# score_list :list for each score, for 2*1230 matches
# Num_team:30
# score_name : 9 score names vector
# sort_score_p : sorted palyoffs score

# estimate compatibility
versus <- expand.grid(1:length(team_name), 1:length(team_name))
h <- head_to_head
for(i in 1:nrow(versus)){
  vs1 <- versus[i,1]
  vs2 <- versus[i,2]
  if(vs1==vs2){next}
  
  a <- apply(head_to_head[[vs1]][[vs2]][,score_name], 2, mean)
  b <- apply(head_to_head[[vs1]][[vs2]][,score_name], 1, function(x){x-a})
  
  h[[vs1]][[vs2]][,score_name] <- t(b)
}


compatibility <- lapply(h, function(x){
  do.call(rbind, x)
})

comp_stack <- do.call(rbind, compatibility)

score_name_child <- 'FGM'
hist(comp_stack[,score_name_child]+20, breaks=30, 
     freq=T, 
     main=paste(score_name_child, 'histogram'),
     xlab=score_name_child
     )
curve(dnorm(x, mean=mean(comp_stack[,score_name_child]), sd=sd(comp_stack[,score_name_child])),
      add=T
      )







