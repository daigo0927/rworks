# Analyze NBA data
# point_score_r, point_score_p : time-sorted match list, regular season, playoffs
# point_by_team[[i]] : match list integurated by i-th team
# head_to_head[[i]][[j]] : match list integurated by team coupling i vs j
# score_sum_matrix[[i]] : 30*30 score sum matrix, FGM, TPM, FTM
# match_num : match number of team i vs j
# Num_team:30


# score name vector
score_name <- colnames(head_to_head[[1]][[1]])
score_name <- score_name[-c(which(score_name==c('Player', 'Opponent', 'state')))]
# histogram and fitting gaussian curve
score_list  <- list()
for(i in 1:length(score_name)){
  score_list[[i]] <- c(point_score_r[, paste0(score_name[i],'1')], point_score_r[,paste0(score_name[i], '2')])
}
names(score_list) <- score_name

# set score index on demand
score_name_child <- 'FGA'

hist(score_list[[score_name_child]], breaks='Scott', freq=F, xlab=score_name_child)
# hist(score_list[[score_name_child]][1:1230], breaks='Scott', freq=F, add=T, col='#FF000040')
# hist(score_list[[score_name_child]][1231:2460], breaks='Scott', freq=F, add=T, col='#00FF0040')
curve(dnorm(x, mean=mean(score_list[[score_name_child]]), sd=sd(score_list[[score_name_child]])), add=T)


# standardize for each versus
# standard_point[[i]] : i-th team 
# ---> not good -------------------------------
standard_point <- list()
for(i in 1:30){
  standard_point[[i]] <- na.omit(matrix(ncol=ncol(head_to_head[[1]][[1]])))
  colnames(standard_point[[i]]) <- colnames(head_to_head[[1]][[1]])
  for(j in 1:30){
    # standardize head_to_head i vs j score
    standard_ij <- head_to_head[[i]][[j]]
    if(nrow(standard_ij)==0){ next }
    standard_ij[,score_name] <- scale(standard_ij[,score_name])
    standard_point[[i]] <- rbind(standard_point[[i]], standard_ij)
  }
}
names(standard_point) <- team_name


# standardize for each team (for each versus)
# ---> not good ---------------------------------------------------------
standard_point2 <- na.omit(matrix(ncol=ncol(head_to_head[[1]][[1]])))
colnames(standard_point2) <- colnames(head_to_head[[1]][[1]])
for(i in 1:30){
  # standardize for each i-th team
  standard_i <- standard_point[[i]]
  standard_i[,score_name] <- scale(standard_i[,score_name])
  standard_point2 <- rbind(standard_point2, standard_i)
}

# standardize for each team
standard_point3 <- na.omit(matrix(ncol=ncol(head_to_head[[1]][[1]])))
colnames(standard_point3) <- colnames(head_to_head[[1]][[1]])
for(i in 1:30){
  # standardize for each i-th team
  standard_i <- point_by_team[[i]]
  standard_i[,score_name] <- scale(standard_i[,score_name])
  standard_point3 <- rbind(standard_point3, standard_i)
}

# split standard_point3 by home/road 
sp3_home <- standard_point3[which(standard_point3[,'Player']==standard_point3[,'state']), ]
sp3_road <- standard_point3[which(standard_point3[,'Opponent']==standard_point3[,'state']), ]

# histogram of standard_point3
score_name_child <- 'FGA'

hist(standard_point3[,score_name_child], breaks=30, freq=F, xlab=paste0(score_name_child,' standardized'))
hist(sp3_home[,score_name_child], breaks=30 , add=T, freq=F, col='#FF000040')
hist(sp3_road[,score_name_child], breaks=30 , add=T, freq=F, col='#0000FF40')
curve(dnorm(x, mean=mean(sp3_home[,score_name_child], sd=sd(sp3_home[,score_name_child]))), add=T)
curve(dnorm(x, mean=mean(sp3_road[,score_name_child], sd=sd(sp3_road[,score_name_child]))), add=T)
curve(dnorm(x), add=T)

# standardize by each team and sum up -------------------------
standard_by_team <- na.omit(matrix(ncol=ncol(head_to_head[[1]][[1]])))
colnames(standard_point3) <- colnames(head_to_head[[1]][[1]])
for(i in 1:length(team_name)){
  standard_by_team <- rbind(standard_by_team, apply(point_by_team[[i]], 2, scale))
}
score_name_child <- 'FGA'
hist(standard_by_team[,score_name_child], breaks='Scott', xlab=paste0(score_name_child, ' standardize')
     , main=paste0(score_name_child, ' histogram'), ylab='frequency'
     )
hist(standard_by_team[,score_name_child], breaks='Scott', xlab=paste0(score_name_child, ' standardize')
     , main=paste0(score_name_child, ' histogram'), ylab='probability density', freq=F
)
curve(dnorm(x), add=T)


# compare home/ road matches
count_hl <- c(0,0)
names(count_hl) <- c('TRUE', 'FALSE')
for(i in 1:30){
  for(j in 1:30){
    if(i==j){next}
    match <- head_to_head[[i]][[j]]
    if(length(which(match[,'state']==match[,'Player']))==1){
      home <- match[which(match[,'state']==match[,'Player']),][score_name]
    }else{
      home <- apply(match[which(match[,'state']==match[,'Player']),], 2, mean)[score_name]
    }
    if(length(which(match[,'state']==match[,'Opponent']))==1){
      road <- match[which(match[,'state']==match[,'Opponent']),][score_name]
    }else{
      road <- apply(match[which(match[,'state']==match[,'Opponent']),], 2, mean)[score_name]
    }
    if(length(table(home>road))==1){
      count_hl[names(table(home>road))] <- count_hl[names(table(home>road))]+table(home>road)
    }else{
      count_hl['TRUE'] <- count_hl['TRUE']+table(home>road)['TRUE']
      count_hl['FALSE'] <- count_hl['FALSE']+ table(home>road)['FALSE']
    }
  }
}



# OD rating for each score, 82 matches for each team -----------------------------------
# score_sum_matrix -> Offense, Defense rate

D_init <- rep(1, Num_team)
names(D_init) <- team_name

Def_rate <- list()
Off_rate <- list()
 
for(i in 1:length(score_sum_matrix)){
  
  Def_rate[[i]] <- D_init
  score <- score_sum_matrix[[i]]
  
  for(j in 1:10000){
    Def_rate[[i]] <- score%*%(1/(t(score)%*%(1/Def_rate[[i]])) )
  }
  Off_rate[[i]] <- t(score)%*%(1/Def_rate[[i]])/82
  
  rownames(Def_rate[[i]]) <- team_name
  rownames(Off_rate[[i]]) <- team_name
  
}

names(Def_rate) <- score_name
names(Off_rate) <- score_name

# make gaussian parameter of OD rating -----------------------------------
D_mean <- Def_rate
O_mean <- Off_rate
D_sd <- list()
O_sd <- list()
D_performed <- list()
O_performed <- list()

for(i in 1:length(score_name)){ 
  
  s <- score_name[i]
  
  D_performed[[i]] <- matrix(nrow=length(team_name), ncol=82)
  rownames(D_performed[[i]]) <- team_name
  O_performed[[i]] <- matrix(nrow=length(team_name), ncol=82)
  rownames(O_performed[[i]]) <- team_name

  n <- rep(1,length=length(team_name))
  names(n) <- team_name
  
  for(j in 1:nrow(point_score_r)){
    match <- as.numeric(point_score_r[j,])
    names(match) <- colnames(point_score_r)
    
    D_performed[[i]][match['Player1'],n[match['Player1']]] <- match[paste0(s,2)]/O_mean[[i]][match['Player2']]
    O_performed[[i]][match['Player1'],n[match['Player1']]] <- match[paste0(s,1)]/D_mean[[i]][match['Player2']]
  
    D_performed[[i]][match['Player2'],n[match['Player2']]] <- match[paste0(s,1)]/O_mean[[i]][match['Player1']]
    O_performed[[i]][match['Player2'],n[match['Player2']]] <- match[paste0(s,2)]/D_mean[[i]][match['Player1']]
    
    n[c(match['Player1'], match['Player2'])] <- n[c(match['Player1'], match['Player2'])]+1
   }
}


D_sd <- lapply(D_performed, function(x){apply(x, 1, sd)})
names(D_sd) <- score_name
O_sd <- lapply(O_performed, function(x){apply(x, 1, sd)})
names(O_sd) <- score_name

# comparative method 1 : head to head fixed gaussian estimation -------------
# will be less robust
htoh_mean <- list(matrix(0,30,30), matrix(0,30,30), matrix(0,30,30), matrix(0,30,30), matrix(0,30,30), matrix(0,30,30), matrix(0,30,30), matrix(0,30,30), matrix(0,30,30))
htoh_sd <- list(matrix(0,30,30), matrix(0,30,30), matrix(0,30,30), matrix(0,30,30), matrix(0,30,30), matrix(0,30,30), matrix(0,30,30), matrix(0,30,30), matrix(0,30,30))
for(i in 1:length(team_name)){
  for(j in 1:length(team_name)){
    for(k in 1:length(score_name)){
      htoh_mean[[k]][j,i] <- mean(head_to_head[[i]][[j]][,score_name[k]])
      htoh_sd[[k]][j,i] <- sd(head_to_head[[i]][[j]][,score_name[k]])
    }
  }
}
names(htoh_mean) <- score_name
names(htoh_sd) <- score_name

# comparative method 2 : opponent free gaussian estimation -----------------------------
of_mean <- list(rep(0,30), rep(0,30), rep(0,30), rep(0,30), rep(0,30), rep(0,30), rep(0,30), rep(0,30), rep(0,30))
of_sd <- list(rep(0,30), rep(0,30), rep(0,30), rep(0,30), rep(0,30), rep(0,30), rep(0,30), rep(0,30), rep(0,30))
for(i in 1:length(team_name)){
  for(k in 1:length(score_name)){
    of_mean[[k]][i] <- mean(point_by_team[[i]][,score_name[k]])
    of_sd[[k]][i] <- sd(point_by_team[[i]][,score_name[k]])
  }
}
names(of_mean) <- score_name
names(of_sd) <- score_name


# conparative method 3 : poisson distribution
poi <- list()
for(i in 1:length(score_name)){ poi[[i]] <- rep(0,30) }
names(poi) <- score_name

for(i in 1:length(team_name)){
  for(k in 1:length(score_name)){
    poi[[k]][i] <- mean(point_by_team[[i]][,score_name[k]])
  }
}


# proposing method 2 : bayesian estimation for head to head -----------------------
# estimate posterior distribution
# likelihood : head_to_head, set sd as apply(point_by_team, 2, sd)
# prior distribution : opponent free
# prepare some parameter for some likelihood variance
bayes_mean <- list()
bayes_sd <- list()

bayes_mean_s <- list() # small variance
bayes_sd_s <- list()

bayes_mean_l <- list() # large variance
bayes_sd_l <- list()

bayes_mean_set <- list() # large variance
bayes_sd_set <- list()

for(i in 1:length(score_name)){
  bayes_mean[[i]] <- matrix(0, nrow=length(team_name), ncol = length(team_name))
  bayes_sd[[i]] <- matrix(0, nrow=length(team_name), ncol = length(team_name))
  
  bayes_mean_s[[i]] <- matrix(0, nrow=length(team_name), ncol = length(team_name))
  bayes_sd_s[[i]] <- matrix(0, nrow=length(team_name), ncol = length(team_name))
  
  bayes_mean_l[[i]] <- matrix(0, nrow=length(team_name), ncol = length(team_name))
  bayes_sd_l[[i]] <- matrix(0, nrow=length(team_name), ncol = length(team_name))
  
  bayes_mean_set[[i]] <- matrix(0, nrow=length(team_name), ncol = length(team_name))
  bayes_sd_set[[i]] <- matrix(0, nrow=length(team_name), ncol = length(team_name))
}
versus <- expand.grid(1:length(team_name), 1:length(team_name))

# estimate posterior parameter
for(i in 1:nrow(versus)){
  vs1 <- versus[i,1]
  vs2 <- versus[i,2]
  if(vs1==vs2){ next }
  
  for(k in 1:length(score_name)){
    # n : data number for likelihood 
    n <- nrow(head_to_head[[vs1]][[vs2]])
    # likelihood precision parameter lambda : set sd as apply(point_by_team, 2, sd)
    lambda <- 1/(of_sd[[k]][vs1]^2)
    lambda_s <- 1/(of_sd[[k]][vs1]^2/2) # large precision : small variance
    lambda_l <- 1/(of_sd[[k]][vs1]^2*2) # small precision : large variance
    lambda_set <- 1/(sd(comp_stack[,score_name[k]])^2)
    
    # likelihood mean : mu_ML <- ML estimater for head_to_head mean
    mu_ML <- htoh_mean[[k]][vs2,vs1]
    
    # prior (opponent_free) precision parameter
    lambda_pri <- 1/(of_sd[[k]][vs1]^2)
    # prior (opponent_free) mean parameter
    mu_pri <- of_mean[[k]][vs1]
    
    bayes_mean[[k]][vs2, vs1] <- (n*lambda*mu_ML+lambda_pri*mu_pri)/(n*lambda+lambda_pri)
    bayes_sd[[k]][vs2, vs1] <- 1/sqrt(n*lambda+lambda_pri)
    
    bayes_mean_s[[k]][vs2, vs1] <- (n*lambda_s*mu_ML+lambda_pri*mu_pri)/(n*lambda_s+lambda_pri)
    bayes_sd_s[[k]][vs2, vs1] <- 1/sqrt(n*lambda_s+lambda_pri)
    
    bayes_mean_l[[k]][vs2, vs1] <- (n*lambda_l*mu_ML+lambda_pri*mu_pri)/(n*lambda_l+lambda_pri)
    bayes_sd_l[[k]][vs2, vs1] <- 1/sqrt(n*lambda_l+lambda_pri)
    
    bayes_mean_set[[k]][vs2, vs1] <- (n*lambda_set*mu_ML+lambda_pri*mu_pri)/(n*lambda_set+lambda_pri)
    bayes_sd_set[[k]][vs2, vs1] <- 1/sqrt(n*lambda_set+lambda_pri)
  }  
}
names(bayes_mean) <- score_name
names(bayes_sd) <- score_name

names(bayes_mean_s) <- score_name
names(bayes_sd_s) <- score_name

names(bayes_mean_l) <- score_name
names(bayes_sd_l) <- score_name

names(bayes_mean_set) <- score_name
names(bayes_sd_set) <- score_name



# check the utility of OD rating for training data : playoffs --------------------------------------
likelihood <- list()
compare_name <- c('OD', 'head_to_head', 'opponent_free', 'Bayes', 'Poisson_of')
for(i in 1:length(score_name)){
  likelihood[[i]] <- matrix(0, nrow=2*nrow(point_score_r), ncol=length(compare_name))
  colnames(likelihood[[i]]) <- compare_name
}
names(likelihood) <- score_name

n <- 1

versus <- expand.grid(1:length(team_name), 1:length(team_name))
for(i in 1:nrow(versus)){
  vs1 <- versus[i,1]
  vs2 <- versus[i,2]
  
  if(vs1 == vs2){
    next
  }
  for(j in 1:nrow(head_to_head[[vs1]][[vs2]])){
    
    match <- head_to_head[[vs1]][[vs2]][j,]
    for(k in 1:length(score_name)){
      
      # OD likelihood
      O_mean_i <- O_mean[[k]][vs1]
      D_mean_j <- D_mean[[k]][vs2]
      O_sd_i <- O_sd[[k]][vs1]
      D_sd_j <- D_sd[[k]][vs2]
      
      mean_ij <- O_mean_i*D_mean_j
      sd_ij <- sqrt(O_sd_i^2*D_sd_j^2 + O_mean_i^2*D_sd_j^2 + D_mean_j^2*O_sd_i^2)
      
      likelihood[[k]][n, 'OD'] <- dnorm(match[score_name[k]], mean=mean_ij, sd=sd_ij)
      
      # head_to_head fixed likelihood
      likelihood[[k]][n, 'head_to_head'] <- dnorm(match[score_name[k]], mean=htoh_mean[[k]][vs2,vs1], sd=htoh_sd[[k]][vs2,vs1])
      
      # opposite free likelihood
      likelihood[[k]][n, 'opponent_free'] <- dnorm(match[score_name[k]], mean=of_mean[[k]][vs1], sd=of_sd[[k]][vs1])
      
      # bayes likelihood
      likelihood[[k]][n, 'Bayes'] <- dnorm(match[score_name[k]], mean=bayes_mean[[k]][vs2,vs1], sd=sqrt(of_sd[[k]][vs1]^2+bayes_sd[[k]][vs2,vs1]^2))
      
      # poisson likelihood
      likelihood[[k]][n, 'Poisson_of'] <- dpois(match[score_name[k]], lambda=poi[[k]][vs1])
      
    }
    n <- n+1
  }
}

score_name_child <- 'FGA'
boxplot(likelihood[[score_name_child]], ylim=c(0,0.2), xlab=paste(score_name_child, 'regular'))




# check utility of each method for test : playoffs data ------------------------------
likelihood_p <- list()
for(i in 1:length(compare_name)){
  likelihood_p[[i]] <- matrix(0, nrow=nrow(sort_score_p), ncol=length(score_name))
  colnames(likelihood_p[[i]]) <- score_name
}
names(likelihood_p) <- colnames(likelihood[[1]])

for(i in 1:nrow(sort_score_p)){
  match <- sort_score_p[i,]
  match['Player'] <- which(match['Player']==team_name)
  match['Opponent'] <- which(match['Opponent']==team_name)
  names(match) <- names(sort_score_p[i,])
  
  s <- as.numeric(match[score_name])
  names(s) <- score_name
  
  vs <- as.integer(match[c('Player', 'Opponent')])
  names(vs) <- c('Player', 'Opponent')
  
  for(k in 1:length(score_name)){
    # OD likelihood
    O_mean_i <- O_mean[[k]][vs[1]]
    D_mean_j <- D_mean[[k]][vs[2]]
    O_sd_i <- O_sd[[k]][vs[1]]
    D_Sd_j <- D_sd[[k]][vs[2]]
    
    mean_ij <- O_mean_i*D_mean_j
    sd_ij <- sqrt(O_sd_i^2*D_sd_j^2 + O_mean_i^2*D_sd_j^2 + D_mean_j^2*O_sd_i^2)
    
    likelihood_p[['OD']][i,score_name[k]] <- dnorm(s[score_name[k]], mean=mean_ij, sd=sd_ij)
    
    # head to head fixed likelihood
    likelihood_p[['head_to_head']][i,score_name[k]] <- dnorm(s[score_name[k]], mean=htoh_mean[[k]][vs[2], vs[1]], sd=htoh_sd[[k]][vs[2], vs[1]])
    
    # opponent free likelihood
    likelihood_p[['opponent_free']][i,score_name[k]] <- dnorm(s[score_name[k]], mean=of_mean[[k]][vs[1]], sd=of_sd[[k]][vs[1]])
    
    # Bayes likelihood
    likelihood_p[['Bayes']][i,score_name[k]] <- dnorm(s[score_name[k]], mean=bayes_mean[[k]][vs[2],vs[1]], sd=sqrt(of_sd[[k]][vs[1]]^2+bayes_sd[[k]][vs[2],vs[1]]^2))
    
    # Poisson likelihood
    likelihood_p[['Poisson_of']][i,score_name[k]] <- dpois(s[score_name[k]], lambda = poi[[k]][vs[1]])
  }
}

score_name_child <- 'FGA'
boxplot(lapply(likelihood_p, function(x){x[,score_name_child]}), ylim=c(0,0.2), xlab=paste(score_name_child ,'playoffs'))

lapply(likelihood_p, function(x){apply(x, 2, mean)})

lapply(likelihood_p, function(x){apply(x, 2, sd)})



# likelihood comparing, regular season differnciated by likelihood variance --------------------------

likelihood_Bayes <- list()
compare_Bayes <- c( 'head_to_head', 'opponent_free', 'Bayes:small variance', 'Bayes:mid variance', 'Bayes:large variance')
for(i in 1:length(score_name)){
  likelihood_Bayes[[i]] <- matrix(0, nrow=2*nrow(point_score_r), ncol=length(compare_Bayes))
  colnames(likelihood_Bayes[[i]]) <- compare_Bayes
}
names(likelihood_Bayes) <- score_name

likelihood_setBayes <- apply(likelihood_of, c(1,2), function(x){x <- 0})

n <- 1

versus <- expand.grid(1:length(team_name), 1:length(team_name))
for(i in 1:nrow(versus)){
  vs1 <- versus[i,1]
  vs2 <- versus[i,2]
  
  if(vs1 == vs2){
    next
  }
  for(j in 1:nrow(head_to_head[[vs1]][[vs2]])){
    
    match <- head_to_head[[vs1]][[vs2]][j,]
    for(k in 1:length(score_name)){
      
      # head_to_head fixed likelihood
      likelihood_Bayes[[k]][n, 'head_to_head'] <- dnorm(match[score_name[k]], mean=htoh_mean[[k]][vs2,vs1], sd=htoh_sd[[k]][vs2,vs1])
      
      # opponent free likelihood
      likelihood_Bayes[[k]][n, 'opponent_free'] <- dnorm(match[score_name[k]], mean=of_mean[[k]][vs1], sd=of_sd[[k]][vs1])
      
      # bayes likelihood : small varance
      likelihood_Bayes[[k]][n, 'Bayes:small variance'] <- dnorm(match[score_name[k]], mean=bayes_mean_s[[k]][vs2,vs1], sd=sqrt(of_sd[[k]][vs1]^2/2+bayes_sd_s[[k]][vs2,vs1]^2))
      
      # bayes likelihood : mid variance
      likelihood_Bayes[[k]][n, 'Bayes:mid variance'] <- dnorm(match[score_name[k]], mean=bayes_mean[[k]][vs2,vs1], sd=sqrt(of_sd[[k]][vs1]^2+bayes_sd[[k]][vs2,vs1]^2))
      
      # bayes likelihood : large variance
      likelihood_Bayes[[k]][n, 'Bayes:large variance'] <- dnorm(match[score_name[k]], mean=bayes_mean_l[[k]][vs2,vs1], sd=sqrt(of_sd[[k]][vs1]^2*2+bayes_sd_l[[k]][vs2,vs1]^2))
      
      # Bayes likelihood : given var
      likelihood_setBayes[n,k] <- dnorm(match[score_name[[k]]]
                                        , mean=bayes_mean_set[[k]][vs2,vs1]
                                        , sd=sqrt(sd(comp_stack[,score_name[k]])^2+bayes_sd_set[[k]][vs2,vs1]^2)
                                        )
    }
    n <- n+1
  }
}

score_name_child <- 'FGA'
boxplot(likelihood_Bayes[[score_name_child]], ylim=c(0,0.2), xlab='Bayes variance comparing', main=paste0('Regular season ',score_name_child))


# playoffs likelihood comparing differnciated by likelihood precision --------------------------

likelihood_Bayes_p <- list()
for(i in 1:length(compare_Bayes)){
  likelihood_Bayes_p[[i]] <- matrix(0, nrow=nrow(sort_score_p), ncol=length(score_name))
  colnames(likelihood_Bayes_p[[i]]) <- score_name
}
names(likelihood_Bayes_p) <- colnames(likelihood_Bayes[[1]])

likelihood_setBayes_p <- apply(likelihood_sBayes_p, c(1,2), function(x){x <- 0})

for(i in 1:nrow(sort_score_p)){
  match <- sort_score_p[i,]
  match['Player'] <- which(match['Player']==team_name)
  match['Opponent'] <- which(match['Opponent']==team_name)
  names(match) <- names(sort_score_p[i,])
  
  s <- as.numeric(match[score_name])
  names(s) <- score_name
  
  vs <- as.integer(match[c('Player', 'Opponent')])
  names(vs) <- c('Player', 'Opponent')
  
  for(k in 1:length(score_name)){
    
    # head to head fixed likelihood
    likelihood_Bayes_p[['head_to_head']][i,score_name[k]] <- dnorm(s[score_name[k]], mean=htoh_mean[[k]][vs[2], vs[1]], sd=htoh_sd[[k]][vs[2], vs[1]])
    
    # opponent free likelihood
    likelihood_Bayes_p[['opponent_free']][i,score_name[k]] <- dnorm(s[score_name[k]], mean=of_mean[[k]][vs[1]], sd=of_sd[[k]][vs[1]])
    
    # Bayes likelihood : small variance
    likelihood_Bayes_p[['Bayes:small variance']][i,score_name[k]] <- dnorm(s[score_name[k]], mean=bayes_mean_s[[k]][vs[2],vs[1]], sd=sqrt(of_sd[[k]][vs[1]]^2/2+bayes_sd_s[[k]][vs[2],vs[1]]^2))
    
    # Bayes likelihood : variance
    likelihood_Bayes_p[['Bayes:mid variance']][i,score_name[k]] <- dnorm(s[score_name[k]], mean=bayes_mean[[k]][vs[2],vs[1]], sd=sqrt(of_sd[[k]][vs[1]]^2+bayes_sd[[k]][vs[2],vs[1]]^2))
    
    # Bayes likelihood : large variance
    likelihood_Bayes_p[['Bayes:large variance']][i,score_name[k]] <- dnorm(s[score_name[k]], mean=bayes_mean_l[[k]][vs[2],vs[1]], sd=sqrt(of_sd[[k]][vs[1]]^2*2+bayes_sd_l[[k]][vs[2],vs[1]]^2))
    
    # Bayes likelihood : given var
    likelihood_setBayes_p[i,score_name[k]] <- dnorm(s[score_name[[k]]]
                                      , mean=bayes_mean_set[[k]][vs[2],vs[1]]
                                      , sd=sqrt(sd(comp_stack[,score_name[k]])^2+bayes_sd_set[[k]][vs[2],vs[1]]^2)
                                      )
    }
}
lh_Bayes <- likelihood_Bayes
lh_Bayes_p <- likelihood_Bayes_p

# reform likelihood list
likelihood_OD <- sapply(likelihood, function(x){x[,'OD']})
likelihood_htoh <- sapply(likelihood, function(x){x[,'head_to_head']})
likelihood_of <- sapply(likelihood, function(x){x[,'opponent_free']})
likelihood_sBayes <- sapply(lh_Bayes, function(x){x[,'Bayes:small variance']})
likelihood_mBayes <- sapply(lh_Bayes, function(x){x[,'Bayes:mid variance']})
likelihood_lBayes <- sapply(lh_Bayes, function(x){x[,'Bayes:large variance']})
likelihood_Poisson <- sapply(likelihood, function(x){x[,'Poisson_of']})

likelihood_OD_p <- likelihood_p[['OD']]
likelihood_htoh_p <- likelihood_p[['head_to_head']]
likelihood_of_p <- likelihood_p[['opponent_free']]
likelihood_sBayes_p <- lh_Bayes_p[['Bayes:small variance']]
likelihood_mBayes_p <- lh_Bayes_p[['Bayes:mid variance']]
likelihood_lBayes_p <- lh_Bayes_p[['Bayes:large variance']]
likelihood_Poisson_p <- likelihood_p[['Poisson_of']]

score_name_child <- 'FGM'
boxplot(
  likelihood_OD[,score_name_child],
  likelihood_htoh[,score_name_child],
  likelihood_mBayes_p[,score_name_child],
  names=c('head to head', 'opponent free', 'Bayes'),
  ylim=c(0,0.2), xlab='methods', main=paste0('Regular season ', score_name_child) 
)










