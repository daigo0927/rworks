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

# hierarchical Bayes estimation ----------------------------------------------------
# use gauss-gamma distribution

# MAP estimation for mu_0, lambda_0, a_0, b_0
# set hyper parameter
mu_0_hyper <- 0
lambda_0_hyper <- 1e-4
a_0_hyper <- 1
b_0_hyper <- 1e-4

# define team parameter
mu_0 <- list()
lambda_0 <- list()
a_0 <- list()
b_0 <- list()
for(k in 1:length(score_name)){
  mu_0[[k]] <- rep(0, length=team_name)
  lambda_0[[k]] <- rep(0, length=team_name)
  a_0[[k]] <- rep(0, length=team_name)
  b_0[[k]] <- rep(0, length=team_name)
}
names(mu_0) <- score_name
names(lambda_0) <- score_name
names(a_0) <- score_name
names(b_0) <- score_name

# MAP estimator for each team parameter -----------------------------
for(i in 1:length(team_name)){
  for(k in 1:length(score_name)){
    
    N <- nrow(point_by_team[[i]])
    mu_ML <- mean(point_by_team[[i]][,score_name[k]])
    m_0 <- mu_0_hyper
    lam_0 <- lambda_0_hyper
    mu_0[[k]][i] <- (N*mu_ML+lam_0*m_0)/(N+lam_0)
    
    lambda_0[[k]][i] <- lam_0+N
    
    a_0[[k]][i] <- a_0_hyper+N/2
    
    sigma_ML <- sd(point_by_team[[i]][,score_name[k]])
    b_0[[k]][i] <- b_0_hyper+1/2*(N*sigma_ML^2+lam_0*N*(mu_ML-m_0)^2/(lam_0+N))
    
  }
}

a_0_init <- 1
b_0_init <- 1e-4

# define posterior distribution parameter -------------------------
mu_N <- list() # gaussian mean
lambda_N <- list() # gaussian precision
a_N <- list() # gamma parameter a
b_N <- list() # gamma parameter b

for(k in 1:length(score_name)){
  mu_N[[k]] <- matrix(0, nrow=length(team_name), ncol = length(team_name))
  lambda_N[[k]] <- matrix(0, nrow=length(team_name), ncol = length(team_name))
  
  a_N[[k]] <- matrix(0, nrow=length(team_name), ncol = length(team_name))
  b_N[[k]] <- matrix(0, nrow=length(team_name), ncol = length(team_name))
}
names(mu_N) <- score_name
names(lambda_N) <- score_name
names(a_N) <- score_name
names(b_N) <- score_name


# compute posterior parameter from regular season score -------------------------------------
versus <- expand.grid(1:length(team_name), 1:length(team_name))
for(i in 1:nrow(versus)){
  vs1 <- versus[i,1]
  vs2 <- versus[i,2]
  
  if(vs1==vs2){next}
  
  # compute mu_N, lambda_N, a_N, b_N
  for(k in 1:length(score_name)){
    # matches between vs1-vs2
    N <- match_num[vs2,vs1]
    
    mu_ML <- htoh_mean[[k]][vs2,vs1]
    
    m_0 <- mu_0[[k]][vs1]
    lam_0 <- lambda_0[[k]][vs1]
    mu_N[[k]][vs2,vs1] <- (N*mu_ML+lam_0*m_0)/(N+lam_0)
    
    lambda_N[[k]][vs2,vs1] <- lam_0+N
    
    a_N[[k]][vs2,vs1] <- a_0_init+N/2
    # a_N[[k]][vs2,vs1] <- a_0[[k]][vs1]+N/2
    
    sigma_ML <- htoh_sd[[k]][vs2,vs1]
    b_N[[k]][vs2,vs1] <- b_0_init+1/2*(N*sigma_ML^2+(lam_0*N*(mu_ML-m_0)^2)/(lam_0+N))
    # b_N[[k]][vs2,vs1] <- b_0[[k]][vs1]+1/2*(N*sigma_ML^2+(lam_0*N*(mu_ML-m_0)^2)/(lam_0+N))
    
  }
  
}

# define the function output student-t ditribution probaility density
# mu:mean, lambda:precision, nu:degree of freedom
dstudent <- function(x, mu, lambda, nu){
  dt <- gamma(nu/2+1/2)/gamma(nu/2)*(lambda/pi/nu)^(1/2)*(1+lambda*(x-mu)^2/nu)^(-nu/2-1/2)
  return(dt)
}

likelihood_student <- apply(likelihood_of, c(1,2), function(x){x <- 0})

n <- 1
versus <- expand.grid(1:length(team_name), 1:length(team_name))
for(i in 1:nrow(versus)){
  vs1 <- versus[i,1]
  vs2 <- versus[i,2]
  
  if(vs1==vs2){next}
  
  for(j in 1:nrow(head_to_head[[vs1]][[vs2]])){
    match <- head_to_head[[vs1]][[vs2]][j,]
    
    for(k in 1:length(score_name)){
      m_N <- mu_N[[k]][vs2,vs1]
      lam <- a_N[[k]][vs2,vs1]/b_N[[k]][vs2,vs1]*lambda_N[[k]][vs2,vs1]/(1+lambda_N[[k]][vs2,vs1])
      nu_N <- 2*a_N[[k]][vs2,vs1]
      
      likelihood_student[n,k] <- dstudent(match[score_name[k]], m_N, lam, nu_N)
    }
    n <- n+1
  }
}


# visualize regular and playoffs head_to_head, opponent_free
quartz()
score_name_child <- 'FGM'
boxplot(
  # likelihood_of[,score_name_child],
  -log(likelihood_htoh[,score_name_child]),
  -log(likelihood_setBayes[,score_name_child]),
  # likelihood_of_p[,score_name_child],
  -log(likelihood_htoh_p[,score_name_child]),
  -log(likelihood_setBayes_p[,score_name_child]),
  names=c(# 'opponent free\nRegular', 
          '最尤推定',
          'ベイズ推定',
          # 'opponent free\nPlayoffs', 
          '最尤推定',
          'ベイズ推定'),
  ylim=c(0, 50), 
  xlab='', ylab='負の対数尤度', main=paste0('Regular and Playoffs ', score_name_child)
)
# dev.off()




# visualize rergular season regression hierarchical Bayes aproach compared with others
score_name_child <- 'FGM'
boxplot(
  likelihood_of[,score_name_child],
  likelihood_htoh[,score_name_child],
  # likelihood_OD[,score_name_child],
  # likelihood_sBayes[,score_name_child],
  likelihood_mBayes[,score_name_child],
  # likelihood_lBayes[,score_name_child],
  likelihood_setBayes[,score_name_child],
  # likelihood_student[,score_name_child],
  names=c('opponent free', 'head to head'
          # , 'OD'
          # , 'Bayes(small var)'
          , 'Bayes(mid var)'
          # , 'Bayes(large var)' 
          , 'Bayes(given var)'
          # , 'Hierarchical Bayes'
          ),
  ylim=c(0,0.2), xlab='methods', ylab='likelihood', main=paste0('Regular ', score_name_child) 
)


likelihood_student_p <- apply(likelihood_htoh_p, c(1,2), function(x){x <- 0})

for(i in 1:nrow(sort_score_p)){
  match <- as.numeric(sort_score_p[i,score_name])
  names(match) <- score_name
  vs1 <- which(sort_score_p[i,'Player']==team_name)
  vs2 <- which(sort_score_p[i,'Opponent']==team_name)
  
  for(k in 1:length(score_name)){
    m_N <- mu_N[[k]][vs2, vs1]
    lam <- a_N[[k]][vs2,vs1]/b_N[[k]][vs2,vs1]*lambda_N[[k]][vs2,vs1]/(1+lambda_N[[k]][vs2,vs1])
    nu_N <- 2*a_N[[k]][vs2,vs1]
    
    likelihood_student_p[i,k] <- dstudent(match[score_name[k]], m_N, lam, nu_N)
  }
  
}

# visualize playoffs likelihood hierarchical Bayes aproach compared with others
score_name_child <- 'FGM'
boxplot(
  likelihood_of_p[,score_name_child],
  likelihood_htoh_p[,score_name_child],
  # likelihood_OD_p[,score_name_child],
  # likelihood_sBayes_p[,score_name_child],
  likelihood_mBayes_p[,score_name_child],
  # likelihood_lBayes_p[,score_name_child],
  likelihood_setBayes_p[,score_name_child],
  # likelihood_student_p[,score_name_child],
  names=c('opponent free', 'head to head'
          # , 'OD'
          # , 'Bayes(small var)'
          , 'Bayes(mid var)'
          # , 'Bayes(large var)' 
          , 'Bayes(given var)'
          # , 'Hierarchical Bayes
          ),
  ylim=c(0,0.2), xlab='methods', ylab='likelihood', main=paste0('Playoffs ', score_name_child) 
)

# check the distribution shape
score_name_child <- 'FGM'
hist(
  as.numeric(sort_score_p[,score_name_child]),
  # breaks='Scott',
  breaks=10, 
  main=paste('Histogram of Playoffs' ,score_name_child),
  xlab=paste('Playoffs',score_name_child)
)

# check the distribution shape
score_name_child <- 'FGM'
hist(
  score_list[[score_name_child]],
  breaks='Scott', 
  main=paste('Histogram of Regular', score_name_child),
  xlab=paste('Regular', score_name_child)
)

# validation
valid_setBayes_htoh <- likelihood_setBayes_p-likelihood_htoh_p
binom.test(length(which(valid_setBayes_htoh[,'FGM']>0)), length(valid_setBayes_htoh[,'FGM']), 0.5)
playoffs_test <- function(method1, method2, score_name_child){
  playoffs_testvec <- method1-method2
  binom.test(length(which(playoffs_testvec[,score_name_child]>0)), length(playoffs_testvec[,score_name_child]), 0.5)
}



