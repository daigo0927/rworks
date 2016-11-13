set.seed(7)
# run MCMC for NBA data Analysis
# estimate parameter of some score

# x:score_mat, trans 'MIN':XX:XX -> XX.XX
trans_time <- function(x){
  a <- strsplit(as.character(x[,'MIN']), ':')
  a <- lapply(a, as.numeric)
  b <- sapply(a, function(y){y[1]+y[2]/60})
  x[,'MIN'] <- b
  return(x)
}

# x:head to head score htoh_i_j[[i]][[j]] htoh_i[[i]], y:player name
p_idx_func <- function(x,y){
  a <- which(as.character(x[,'Player'])==y)
  return(a)
}

# team_name[6]:Cleveland Cavaliers
t_name <- as.character(team_name[6])
# LeBron James
p_name <- team_member[[t_name]][7]

dcpois <- function(x, lambda){
  return(exp(-lambda)^x/gamma(x+1))
}

# draw histogram -------------------------------------------------
# LeBron James ------------
score_name_child  <- 'PTS'
LeBron_idx <- p_idx_func(htoh_i[[t_name]], p_name)
LeBron_score <- htoh_i[[t_name]][LeBron_idx,]
LeBron_score <- trans_time(LeBron_score)
LeBron_pts <- LeBron_score[,score_name_child]/LeBron_score[,'MIN']*12
hist(LeBron_pts, breaks=10)

# All member ---------------
score_name_child <- 'PTS'
All_score <- trans_time(htoh_All)
All_pts <- All_score[,score_name_child]/All_score[,'MIN']*12
hist(All_pts, breaks='Scott', xlim=c(0, 40))

except_idx <- c(which(All_pts==0), which(All_pts>200), which(is.na(All_pts)==T))

hist(All_pts[-c(which(All_pts>200)
                # , which(All_pts==0)
                , which(is.na(All_pts)==T)
                )]
     , breaks='Scott', xlim=c(0, 40)
     , ylim=c(0,0.20)
     , freq=F
     , xlab=score_name_child
     , ylab='density'
     , main=paste('All result', score_name_child)
     )

par(new=T)
plot(dpois(seq(0,40,1)
           # , lambda=mean(All_pts[-except_idx])
           , lambda =mean(All_pts[-c(which(All_pts>200)
                                     # , which(All_pts==0)
                                     , which(is.na(All_pts)==T)
                                     )])
           )
     , xlim=c(0, 40)
     , ylim=c(0,0.20)
     , ylab=''
     , xlab=''
     , main=''
     , type='l')

# check participation time quarter/match ------------
t_idx <- 6
part_time <- apply(
  as.matrix(team_member[[t_idx]])
  , 1
  , function(x){
    p_name <- x
    score_mat <- trans_time(htoh_i[[t_idx]])
    
    p_idx <- p_idx_func(htoh_i[[t_idx]], p_name)
    
    q <- sum(na.omit(score_mat[p_idx,'MIN']))/12
    m <- sum(na.omit(score_mat[p_idx,'MIN']))/48
    q_m <- c(q,m)
    names(q_m) <- c('quarter', 'match')
    return(q_m)
  }
)
part_time <- t(part_time)
rownames(part_time) <- team_member[[t_idx]]


# confirm distribution shape ---------------------------------
plot(x <- seq(0, 20, 0.01)
     , y <- dgamma(x, shape=10 , scale=0.3)
     # , y <- dnorm(x, mean=3, sd=1)
     , xlim=c(min(x),max(x))
     , type='l'
)


# code for MCMC -------------------------------------------------------
t_idx <- 6
# score list focused to t_idx team
score_list <- lapply(htoh_i_j[[t_idx]], trans_time)


# preprocessing --------------------------------------------------
# sampling result list-------
NBA_MCMC_res <- list()
# define parameters to be estimated
# mu : general performance of the player
# lambda : precision for opponent team
# alpha_i : plus component for i-th opponent
parameter_name <- c('mu', 'lambda', paste0('alpha_',as.character(seq(1,30))))

# sampling length ---------------
sample_num <- 5000

# initialize parameters to be estimated ---------------------
parameter_init <- c(10, 1, runif(30, -1, 1))
names(parameter_init) <- parameter_name

# initialize result list
for(i in 1:length(parameter_name)){
  NBA_MCMC_res[[i]] <- matrix(0, nrow=sample_num+1, ncol=length(team_member[[t_idx]]))
  colnames(NBA_MCMC_res[[i]]) <- team_member[[t_idx]]
  NBA_MCMC_res[[i]][1,] <- parameter_init[i]
}
names(NBA_MCMC_res) <- parameter_name

# prior distribution for mu : general performance 
# and prior ditribution for lambda : precision for opponent team
# informationless gamma distribution 
k_hyper <- 1
theta_hyper <- 1000

# prior distribution for alpha_i : plus component for i-th opponent
lambda_i_hyper <- 0.001
sigma_set <- 1 # given variance for likelihood


# gibbs sampling 5000 iteration-----------------------------------
# for each member
for(x in team_member[[t_idx]]){
  
  # x : member name
  for(i in 1:sample_num){
    x <- as.character(x)
    
    # get recent sample
    recent_parameter <- sapply(NBA_MCMC_res, function(y){
      return(y[i,x])
    })
    mu_i <- recent_parameter[1]
    lambda_i <- recent_parameter[2]
    alpha_ij <- recent_parameter[-c(1,2)]
    
    
    # score_list focused to x member
    s_list_p <- lapply(score_list, function(y){
      return(y[which(y$Player==x),])
    })
    
    # PTS obtained from j-th team
    y_ij <- sapply(s_list_p, function(y){
      if(is.null(y)==T) return(0)
      else return(sum(na.omit(y$PTS)))
    })
    
    # quarters performed between j-th team
    n_q_ij <- sapply(s_list_p, function(y){
      if(is.null(y)==T) return(0)
      else return(sum(na.omit(y$MIN))/12)
    })
    
    # learning general performance -------------------------
    # update mu distribution
    k1_new <- k_hyper + sum(y_ij - n_q_ij*alpha_ij)
    theta1_new <- (1/theta_hyper + sum(n_q_ij))^(-1)
    # general performance mu sampling
    mu_sample <- rgamma(1, shape=k1_new, scale=theta1_new)
    
    # learning precision for opponent team --------------------
    # 29 : team number except for oneself
    k2_new <- k_hyper + 29/2
    theta2_new <- (1/theta_hyper + 29/2*sd(alpha_ij[-t_idx])^2)^(-1)
    # precision lambda sampling
    lambda_sample <- rgamma(1, shape=k2_new, scale=theta2_new)
    
    # learning plus component for j-th opponent -----------------
    alpha_sample <- rep(0, 30)
    lam_set <- 1/(sigma_set^2)
    mu_ML <- (y_ij - n_q_ij*mu_i)/n_q_ij
    
    lambda_new <- n_q_ij*lam_set+lambda_i
    mu_new <- n_q_ij*lam_set*mu_ML/lambda_new
    mu_new[which(is.nan(mu_new)==T)] <- 0
    
    # plus component alpha sampling
    for(j in 1:30){
      if(j==t_idx){next}
      alpha_sample[j] <- rnorm(1, mean=mu_new[j], sd=1/sqrt(lambda_new[j]))
    }
    
    
    # record sampling result
    NBA_MCMC_res$mu[i+1,x] <- mu_sample
    NBA_MCMC_res$lambda[i+1, x] <- lambda_sample
    for (j in 1:30){
      NBA_MCMC_res[[j+2]][i+1, x] <- alpha_sample[j]
    }
    
  }
  
  print(paste0(x, ' sampling completed'))
}


# visualize MCMC result ----------------------------------------
burn_in <- 1:1001

# MCMC sampling process
dev.off()
p_name <- team_member[[t_idx]][7]
plot(
  x <- 1:sample_num+1
  , y <- NBA_MCMC_res[['mu']][x, p_name]
  , type='l'
  , xlab='sampling iteration'
  , ylab=paste('PTS of', p_name)
  , main='MCMC sampling process'
)

# pararell plot of mu : general performance
par(mfrow=c(4,1))
for(i in c(3,7,13,18)){
  p_name <- team_member[[t_idx]][i]
  plot(
    density(NBA_MCMC_res[['mu']][-burn_in, p_name])
    , xlab='mu'
    , main=paste('general performance of', p_name)
    , xlim=c(0,10)
  )
  rug(NBA_MCMC_res[['mu']][-burn_in, p_name])
}

# pararell plot of lambda : precision for opponent team
par(mfrow=c(4,1))
for(i in c(1,7,13,18)){
  p_name <- team_member[[t_idx]][i]
  plot(
    density(1/NBA_MCMC_res[['lambda']][-burn_in,p_name])
    , xlab='variance'
    , main = paste('variance for opponents of', p_name)
    , xlim=c(0,10)
    )
  rug(1/NBA_MCMC_res[['lambda']][-burn_in, p_name])
}

# pararell plot of alpha : compatibility for each team
par(mfrow=c(4,1))
# p_name <- 'LeBron James'
# p_name <- team_member[[t_idx]][18]
p_name <- 'Cleveland Cavaliers'
for(i in c(1, 10, 20, 30)){
  param <- paste0('alpha_' ,as.character(i))
  plot(
    density(NBA_MCMC_res[[param]][-burn_in, p_name])
    , xlab = 'compatibility'
    , main = paste0(p_name, ' compatibility for ',as.character(team_name[i]))
    , xlim = c(-2,2)
    # , xlim = c(-3,3)
  )
  rug(NBA_MCMC_res[[param]][-burn_in, p_name])
}









