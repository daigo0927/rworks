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
t_name <- as.character(team_name[1])
graphics.off()
for(p_name in team_member[[t_name]]){
  # player name
  # p_name <- team_member[[t_name]][7]
  # draw histogram -------------------------------------------------
  # a player ------------
  score_name_child  <- 'PTS'
  player_idx <- p_idx_func(htoh_i[[t_name]], p_name)
  player_score <- htoh_i[[t_name]][player_idx,]
  player_score <- trans_time(player_score)
  player_pts <- player_score[,score_name_child]/player_score[,'MIN']*12
  # dev.off()
  quartz(width=7, height=4)
  hist(player_pts, breaks=10, main=paste('1クォーターあたりの得点ヒストグラム')
       , xlim=c(0, 15), col='#0000FF40', ylim=c(0,0.25), freq=F, xlab=paste(p_name, 'points'), ylab='density')
  # par(new=T)
  # plot(dpois(seq(0,15, 1), lambda=mean(na.omit(player_pts))), type='b', lwd='2'
  #      , xlab='', xlim=c(0,15), ylim=c(0, 0.25), ylab='')
}

# introduce tiredness ----------------------------
t_name <- as.character(team_name[1])
graphics.off()
for(p_name in team_member[[t_name]]){
  player_idx <- p_idx_func(htoh_i[[t_name]], p_name)
  player_score <- htoh_i[[t_name]][player_idx,]
  player_score <- trans_time(player_score)
  
}

# All member ---------------
dev.off()
quartz(width = 7, height = 5)
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
     # , ylim=c(0,0.25)
     # , freq=F
     , xlab='1クォーターあたりの得点'
     # , ylab='density'
     , main=''
     )

hist(
  
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
     , ylim=c(0,0.25)
     , ylab=''
     , xlab=''
     , main=''
     , type='l')

# check participation time quarter/match ------------
part_time <- list()
for(i in 1:30){
  t_idx <- i
  part_time[[i]] <- apply(
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
  part_time[[i]] <- t(part_time[[i]])
  rownames(part_time[[i]]) <- team_member[[t_idx]]
}


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

save(NBA_MCMC_res, file='NBA_MCMC_res.Rdata')


# visualize MCMC result ----------------------------------------
burn_in <- 1:1001

# MCMC sampling process
dev.off()
quartz(width=7, height=4)
p_name <- 'LeBron James'
plot(
  x <- 1:sample_num+1
  , y <- NBA_MCMC_res[['mu']][x, p_name]
  , type='l'
  , xlab='sampling iteration'
  , ylab='平均的得点能力パラメータ'
  # , ylab=paste('PTS of', p_name)
  # , main='MCMC sampling process'
)

# pararell plot of mu : general performance ----------------
# for paper
dev.off()
m_length <- length(team_member[[t_idx]])
quartz(width=12, height=16)
par(mfrow=c(ceiling(m_length/2), 2))
for(i in 1:length(team_member[[t_idx]])){
  p_name <- team_member[[t_idx]][i]
  plot(
    density(NBA_MCMC_res[['mu']][-burn_in, p_name])
    , xlab='mu'
    , main=paste0('平均的得点能力：',p_name)
    , xlim=c(0,10)
    # , ylim=c(0,1.0)
  )
  rug(NBA_MCMC_res[['mu']][-burn_in, p_name])
}

# for presentation
dev.off()
quartz(width=8, height=4)
par(mfrow=c(2, 2))
for(i in c(3,7,13,18)){
  p_name <- team_member[[t_idx]][i]
  plot(
    density(NBA_MCMC_res[['mu']][-burn_in, p_name])
    , xlab='mu'
    , main=paste0('平均的得点能力：',p_name)
    , xlim=c(0,10)
    # , ylim=c(0,1.0)
  )
  rug(NBA_MCMC_res[['mu']][-burn_in, p_name])
}

# pararell plot of lambda : precision for opponent team ---------------
# for paper
m_length <- length(team_member[[t_idx]])
dev.off()
quartz(width=12, height=16)
par(mfrow=c(ceiling(m_length/2), 2))
for(i in team_member[[t_idx]]){
  p_name <- i
  plot(
    density(1/NBA_MCMC_res[['lambda']][-burn_in,p_name])
    # , xlab='precision'
    , xlab='variance'
    # , main = paste0('相性の分散の逆数（精度）：',p_name)
    , main = paste0('相性の分散：', p_name)
    , xlim=c(0,10)
    )
  rug(1/NBA_MCMC_res[['lambda']][-burn_in, p_name])
}
# for presentation
dev.off()
quartz(width=8, height=4)
par(mfrow=c(2, 2))
for(i in c(3,7,13,18)){
  p_name <- team_member[[t_idx]][i]
  plot(
    density(1/NBA_MCMC_res[['lambda']][-burn_in,p_name])
    # , xlab='precision'
    , xlab='variance'
    # , main = paste0('相性の分散の逆数（精度）：',p_name)
    , main = paste0('相性の分散：', p_name)
    , xlim=c(0,10)
  )
  rug(1/NBA_MCMC_res[['lambda']][-burn_in, p_name])
}

# pararell plot of alpha : compatibility for each team ------------------
# for paper
m_length <- length(team_member[[t_idx]])
dev.off()
quartz(width=16, height=20)
par(mfrow=c(10,3))
p_name <- 'LeBron James'
# p_name <- team_member[[t_idx]][18]
# p_name <- 'Cleveland Cavaliers'
# p_name <- 'Jared Cunningham'
for(i in 1:30){
  if(i == t_idx){next}
  param <- paste0('alpha_' ,as.character(i))
  plot(
    density(NBA_MCMC_res[[param]][-burn_in, p_name])
    , xlab = '相性'
    , main = paste0('相性：', p_name, ' for ',as.character(team_name[i]))
    , xlim = c(-5,5)
    # , xlim = c(-3,3)
  )
  rug(NBA_MCMC_res[[param]][-burn_in, p_name])
}
# for presentation
dev.off()
quartz(width=8, height=4)
par(mfrow=c(2,2))
p_name <- 'LeBron James'
# p_name <- team_member[[t_idx]][18]
# p_name <- 'Cleveland Cavaliers'
# p_name <- 'Jared Cunningham'
for(i in c(1,10,11,16)){
  if(i == t_idx){next}
  param <- paste0('alpha_' ,as.character(i))
  plot(
    density(NBA_MCMC_res[[param]][-burn_in, p_name])
    , xlab = '相性'
    , main = paste0('相性：for ',as.character(team_name[i]))
    , xlim = c(-5,5)
    # , xlim = c(-3,3)
  )
  rug(NBA_MCMC_res[[param]][-burn_in, p_name])
}

# pararell plot of mu+alpha : practical PTS for GWS --------------------
# for paper
dev.off()
m_length <- length(team_member[[t_idx]])
o_idx <- '10' # GSW
alpha_idx <- paste0('alpha_', o_idx)
quartz(width=12, height=16)
# par(mfrow=c(ceiling(m_length/2), 2))
# par(mfrow=c(floor(m_length/2), 2))
par(mfrow=c(7,2))
for(i in team_member[[t_idx]]){
  if(i == as.character(team_name[t_idx])){next}
  if(i == 'Anderson Varejao'){next}
  if(i == 'Jared Cunningham'){next}
  if(i == 'Joe Harris'){next}
  if(i == 'Sasha Kaun'){next}
  if(i == 'Jordan McRae'){next}
  mu_ij <- NBA_MCMC_res[['mu']][-burn_in, i]+NBA_MCMC_res[[alpha_idx]][-burn_in, i]
  plot(
    density(mu_ij)
    , xlab = 'PTS for GSW'
    , main = paste0('得点：', i, ' for GSW')
    , xlim = c(0, 10)
  )
  rug(mu_ij)
  # print(paste(i, mean(mu_ij)))
}
m_j <- apply(
  as.matrix(team_member[[t_idx]])
  , 1
  , function(x){
    return(mean(NBA_MCMC_res[['mu']][-burn_in, x]+NBA_MCMC_res[[alpha_idx]][-burn_in, x]))
  }
)
names(m_j) <- team_member[[t_idx]]
except_member <- c('Cleveland Cavaliers', 'Anderson Varejao', 'Jared Cunningham'
                   , 'Joe Harris', 'Sasha Kaun', 'Jordan McRae')
# for presentation
dev.off()
o_idx <- '10' # GSW
alpha_idx <- paste0('alpha_', o_idx)
quartz(width=16, height=8)
# par(mfrow=c(ceiling(m_length/2), 2))
# par(mfrow=c(floor(m_length/2), 2))
par(mfrow=c(3,5))
for(i in team_member[[t_idx]]){
  if(i == as.character(team_name[t_idx])){next}
  if(i == 'Anderson Varejao'){next}
  if(i == 'Jared Cunningham'){next}
  if(i == 'Joe Harris'){next}
  if(i == 'Sasha Kaun'){next}
  if(i == 'Jordan McRae'){next}
  mu_ij <- NBA_MCMC_res[['mu']][-burn_in, i]+NBA_MCMC_res[[alpha_idx]][-burn_in, i]
  plot(
    density(mu_ij)
    , xlab = 'PTS for GSW'
    , main = paste0('得点：', i, ' for GSW')
    , xlim = c(0, 10)
  )
  rug(mu_ij)
}



