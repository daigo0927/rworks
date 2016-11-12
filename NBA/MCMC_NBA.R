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

# code for MCMC -------------------------------------------------------
t_idx <- 6
score_list <- lapply(htoh_i_j[[t_idx]], trans_time)

# sampling process list
NBA_MCMC_res <- list()
# parameter to be estimated
# mu : general performance of the player
# lambda : precision for opponent team
# alpha_i : plus component for i-th opponent
parameter_name <- c('mu', 'lambda', paste0('alpha_',as.character(seq(1,30))))
sample_num <- 10000
for(i in 1:length(parameter_name)){
  NBA_MCMC_res[[i]] <- matrix(0, nrow=sample_num, ncol=length(team_member[[t_idx]]))
  colnames(NBA_MCMC_res[[i]]) <- team_member[[t_idx]]
}
names(NBA_MCMC_res) <- parameter_name

# gibbs sampling 10000 iteration
# for each member
apply(
  as.matrix(team_member[[t_idx]])
  , 1
  , function(x){
    for(i in 1:sample_num){
      
    }
  }
)














