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

# draw histogram -------------------------------------------------
score_name_child  <- 'PTS'
LeBron_idx <- p_idx_func(htoh_i[[t_name]], p_name)
LeBron_score <- htoh_i[[t_name]][LeBron_idx,]
LeBron_score <- trans_time(LeBron_score)
LeBron_pts <- LeBron_score[,score_name_child]/LeBron_score[,'MIN']*12
hist(LeBron_pts, breaks=10)

All_score <- trans_time(htoh_All)
All_pts <- All_score[,score_name_child]/All_score[,'MIN']*12
hist(All_pts, breaks='Scott', xlim=c(0, 40))

except_idx <- c(which(All_pts==0), 7351, which(is.na(All_pts)==T))
dcpois <- function(x, lambda){
  return(exp(-lambda)^x/gamma(x+1))
}
hist(All_pts[-except_idx], breaks='Scott', xlim=c(0, 40), ylim=c(0,0.20), freq=F)
par(new=T)
plot(dpois(seq(0,40,1), lambda=mean(All_pts[-except_idx])), xlim=c(0, 40), ylim=c(0,0.20), type='l')

