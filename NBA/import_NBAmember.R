# import and preprocess NBA box score data

# read 2016 season all data --------------------------------------------

# read regular season data -----------------
head_to_head_Lr <- list()
for(i in 1:30){
  head_to_head_Lr[[i]] <- list()
  for(j in 1:30){
    head_to_head_Lr[[i]][[j]] <- list()
  }
}

m_mat <- matrix(0, ncol=30, nrow=30)

for(i in 1:1231){
  # omit Allstar game
  if(i == 802){next}
  
  path1 <- paste0('~/git/Data/NBA/2016/Regular/score', i, '_1.csv')
  path2 <- paste0('~/git/Data/NBA/2016/Regular/score', i, '_2.csv')
  match_i_1 <- read.csv(path1)
  match_i_2 <- read.csv(path2)
  
  match_i_1 <- match_i_1[,-c(21, 22)]
  match_i_2 <- match_i_2[,-c(21, 22)]
  
  s_name <- colnames(match_i_1)
  s_name <- sub('X3', 'T', s_name)
  s_name <- sub('\\.', 'per', s_name)
  
  colnames(match_i_1) <- s_name
  colnames(match_i_2) <- s_name
  
  team_i_1 <- match_i_1[1,'Player']
  team_i_2 <- match_i_2[1,'Player']
  
  idx_1 <- which(as.character(team_i_1)==as.character(team_name))
  idx_2 <- which(as.character(team_i_2)==as.character(team_name))
  
  m_mat[idx_2, idx_1] <- m_mat[idx_2, idx_1]+1
  m_mat[idx_1, idx_2] <- m_mat[idx_1, idx_2]+1
  
  head_to_head_Lr[[idx_1]][[idx_2]][[m_mat[idx_2, idx_1]]] <- match_i_1
  head_to_head_Lr[[idx_2]][[idx_1]][[m_mat[idx_1, idx_2]]] <- match_i_2
}


# read playoffs data  -------------------------------------
head_to_head_Lp <- list()
for(i in 1:30){
  head_to_head_Lp[[i]] <- list()
  for(j in 1:30){
    head_to_head_Lp[[i]][[j]] <- list()
  }
}

m_mat_p <- matrix(0, ncol=30, nrow=30)
for(i in 1:86){
  path1 <- paste0('~/git/Data/NBA/2016/Playoffs/score', i, '_1.csv')
  path2 <- paste0('~/git/Data/NBA/2016/Playoffs/score', i, '_2.csv')
  match_i_1 <- read.csv(path1)
  match_i_2 <- read.csv(path2)
  
  match_i_1 <- match_i_1[,-c(21, 22)]
  match_i_2 <- match_i_2[,-c(21, 22)]
  
  s_name <- colnames(match_i_1)
  s_name <- sub('X3', 'T', s_name)
  s_name <- sub('\\.', 'per', s_name)
  
  colnames(match_i_1) <- s_name
  colnames(match_i_2) <- s_name
  
  team_i_1 <- match_i_1[1,'Player']
  team_i_2 <- match_i_2[1,'Player']
  
  idx_1 <- which(as.character(team_i_1)==as.character(team_name))
  idx_2 <- which(as.character(team_i_2)==as.character(team_name))
  
  m_mat_p[idx_2, idx_1] <- m_mat_p[idx_2, idx_1]+1
  m_mat_p[idx_1, idx_2] <- m_mat_p[idx_1, idx_2]+1
  
  head_to_head_Lp[[idx_1]][[idx_2]][[m_mat_p[idx_2, idx_1]]] <- match_i_1
  head_to_head_Lp[[idx_2]][[idx_1]][[m_mat_p[idx_1, idx_2]]] <- match_i_2
}

# connect regular season and playoffs data ---------------------------------
head_to_head_All <- list()
for(i in 1:30){
  head_to_head_All[[i]] <- list()
  for(j in 1:30){
    head_to_head_All[[i]][[j]] <- list()
  }
}

versus <- expand.grid(1:30, 1:30)
for(i in 1:nrow(versus)){
  vs1 <- versus[i,1]
  vs2 <- versus[i,2]
  
  head_to_head_All[[vs1]][[vs2]] <- c(head_to_head_Lr[[vs1]][[vs2]], head_to_head_Lp[[vs1]][[vs2]])
}

htoh_i <- list()
htoh_i_j <- list()
for(i in 1:30){
  htoh_i_j[[i]] <- list()
  for(j in 1:30){
    htoh_i_j[[i]][[j]] <- do.call(rbind, head_to_head_All[[i]][[j]])
  }
  htoh_i[[i]] <- do.call(rbind, htoh_i_j[[i]])
}
htoh_All <- do.call(rbind, htoh_i)

team_member <- lapply(htoh_i, function(x){
  sort(unique(x[,'Player']))
})
names(team_member) <- team_name

names(htoh_i) <- as.character(team_name)








