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




# read 2016 regular season box score *********************************************
regular_score <- read.csv("../Daigo/Desktop/NBA/2016/score_r.csv", header=T)
playoffs_score <- read.csv('../Daigo/Desktop/NBA/2016/score_p.csv', header=T)

# use FGM, 3PM, FTM data
point_score_name <- c('Player1', 'FGM1', 'FGA1', 'FG.1', 'X3PM1', 'X3PA1', 'X3P.1', 'FTM1', 'FTA1', 'FT.1', 'Player2', 'FGM2', 'FGA2', 'FG.2', 'X3PM2', 'X3PA2', 'X3P.2', 'FTM2', 'FTA2', 'FT.2')
point_score_col <- match(point_score_name, colnames(regular_score))

point_score_r <- regular_score[,point_score_col]
colnames(point_score_r) <- c('Player1', 'FGM1', 'FGA1', 'FGper1', 'TPM1', 'TPA1', 'TPper1', 'FTM1', 'FTA1', 'FTper1', 'Player2', 'FGM2', 'FGA2', 'FGper2', 'TPM2', 'TPA2', 'TPper2', 'FTM2' ,'FTA2', 'FTper2')
point_score_r$state <- as.numeric(point_score_r$Player2)

point_score_p <- playoffs_score[,point_score_col]
colnames(point_score_p) <- c('Player1', 'FGM1', 'FGA1', 'FGper1', 'TPM1', 'TPA1', 'TPper1', 'FTM1', 'FTA1', 'FTper1', 'Player2', 'FGM2', 'FGA2', 'FGper2', 'TPM2', 'TPA2', 'TPper2', 'FTM2', 'FTA2', 'FTper2')
for(i in 1:nrow(point_score_p)){
  point_score_p$state[i] <- which(as.character(point_score_p$Player2[i])==team_name)
}


# change FGM to 2 point score
point_score_r[,'FGM1'] <- point_score_r[,'FGM1'] - point_score_r[,'TPM1']
point_score_r[,'FGA1'] <- point_score_r[,'FGA1'] - point_score_r[,'TPA1']
point_score_r[,'FGM2'] <- point_score_r[,'FGM2'] - point_score_r[,'TPM2']
point_score_r[,'FGA2'] <- point_score_r[,'FGA2'] - point_score_r[,'TPA2']
point_score_r[,'FGper1'] <- point_score_r[,'FGM1'] / point_score_r[,'FGA1']*100
point_score_r[,'FGper2'] <- point_score_r[,'FGM2'] / point_score_r[,'FGA2']*100


point_score_p[,'FGM1'] <- point_score_p[,'FGM1'] - point_score_p[,'TPM1']
point_score_p[,'FGA1'] <- point_score_p[,'FGA1'] - point_score_p[,'TPA1']
point_score_p[,'FGM2'] <- point_score_p[,'FGM2'] - point_score_p[,'TPM2']
point_score_p[,'FGA2'] <- point_score_p[,'FGA2'] - point_score_p[,'TPA2']
point_score_p[,'FGper1'] <- point_score_p[,'FGM1'] / point_score_p[,'FGA1']*100
point_score_p[,'FGper2'] <- point_score_p[,'FGM2'] / point_score_p[,'FGA2']*100




team_name <- sort(unique(point_score_r$Player1))


# make point score list by each team ---------------------------------------------------------
point_by_team <- list()
for(i in 1:length(team_name)){
  
  # matches the i-th team joined
  game_num <- sort(append(grep(team_name[i], point_score_r[,'Player1']), grep(team_name[i], point_score_r[,'Player2'])))
  
  point_by_team[[i]] <- matrix(0, nrow=length(game_num), ncol=12)
  colnames(point_by_team[[i]]) <- c('Player', 'FGM', 'FGA', 'FGper', 'TPM', 'TPA', 'TPper', 'FTM', 'FTA', 'FTper', 'Opponent', 'state')
  
  for(j in 1:length(game_num)){
    # Player
    point_by_team[[i]][j,1] <- team_name[i]
    
    if(point_score_r$Player1[game_num[j]] == team_name[i]){
      # FGM
      point_by_team[[i]][j,'FGM'] <- point_score_r[game_num[j], 'FGM1']
      # FGA
      point_by_team[[i]][j,'FGA'] <- point_score_r[game_num[j], 'FGA1']
      # FGper
      point_by_team[[i]][j,'FGper'] <- point_score_r[game_num[j], 'FGper1']
      # 3PM
      point_by_team[[i]][j,'TPM'] <- point_score_r[game_num[j], 'TPM1']
      # 3PA
      point_by_team[[i]][j,'TPA'] <- point_score_r[game_num[j], 'TPA1']
      # 3Pper
      point_by_team[[i]][j,'TPper'] <- point_score_r[game_num[j], 'TPper1']
      # FTM
      point_by_team[[i]][j,'FTM'] <- point_score_r[game_num[j], 'FTM1']
      # FTA
      point_by_team[[i]][j,'FTA'] <- point_score_r[game_num[j], 'FTA1']
      # FTper
      point_by_team[[i]][j,'FTper'] <- point_score_r[game_num[j], 'FTper1']
      # Opponent
      point_by_team[[i]][j,'Opponent'] <- point_score_r[game_num[j], 'Player2']
      
    }else if(point_score_r$Player2[game_num[j]] == team_name[i]){
      # FGM
      point_by_team[[i]][j,'FGM'] <- point_score_r[game_num[j], 'FGM2']
      # FGA
      point_by_team[[i]][j,'FGA'] <- point_score_r[game_num[j], 'FGA2']
      # FGper
      point_by_team[[i]][j,'FGper'] <- point_score_r[game_num[j], 'FGper2']
      # TPM
      point_by_team[[i]][j,'TPM'] <- point_score_r[game_num[j], 'TPM2']
      # TPM
      point_by_team[[i]][j,'TPA'] <- point_score_r[game_num[j], 'TPA2']
      # TPper
      point_by_team[[i]][j,'TPper'] <- point_score_r[game_num[j], 'TPper2']
      # TPM
      point_by_team[[i]][j,'FTM'] <- point_score_r[game_num[j], 'FTM2']
      # TPM
      point_by_team[[i]][j,'FTA'] <- point_score_r[game_num[j], 'FTA2']
      # TPper
      point_by_team[[i]][j,'FTper'] <- point_score_r[game_num[j], 'FTper2']
      # Opponent
      point_by_team[[i]][j,'Opponent'] <- point_score_r[game_num[j], 'Player1']
    }
    # state
    point_by_team[[i]][j,'state'] <- point_score_r[game_num[j], 'state']
  }
}
names(point_by_team) <- team_name


# make point list, head to head ------------------------------------------
# head_to_head[[one]][[opponent]]
head_to_head <- list()
for(i in 1:30){
  head_to_head[[i]] <- list()
  for(j in 1:30){
    head_to_head[[i]][[j]] <- point_by_team[[i]][which(point_by_team[[i]][,'Opponent']==j), ] 
  }
}
names(head_to_head) <- team_name


# make point score matrix team by team for each score( FGM, 3PM, FTM) -----------------------------------
# matching number of times matrix
# because of after division, diagonal elements are set 1
match_num <- diag(1, nrow=30, ncol=30)
# score matrix list, composed by each point score(FGM, 3PM, FTM) matrix
score_sum_matrix <- list(matrix(0,30,30), matrix(0,30,30), matrix(0,30,30), matrix(0,30,30), matrix(0,30,30), matrix(0,30,30), matrix(0,30,30), matrix(0,30,30), matrix(0,30,30))
names(score_sum_matrix) <- c('FGM', 'FGA', 'FGper', 'TPM', 'TPA', 'TPper', 'FTM', 'FTA', 'FTper')

# input component of score_sum_matrix
for(i in 1:30){
  for(j in 1:30){
    table <- head_to_head[[i]][[j]]
    score_i_j <- apply(table[,-which(colnames(table)==c('Player', 'Opponent', 'state'))], 2, sum)
    for(k in 1:length(score_name)){
      score_sum_matrix[[k]][j,i] <- score_i_j[score_name[k]]
    }
    
    # input i vs j match_num
    match_num[j,i] <- match_num[j,i]+nrow(head_to_head[[i]][[j]])
  }
}

# sort playoffs result -------------------------------------------
cname1 <- colnames(head_to_head[[1]][[1]])
cname1[-which(cname1=='state')] <- paste0(cname1[-which(cname1=='state')],'1')
cname1[which(cname1=='Opponent1')] <- 'Player2'
cname2 <- colnames(head_to_head[[1]][[1]])
cname2[-which(cname2=='state')] <- paste0(cname2[-which(cname2=='state')],'2')
cname2[which(cname2=='Opponent2')] <- 'Player1'
score1 <- point_score_p[,cname1]
score2 <- point_score_p[,cname2]

sort_score_p <- matrix(0, nrow=2*nrow(point_score_p), ncol=ncol(head_to_head[[1]][[1]]) )
cname <- colnames(head_to_head[[1]][[1]])
colnames(score1) <- cname
colnames(score2) <- cname
for(i in 1:nrow(score1)){
  sort_score_p[2*i-1,] <- t(score1[i,])
  sort_score_p[2*i,] <- t(score2[i,])
}
colnames(sort_score_p) <- cname







