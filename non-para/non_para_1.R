# try to coding nonparametric bayes method for 3 gaussian mixture

# problem setting 
# the data sampled from 3 gaussian mixture 
# purpose : estimate the cluster by non-parametric method, CRP

set.seed(1)

# mixture proportion
mix_prop <- c(0.2, 0.5, 0.3)

dist_index <- c(1,2,3)

mean_vec <- c(20, 50, 80)
sd_vec <- c(10,10,10)

sample_num <- 1000
N <- sample_num

# index of cluster the sample produced <- latent variable
data1 <- sample(dist_index, sample_num, prob=mix_prop, replace=T)

# value of samples
data2 <- numeric(sample_num)
# sample value generation
for(i in 1:sample_num){
  data2[i] <- rnorm(1, mean=mean_vec[data1[i]], sd=sd_vec[data1[i]])
}

hist(data2, breaks = 20)

hist(data2[which(data1==1)], add=T, col='#ff000040')
hist(data2[which(data1==2)], add=T, col='#00ff0040')
hist(data2[which(data1==3)], add=T, col='#0000ff40')

# Gibbs sampling : sampling the cluster which produces given samples(data2) ************

# initialize cluster allocation for given samples (data2)
init_cluster <- rep(1, sample_num)
cluster <- init_cluster

gamma <- 0.5 # hyper parameter of CRP

mean_prior <- 50                     # mean of prior ditribution
sd_prior <- 20                       # standard deviation of prior distribution
beta_prior <- 1/(sd_prior^2)         # precision of prior distribution
sd_set <- 10                         # initial setting of standard deviation of unkwown gaussian distribution
beta_set <- 1/(sd_set^2)             # initial setting of precision of unknwon gaussian distribution

for(k in 1:10){
  # Gibbs sampling
  for(i in 1:sample_num){
  
    # maintain cluster index of i-th data
    pre_cluster <- cluster[i]
    
    # initialize cluster of i-th data
    cluster[i] <- 0
    
    # confirm cluster existence
    # if cluster vanished, substitute exsiting cluster
    if(any(cluster==pre_cluster) == F && pre_cluster < max(cluster)){
      cluster[which(cluster==max(cluster))] <- pre_cluster
    }
    
    # exsisting cluster index
    exist_cluster <- sort(unique(cluster[-i]))
    
    # number of data belongs to each cluster (except for i-th data)
    cluster_num <- table(cluster[-i])
    c_num <- cluster_num
    
    # likelihood p(z_i=k | z_(-i))
    lh_1 <- vector()
    # likelihood p(x_i | z_i=k, theta) theta:distribution parameter
    lh_2 <- vector()
    
    # lh_1, lh_2 computing, for existing cluster
    for(j in exist_cluster ){
      
      # number of data in j-th cluster
      N_j <- c_num[j]
      
      lh_1[j] <- N_j/(N-1+gamma)
      
      # mean of j-th cluster estimated by Max Likelihood 
      mean_ml <- mean(data2[which(cluster==j)])
      # mean, precision of j-th cluster by bayesian estimation
      mean_bayes <- (beta_set*mean_ml+beta_prior/N_j*mean_prior)/(beta_set+beta_prior/N_j)
      beta_bayes <- beta_prior+N_j*beta_set
      
      lh_2[j] <- dnorm(data2[i], mean=mean_bayes, sd=sqrt(1/beta_set+1/beta_bayes))
      
    }
    # append new cluster likelihood
    lh_1 <- c(lh_1, gamma/(N-1+gamma))
    lh_2 <- c(lh_2, dnorm(data2[i], mean=mean_prior, sd=sqrt(1/beta_set+1/beta_prior)))
    
    lh_12 <- lh_1*lh_2
    
    cluster[i] <- sample(1:length(lh_12), 1, prob=Normalize(lh_12))
    
    
    
  }
}



cols <- rainbow(max(exist_cluster))
cols <- substr(cols, 1,7)
cols <- paste0(cols, '40')

hist(data2 ,breaks=seq(-20,120,5))

for(i in exist_cluster){
  hist(data2[which(cluster==i)], add=T, col=cols[i], breaks=seq(-20,120,5))
}






