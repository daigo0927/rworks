# artificial data test
set.seed(7)

match_num <- 100

score_strong <- rnorm(match_num, mean=70, sd=10)
score_normal <- rnorm(match_num, mean=80, sd=10)
score_weak <- rnorm(match_num, mean=90, sd=10)

score_full <- c(score_strong, score_normal, score_weak)

mean_full <- mean(score_full)
sd_full <- sd(score_full)

mean_each <- c(mean(score_strong), mean(score_normal), mean(score_weak))
sd_each <- c(sd(score_strong), sd(score_normal), sd(score_weak))

lh_full <- dnorm(score_full, mean=mean_full, sd=sd_full)

lh_each <- c(
  dnorm(score_strong, mean=mean_each[1], sd=sd_each[1]),
  dnorm(score_normal, mean=mean_each[2], sd=sd_each[2]), 
  dnorm(score_normal, mean=mean_each[3], sd=sd_each[3])
)

lh_b_each

boxplot(lh_full, lh_each)