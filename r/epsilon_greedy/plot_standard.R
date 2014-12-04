library("plyr")
library("ggplot2")

filepath = "~/Mining-the-Social-Web-2nd-Edition/ipynb/algorithms/epsilon_greedy/"
results <- read.csv(paste(filepath,"standard_results.tsv", sep=""), header = FALSE, sep = "\t")

names(results) <- c("Epsilon", "Sim", "T", "ChosenArm", "Reward", "CumulativeReward")
results <- transform(results, Epsilon = factor(Epsilon))

#head(results, n=100)
#tail(results, n=100)
#sapply(results, class)

# Plot average reward (of all simulations) as a function of time.
stats <- ddply(results,
               c("Epsilon", "T"),
               function (df) {mean(df$Reward)})
#ddply: For each subset of a data frame, apply function then combine results into a data frame
#equivalent of:
#library("sqldf")
#a<-sqldf("select Epsilon, T, avg(Reward) from results group by 1,2")

ggplot(stats, aes(x = T, y = V1, group = Epsilon, color = Epsilon)) +
  geom_line() +
  ylim(0, 1) +
  xlab("Time") +
  ylab("Average Reward") +
  ggtitle("Performance of the Epsilon Greedy Algorithm")
ggsave(paste(filepath,"/standard_epsilon_greedy_average_reward.pdf", sep=""))

# Plot frequency of selecting correct arm as a function of time.
# In this instance, 5 is the correct arm.  #
stats <- ddply(results,
               c("Epsilon", "T"),
               function (df) {mean(df$ChosenArm == 5)})

ggplot(stats, aes(x = T, y = V1, group = Epsilon, color = Epsilon)) +
  geom_line() +
  ylim(0, 1) +
  xlab("Time") +
  ylab("Probability of Selecting Best Arm") +
  ggtitle("Accuracy of the Epsilon Greedy Algorithm")
ggsave(paste(filepath,"/standard_epsilon_greedy_average_accuracy.pdf", sep=""))

# Plot variance of chosen arms as a function of time.
stats <- ddply(results,
               c("Epsilon", "T"),
               function (df) {var(df$ChosenArm)})
ggplot(stats, aes(x = T, y = V1, group = Epsilon, color = Epsilon)) +
  geom_line() +
  xlab("Time") +
  ylab("Variance of Chosen Arm") +
  ggtitle("Variability of the Epsilon Greedy Algorithm")
ggsave(paste(filepath,"/standard_epsilon_greedy_variance_choices.pdf", sep=""))

# Plot cumulative reward as a function of time.
stats <- ddply(results,
               c("Epsilon", "T"),
               function (df) {mean(df$CumulativeReward)})

ggplot(stats, aes(x = T, y = V1, group = Epsilon, color = Epsilon)) +
  geom_line() +
  xlab("Time") +
  ylab("Cumulative Reward of Chosen Arm") +
  ggtitle("Cumulative Reward of the Epsilon Greedy Algorithm")
ggsave(paste(filepath,"/standard_epsilon_greedy_cumulative_reward.pdf", sep=""))
