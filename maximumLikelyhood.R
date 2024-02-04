library(dplyr)
library(ggplot2)

# Calculating MLE using that it is equal to average estimator
num_observations <- nrow(MentalHealth)
yes_count <- sum(MentalHealth[["Work_Interest"]] == "Yes")
max_like_binom <- yes_count/num_observations

#(proof) Perform MLE using the optim function 

MentalHealth$response_numeric <- ifelse(MentalHealth$Work_Interest == "Yes", 1, 0)

likelihood_function <- function(probability, data) {
  -sum(data * log(probability) + (1 - data) * log(1 - probability))
}
mle_result <- optim(par = 0.5, fn = likelihood_function, data = MentalHealth$response_numeric, method = "Brent", lower = 0, upper = 1)
mle_estimate <- mle_result$par
proof <- signif(max_like_binom)==signif(mle_estimate)

#probability mass function of binomial distribution with MLE as a parameter
ggplot(data.frame(x=c(0:num_observations)), aes(x)) +
  geom_point(aes(y=dbinom(x, size = num_observations, prob = max_like_binom)),
             colour="blue") + theme_light() +
  geom_vline(xintercept = 5, colour = "black")






ggplot(MentalHealth, aes(x = as.factor(response_numeric))) +
  geom_bar(stat = "count", width = 0.7, fill = "blue", color = "black") 


