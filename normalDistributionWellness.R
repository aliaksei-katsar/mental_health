#Distribution of Psychological Unwellness in histogram
ggplot(MentalHealth_wellness, aes(x = MentalHealth_wellness$Mental_Wellness)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Psychological Unwellness",
       x = "Psychological Unwellness",
       y = "Count") +
  theme_minimal()
#Density funktion of Psychological Unwellness (as continious function)
ggplot(MentalHealth_wellness, aes(x = MentalHealth_wellness$Mental_Wellness)) +
  geom_density(fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Psychological Unwellness",
       x = "Psychological Unwellness",
       y = "Density") +
  theme_minimal()


#Comparison of the densities from Psychological Unwellness and Normal Distribution
normal_density <- data.frame(x = seq(min(MentalHealth_wellness$Mental_Wellness), max(MentalHealth_wellness$Mental_Wellness), length = 1000),
                             y = dnorm(seq(min(MentalHealth_wellness$Mental_Wellness), max(MentalHealth_wellness$Mental_Wellness), length = 1000), mean = mean(MentalHealth_wellness$Mental_Wellness), sd = sd(MentalHealth_wellness$Mental_Wellness)))

ggplot(MentalHealth_wellness, aes(x = MentalHealth_wellness$Mental_Wellness)) +
  geom_density(fill = "blue", alpha = 0.7) +
  geom_line(data = normal_density, aes(x = x, y = y), color = "red", size = 1) +
  labs(title = "Density Plot of Psychological Unwellness with Normal Distribution Overlay",
       x = "Psychological Unwellness",
       y = "Density") +
  theme_minimal()

#we can conclude that this factor is pretty much normal distributed:)

