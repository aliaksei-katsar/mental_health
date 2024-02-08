
library(tidyverse)
MentalHealth = read_csv("data/MentalHealthDataSet.csv")

#Finding total mental health wellness of each person by calculation the amount of bad factors that he/she has
MentalHealth_wellness <- MentalHealth %>% 
  mutate(Mental_Unwellness = rowSums(select(., -Coping_Struggles, -Work_Interest, -Mental_Health_History) == "Yes") +
           rowSums(select(.,Coping_Struggles,Work_Interest) == "No") + 
           rowSums(select(., Mood_Swings) == "High") +
           rowSums(. == "Maybe") / 2 +
           rowSums(select(., Mood_Swings) == "Medium") / 2)

#Distribution of Psychological Unwellness in histogram
ggplot(MentalHealth_wellness, aes(x = Mental_Unwellness)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Psychological Unwellness",
       x = "Psychological Unwellness",
       y = "Count") +
  theme_minimal()
#Density funktion of Psychological Unwellness (as continious function)
ggplot(MentalHealth_wellness, aes(x = Mental_Unwellness)) +
  geom_density(fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Psychological Unwellness",
       x = "Psychological Unwellness",
       y = "Density") +
  theme_minimal()


#Comparison of the densities from Psychological Unwellness and Normal Distribution
normal_density <- data.frame(x = seq(min(MentalHealth_wellness$Mental_Unwellness), max(MentalHealth_wellness$Mental_Unwellness), length = 1000),
                             y = dnorm(seq(min(MentalHealth_wellness$Mental_Unwellness), max(MentalHealth_wellness$Mental_Unwellness), length = 1000), mean = mean(MentalHealth_wellness$Mental_Unwellness), sd = sd(MentalHealth_wellness$Mental_Unwellness)))

ggplot(MentalHealth_wellness, aes(x = Mental_Unwellness)) +
  geom_density(fill = "blue", alpha = 0.7) +
  geom_line(data = normal_density, aes(x = x, y = y), color = "red", size = 1) +
  labs(title = "Density Plot of Psychological Unwellness with Normal Distribution Overlay",
       x = "Psychological Unwellness",
       y = "Density") +
  theme_minimal()

#we can conclude that this factor is pretty much normal distributed:)

