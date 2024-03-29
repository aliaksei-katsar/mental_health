

library(tidyverse)
MentalHealth = read_csv("data/MentalHealthDataSet.csv")

#Finding total mental health wellness of each person by calculation the amount of bad factors that he/she has
MentalHealth_wellness <- MentalHealth %>% 
  mutate(Mental_Unwellness = rowSums(select(., -Coping_Struggles, -Work_Interest, -Mental_Health_History) == "Yes") +
                         rowSums(select(.,Coping_Struggles,Work_Interest) == "No") + 
                         rowSums(select(., Mood_Swings) == "High") +
                         rowSums(. == "Maybe") / 2 +
                         rowSums(select(., Mood_Swings) == "Medium") / 2)

#Checking if a person has a "good" mental wellness(amount of bad factors > 4) or not( <= 4)
MentalHealth_wellness$Mental_Unwellness <- ifelse(MentalHealth_wellness$Mental_Unwellness > 4, 1, 0)

#Making chisq tests
chisq.test(MentalHealth_wellness$Age, MentalHealth_wellness$Mental_Unwellness)
chisq.test(MentalHealth_wellness$Gender, MentalHealth_wellness$Mental_Unwellness)
chisq.test(MentalHealth_wellness$Occupation, MentalHealth_wellness$Mental_Unwellness)
chisq.test(MentalHealth_wellness$Days_Indoors, MentalHealth_wellness$Mental_Unwellness)
chisq.test(MentalHealth_wellness$Mental_Health_History, MentalHealth_wellness$Mental_Unwellness)
