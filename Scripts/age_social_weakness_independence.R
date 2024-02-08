

library(tidyverse)
MentalHealth = read_csv("data/MentalHealthDataSet.csv")

#Generating table with amount of people by age who have social weakness(yes/no/maybe)
age_social_weakness_table <- MentalHealth %>%
  count(Age, Social_Weakness) %>%
  pivot_wider(names_from = Social_Weakness, values_from = n, values_fill = 0)

#Extending the table with total amount of people with the same age and total amount of people with the same social weakness 
age_social_weakness_table <- age_social_weakness_table %>%
  mutate(Total = rowSums(select_(.,"Maybe", "No", "Yes"))) %>%
  bind_rows(summarise(., across(where(is.numeric), sum),
                      across(where(is.character), ~'Total')))

#Finding the expected frequencies table if Variables Age and Social_Weakness are independent 
v1 <- data.matrix(age_social_weakness_table[1:4,5])
v2 <- data.matrix(age_social_weakness_table[5,2:4])
exp_age_social_weakness_table <- v1 %*% v2 / as.numeric(age_social_weakness_table[5,5])

#Calculating statistic for hypothesis test(with null hypothesis variable are independent)
v <- data.matrix(age_social_weakness_table[1:4,2:4])
X <- sum((exp_age_social_weakness_table - v) ^ 2 / exp_age_social_weakness_table)

#Rejecting null hypothesis
df <- (n_distinct(MentalHealth$Age) - 1) * (n_distinct(MentalHealth$Social_Weakness) - 1)
decision <- qchisq(0.95, df, lower.tail = TRUE) > X

chisq.test(MentalHealth$Age, MentalHealth$Social_Weakness)

