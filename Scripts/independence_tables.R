
library(tidyverse)
MentalHealth = read_csv("data/MentalHealthDataSet.csv")

#Creating 2 empty independence tables for p-value and X^2 value
p_value_table_independence <-
  matrix(NA, nrow = ncol(MentalHealth), ncol = ncol(MentalHealth))

rownames(p_value_table_independence) <-
  colnames(p_value_table_independence) <-
  colnames(MentalHealth)


Xsq_value_table_independence <- 
  matrix(NA, nrow = ncol(MentalHealth), ncol = ncol(MentalHealth))

rownames(Xsq_value_table_independence) <- 
  colnames(Xsq_value_table_independence) <- 
  colnames(MentalHealth)


#Making chisq test for each pair of factors and deciding if they are independent
for (i in 1:(ncol(MentalHealth) - 1)) {
  for (j in (i + 1):ncol(MentalHealth)) {
    
    chi2_result <- chisq.test(MentalHealth[[i]], 
                                     MentalHealth[[j]])
    
    p_value_table_independence[i, j] <- chi2_result$p.value > 0.05
    p_value_table_independence[j, i] <- p_value_table_independence[i, j]
    
    Xsq_value_table_independence[i, j] <- chi2_result$statistic < qchisq(0.95, chi2_result$parameter, lower.tail = TRUE)
    Xsq_value_table_independence[j, i] <- Xsq_value_table_independence[i, j]
  }
}



