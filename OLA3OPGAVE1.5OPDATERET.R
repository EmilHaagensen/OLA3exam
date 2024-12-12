# Find den bedste indikator, der alene består af mikroøkonomiske spørgsmål i 
# forbrugertillidsundersøgelsen og sammenlign indikatoren med jeres tidligere svar i opgave 1. 
    ## NOTER TIL SELV: 
# Q1_Fam_nu - micro                    
# Q2_Fam_etår - micro                    
# Q3_Dan_nu - macro                  
# Q4_Dan_etår - macro                          
# Q5_Forbrugsgoder_nu - macro
# Q6_Priser_nu - macro                                      
# Q7_Priser_etår - macro                                                 
# Q8_Arbejdsløshed - macro                                 
# Q9_Forbrugsgoder_etår - micro
# Q10_Sparop_nu - macro 
# Q11_Sparop_etår - micro    
# Q12_Famøk_nu - micro

microvektor <- colnames(ftillid_k_period) %in% c("Q1_Fam_nu", "Q2_Fam_etår", "Q9_Forbrugsgoder_etår", "Q11_Sparop_etår", "Q12_Famøk_nu")
micro_df <- ftillid_k_period[,microvektor]

# Samme procedure bare kun med micro
micro_comb_list <- list()
micro_comb_names <- list()

micro_n_cols <- ncol(micro_df)

# 
for (j in 1:micro_n_cols) {
  micro_combinations <- combn(1:micro_n_cols, j, simplify = FALSE)
  

  for (cols in micro_combinations) {
    micro_avg_value <- rowMeans(micro_df[, cols, drop = FALSE], na.rm = TRUE)  
    micro_comb_list[[length(micro_comb_list) + 1]] <- micro_avg_value  
    
    # Store the combination name as a character string
    micro_comb_names[[length(micro_comb_names) + 1]] <- paste(colnames(micro_df)[cols], collapse = ", ")
  }
}


length(micro_comb_list)  # 31

micro_comb_list <- as.data.frame(micro_comb_list)
colnames(micro_comb_list) <- micro_comb_names

#### Samme procedure med udregning af r^2 også


micro_r_squared_values <- numeric(ncol(micro_comb_list))


for (i in 1:ncol(micro_comb_list)) {

  micro_model <- lm(forbrug_period$Vaekst ~ micro_comb_list[, i])
  

  micro_r_squared_values[i] <- summary(micro_model)$r.squared
}


micro_best_r_squared_index <- which.max(micro_r_squared_values)


micro_best_r_squared <- micro_r_squared_values[micro_best_r_squared_index]

micro_comb_column_name <- colnames(micro_comb_list)[micro_best_r_squared_index]
print(micro_comb_column_name)

R2_df_micro <- data.frame(comb_names = unlist(micro_comb_names),
                    r_squared_values = micro_r_squared_values)

micro_indicator <- data.frame(
  vaekst=forbrug_period$Vaekst,
  combi = micro_df[,micro_best_r_squared_index])

micro_opt_model <- lm(vaekst ~ combi, data=micro_indicator)
summary(micro_opt_model)


micro_ind_Q4 <- mean(c(ftillid_wide$Q9_Forbrugsgoder_etår[346:347]))

micro_ind <- data.frame(combi=c(micro_ind_Q4))

pred_micro <- predict(micro_opt_model, newdata=micro_ind, type="response")
print(pred_micro)
# 1,067872




