#### Prediction af  4. kvartal ####
library(corrplot)


# Model for vores optimale indikator (comb 666)
optimalecomb <- data.frame(
  vaekst=forbrug_period$Vaekst,
  combi = comb_df[,best_r_squared_index])

optimal_model <- lm(vaekst ~ combi, data=optimalecomb)
summary(optimal_model)

# Har ikke FTI data for Dec. - dermed laver vi mean på 2/3 måneder, som må fremstå som placeholder før dec. tallene findes. 
opt_ind_Q4 <- mean(c(ftillid_wide$Q3_Dan_nu[346:347],ftillid_wide$Q9_Forbrugsgoder_etår[346:347],
                      ftillid_wide$Q11_Sparop_etår[346:347],ftillid_wide$Q12_Famøk_nu[346:347]))

opt_ind <- data.frame(combi=c(opt_ind_Q4))

# predict for 4. kvartal
pred_optimal <- predict(optimal_model, newdata=opt_ind, type="response")
print(pred_optimal) # 0,7280459


#### Korrelation ####

cor_matrix <- cor(data.frame(y=optimalecomb$vaekst, x=optimalecomb$combi))

corrplot(cor_matrix, tl.cex = 0.5,
        method = "color",
        type = "upper",
        addCoef.col = "black",
        number.cex = 1,
        diag = F,
        outline = T)

cor_matrix_k <- cor(forbrug_period$Vaekst, ftillid_k_period[,3:14])
corrplot(cor_matrix_k,
         method="color",
         addCoef.col = "black",
         number.cex = 1,
         outline = T,
         cl.pos = "n")


