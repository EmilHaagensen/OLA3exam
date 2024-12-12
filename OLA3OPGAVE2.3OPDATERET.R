#### Predictions

Q4 <- c(mean(c(ftillid_wide$Q1_Fam_nu[346:347])),
        mean(c(ftillid_wide$Q2_Fam_etår[346:347])),
        mean(c(ftillid_wide$Q3_Dan_nu[346:347])),
        mean(c(ftillid_wide$Q4_Dan_etår[346:347])),
        mean(c(ftillid_wide$Q5_Forbrugsgoder_nu[346:347])),
        mean(c(ftillid_wide$Q6_Priser_nu[346:347])),
        mean(c(ftillid_wide$Q7_Priser_etår[346:347])),
        mean(c(ftillid_wide$Q8_Arbejdsløshed[346:347])),
        mean(c(ftillid_wide$Q9_Forbrugsgoder_etår[346:347])),
        mean(c(ftillid_wide$Q10_Sparop_nu[346:347])),
        mean(c(ftillid_wide$Q11_Sparop_etår[346:347])),
        mean(c(ftillid_wide$Q12_Famøk_nu[346:347])))

sum(Q4*loadings_DTS_PCR[1:12])

weighted_ind <- as.data.frame(matrix(ncol=12, nrow=1))
weighted_ind[1,] <- Q4

colnames(weighted_ind) <- c("Q1_Fam_nu", "Q2_Fam_etår", "Q3_Dan_nu", "Q4_Dan_etår", 
                            "Q5_Forbrugsgoder_nu", "Q6_Priser_nu", "Q7_Priser_etår", 
                            "Q8_Arbejdsløshed", "Q9_Forbrugsgoder_etår", "Q10_Sparop_nu", 
                            "Q11_Sparop_etår", "Q12_Famøk_nu")

# Subset `weighted_ind` to have only these predictors
pred_weighted_2 <- predict(DST_PCR, newdata = weighted_ind, ncomp=1)
print(pred_weighted_2)
# -0,3767277


