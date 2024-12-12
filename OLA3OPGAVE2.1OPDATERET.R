library(pls)

#### PCA analysis ####

# Henter årlig realvækst pr. kvar. + 12 underspørgsmål fra opgave 1
    # redegørelsen for indhentningen findes dermed i de scripts


PCR_df <- data.frame(forbrug_period$Vaekst,
                    ftillid_k_period$Q1_Fam_nu,
                    ftillid_k_period$Q2_Fam_etår,
                    ftillid_k_period$Q3_Dan_nu,
                    ftillid_k_period$Q4_Dan_etår,
                    ftillid_k_period$Q5_Forbrugsgoder_nu,
                    ftillid_k_period$Q6_Priser_nu,
                    ftillid_k_period$Q7_Priser_etår,
                    ftillid_k_period$Q8_Arbejdsløshed,
                    ftillid_k_period$Q9_Forbrugsgoder_etår,
                    ftillid_k_period$Q10_Sparop_nu,
                    ftillid_k_period$Q11_Sparop_etår,
                    ftillid_k_period$Q12_Famøk_nu)

colnames(PCR_df) <- c("Vaekst", "Q1_Fam_nu", "Q2_Fam_etår", "Q3_Dan_nu", "Q4_Dan_etår", 
                            "Q5_Forbrugsgoder_nu", "Q6_Priser_nu", "Q7_Priser_etår", 
                            "Q8_Arbejdsløshed", "Q9_Forbrugsgoder_etår", "Q10_Sparop_nu", 
                            "Q11_Sparop_etår", "Q12_Famøk_nu")

### LAVER PCR MODEL

DST_PCR <- pcr(Vaekst ~ ., data=PCR_df, scale=TRUE, validation="CV")
summary(DST_PCR)

# Henter loadings
loadings_DTS_PCR <- DST_PCR$loadings
weights <- loadings_DTS_PCR[1:12]^2
sum(weights) # skal være == 1, ellers er der fejl 

validationplot(DST_PCR, val.type="MSEP") # val-plot for mean squared error of predictions, faktisk ret høj ved kun 1. PC valgt

#### Udregner vægtet værdi for underspg. (sammensat PC1)
    # og opretter dataframen til det, naturligvis..



pcadf = data.frame(vækst = forbrug_period$Vaekst,
                   vægtetindikator =  ftillid_k_period$Q1_Fam_nu*loadings_DTS_PCR[1] + 
                     ftillid_k_period$Q2_Fam_etår*loadings_DTS_PCR[2] +
                     ftillid_k_period$Q3_Dan_nu*loadings_DTS_PCR[3] + 
                     ftillid_k_period$Q4_Dan_etår*loadings_DTS_PCR[4] +
                     ftillid_k_period$Q5_Forbrugsgoder_nu*loadings_DTS_PCR[5] + 
                     ftillid_k_period$Q6_Priser_nu*loadings_DTS_PCR[6] +
                     ftillid_k_period$Q7_Priser_etår*loadings_DTS_PCR[7] + 
                     ftillid_k_period$Q8_Arbejdsløshed*loadings_DTS_PCR[8] +
                     ftillid_k_period$Q9_Forbrugsgoder_etår*loadings_DTS_PCR[9] + 
                     ftillid_k_period$Q10_Sparop_nu*loadings_DTS_PCR[10] +
                     ftillid_k_period$Q11_Sparop_etår*loadings_DTS_PCR[11] + 
                     ftillid_k_period$Q12_Famøk_nu*loadings_DTS_PCR[12])


# Plot the indicator
scale_factor <- 0.5
pcadf$TID <- ftillid_k_period$TID

library(ggplot2)
ggplot(pcadf, aes(x=TID)) +
  geom_line(aes(y=vægtetindikator)) +
  geom_point(aes(y=vægtetindikator)) +
  geom_bar(aes(y=vækst / scale_factor), stat="identity", fill="blue", alpha=0.5) +
  geom_hline(yintercept=mean(pcadf$vægtetindikator), color="orange") +
  annotate("text", x = as.Date("2012-01-01"), y = mean(pcadf$vægtetindikator) + 4, label = "PC1 historisk gennemsnit", hjust = 2.2, color = "orange") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2000-03-01", "2024-06-01"))) +
  scale_y_continuous(name = "PC1-indikator", sec.axis = sec_axis(~ . * scale_factor, name = "Kvartalsvise årlige realvækst")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x=NULL, title="Vores PC1-indikator følger samme udvikling som den kvartalsvise årlige realvækst",
       subtitle = "Plot over udviklingen af den kvartalsvise årlige realvækst og PC1-indikatoren")



