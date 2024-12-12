#### Hent data om forbrugertillid fra DST ####
library(dkstat)
library(reshape)
library(dplyr)


ftillid_meta = dst_meta(table = "FORV1", lang = "da")
myquery <- list(
  INDIKATOR = "*",
  Tid = "*")

ftillid_meta = dst_get_data(table = "FORV1", query = myquery, lang = "da")

#### Lav dataframen om så alle indikatorer er i forskellige kolonner / wide format ####

ftillid_wide = pivot_wider(ftillid_meta,
                           names_from = "INDIKATOR",
                           values_from = "value")

colnames(ftillid_wide) = c("TID", "DST_FTI", "Q1_Fam_nu", "Q2_Fam_etår", "Q3_Dan_nu", "Q4_Dan_etår", "Q5_Forbrugsgoder_nu", "Q6_Priser_nu", "Q7_Priser_etår", "Q8_Arbejdsløshed", "Q9_Forbrugsgoder_etår", "Q10_Sparop_nu", "Q11_Sparop_etår", "Q12_Famøk_nu")

# Q6 og Q8 skal ganges med -1 på grund af den måde spørgsmål og svar er bygget på
ftillid_wide$Q6_Priser_nu = ftillid_wide$Q6_Priser_nu*(-1)
ftillid_wide$Q8_Arbejdsløshed = ftillid_wide$Q8_Arbejdsløshed*(-1)

## Lav dataen om til kvartaler
ftillid_wide = ftillid_wide[-c(1:255),]
row.names(ftillid_wide) = NULL

ftillid_wide = as.data.frame(ftillid_wide)

ftillid_k = ftillid_wide[0, ]
ftillid_k = ftillid_k %>% 
  mutate_all(as.numeric)

# Lav indikatorerne om til kvartaler med loop
for (i in seq(3, nrow(ftillid_wide), by = 3)) {
  
  kvartal_måned = as.Date(ftillid_wide[i,1])
  # Tidsperioden
  kvartal_total = kvartal_måned
  
  for (j in 2:ncol(ftillid_wide)) {
    meankolonne = round(mean(as.numeric(ftillid_wide[(i-2):i, j]), na.rm = T), 2)
    kvartal_total = c(kvartal_total, meankolonne)
  }
  
  ftillid_k = rbind(ftillid_k, kvartal_total) 
}

colnames(ftillid_k) = colnames(ftillid_wide)  
ftillid_k$TID = as.Date(ftillid_k$TID)

 

#### sætter startpunkt til 2000K1 ####
ftillid_k_period = ftillid_k[-c(1:16),]
row.names(ftillid_k_period) <- NULL

#### Lav alle kombinationer #####


# Laver lister til combinationerne og deres navne
comb_list <- list()
comb_names <- list()  

# Definerer vores kolonner til vores loop (altså underspørgsmålene)
n_cols <- ncol(ftillid_k_period) - 2  # Excluding columns 1 and 2

# Looper gennem underspørgsmålene ved brug af combn funktion
for (j in 1:n_cols) {
  combinations <- combn(3:14, j, simplify = FALSE)  # Get all combinations of columns 3 to 14
  
  # Udregner gns. for hver kombination / vi undgår multikol. 
  for (cols in combinations) {
    avg_value <- rowMeans(ftillid_k_period[, cols, drop = FALSE], na.rm = TRUE) 
    comb_list[[length(comb_list) + 1]] = avg_value  # Gemmer resultaterne i vores comb_list
    
    # Gemmer navnet på vores combination (sammensat string af underspørgsmålene) 
    comb_names[[length(comb_names) + 1]] = paste(colnames(ftillid_k_period)[cols], collapse = ", ")
  }
}

# Vi tjekker antal kombinationer 
length(comb_list)  # 4095 forskellige kombinationer for 12 underspørgsmål 








