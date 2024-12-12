#### Hent data for forbruget ####
library(dkstat)
library(tidyverse)

forbrug_meta <- dst_meta(table = "NKH1", lang = "da")


myquery_forbrug <- list(
  TRANSAKT = "P.31 Husholdningernes forbrugsudgifter",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

forbrug_meta <- dst_get_data(table = "NKH1", query = myquery_forbrug, lang = "da")
forbrug_meta <- forbrug_meta[-c(1:4),-c(1:3)]
Vaekst <- diff(log(as.numeric(forbrug_meta$value)), lag = 4)*100
forbrug_meta$Vaekst <- c(rep(NA,4),Vaekst)


# Justerer til starttidspunkt: 2000 1. Kvar
forbrug_period <- forbrug_meta[-c(1:36),]
row.names(forbrug_period) <- NULL


#### Omdanner vores comb_liste til dataframe vha. cbind og do-call 
comb_df <- do.call(cbind, comb_list)

# indsætter vores tidligere lavede strings med kombinationerne, stored i comb_names
colnames(comb_df) <- unlist(comb_names)

#### Udregning af r^2 

# Igangsætter en vektor til r^2 værdierne - naturligvis med længde ift. antal kombinationer, hence ncol da vi har cbindet
r_squared_values <- numeric(ncol(comb_df))

# Looper gennem hver kombination i vores dataframe
for (i in 1:ncol(comb_df)) {
  # laver en model som laver en SLR mellem realvæksten og den nuværende iteration (den kombi som loopet er nået til)
  model <- lm(forbrug_period$Vaekst ~ comb_df[, i])
  
  # Gemmer vores r^2 værdi i vores vektor
    r_squared_values[i] <- summary(model)$r.squared
}

# finder vores index for kombinationen med højeste r^2 
best_r_squared_index <- which.max(r_squared_values)

# Finder selve kombinationen
best_r_squared <- r_squared_values[best_r_squared_index]

comb_column_name <- colnames(comb_df)[best_r_squared_index]
print(comb_column_name)

# laver en dataframe med kombinationerne og tilsvarende r^2 værdierne 
R2_df <- data.frame(comb_names = unlist(comb_names),
                    r_squared_values = r_squared_values)


R2_df <- arrange(R2_df, desc(r_squared_values))

which(colnames(comb_df) == R2_df[1, 1]) # 666


###################### Checker signifikans og DIs FTI 
lm.bestr2 <- lm(forbrug_period$Vaekst ~ comb_df[,666])
summary(lm.bestr2)



# Udregner difti og tjekker den 
diftidf = data.frame(
  TID = ftillid_k_period$TID,
  difti = rowMeans(ftillid_k_period[, c("Q1_Fam_nu",
                                        "Q3_Dan_nu",
                                        "Q5_Forbrugsgoder_nu",
                                        "Q9_Forbrugsgoder_etår")]))
lm.bestr2.di = lm(forbrug_period$Vaekst ~ diftidf$difti)
summary(lm.bestr2.di)

# Korrelation til opgave 1.3 

cor13 = cor(forbrug_period$Vaekst, ftillid_k_period[, 3:14])

library(corrplot)
corrplot(cor13,
         outline = T, 
         method = "color",
         addCoef.col = "black",
         number.cex = 0.8,
         tl.cex = 1.2)
