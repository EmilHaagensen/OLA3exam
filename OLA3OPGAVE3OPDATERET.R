    #####################
    ### OLA3 OPGAVE 3 ### 
    #####################

library(ggplot2)
library(caret)
library(pROC)
library(dkstat)
library(tidyr)
library(dplyr)
    
#################################
### Opgave 3.1 - Forudsigelse ###
#################################

    
# 3.1 Opgavebeskrivelse; 
    # Lav en machine Learning model, der med afsæt i DST's forbrugertillidsindikator,
    # kan forudsige om julehandlen i 2024 er større end i 2023.
    
# Laver ny dataframe med data som er indhentet for opgave 1. 
    
# Indhenter Forbrugsdata
    
    forbrugmeta = dst_meta(table = "NKH1", lang = "da")
    
    forbrugquery = list(
      TRANSAKT = "P.31 Husholdningernes forbrugsudgifter",
      PRISENHED = "2020-priser, kædede værdier",
      SÆSON = "Sæsonkorrigeret",
      Tid = "*")
    
    forbrugdata = dst_get_data(table = "NKH1", query = forbrugquery, lang = "da")
    
    forbrugdf = pivot_wider(forbrugdata, names_from = TRANSAKT, values_from = value)
    
    
    # fjerner ligegyldige kolonner
    forbrugdf = forbrugdf[,-1:-2]
    
    # udregner årlig realvækst pr. kvartal
    # grundet 4 NA-værdier i de første 4 rækker, gøres som følgende: 
    
    realvækst = diff(log(as.numeric(forbrugdf$`P.31 Husholdningernes forbrugsudgifter`)), lag = 4) * 100
    realvækstna = rep(NA, nrow(forbrugdf))
    realvækstna[5:nrow(forbrugdf)] = realvækst
    forbrugdf$realvækst = realvækstna

    
    DIFTI.meta = dst_meta(table = "FORV1", lang = "da") 
    
    DIFTIquery = list(
      INDIKATOR = c(
        "Forbrugertillidsindikatoren"),
      Tid = "*")
    
    DIFTIdf = dst_get_data(table = "FORV1", query = DIFTIquery, lang = "da")
    
    DIFTIdf.wide = pivot_wider(DIFTIdf, names_from = INDIKATOR, values_from = value)
    which(DIFTIdf.wide$TID == "1998-01-01") # 280
    which(DIFTIdf.wide$TID == "2024-07-1") # 598
    
    DIFTIdf.wide = DIFTIdf.wide[280:601, ]
    
    DIFTIdf.wide = as.data.frame(DIFTIdf.wide)
  
    difti.kvartal = DIFTIdf.wide[0 , ]
    
    for (i in seq(1, nrow(DIFTIdf.wide), by = 3)) { 
      kvartal.måned = DIFTIdf.wide[i, 1] 
      kvartal.total = kvartal.måned
      
      for (j in 2:ncol(DIFTIdf.wide)) { 
        gnscol = mean(as.numeric(DIFTIdf.wide[i:(i+2), j]), na.rm = T)
        kvartal.total = c(kvartal.total, gnscol)
      }
      difti.kvartal = rbind(difti.kvartal, kvartal.total)
    }
    
    colnames(difti.kvartal) = colnames(DIFTIdf.wide)
    difti.kvartal$TID = as.Date(difti.kvartal$TID)
    
    which(forbrugdf$TID == "2000-01-01") # 41
    max(forbrugdf$TID) # 2024-07-01
    
    which(difti.kvartal$TID == "2000-01-01") #102
    which(difti.kvartal$TID == "2024-07-01") # 200
    

    
    julehandel.df = data.frame(Tid = forbrugdf$TID[41:nrow(forbrugdf)],
                               DSTFTI = round(difti.kvartal$Forbrugertillidsindikatoren[102:200],2),
                               realvækst = round(forbrugdf$realvækst[41:nrow(forbrugdf)],2))
    
# Tilføjer dummy-variabel i ny kolonne 'logfactor' (dummy-varibel med niveauer: 1, 0)
julehandel.df$logfactor = ifelse(julehandel.df$realvækst >= 0 , 1, 0)
julehandel.df$logfactor = as.factor(julehandel.df$logfactor)
levels(julehandel.df$logfactor)

#Tjekker fordeling på dummy-variabel; 1 = vækst, 0 = nedgang 
freqtable = data.frame(table(julehandel.df$logfactor))
View(freqtable)
colnames(freqtable) = c("Dummyvariabel", "Frekvens")
rownames(freqtable) = c("Nedgang", "Vækst")

#visualiser fordelingen
ggplot(freqtable, aes(x =Dummyvariabel, y = Frekvens, fill = Dummyvariabel)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(x = "Udvikling af den kvartalsvis årlige realvækst for husholdningernes forbrugsudgifter; Vækst = 1, Nedgang = 0",
       title = "Der er et stort overtal af vækstkvartaler i perioden 2000K1 - 2024K3",
       subtitle = "Frekvensfordeling af dummyvariabelen.",
       caption = "Kilde = Statistikbanken, Tabel NKH1") +
  scale_fill_manual(values = c("0" = "red", "1" = "green")) +
  theme_bw() +  theme(
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(vjust = 1, size = 12),
    legend.position = "right",
    legend.background = NULL)
  

  

# Laver logisitsk regressionsmodel for dummyvariabel ~ DSTs FTI


julehandel.log.model = glm(logfactor ~ DSTFTI,
                           data = julehandel.df,
                           family = "binomial")

summary(julehandel.log.model)

### visualisering til powerpoint ###
  # inddrages ikke i opgaven - IKKE OPDATERET

julehandel.df$predictprob = predict(julehandel.log.model, type = "response")

ggplot(julehandel.df, aes(x = DSTFTI, y = predictprob)) +
  geom_line(color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +
  #geom_hline(yintercept = 0.19, linetype = "dashed", color = "blue") +
  labs(title = "Forudsete sandsynligheder for 3. kvartal og oktober måned", y = "forudsagt sandsynlighed", x = "forbrugertillidsindikator") + 
  theme_minimal() +
  geom_point(aes(x = -6.53, y = 0.642), color = "green") + 
  geom_point(aes(x = -8.9, y = 0.568), color = "darkgreen")

# Prediction for julehandlen i 2024 (Q4)

  # Skaber først en newdata-dataframe for hver kvartal

new.data.julehandel = data.frame(DSTFTI = difti.kvartal[201, 2]) # Udtrækker Q4 fra dataframe fra opgave 1.

prediction.julehandel = predict(julehandel.log.model, 
                                newdata = new.data.julehandel, type = "response")

# 3.2 Opgavebeskrivelse;
    # Lav en vurdering af validiteten af jeres model fra opgave 3.1.

# Laver dataframe med prediction-values + fti

prediction.dataframe = data.frame(DSTFTI = julehandel.df$DSTFTI)
prediction.values.jul = predict(julehandel.log.model, 
                                prediction.dataframe, type = "response")

# Skaber en tom vektor
prediction.values.jul.vektor = c()

# Looper sandsynlighed med standardgrænseværdi (0.5)

for (i in 1:length(prediction.values.jul)) {
  if (is.na(prediction.values.jul[i])) {
    prediction.values.jul.vektor[i] = "NULL"
  } else if (prediction.values.jul[i] >= 0.5) {
    prediction.values.jul.vektor[i] = "1"
  } else {
    prediction.values.jul.vektor[i] = "0"
  }
}

# Indsætter prediction.values.jul.vektor i julehandel.df (original df) 

julehandel.df$prediction.values = as.factor(prediction.values.jul.vektor)

# Laver en konfusionmatrice over TPR og FPR 

confusionmatrix.threshold50 = confusionMatrix(julehandel.df$prediction.values,
                                              julehandel.df$logfactor)



confusionmatrix.threshold50 = confusionMatrix(logdf$predictions50, logdf$logfactor)

cm.50 = as.data.frame(as.table(confusionmatrix.threshold50))
ggplot(cm.50, aes(Prediction, Reference)) +
  geom_tile(aes(fill = Freq), color = "white", size = 1.5) +  # Create the heatmap
  scale_fill_gradient(low = "white", high = "blue") +  # Color scale for the heatmap
  geom_text(aes(label = sprintf("%d", Freq)), vjust = 1) +  # Add text labels
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0)) +  # Rotate axis labels
  labs(x = "forudsagte værdier", y = "aktuelle værdier", title = "Utrolig optismistisk model: rigtig god til forudsigelse af positive værdier, dårlig til negative",
       subtitle = "Konfusionsmatrice med standardgrænseværdi: 0.5 (50%)")

# TPR = TP/TP + FN = 73 / (73 + 16) = 0.82 ..
# FPR = FP/FP + TN = 3 / (3 + 5) = 0.375
# TNR = TN/TN+FP = 6 / (6 + 4) = 0.625
# FNR = FN/TP+FN = 16 / (73 + 16) = 0.177 ..

# sammenfattende viser ovenstående tal, at modellen generelt er god til at opdage vækster (1),
# men at der stadigvæk er udfordringer. Modellen laver for mange FP og FN, og det er noget som kan forbedres gennem en bedre grænseværdi. 

# mange flere positiver end engativer = indikator for at øge threshold
# høj TPR = model er god til at identificere positiver korrekt
# høj FPR = model er dårlig til at identificere negativer korrekt
# igen en indikator for at øge threshold.

# For et bedre overblik skaber vi en ROC-kurve for at få en ide om det optimale threshold hvor AUC er højest. 

predicted.probs.ROC = predict(julehandel.log.model, newdata = julehandel.df, type = "response")
ROC.kurve = roc(julehandel.df$logfactor, predicted.probs.ROC, plot = TRUE, print.AUC = TRUE)

# som det fremstår af ROC-kurven ligner det, at AUC er højest nær ~ 0.8

# for at teste om det er rigtigt bruger vi youden's metode til at finde OT (optimal threshold)


optimal.grænse = coords(ROC.kurve, "best", best.method = "youden", ret = "threshold")
optimal.threshold = optimal.grænse # 0.788 

# gør det samme igen med 0.788 som threshold 

prediction.ot.dataframe = data.frame(DSTFTI = julehandel.df$DSTFTI)


prediction.values.jul.vektor.ot = c()

for (i in 1:length(prediction.values.jul)) {
  if (is.na(prediction.values.jul[i])) {
    prediction.values.jul.vektor.ot[i] = "NULL"
  } else if (prediction.values.jul[i] >= optimal.threshold) {
    prediction.values.jul.vektor.ot[i] = "1"
  } else {
    prediction.values.jul.vektor.ot[i] = "0"
  }
}

julehandel.df$ot.prediction.values = as.factor(prediction.values.jul.vektor.ot)

confusion.matrix.thresholdot = confusionMatrix(julehandel.df$ot.prediction.values,
                                                julehandel.df$logfactor)
cm.ot = as.data.frame(as.table(confusion.matrix.thresholdot))
ggplot(cm.ot, aes(Prediction, Reference)) +
  geom_tile(aes(fill = Freq), color = "white", size = 1.5) +  # Create the heatmap
  scale_fill_gradient(low = "white", high = "blue") +  # Color scale for the heatmap
  geom_text(aes(label = sprintf("%d", Freq)), vjust = 1) +  # Add text labels
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0)) +  # Rotate axis labels
  labs(x = "forudsagte værdier", y = "aktuelle værdier", title = "Balanceret model: Modellen er meget bedre til negativer, og stadig nogenlunde til positiver",
       subtitle = "Konfusionsmatrice med optimal grænseværdi for balace: Youdens (0.788)")
 # accuracy på 0.76, men en langt bedre fordeling af TP/TN; ergo skaber OT en konfusionmatrice, som vægter højest,
 # at finde den grænseværdi, som finder den bedste balance med TP/TN fremfor 'overall accuracy' 

 # hvis vi derimod gerne ville finde det threshold som yder den højeste accuracy ift. konfusionsmatricen, kan der gøres som følgende: 


grænsevektor <- seq(0, 1, by = 0.01) 
accuracy <- numeric(length(grænsevektor)) 

for (i in seq_along(grænsevektor)) {
  grænseværdi <- grænsevektor[i]
  predicted_classes <- ifelse(predicted.probs.ROC >= grænseværdi, 1, 0)
  cm <- table(Predicted = predicted_classes, Actual = julehandel.df$logfactor)
  accuracy[i] <- sum(diag(cm)) / sum(cm)  # (TP + TN) / total
}

optimal.grænse.modelacc <- grænsevektor[which.max(accuracy)]
max_accuracy <- max(accuracy)

cat("Optimale grænseværdi for Modelaccuracy er ", optimal.grænse.modelacc, "\n")
cat("Max Accuracy:", max_accuracy, "\n")


    ## hermed yder en grænseværdi på 0.17 den højeste model accuracy. 

    # kan eventuelt lave konfusionsmatrix osv. for dette men ved ikke om det er nødvendigt?

prediction.values.jul.vektor.maxacc = c()

for (i in 1:length(prediction.values.jul)) {
  if (is.na(prediction.values.jul[i])) {
    prediction.values.jul.vektor.maxacc[i] = "NULL"
  } else if (prediction.values.jul[i] >= 0.19) {
    prediction.values.jul.vektor.maxacc[i] = "1"
  } else {
    prediction.values.jul.vektor.maxacc[i] = "0"
  }
}

julehandel.df$maxacc.prediction.values = as.factor(prediction.values.jul.vektor.maxacc)

confusion.matrix.thresholdmaxacc = confusionMatrix(julehandel.df$maxacc.prediction.values,
                                               julehandel.df$logfactor)
cm.ma = as.data.frame(as.table(confusion.matrix.thresholdmaxacc))
ggplot(cm.ma, aes(Prediction, Reference)) +
  geom_tile(aes(fill = Freq), color = "white", size = 1.5) +  # Create the heatmap
  scale_fill_gradient(low = "white", high = "blue") +  # Color scale for the heatmap
  geom_text(aes(label = sprintf("%d", Freq)), vjust = 1) +  # Add text labels
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0)) +  # Rotate axis labels
  labs(x = "forudsagte værdier", y = "aktuelle værdier", title = "Optimeret model for MA: Modellen er perfekt til positive vækstperioder og uduelig til nedgangsperioder",
       subtitle = "Konfusionsmatrice med optimal grænseværdi for Model accuracy: 0.17")



