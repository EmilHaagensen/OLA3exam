#### Der indhentes data fra tidligere opgaver i OLAen ####
library(tidyverse)

comb_df2 <- as.data.frame(comb_df) # omdanner fra matrix/array
forbrug_period2 <- forbrug_period

#### stabilitetstest  ####

# r^2 liste
r_squared_df <- NULL
r_squared_df <- list()

# While loopet kører ned til 2015-07-01 (3. kvartal 2015) - fjerner 35 kvartaler for at teste stabilitet
while (nrow(comb_df2) > 63) {
  
  # vektor som holder på R^2 værdierne
  opt_r_squared_values <- numeric(ncol(comb_df2))
  
  # Looper gennem alle kolonnerne (kombinationerne)
  for (i in 1:ncol(comb_df2)) {
    # SLR model for hver komb
    opt_model <- lm(forbrug_period2$Vaekst ~ comb_df2[, i])
    
    # gemmer r^2 i vektor
    opt_r_squared_values[i] <- summary(opt_model)$r.squared
  }
  
  # Tilføjer r^2 vektoren til listen
  r_squared_df[[length(r_squared_df) + 1]] <- opt_r_squared_values
  
  # Fjerner en række / kvartal for hver iteration - fjerner i alt 35 kvartaler
  comb_df2 <- comb_df2[-nrow(comb_df2), ]
  forbrug_period2 <- forbrug_period2[-nrow(forbrug_period2), ]
}

r_squared_df <- as.data.frame(r_squared_df)

#Skaber en dynamisk vektor, så vi ved hvor mange perioder der med i hver iteration af stabtest

num_columns <- ncol(r_squared_df)
new_column_names <- paste0("forklaringsgrad", seq(from = 99, by = -1, length.out = num_columns))

colnames(r_squared_df) <- new_column_names
row.names(r_squared_df) <- comb_names



###### DOUBLECHECK FOR MED 90 KVARTALER

r_squared_df2 <- list()

# Gendan dataframerne
comb_df2 <- as.data.frame(comb_df)
forbrug_period2 <- forbrug_period

comb_df2 <- comb_df2[-c(91:nrow(comb_df2)), ]
forbrug_period2 <- forbrug_period2[-c(91:nrow(forbrug_period2)), ]

for(i in 1:ncol(comb_df2)) {
model <- lm(forbrug_period2$Vaekst ~ comb_df2[,i])
r_squared_values[i] <- summary(model)$r.squared
}

test = ifelse(r_squared_values == r_squared_df$forklaringsgrad90, T, F)
sum(test) #Returnerer antal TRUE - altså om vores loop fungerer korrekt. hvis alle er true så det korrekt.

# udregner gennemsnitlig r^2 for sammensætning af alle `perioder` så at sige
r_squared_df$GNS <- NULL
r_squared_df$GNS <- rowMeans(r_squared_df)

# tjekker udsving vha. standardafvigelsen
r_squared_df$StD <- NULL
r_squared_df$StD <- apply(r_squared_df[, 1:35], 1, sd)
# unique <- unique(r_squared_df$StD) 

# Make a new dataframe with the new values
optimal_r_squared <- r_squared_df[,c(37:38)]

# laver en index rank på baggrund af en sammensætning gns r^2 og standardafv.
optimal_r_squared$index <- NULL
optimal_r_squared$index <- optimal_r_squared$GNS/optimal_r_squared$StD

# tilføjer rang med afsæt i gns. r^2 og standardafv.
optimal_r_squared <- arrange(optimal_r_squared, desc(GNS))
optimal_r_squared$GNS_ranking <- c(1:4095)

optimal_r_squared <- arrange(optimal_r_squared, StD)
optimal_r_squared$StD_ranking <- c(1:4095)

optimal_r_squared <- arrange(optimal_r_squared, desc(index))
optimal_r_squared$index_ranking <- c(1:4095)

which(row.names(optimal_r_squared) == "Q3_Dan_nu, Q9_Forbrugsgoder_etår, Q11_Sparop_etår, Q12_Famøk_nu") # 159
print(optimal_r_squared[159,])