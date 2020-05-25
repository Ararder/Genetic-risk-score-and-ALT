
library(tidyverse)
library(broom)
library(sandwich)
library(lmtest)

#Dataframe of scores on A-TAC, genetic risk scores and principal components
aut_gwas <- read_tsv("~/Programming/R/summerSchool/raw_data/alt_pheno_grs_pcs.txt")







many_models <- vector("list", 32)
r2 <- tibble()
coefs <- tibble()
c <- 0


#generating a model without grs as predictor, to calculate r2 of only GRS  
for(a in 4:7) { 
  for(i in 8:15) {#aut_gwas[[4:7 ]]corresponds to the column number for the ALT domains
    c <- (c + 1) #adding a counter to assign each model to right place in list
    model <- lm(aut_gwas[[a]] ~ aut_gwas[[16]] + aut_gwas[[17]] + aut_gwas[[18]] + # 16:25 corresponds to principal component 1:10
                  aut_gwas[[19]] + aut_gwas[[20]] + aut_gwas[[3]],    # aut_gwas[3] is age
                data = aut_gwas)
    many_models[[c]] <- model
  }
}

### extraction loop with sandwich estimator for robust errors.
for (i in 1:32) {
  r2[i,1:3] <- glance(many_models[[i]]) %>%
    select(1:2, 5)
  #this is where i apply sandwich estimator.
  temp <- coeftest(many_models[[i]], vcov = vcovCL, cluster = ~ FID)[2,]
  coefs[i,1] <- temp[[1]]
  coefs[i,2] <- temp[[2]]
  coefs[i,3] <- temp[[3]]
  output_r2_subtract <- bind_cols(r2, coefs)
}

#extract the r2 without the genetic risk score
r2_without_GRS <- output_r2_subtract$adj.r.squared

#extract r2 when genetic risk score was included
r2_with_grs <- output$adj.r.squared

#subtract them, and get nagelkerkes R2
r2_of_predictor <- r2_with_grs - r2_without_GRS


#bind into final column
final_aut_reg_robust <- bind_cols(final_aut_reg_robust, prsR2 = r2_of_predictor)



#apply correction for multiple comparisons
final_aut_reg_robust <- final_aut_reg_robust %>% 
  mutate(p.beta = p.adjust(p.beta, method = "fdr", n = 32))


