
library(tidyverse)
library(broom)
library(sandwich)
library(lmtest)

#Dataframe of scores on A-TAC, genetic risk scores and principal components
aut_gwas <- read_tsv("~/Programming/R/summerSchool/raw_data/alt_pheno_grs_pcs.txt")



# defining output tibbles and vectors, and a counting variable; c
# the loop is run twice, to get two values of R^2. 
#
#----------

# Loop 1: with GRS as predictor
#

many_models <- vector("list", 32)
r2 <- tibble()
coefs <- tibble()
c <- 0



# To generate a total of 32 regressions, we loop over each of the A-TAC scores,
# creating a linear model for each level of genetic risk score. 

for(a in 4:7) { 
  for(i in 8:15) {#aut_gwas[[4:7 ]]corresponds to the column number for the ALT domains
    c <- (c + 1) #adding a counter to assign each model to right place in list
    model <- lm(aut_gwas[[a]] ~ aut_gwas[[i]] + aut_gwas[[16]] + aut_gwas[[17]] + aut_gwas[[18]] + # 16:25 corresponds to principal component 1:10
                  aut_gwas[[19]] + aut_gwas[[20]] + aut_gwas[[3]],    # aut_gwas[3] is age
                data = aut_gwas)
    many_models[[c]] <- model
  }
}

#extraction loop with sandwich estimator for robust standard errors.
for (i in 1:32) {
  r2[i,1:3] <- glance(many_models[[i]]) %>%
    select(1:2, 5)
  #this is where i apply sandwich estimator,and cluster
  temp <- coeftest(many_models[[i]], vcov = vcovCL, cluster = ~ FID)[2,c(1,2,4)] 
  coefs[i,1] <- temp[[1]]
  coefs[i,2] <- temp[[2]]
  coefs[i,3] <- temp[[3]]
  
  output <- bind_cols(r2, coefs)
}


output <- output %>% 
  rename( Estimate = ...1, Std.Err = ...2, p.beta = ...3)

p_cut <- rep(c(0.001, 0.01, 0.05, 0.1,0.2, 0.3, 0.5,1), 4)
#the order of the regressions was the same as in the dataset. ie, 1,language 2, social 3, flex 4,totscore 
test <- c(rep(c("Language"), times = 8), rep(c("Social"), times = 8), rep(c("Flexibility"), times = 8),
          rep(c("Total"), times = 8))
# addinags columns with information on which subdomain was regressed on
# and what p_value was used to generate the GRS.


final_aut_reg_robust <- bind_cols(output, Domain = test, pcutoff = p_cut)


