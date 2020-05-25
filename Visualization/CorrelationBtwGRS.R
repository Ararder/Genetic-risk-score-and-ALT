
library(tidyverse)
library(broom)
library(sandwich)
library(lmtest)

my_theme <- theme(panel.grid = element_blank(), axis.line = element_line(color = "black"),
                  panel.background = element_blank(), axis.text.x = element_text(color = "black"), 
                  axis.text.y = element_text(color = "black"))

aut_gwas <- read_tsv("~/Programming/R/summerSchool/raw_data/alt_pheno_grs_pcs.txt")





corr_temp <- tibble(p0.001 = aut_gwas$S1_0.001, p0.01 = aut_gwas$S2_0.01, 
                    p0.05 = aut_gwas$S3_0.05, p0.1 = aut_gwas$S4_0.1, p0.2  = aut_gwas$S5_0.2,
                    p0.3 = aut_gwas$S6_0.3, p0.5 = aut_gwas$S7_0.5, p1 = aut_gwas$S8_1) %>% 
  cor(method = "pearson") %>% 
  matrix()

corr_graph <- corr_temp[1:8,] %>% 
  tibble()

pcut <- c(0.001, 0.01, 0.05, 0.1,0.2, 0.3, 0.5,1)

corr_graph_finished <- bind_cols(pcut = pcut, corr = corr_graph) %>% 
  rename(corr = ".")

corr_graph_finished %>% 
  filter(corr < 1) %>% 
  ggplot(aes( x = factor(pcut), y = corr)) +
  geom_point() +
  my_theme +
  ylab("Correlation") +
  xlab("P Treshhold") +
  ggtitle("Correlation between risk scores generated with different p value treshholds")












