
# aim : visualize which PCs are significant,
# to help decide how many PCs should be included in the final analysis
#



library(tidyverse)
library(broom)
library(sandwich)
library(lmtest)

aut_gwas <- read_tsv("~/Programming/R/summerSchool/raw_data/alt_pheno_grs_pcs.txt")



#defining a theme i use to remove a lot gridlines and other stuff
my_theme <- theme(panel.grid = element_blank(), axis.line = element_line(color = "black"),
                  panel.background = element_blank(), axis.text.x = element_text(color = "black"), 
                  axis.text.y = element_text(color = "black"))



model_pc <- lm(aut_gwas[["totscore"]] ~ aut_gwas[[16]] + aut_gwas[[17]] + aut_gwas[[18]] + # 16:25 corresponds to principal component 1:10
                 aut_gwas[[19]] + aut_gwas[[20]] + aut_gwas[[21]] + aut_gwas[[22]] +      
                 aut_gwas[[23]] + aut_gwas[[24]] + aut_gwas[[25]] + aut_gwas[[3]]
               + aut_gwas[[26]] + aut_gwas[[27]] + aut_gwas[[28]] + aut_gwas[[29]] +
                 aut_gwas[[30]] + aut_gwas[[31]] + aut_gwas[[32]] + aut_gwas[[33]]
               + aut_gwas[[34]] + aut_gwas[[35]],# aut_gwas[3] is age
               data = aut_gwas)
tidy(model_pc) %>% 
  mutate(adj.p = p.value*20) %>% # making a variable with bonferroni correction
  arrange(p.value)

tidy(model_pc) %>% 
  mutate(adj.p = p.value*20) %>% # making a variable with bonferroni correction
  arrange(-log10(p.value)) %>% 
  ggplot(aes( y = -log10(p.value), x = factor(term))) +
  geom_point() +
  geom_hline(yintercept = -log10(0.05), color = "red")

