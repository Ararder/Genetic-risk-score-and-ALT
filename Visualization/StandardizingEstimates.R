
# Aim: Visualize the effect sizes of the genetic risk scores on the domains of A-TAC
# Requisites: The regression scripts need to be run before this script.
# 
#




# standardizing effect size estimates




lang_standard <- final_aut_reg_robust %>% 
  filter(Domain == "Language") %>% 
  select(Estimate, Std.Err) %>% 
  mutate(Estimate = Estimate/ ( sd(aut_gwas$language)),
         Std.Err = Std.Err/ ( sd(aut_gwas$language)))

social_standard <- final_aut_reg_robust %>% 
  filter(Domain == "Social") %>% 
  select(Estimate, Std.Err) %>% 
  mutate(Estimate = Estimate / ( sd(aut_gwas$social)),
         Std.Err = Std.Err / (sd(aut_gwas$social)))

flex_standard <- final_aut_reg_robust %>% 
  filter(Domain == "Flexibility") %>% 
  select(Estimate, Std.Err) %>% 
  mutate(Estimate = Estimate/(sd(aut_gwas$flex)),
         Std.Err = Std.Err/ (sd(aut_gwas$flex)))

tot_standard <- final_aut_reg_robust %>% 
  filter(Domain == "Total") %>% 
  select(Estimate, Std.Err) %>% 
  mutate(Estimate = Estimate/(sd(aut_gwas$totscore)),
         Std.Err = Std.Err / (sd(aut_gwas$totscore)))

#i bind the standardized estimates into one dataframe.
standardized_estimates <- bind_rows(lang_standard, social_standard, flex_standard, tot_standard)

#creating a dataframe to visualize

graph_estimates <- final_aut_reg_robust %>% 
  select(Estimate, Std.Err, p.beta, Domain, pcutoff)

#putting in the standardized estimates and errors.
graph_estimates$Estimate <- standardized_estimates$Estimate
graph_estimates$Std.Err <- standardized_estimates$Std.Err

