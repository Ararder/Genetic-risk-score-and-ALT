# About the project

This repository contains the code and analysis pipeline for a project entitled  "The association between a genetic risk score and autistic-like traits" which was done as part of [KI Summer School in Medical Research](https://education.ki.se/ki-summer-school-in-medical-research). Together with my supervisor, Dr [Yi Lu](https://scholar.google.com.au/citations?hl=en&user=OucCRnoAAAAJ&view_op=list_works&sortby=pubdate), we investigated how a genetic risk scores relates to the three core symptoms in autism-spectrum disorder(ASD).


## Autistic like traits

One meta-analysis estimate the heritability of ASD to be between 73-91%. [1](https://onlinelibrary-wiley-com.proxy.kib.ki.se/doi/full/10.1111/jcpp.12499), which indicate that genetic effects have a relatively large role to play in the etiology of ASD. ASD is often conceptualized as a trio of three core symptoms; 1issues with social reciprocity, repetitive and stereotypical behaviour or interests, and impairments in verbal and nonverbal communication.

We wanted to investigate if the association between genetic factors was stronger for one of these symptom triads. To do this, we calculated a genetic risk score, a measure of the total genetic liability for all the individuals in a sample of twins from CATSS, which is a part of the [Swedish Twin Registry](https://ki.se/en/research/swedish-twin-registry-for-researchers).


## Showcase of pipeline.

To see if the association between genetic factors was stronger for any of the core triad symtoms, we used data from the A-TAC questionnaire [2](https://www-cambridge-org.proxy.kib.ki.se/core/journals/the-british-journal-of-psychiatry/article/psychiatric-telephone-interview-with-parents-for-screening-of-childhood-autism-tics-attentiondeficit-hyperactivity-disorder-and-other-comorbidities-atac/BA1E4F42D934E2D20D0B580F50B5BEDEO). 

We then fit a linear model, with the outcome variable being the score on the three subscales measuring each of the core triad traits, and total score. Furthermore, for each of scale scores, we tested the association with eight different genetic risk scores, generated at different significance thresholds. We controlled for age, sex, and the first ten principal components. Since the data is on twins, we used robust clustered standard errors to account for the twin pairs.

```R

for(a in 4:7) { 
  for(i in 8:15) {
    c <- (c + 1) 
    model <- lm(aut_gwas[[a]] ~ aut_gwas[[i]] + aut_gwas[[16]] + aut_gwas[[17]] + aut_gwas[[18]] + # 16:25 corresponds to principal component 1:10
                  aut_gwas[[19]] + aut_gwas[[20]] + aut_gwas[[3]],    
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
```


## Results
The association between the genetic risk score as significant for the flexibility trait, and the total score. For language issues and social issues, the association was not significant.

![](/graphs/pvalues_plot.png)





The effect sizes in the plot are standardized by the scale standard deviation. For social and language issues, the confidence interval crosses 0. The effect size is largest for the flexibility trait.

![](/graphs/Rplot.png)







## Discussion

The results could indicate that common variants play a comparatively large role in the genesis of symptoms related to flexibility issues.

