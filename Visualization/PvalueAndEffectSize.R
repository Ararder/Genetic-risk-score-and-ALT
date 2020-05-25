# Aim: Visualize the effect sizes of the genetic risk scores on the domains of A-TAC
# Requisites: The regression scripts need to be run before this script.
# and the StandardizingEstimates script.


my_theme <- theme(panel.grid = element_blank(), axis.line = element_line(color = "black"),
                  panel.background = element_blank(), axis.text.x = element_text(color = "black"), 
                  axis.text.y = element_text(color = "black"))



## a plot of the effect sizes of the genetic risk score on the autistic-like trait scores.

graph_estimates %>%
  ggplot(aes(x = factor(pcutoff), y = Estimate, color = Domain)) +
  geom_point() +
  geom_errorbar(aes(ymin = Estimate - 1.97*Std.Err, ymax = Estimate + 1.97*Std.Err), color = "black", alpha = 0.4) +
  theme(panel.grid = element_blank()) +
  my_theme +
  ggtitle(" Estimates of GRS on ALT domains") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  facet_grid(.~ Domain) +
  theme(legend.key = element_blank(), legend.title = element_blank(),
        strip.background = element_blank(), axis.text.x = element_text(size = 5)) +
  theme(axis.text.x = element_text(angle = 45))


#
# Graph of the p-values of the association between genetic risk scores and autistic-like traits
#


graph_estimates %>% 
  ggplot(aes(y = -log10(p.beta), x = factor(pcutoff), color = Domain)) +
  geom_point() +
  theme(panel.grid = element_blank()) +
  my_theme +
  ylab("-log10 p value") +
  ggtitle("p Value for the association between a genetic risk score 
                and scores on autistic-like traits") +
  theme(axis.title.x = element_blank()) +
  facet_grid(.~ Domain) +
  theme(strip.text.x = element_blank()) +
  theme(legend.key = element_blank(), legend.title = element_blank(),
        strip.background = element_blank(), axis.text.x = element_text(size = 5)) +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_hline(aes( yintercept = -log10(0.05)))


