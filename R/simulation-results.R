
########################################################################

# R script to generate the main results of the article 
# entitled ------------
# published in ------------

# Author
# Lineu Alberto Cavazani de Freitas (lineuacf@gmail.com)

########################################################################

library(ggplot2)

#----------------------------------------------------------------------

# Simulation study results for regression paramters

load("simulation-results-betas.RData")

#----------------------------------------------------------------------

results_betas <- rbind(normal_uni_n50$df_final,
                       poisson_uni_n50$df_final,
                       bernoulli_uni_n50$df_final,
                       
                       normal_uni_n100$df_final,
                       poisson_uni_n100$df_final,
                       bernoulli_uni_n100$df_final,
                       
                       normal_uni_n250$df_final,
                       poisson_uni_n250$df_final,
                       bernoulli_uni_n250$df_final,
                       
                       normal_uni_n500$df_final,
                       poisson_uni_n500$df_final,
                       bernoulli_uni_n500$df_final,
                       
                       normal_uni_n1000$df_final,
                       poisson_uni_n1000$df_final,
                       bernoulli_uni_n1000$df_final,
                       
                       normal_tri_n50$df_final,
                       poisson_tri_n50$df_final,
                       bernoulli_tri_n50$df_final,
                       
                       normal_tri_n100$df_final,
                       poisson_tri_n100$df_final,
                       bernoulli_tri_n100$df_final,
                       
                       normal_tri_n250$df_final,
                       poisson_tri_n250$df_final,
                       bernoulli_tri_n250$df_final,
                       
                       normal_tri_n500$df_final,
                       poisson_tri_n500$df_final,
                       bernoulli_tri_n500$df_final,
                       
                       normal_tri_n1000$df_final,
                       poisson_tri_n1000$df_final,
                       bernoulli_tri_n1000$df_final
)

#----------------------------------------------------------------------

results_betas$sample_size <- as.factor(results_betas$sample_size)

names(results_betas)[4] <- "Sample size"

results_betas$distribution <- 
  factor(results_betas$distribution,
         levels = c("uni normal", "uni poisson", 
                    "uni bernoulli", "tri normal", 
                    "tri poisson", "tri bernoulli"))

levels(results_betas$distribution) <- c("Univariate Normal",
                                        "Univariate Poisson",
                                        "Univariate Bernoulli",
                                        "Trivariate Normal",
                                        "Trivariate Poisson",
                                        "Trivariate Bernoulli")

#----------------------------------------------------------------------

figure2 <- ggplot(data = results_betas,
                  mapping = aes(x = dist, 
                                y = rej, 
                                #col = `Sample size`,
                                shape = `Sample size`
                  )) + 
  geom_point() + 
  geom_line(aes(linetype=`Sample size`)) + 
  facet_wrap(~distribution)+
  ylab("Rejection (%)") +
  xlab("Distance") +
  ggtitle("")+ 
  theme_bw() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(family = "Helvetica", 
                                  face = "bold", 
                                  size = (15),
                                  hjust = 0.5),
        axis.title = element_text(face = "bold",
                                  size = 10),
        text = element_text(size=10)) 

#----------------------------------------------------------------------

ggsave(filename='FIGURE2.tiff', 
       plot=figure2, device="tiff", 
       path=getwd(),
       dpi=800, 
       height = 6, 
       width = 8)

ggsave(filename='FIGURE2.eps', 
       plot=figure2, device="eps", 
       path=getwd(),
       dpi=800, 
       height = 6, 
       width = 8)

#----------------------------------------------------------------------

# Simulation study results for regression paramters

load("simulation-results-taus.RData")

#----------------------------------------------------------------------

results_taus <- rbind(normal_uni_n50$df_final,
                       poisson_uni_n50$df_final,
                       bernoulli_uni_n50$df_final,
                       
                       normal_uni_n100$df_final,
                       poisson_uni_n100$df_final,
                       bernoulli_uni_n100$df_final,
                       
                       normal_uni_n250$df_final,
                       poisson_uni_n250$df_final,
                       bernoulli_uni_n250$df_final,
                       
                       normal_uni_n500$df_final,
                       poisson_uni_n500$df_final,
                       bernoulli_uni_n500$df_final,
                       
                       normal_uni_n1000$df_final,
                       poisson_uni_n1000$df_final,
                       bernoulli_uni_n1000$df_final,
                       
                       normal_tri_n50$df_final,
                       poisson_tri_n50$df_final,
                       bernoulli_tri_n50$df_final,
                       
                       normal_tri_n100$df_final,
                       poisson_tri_n100$df_final,
                       bernoulli_tri_n100$df_final,
                       
                       normal_tri_n250$df_final,
                       poisson_tri_n250$df_final,
                       bernoulli_tri_n250$df_final,
                       
                       normal_tri_n500$df_final,
                       poisson_tri_n500$df_final,
                       bernoulli_tri_n500$df_final,
                       
                       normal_tri_n1000$df_final,
                       poisson_tri_n1000$df_final,
                       bernoulli_tri_n1000$df_final
)

#----------------------------------------------------------------------

results_taus$sample_size <- as.factor(results_taus$sample_size)

names(results_taus)[4] <- "Sample size"

results_taus$distribution <- 
  factor(results_taus$distribution,
         levels = c("uni normal", "uni poisson", 
                    "uni bernoulli", "tri normal", 
                    "tri poisson", "tri bernoulli"))

levels(results_taus$distribution) <- c("Univariate Normal",
                                        "Univariate Poisson",
                                        "Univariate Bernoulli",
                                        "Trivariate Normal",
                                        "Trivariate Poisson",
                                        "Trivariate Bernoulli")

#----------------------------------------------------------------------

figure3 <- ggplot(data = results_taus,
                  mapping = aes(x = dist, 
                                y = rej, 
                                #col = `Sample size`,
                                shape = `Sample size`
                  )) + 
  geom_point() + 
  geom_line(aes(linetype=`Sample size`)) + 
  facet_wrap(~distribution)+
  ylab("Rejection (%)") +
  xlab("Distance") +
  ggtitle("")+ 
  theme_bw() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(family = "Helvetica", 
                                  face = "bold", 
                                  size = (15),
                                  hjust = 0.5),
        axis.title = element_text(face = "bold",
                                  size = 10),
        text = element_text(size=10)) 

#----------------------------------------------------------------------

ggsave(filename='FIGURE3.tiff', 
       plot=figure3, device="tiff", 
       path=getwd(),
       dpi=800, 
       height = 6, 
       width = 8)

ggsave(filename='FIGURE3.eps', 
       plot=figure3, device="eps", 
       path=getwd(),
       dpi=800, 
       height = 6, 
       width = 8)

#----------------------------------------------------------------------