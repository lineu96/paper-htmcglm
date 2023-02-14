
########################################################################

# R script to generate the main results of the article 
# entitled ------------
# published in ------------

# Author
# Lineu Alberto Cavazani de Freitas (lineuacf@gmail.com)

########################################################################

# Packages

library(mcglm)
library(Matrix)
library(tidyverse)

#----------------------------------------------------------------------

# Data

load("data.RData")

# id - patient identifier factor
# grupo - group identifying factor (Placebo, Probiotic)
# moment - moment identifier factor (T0, T1, T2)
# YFAS - number of symptoms that characterize addiction (0-7)
# BES - score that characterizes compulsion (0-46)
# YFAS_unit - proportion of symptoms that characterize addiction
# BES_unit - proportion of the score that characterizes compulsion

#----------------------------------------------------------------------

# Exploratory data analysis

#----------------------------------------------------------------------

## Table 1

table1 <- data[,c('group','moment',
                  'YFAS_unit','BES_unit')] %>%
  group_by(group, moment) %>%
  summarise(n_YFAS = length(YFAS_unit),
            mean_YFAS = round(mean(YFAS_unit),2),
            sd_YFAS = round(sd(YFAS_unit),2),
            n_BES = length(BES_unit),
            mean_BES = round(mean(BES_unit),2), 
            sd_BES = round(sd(BES_unit),2))

table1

#----------------------------------------------------------------------

## Figure 1

a <- ggplot(data, aes(x = YFAS_unit)) +
  geom_histogram(col = 1, fill='white',
                 breaks = hist(data$YFAS_unit, 
                               plot = F)$breaks) +
  xlab('YFAS') +
  ylab('Frequency') +
  xlim(c(0,1))+
  theme_bw() +
  ggtitle('a')

b <- ggplot(data = data, 
            mapping = aes_string(x='group', y='YFAS_unit')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun=mean, 
               geom="point", 
               shape=8, 
               size=2)+
  xlab('Group') + 
  ylab('YFAS') + 
  ylim(c(0,1))+
  ggtitle('b')

c <- ggplot(data = data, 
            mapping = aes_string(x='moment', y='YFAS_unit')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun=mean, 
               geom="point", 
               shape=8, 
               size=2)+
  xlab('Moment') + 
  ylab('YFAS') + 
  ylim(c(0,1))+
  ggtitle('c')

d <- ggplot(data, aes(x = BES_unit)) +
  geom_histogram(col = 1, fill='white',
                 breaks = hist(data$BES_unit, 
                               plot = F)$breaks) +
  xlab('BES') +
  ylab('Frequency') +
  xlim(c(0,1))+
  theme_bw() +
  ggtitle('d')

e <- ggplot(data = data, 
            mapping = aes_string(x='group', y='BES_unit')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun=mean, 
               geom="point", 
               shape=8, 
               size=2)+
  xlab('Group') +
  ylab('BES') + 
  ylim(c(0,1))+
  ggtitle('e')

f <- ggplot(data = data, 
            mapping = aes_string(x='moment', y='BES_unit')) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(alpha = 1)+
  theme_light() +
  stat_summary(fun=mean, 
               geom="point", 
               shape=8, 
               size=2)+
  xlab('Moment') + 
  ylab('BES') + 
  ylim(c(0,1))+
  ggtitle('f')

g <- ggpubr::ggarrange(a,b,c,d,e,f,
                       nrow = 2, ncol = 3)

#----------------------------------------------------------------------

ggsave(filename='FIGURE1.jpeg',
       plot=g, device="jpeg",
       path=getwd(),
       dpi=300,
       height = 6,
       width = 8)

# ggsave(filename='FIGURE1.eps', 
#        plot=g, device="eps", 
#        path=getwd(),
#        dpi=300, 
#        height = 6, 
#        width = 8)

#----------------------------------------------------------------------

# Model

#----------------------------------------------------------------------

## Linear Predictor

pred_YFAS <- YFAS_unit ~ moment + group + moment:group
pred_BES <- BES_unit ~ moment + group + moment:group

#----------------------------------------------------------------------

## Matrix linear predictor

Z0 <- mc_id(data) # Identity
Z1 <- mc_mixed(~0 + factor(id), data = data) # Subject

#----------------------------------------------------------------------

## Model

fit <- 
  mcglm(linear_pred = c(pred_YFAS,
                        pred_BES),
        matrix_pred = list(c(Z0,Z1),
                           c(Z0,Z1)),
        link = c("logit", "logit"),
        variance = c("binomialP", "binomialP"), 
        control_algorithm = list(verbose = T, 
                                 tuning = 0.1,
                                 max_iter = 110,
                                 tol = 1e-4),
        power_fixed = c(F,F),
        data = data)

#----------------------------------------------------------------------

## Elements for table 3

### Default summary
summary <- summary(fit)

### Regression
betas_YFAS <- round(summary[[1]]$Regression, 4)[,c(1,4)]
betas_BES <- round(summary[[2]]$Regression, 4)[,c(1,4)]

### Dispersion
round(summary$`Resp.Variable 1`$Power,4)[,c(1,4)]
round(summary$`Resp.Variable 1`$tau,4)[,c(1,4)]
round(summary$`Resp.Variable 2`$Power,4)[,c(1,4)]
round(summary$`Resp.Variable 2`$tau,4)[,c(1,4)]

### Confidence intervals
round(confint(fit), 2)

#----------------------------------------------------------------------

# Residuals

#----------------------------------------------------------------------

## Obtaining the residuals

### chol(vcov) inverse
chol_inv <- Matrix::chol(fit$inv_C)

### Stacked residuals
residuals <- as.numeric(residuals(fit, type = 'raw'))

### Matrix product
pearson <- as.numeric(chol_inv%*%residuals)

### Fitted
fitted <- fit$fitted

### Dataframe
res_pred <- data.frame(index = rep(1:nrow(data),2),
                       resp = c(rep('YFAS', nrow(data)),
                                rep('BES', nrow(data))),
                       observado = c(data$YFAS_unit,
                                     data$BES_unit),
                       fitted = fitted,
                       pearson = pearson,
                       raw = residuals
)

### Figure 4

a <-ggplot(data = subset(res_pred, resp == 'YFAS'), 
           aes(x=pearson))+
  geom_histogram(col = 1, fill='white',
                 breaks = hist(res_pred$pearson, 
                               plot = F)$breaks) +
  theme_bw()+
  xlab('Residuals')+
  ylab('Frequency')+
  ggtitle('Pearson Residual for YFAS')

b <- ggplot(data = subset(res_pred, resp == 'BES'), 
            aes(x=pearson))+
  geom_histogram(col = 1, fill='white',
                 breaks = hist(res_pred$pearson, 
                               plot = F)$breaks) +
  theme_bw()+
  xlab('Residuals')+
  ylab('Frequency')+
  ggtitle('Pearson Residual for BES')


g <- ggpubr::ggarrange(a,b,
                       nrow = 1, ncol = 2)

#----------------------------------------------------------------------

ggsave(filename='FIGURE4.jpeg',
       plot=g, device="jpeg",
       path=getwd(),
       dpi=300,
       height = 3,
       width = 7)

# ggsave(filename='FIGURE4.eps', 
#        plot=g, device="eps", 
#        path=getwd(),
#        dpi=300, 
#        height = 3, 
#        width = 7)

#----------------------------------------------------------------------

## Figure 5

a <- ggplot(data = subset(res_pred, resp == 'YFAS'), 
            aes(y=pearson,x=fitted))+
  geom_jitter()+
  theme_bw()+
  geom_smooth(col= 1, method = 'loess', se=F)+
  xlab('Fitted')+
  ylab('Residuals') + 
  ggtitle('Residuals x Fitted for YFAS')

b <- ggplot(data = subset(res_pred, resp == 'BES'), 
            aes(y=pearson,x=fitted))+
  geom_jitter()+
  theme_bw()+
  geom_smooth(col=1, method = 'loess', se=F)+
  xlab('Fitted')+
  ylab('Residuals') + 
  ggtitle('Residuals x Fitted for BES')

g <- ggpubr::ggarrange(a,b,
                       nrow = 1, ncol = 2)

#----------------------------------------------------------------------

ggsave(filename='FIGURE5.jpeg',
       plot=g, device="jpeg",
       path=getwd(),
       dpi=300,
       height = 3,
       width = 7)

# ggsave(filename='FIGURE5.eps', 
#        plot=g, device="eps", 
#        path=getwd(),
#        dpi=300, 
#        height = 3, 
#        width = 7)

#----------------------------------------------------------------------

# Fitted values

#----------------------------------------------------------------------

## Table for fitted values

table <- expand.grid(Group = levels(data$group),
                     Moment = levels(data$moment))

# YFAS

table$lin_pred_YFAS <- 
  
  betas_YFAS$Estimate[1] +
  
  betas_YFAS$Estimate[2]*I(table$Moment==levels(data$moment)[2])+
  betas_YFAS$Estimate[3]*I(table$Moment==levels(data$moment)[3])+
  betas_YFAS$Estimate[4]*I(table$Group==levels(data$group)[2])+
  
  betas_YFAS$Estimate[5]*I(table$Moment==levels(data$moment)[2])*I(table$Group==levels(data$group)[2])+
  betas_YFAS$Estimate[6]*I(table$Moment==levels(data$moment)[3])*I(table$Group==levels(data$group)[2])

table$mean_pred_YFAS <- round(1/(1+exp(-table$lin_pred_YFAS)),2)
table$lin_pred_YFAS <- round(table$lin_pred_YFAS,2)

# BES

table$lin_pred_BES <- 
  
  betas_BES$Estimate[1] +
  
  betas_BES$Estimate[2]*I(table$Moment==levels(data$moment)[2])+
  betas_BES$Estimate[3]*I(table$Moment==levels(data$moment)[3])+
  betas_BES$Estimate[4]*I(table$Group==levels(data$group)[2])+
  
  betas_BES$Estimate[5]*I(table$Moment==levels(data$moment)[2])*I(table$Group==levels(data$group)[2])+
  betas_BES$Estimate[6]*I(table$Moment==levels(data$moment)[3])*I(table$Group==levels(data$group)[2])

table$mean_pred_BES <- round(1/(1+exp(-table$lin_pred_BES)),2)
table$lin_pred_BES <- round(table$lin_pred_BES,2)


table <- table[,c(1,2,4,6)]
names(table) <- c('Group', 'Moment', 'Fitted YFAS', 'Fitted BES')

table

#----------------------------------------------------------------------

## Figure 6

table_plot <- data.frame(Group = rep(table$Group,2),
                         Moment = rep(table$Moment,2),
                         Metric = c(rep('YFAS',6),
                                    rep('BES',6)),
                         Fitted = c(table$`Fitted YFAS`, 
                                    table$`Fitted BES`))

a <- ggplot(subset(table_plot, Metric == 'YFAS'), 
            aes(x=Moment, 
                y=Fitted, 
                group = Group))+ 
  theme_bw() + 
  geom_line(aes(group=Group, linetype=Group)) +
  theme(legend.position = 'bottom') + 
  xlab('Moment') + 
  ylab('Estimate')+ 
  ggtitle("Fitted for YFAS")+
  geom_point()

b <- ggplot(subset(table_plot, Metric == 'BES'), 
            aes(x=Moment, 
                y=Fitted, 
                group = Group))+ 
  theme_bw() + 
  geom_line(aes(group=Group, linetype=Group)) +
  theme(legend.position = 'bottom') + 
  xlab('Moment') + 
  ylab('Estimate')+ 
  ggtitle("Fitted for BES")+
  geom_point()

g <- ggpubr::ggarrange(a,b,
                       nrow = 1, ncol = 2,
                       common.legend = T,
                       legend = 'bottom')

#----------------------------------------------------------------------

ggsave(filename='FIGURE6.jpeg',
       plot=g, device="jpeg",
       path=getwd(),
       dpi=300,
       height = 4,
       width = 7)

# ggsave(filename='FIGURE6.eps', 
#        plot=g, device="eps", 
#        path=getwd(),
#        dpi=300, 
#        height = 4, 
#        width = 7)

#----------------------------------------------------------------------

# Hypothesis tests

#----------------------------------------------------------------------

#library(devtools)
#install_github("lineu96/htmcglm")
library(htmcglm)

#----------------------------------------------------------------------

## TABLE 4
mc_manova_II(fit)

## TABLE 5
mc_anova_II(fit)

## TABLE 6
mc_mult_multcomp(object = fit, 
                 effect = c('moment'), 
                 data = data) 

## TABLE 7
mc_mult_multcomp(object = fit, 
                 effect = c('moment', 'group'), 
                 data = data)

## TABLE 8
mc_manova_dispersion(fit,
                     p_var = c(0,1),
                     names = c('tau11', 'tau21')) 

#----------------------------------------------------------------------
