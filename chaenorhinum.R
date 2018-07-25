
###########################################################################################
######       CHAENORHINUM RODRIGUEZII         #############################################
######                                        #############################################
######         Weight seed types              #############################################
###########################################################################################

# chaenorhinum.R
#
# Created on: 2017-2018
#
# Contact: Xavier Rotllan-Puig (xavi@rotllanpuig.cat)
#
# Description: The aim of this script is to explore the breeding system of C. rodriguezii. 
# Here we analyse the data obtained in field experiments (i.e. bagging) as well as other 
# data collected (i.e. floral visitors, pollen/ovule ratio)
#

# ------------------------------------------------------------------------------------------

#### Used Packages ####
if(require(xlsx)==FALSE){install.packages("xlsx", repos = "https://cloud.r-project.org"); library(xlsx)
} else {library(xlsx)}
if(require(dplyr)==FALSE){install.packages("dplyr", repos = "https://cloud.r-project.org"); library(dplyr)
} else {library(dplyr)}
if(require(car)==FALSE){install.packages("car", repos = "https://cloud.r-project.org"); library(car)
} else {library(car)}
#if(require(MASS)==FALSE){install.packages("MASS", repos = "https://cloud.r-project.org"); library(MASS)
#} else {library(MASS)}
#if(require(pgirmess)==FALSE){install.packages("pgirmess", repos = "https://cloud.r-project.org"); library(pgirmess)
#} else {library(pgirmess)}
if(require(PMCMR)==FALSE){install.packages("PMCMR", repos = "https://cloud.r-project.org"); library(PMCMR)
} else {library(PMCMR)}
if(require(lme4)==FALSE){install.packages("lme4", repos = "https://cloud.r-project.org"); library(lme4)
} else {library(lme4)}
#if(require(knitr)==FALSE){install.packages("knitr", repos = "https://cloud.r-project.org"); library(knitr)
#} else {library(knitr)}

#if(require(rmarkdown)==FALSE){install.packages("rmarkdown", repos = "https://cloud.r-project.org"); library(rmarkdown)
#} else {library(rmarkdown)}


if(Sys.info()[4] == "MacBook-Pro-de-Xavier.local"){ #checks machine name
  path <- "/Users/xavi/Dropbox/chaenorhinum_rodriguezii/data/"
  #}else if(Sys.info()[4] == ""){ # to add any other machine name
}

chaenos_DF <- read.xlsx(paste0(path, "/clean_data/reprod_biol.xlsx"), sheetName="breeding")
head(chaenos_DF)
str(chaenos_DF)

chaenos_DF[, 2:4] %>% group_by %>% summarise_all(count)

tble <- table(chaenos_DF$population, chaenos_DF$year, chaenos_DF$treatment)
ftable(tble)


#### Difference weight of seed types ####
# types of seeds: 1 = white, 2 = black, 3 = deformed

ch_11_17 <- chaenos_DF
names(ch_11_17)
#View(ch_11_17)

ch_11_17_w <- ch_11_17[ !is.na(ch_11_17$weight100_white) , c(2:3, 6)]   # selecting white seeds
ch_11_17_w$seed_type <- 1   # giving number 1 to white seeds
names(ch_11_17_w)[3] <- "weight100"
nrow(ch_11_17_w)

ch_11_17_b <- ch_11_17[ !is.na(ch_11_17$weight100_black) , c(2:3, 8)]
ch_11_17_b$seed_type <- 2
names(ch_11_17_b)[3] <- "weight100"
nrow(ch_11_17_b)
ch_11_17_b

ch_11_17_d <- ch_11_17[ !is.na(ch_11_17$weight100_deformed) , c(2:3, 10)]
ch_11_17_d$seed_type <- 3
names(ch_11_17_d)[3] <- "weight100"
nrow(ch_11_17_d)

weight_seeds <- rbind(ch_11_17_w, ch_11_17_b, ch_11_17_d)
weight_seeds$seed_type <- as.factor(weight_seeds$seed_type)
head(weight_seeds)
nrow(weight_seeds)
unique(weight_seeds$population)


## Removing outliers (probably due to an error when classifying as type 2 instead of 3)
weight_seeds[weight_seeds$year == "2017" & weight_seeds$weight100 < 2, ]
weight_seeds <- weight_seeds[!(weight_seeds$year == "2017" & weight_seeds$weight100 < 2), ]

table(weight_seeds[, 1:2])

pdf(paste0(path, "outputs2018/boxPlot_weight_seeds_2011-17.pdf"))
boxplot(weight100 ~ seed_type, data = weight_seeds, xlab = "type of seeds: 1 = white, 2 = black, 3 = deformed", ylab = "weight of 100 seeds in mg")
# types: 1 = white, 2 = black, 3 = deformed
dev.off()

pdf(paste0(path, "outputs2018/histogram_weight_seeds_2011-17.pdf"))
par(mfrow = c(2,2))
hist(weight_seeds$weight100, xlab = "weight of 100 seeds (mg)", main = "Seed Type = All")
for(tp in 1:3) {
  if (tp == 1) typ <- "White" else if (tp == 2) typ <- "Black" else typ <- "Deformed"
  hist(weight_seeds[weight_seeds$seed_type %in% tp, 3], #prob = TRUE,
       xlab = "weight of 100 seeds (mg)", main = paste0("Seed Type = ", typ))
  #lines(density(weight_seeds$weight100, adjust=2), col = "red")
}
dev.off()


## Are there significant differences between the three groups?
#Shapiro-Wilk to check for Normality of each type
sh_w <- shapiro.test(weight_seeds[weight_seeds$seed_type == 1, 3])
sh_w     # n = 26, W = 0.93284, p-value = 0.09057  <-- normality
sh_b <- shapiro.test(weight_seeds[weight_seeds$seed_type == 2, 3])
sh_b     # n = 79, W = 0.9697, p-value = 0.05695  <-- normality
sh_d <- shapiro.test(weight_seeds[weight_seeds$seed_type == 3, 3])
sh_d     # n = 14, W = 0.9161, p-value = 0.1932  <-- normality

# Test for homoscedasticity
lev <- leveneTest(weight100 ~ seed_type, data = weight_seeds)
lev    # Not equal variances (n = 119, F = 5.265, p = 0.006476 **) <-- NOT Homoscedasticity <-- we should use a non parametrical test

# However, a rule of thumb is that linear models are fairly robust to heterogeneity of variance so long as the maximum variance is no more than 4× greater than the minimum variance
var_w <- var(weight_seeds[weight_seeds$seed_type == 1, 3])
var_b <- var(weight_seeds[weight_seeds$seed_type == 2, 3])
var_d <- var(weight_seeds[weight_seeds$seed_type == 3, 3])
variances <- c(var_w, var_b, var_d)
max(variances)/min(variances)  # around 2.5, therefore a parametric test could be robust enough


# Fitting an ANOVA (Probably would be better using a mixed model with random effects: population and year)
aov_weight_seeds <- aov(weight100 ~ seed_type, data = weight_seeds)
summary(aov_weight_seeds) # There are significant differences between the 3 types of seeds (F = 61.55, p < 2e-16 ***)
print(summary(aov_weight_seeds))

pdf(paste0(path, "outputs2018/aov_weight_seeds_2011-17.pdf"))
par(mfrow=c(2,2))
plot(aov_weight_seeds)
dev.off()

# We can easily check a non-parametric test
# Kruskal-Wallis test (non parametric test for ANOVA). Less power than transformations of the variables, but the interpretation is easyer if no transformations are done 
kt <- kruskal.test(weight100 ~ seed_type, data = weight_seeds)
kt   # chi-squared = 41.03, df = 2, p-value = 1.231e-09
# There are significant differences between the three groups

# Let's see which differences exist between groups are significant
# Using TukeyHSD test (parametric test)

tukey <- TukeyHSD(aov_weight_seeds)
tukey   # There are significant differences

#        diff        lwr        upr    p adj
# 2-1  1.013583  0.3915998  1.635567 0.0005277
# 3-1 -2.665495 -3.5774301 -1.753560 0.0000000
# 3-2 -3.679079 -4.4767949 -2.881362 0.0000000

## CONCLUSION: All types of seed are different. When studying the breeding system, we should use only
#  black seeds for weight, but we can use together black and white for number of seed (think about using 
#  deformed or not). 
#  We could see if there are some differences in number of deformed seeds in the Autogamia treatment vs Control


# Using a non-parametrical test after Kruskal-Wallis test (TukeyHSD is the parametric test)
# Dunn uses the same ranks used in Kruskal-Wallis. It's appropriate for groups with unequal numbers of observations
# Bonferroni correction is more conservative regarding TypeI errors (false positives)
krus_dunn <- posthoc.kruskal.dunn.test(weight_seeds$weight100 ~ weight_seeds$seed_type, p.adjust.method = "bonferroni")
krus_dunn  # All types of seed are different.

#      1         2      
# 2  0.0078      -      
# 3  0.0028   2.7e-09

# Another option (non-parametric) but I would use Dunn's test
krus_conover <- posthoc.kruskal.conover.test(weight_seeds$weight100 ~ weight_seeds$seed_type, p.adjust.method = "bonferroni")
krus_conover  # All types of seed are different.



## Fitting a Mixed Model with random effects: population and year ##
# A fixed effect "exhausts the population of interest" -> "year" could be any, but we have only 3; thus, it is random
#                                                      -> "population" could also be more than Puig Major and Massanella; thus, also random
head(weight_seeds)
nrow(weight_seeds)
str(weight_seeds)
boxplot(weight100 ~ seed_type * population, data = weight_seeds, xlab = "type of seeds: 1 = white, 2 = black, 3 = deformed", ylab = "weight of 100 seeds in mg")


sh_test <- shapiro.test(weight_seeds$weight100)
sh_test   # n = 119, W = 0.95336, p-value = 0.000409  <-- Normally distributed
qqp(weight_seeds$weight100, "norm")

table(weight_seeds$seed_type) # my data is unbalanced (i.e. dissimilar sample sizes in each factor group)

mix_mod <- lmer(weight100 ~ seed_type + (1 | year) + (1 | population), 
                data = weight_seeds,
                REML = TRUE)   # REML = TRUE because my data is unbalanced (i.e. dissimilar sample sizes in each factor group)
summary(mix_mod)  

# Variance of year is 0.2154; if it was 0, it would make not sense including it
# Variance of population is 0

mix_mod <- lmer(weight100 ~ seed_type + (1 | year), 
                data = weight_seeds,
                REML = TRUE)   # REML = TRUE because my data is unbalanced (i.e. dissimilar sample sizes in each factor group)
summary(mix_mod)  

anova_mix_mod <- Anova(mix_mod) # if we need to get p-values

# From Estimate we see that type 2 (black) are 1.2924 mg heavier than type 1 (white) and type 1 are 2.3922 mg heavier than type 3 (deformed)
# p-value < 2.2e-16 ***   <-- types of seed are significantly different




#### Session Info ####
sessionInfo()

#R version 3.3.3 (2017-03-06)
#Platform: x86_64-apple-darwin13.4.0 (64-bit)
#Running under: macOS  10.13.1
#locale:
# [1] ca_ES.UTF-8/ca_ES.UTF-8/ca_ES.UTF-8/C/ca_ES.UTF-8/ca_ES.UTF-8
#attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
#other attached packages:
# [1] PMCMR_4.1      car_2.1-4      xlsx_0.5.7     xlsxjars_0.6.1 rJava_0.9-9   
#loaded via a namespace (and not attached):
# [1] Rcpp_0.12.14       lattice_0.20-34    MASS_7.3-45        grid_3.3.3         MatrixModels_0.4-1
# [6] nlme_3.1-131       SparseM_1.74       minqa_1.2.4        nloptr_1.0.4       Matrix_1.2-8    
# [11] splines_3.3.3      lme4_1.1-12        tools_3.3.3        parallel_3.3.3     pbkrtest_0.4-6    
# [16] yaml_2.1.14        mgcv_1.8-17        nnet_7.3-12        quantreg_5.29     
 
