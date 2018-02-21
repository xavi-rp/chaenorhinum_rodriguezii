#source("/Users/xavi/Dropbox/chaenorhinum_rodriguezii/data/pes_tipus_llav_revisio2016.r")

###########################################################################################
######       CHAENORHINUM RODRIGUEZII         #############################################
######                                        #############################################
######         Weight seed types              #############################################
###########################################################################################

# chaenorhinum.R

# Created on: 2017-2018

# Contact: Xavier Rotllan-Puig (xavi@rotllanpuig.cat)

# Description: The aim of this script is to explore the differences in weight between the 
# three types of C. rodriguezii seeds found in the experiments of its breeding system 
# Types of seeds: 1 = white, 2 = black, 3 = deformed


# ------------------------------------------------------------------------------------------

#### Used Packages ####
if(require(xlsx)==FALSE){install.packages("xlsx", repos = "https://cloud.r-project.org"); library(xlsx)
} else {library(xlsx)}

if(require(car)==FALSE){install.packages("car", repos = "https://cloud.r-project.org"); library(car)
} else {library(car)}

#if(require(pgirmess)==FALSE){install.packages("pgirmess", repos = "https://cloud.r-project.org"); library(pgirmess)
#} else {library(pgirmess)}

if(require(PMCMR)==FALSE){install.packages("PMCMR", repos = "https://cloud.r-project.org"); library(PMCMR)
} else {library(PMCMR)}

#if(require(knitr)==FALSE){install.packages("knitr", repos = "https://cloud.r-project.org"); library(knitr)
#} else {library(knitr)}

#if(require(rmarkdown)==FALSE){install.packages("rmarkdown", repos = "https://cloud.r-project.org"); library(rmarkdown)
#} else {library(rmarkdown)}


if(Sys.info()[4] == "MacBook-Pro-de-Xavier.local"){ #checks machine name
  path <- "/Users/xavi/Dropbox/chaenorhinum_rodriguezii/data/"
  #}else if(Sys.info()[4] == ""){ # to add any other machine name
}

chaenos_DF <- read.xlsx(paste0(path, "bio_rep_final_1216.xlsx"), sheetName="biorep_desembre16")
head(chaenos_DF)
str(chaenos_DF)



#### 2011 - 2012 data ####

ch_11_12 <- chaenos_DF

# types of seeds: 1 = white, 2 = black, 3 = deformed
ch_11_12_w <- ch_11_12[ !is.na(ch_11_12$Pes.100.llavors.blanques..mg.) , c(1, 12)] # removing NA
ch_11_12_w <- ch_11_12_w[ ch_11_12_w$Pes.100.llavors.blanques..mg. != 0 , ]  #removing rows with 0
ch_11_12_w$ID <- 1   # giving number 1 to white seeds
names(ch_11_12_w) <- c("type", "weight")  # changing column names
ch_11_12_w

ch_11_12_b <- ch_11_12[ !is.na(ch_11_12$Pes.100.llavors.negres..mg.) , c(1, 15)]
ch_11_12_b <- ch_11_12_b[ ch_11_12_b$Pes.100.llavors.negres..mg. != 0 , ]
ch_11_12_b$ID <- 2
names(ch_11_12_b) <- c("type", "weight")
nrow(ch_11_12_b)
ch_11_12_b

ch_11_12_d <- ch_11_12[ !is.na(ch_11_12$Pes.100.llavors.deformades..mg.) , c(1, 18)]
ch_11_12_d <- ch_11_12_d[ ch_11_12_d$Pes.100.llavors.deformades..mg. != 0 , ]
ch_11_12_d$ID <- 3
names(ch_11_12_d) <- c("type", "weight")
nrow(ch_11_12_d)

weight_seeds <- rbind(ch_11_12_w, ch_11_12_b, ch_11_12_d)
weight_seeds$type <- as.factor(weight_seeds$type)
weight_seeds
nrow(weight_seeds)

pdf(paste0(path, "outputs2017/boxPlot_weight_seeds_2011_12.pdf"))
boxplot(weight ~ type, data = weight_seeds, xlab = "type of seeds: 1 = white, 2 = black, 3 = deformed", ylab = "weight of 100 seeds in g")
# types: 1 = white, 2 = black, 3 = deformed
dev.off()


# Are there significant differences between the three groups?

#Shapiro-Wilk to check for Normality of each type
sh_w <- shapiro.test(ch_11_12_w$weight)
sh_w     # W = 0.8852, p-value = 0.06895  <-- normality
sh_b <- shapiro.test(ch_11_12_b$weight)
sh_b     # W = 0.97632, p-value = 0.2463  <-- normality
sh_d <- shapiro.test(ch_11_12_d$weight)
sh_d     # W = 0.90902, p-value = 0.1778  <-- normality

# Test for homoscedasticity
lev <- leveneTest(weight ~ type, data = weight_seeds)
lev    # Not equal variances (F = 0.9098, p = 0.4063) <-- homoscedasticity

# Fitting an ANOVA (Probably would be better using a mixed model with random effects: population and year)
aov_weight_seeds <- aov(weight ~ type, data = weight_seeds)
summary(aov_weight_seeds) # There are significant differences between the 3 types of seeds (F = 71.26, p < 2e-16)
print(summary(aov_weight_seeds))

pdf(paste0(path, "outputs2017/aov_weight_seeds_2011_12.pdf"))
par(mfrow=c(2,2))
plot(aov_weight_seeds)
dev.off()


# Let's see which differences exist between groups are significant
# Using TukeyHSD test (parametric test)

tukey <- TukeyHSD(aov_weight_seeds)
tukey   # There are significant differences

#        diff        lwr        upr    p adj
# 2-1  1.520344  0.9121779  2.1285110 1.00e-07
# 3-1 -1.430306 -2.2253223 -0.6352903 1.34e-04
# 3-2 -2.950651 -3.5777678 -2.3235336 0.00e+00

## CONCLUSION: All types of seed are different. When studying the breeding system, we should use only
#  black seeds for weight, but we can use together black and white for number of seed (think about using 
#  deformed or not). 
#  We could see if there are some differences in number of deformed seeds in the Autogamia treatment vs Control




#### Only with 2011 data ####

ch_2011 <- chaenos_DF[chaenos_DF$any == 2011, ] #selecting 2011 data
ch_2011 <- droplevels(ch_2011) 
nrow(ch_2011)
head(ch_2011)
str(ch_2011)
unique(ch_2011$poblacio)  #to confirm that in 2011 we only have data from Puig Major

# types of seeds: 1 = white, 2 = black, 3 = deformed
ch_2011_w <- ch_2011[ !is.na(ch_2011$Pes.100.llavors.blanques..mg.) , c(1, 12)] # removing NA
ch_2011_w <- ch_2011_w[ ch_2011_w$Pes.100.llavors.blanques..mg. != 0 , ]  #removing rows with 0
ch_2011_w$ID <- 1   # giving number 1 to white seeds
names(ch_2011_w) <- c("type", "weight")  # changing column names
ch_2011_w

ch_2011_b <- ch_2011[ !is.na(ch_2011$Pes.100.llavors.negres..mg.) , c(1, 15)]
ch_2011_b <- ch_2011_b[ ch_2011_b$Pes.100.llavors.negres..mg. != 0 , ]
ch_2011_b$ID <- 2
names(ch_2011_b) <- c("type", "weight")
nrow(ch_2011_b)
ch_2011_b

ch_2011_d <- ch_2011[ !is.na(ch_2011$Pes.100.llavors.deformades..mg.) , c(1, 18)]
ch_2011_d <- ch_2011_d[ ch_2011_d$Pes.100.llavors.deformades..mg. != 0 , ]
ch_2011_d$ID <- 3
names(ch_2011_d) <- c("type", "weight")
nrow(ch_2011_d)

weight_seeds_2011 <- rbind(ch_2011_w, ch_2011_b, ch_2011_d)
weight_seeds_2011$type <- as.factor(weight_seeds_2011$type)
weight_seeds_2011
nrow(weight_seeds_2011)

pdf(paste0(path, "outputs2017/boxPlot_weight_seeds_2011.pdf"))
boxplot(weight ~ type, data = weight_seeds_2011, xlab = "type of seeds: 1 = white, 2 = black, 3 = deformed", ylab = "weight of 100 seeds in g")
# types: 1 = white, 2 = black, 3 = deformed
dev.off()


# Are there significant differences between the three groups?

#Shapiro-Wilk to check for Normality of each type
sh_w <- shapiro.test(ch_2011_w$weight)
sh_w     # W = 0.95362, p-value = 0.7695  <-- normality
sh_b <- shapiro.test(ch_2011_b$weight)
sh_b     # W = 0.95392, p-value = 0.3287  <-- normality
sh_d <- shapiro.test(ch_2011_d$weight)
sh_d     # W = 0.90902, p-value = 0.1778  <-- normality

# Test for homoscedasticity
lev <- leveneTest(weight ~ type, data = weight_seeds_2011)
lev    # Not equal variances (p = 0.01559) <-- heteroscedasticity <-- should use a non parametrical

# A rule of thumb is that linear models are fairly robust to heterogeneity of variance so long as the maximum variance is no more than 4× greater than the minimum variance
var_w <- var(ch_2011_w$weight)
var_b <- var(ch_2011_b$weight)
var_d <- var(ch_2011_d$weight)
variances <- c(var_w, var_b, var_d)
max(variances)/min(variances)  # almost 9... the difference is too big


# Kruskal-Wallis test (non parametric test for ANOVA). Less power than transformations of the variables, but the interpretation is easyer if no transformations are done 
kt <- kruskal.test(weight ~ type, weight_seeds_2011)
kt   # chi-squared = 25.102, df = 2, p-value = 3.541e-06
# There are significant differences between the three groups

# We can easily chek the results with the parametric ANOVA
aov_weight_seeds_2011 <- aov(weight ~ type, data = weight_seeds_2011)
summary(aov_weight_seeds_2011) # Same results, there are significant differences between the 3 types of seeds (p = 4.77e-12)

pdf(paste0(path, "outputs2017/aov_weight_seeds_2011.pdf"))
par(mfrow=c(2,2))
plot(aov_weight_seeds_2011)
dev.off()


# Let's see which differences exist between groups are significant
# Using a non-parametrical test after Kruskal-Wallis test (TukeyHSD is the parametric test)
# Dunn uses the same ranks used in Kruskal-Wallis. It's appropriate for groups with unequal numbers of observations
# Bonferroni correction is more conservative regarding TypeI errors (false positives)
krus_dunn <- posthoc.kruskal.dunn.test(weight_seeds_2011$weight ~ weight_seeds_2011$type, p.adjust.method = "bonferroni")
# Differences only in 2-3 (black-deformed). So white shows no difference neither with black nor with deformed
krus_dunn

#Another option (non-parametric) but I would use Dunn's test
krus_conover <- posthoc.kruskal.conover.test(weight_seeds_2011$weight ~ weight_seeds_2011$type, p.adjust.method = "bonferroni")
# differences between white-deformed and black-deformed
# no differences black-white
krus_conover

# Checking the parametric option
tukey <- TukeyHSD(aov_weight_seeds_2011)
tukey

## CONCLUSION: If we are strict with the statistics, we should use non-parametric tests.
#  If so, there are only significant differences between black and deformed seeds. 
#  This would mean that the white ones could be used together with the black ones for the
#  study. 



#### Only with 2012 data ####

ch_2012 <- chaenos_DF[chaenos_DF$any == 2012, ] #selecting 2011 data
ch_2012 <- droplevels(ch_2012) 
nrow(ch_2012)
head(ch_2012)
str(ch_2012)
unique(ch_2012$poblacio)  #to confirm that in 2011 we only have data from Puig Major

# types of seeds: 1 = white, 2 = black, 3 = deformed
ch_2012_w <- ch_2012[ !is.na(ch_2012$Pes.100.llavors.blanques..mg.) , c(1, 12)] # removing NA
ch_2012_w <- ch_2012_w[ ch_2012_w$Pes.100.llavors.blanques..mg. != 0 , ]  #removing rows with 0
ch_2012_w$ID <- 1   # giving number 1 to white seeds
names(ch_2012_w) <- c("type", "weight")  # changing column names
ch_2012_w

ch_2012_b <- ch_2012[ !is.na(ch_2012$Pes.100.llavors.negres..mg.) , c(1, 15)]
ch_2012_b <- ch_2012_b[ ch_2012_b$Pes.100.llavors.negres..mg. != 0 , ]
ch_2012_b$ID <- 2
names(ch_2012_b) <- c("type", "weight")
nrow(ch_2012_b)
ch_2012_b

ch_2012_d <- ch_2012[ !is.na(ch_2012$Pes.100.llavors.deformades..mg.) , c(1, 18)]
ch_2012_d <- ch_2012_d[ ch_2012_d$Pes.100.llavors.deformades..mg. != 0 , ]
ch_2012_d$ID <- 3
names(ch_2012_d) <- c("type", "weight")
nrow(ch_2012_d)

weight_seeds_2012 <- rbind(ch_2012_w, ch_2012_b, ch_2012_d)
weight_seeds_2012$type <- as.factor(weight_seeds_2012$type)
weight_seeds_2012
nrow(weight_seeds_2012)

#pdf(paste0(path, "outputs2017/boxPlot_weight_seeds_2012.pdf"))
boxplot(weight ~ type, data = weight_seeds_2012, xlab = "type of seeds: 1 = white, 2 = black, 3 = deformed", ylab = "weight of 100 seeds in g")
# types: 1 = white, 2 = black, 3 = deformed
#dev.off()


# Are there significant differences between the three groups?

#Shapiro-Wilk to check for Normality of each type
sh_w <- shapiro.test(ch_2012_w$weight)
sh_w     # W = 0.6896, p-value = 0.001717  <-- No normality
sh_b <- shapiro.test(ch_2012_b$weight)
sh_b     # W = W = 0.96571, p-value = 0.248  <-- normality

# Test for homoscedasticity
lev <- leveneTest(weight ~ type, data = weight_seeds_2012)
lev    # Not equal variances (p = 0.09923) <-- Not heteroscedasticity

# A rule of thumb is that linear models are fairly robust to heterogeneity of variance so long as the maximum variance is no more than 4× greater than the minimum variance
var_w <- var(ch_2012_w$weight)
var_b <- var(ch_2012_b$weight)
var_d <- var(ch_2012_d$weight)
variances <- c(var_w, var_b, var_d)
max(variances, na.rm = TRUE)/min(variances, na.rm = TRUE)  # almost 9... the difference is too big


# Mann-Whitney U Test (non parametric test for t-test). 
wt <- wilcox.test(weight ~ type, weight_seeds_2012)
wt   # W = 21.5, p-value = 0.0001223 
# There are significant differences between the two groups

# We can easily chek the results with the parametric t-test
tt <- t.test(weight ~ type, data = weight_seeds_2012)
tt   # t = -5.7256, df = 10.537, p-value = 0.0001569

## CONCLUSION: There are significant differences between black and white seeds. 



#### Session Info ####
sessionInfo()

#R version 3.3.2 (2016-10-31)
#Platform: x86_64-apple-darwin13.4.0 (64-bit)
#Running under: macOS Sierra 10.12.2
#locale:
# [1] ca_ES.UTF-8/ca_ES.UTF-8/ca_ES.UTF-8/C/ca_ES.UTF-8/ca_ES.UTF-8
#attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
#other attached packages:
# [1] PMCMR_4.1      car_2.1-4      xlsx_0.5.7     xlsxjars_0.6.1 rJava_0.9-8   
#loaded via a namespace (and not attached):
# [1] Rcpp_0.12.8        lattice_0.20-34    MASS_7.3-45        grid_3.3.2         MatrixModels_0.4-1
# [6] nlme_3.1-128       SparseM_1.74       minqa_1.2.4        nloptr_1.0.4       Matrix_1.2-7.1    
# [11] splines_3.3.2      lme4_1.1-12        tools_3.3.2        pbkrtest_0.4-6     parallel_3.3.2    
# [16] mgcv_1.8-15        nnet_7.3-12        quantreg_5.29     
 
