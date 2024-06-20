setwd("//35.8.175.161/mnl/Data/Adaptation/structural_interference/manuscript/R/Routput")
load("//35.8.175.161/mnl/Data/Adaptation/structural_interference/manuscript/R/Routput/221012_strint_manuscript_WS.RData")
library(tidyverse)
library(afex)
library(rstatix)
time_exp = c(5,6)
time_pe = c(19,22)

# Try the base package -------
# Subset your data
subset_data <- subset(lhKinematics_stats, block %in% time_exp)

# Create a formula for the MANOVA
formula <- cbind(ide.bc, ede.bc, LFvp.bc, LFoff.bc) ~ group

# Run MANOVA
manova_result <- manova(formula, data = subset_data)

# Print MANOVA summary
summary(manova_result)

# Look to see which differ
summary.aov(manova_result)

# Check covariance matrices for each group
# Subset your data by group
group1_data <- subset(subset_data, group == "1")
group2_data <- subset(subset_data, group == "2")

# Calculate covariance matrices for each group
cov_matrix_group1 <- cov(group1_data[, c('ide.bc', 'ede.bc', 'EP_X.bc', 'LFvp.bc', 'LFoff.bc')])
cov_matrix_group2 <- cov(group2_data[, c('ide.bc', 'ede.bc', 'EP_X.bc', 'LFvp.bc', 'LFoff.bc')])

# Print the covariance matrices for each group
print(cov_matrix_group1)
print(cov_matrix_group2)

# Perform Levene's test for homogeneity of variance
subset_data %>% 
  gather(key = "variable", value = "value", ide.bc, ede.bc, EP_X.bc, LFvp.bc, LFoff.bc) %>%
  group_by(variable) %>%
  levene_test(value ~ group)


# Non-parametric MANOVA ------------
# Install and load the vegan package
#install.packages("vegan")
# https://search.r-project.org/CRAN/refmans/vegan/html/adonis.html
library(vegan)

# Subset your data
subset_data <- subset(lhKinematics_stats, block %in% time_exp)

# Create a formula for NP-MANOVA
formula <- cbind(ide.bc, ede.bc, EP_X.bc, LFvp.bc, LFoff.bc) ~ group * block
#formula <- cbind(ide.bc, ede.bc, EP_X.bc) ~ group * block # kinematics only
#formula <- cbind(LFvp.bc, LFoff.bc) ~ group * block # kinetics only

# Perform permutation-based NP-MANOVA
np_manova_result <- adonis(formula, data = subset_data, permutations = 999)

# Print NP-MANOVA results
print(np_manova_result$aov.tab)