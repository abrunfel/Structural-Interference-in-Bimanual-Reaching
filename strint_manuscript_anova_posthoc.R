# strint stats ------------
# Step 1: Load workspace (must include lhKinematics_stats, rhKinematics_stats, FC_3way, and FC_3way.ex)
#load("//Volumes//mnl//Data//Adaptation//structural_interference//manuscript//R//Routput//221012_strint_manuscript_WS.RData") # Load in data (make sure its the most up-to-date)
load('Z:\\Data\\Adaptation\\structural_interference\\manuscript\\R\\Routput\\221012_strint_manuscript_WS.RData')
# Step 2:: Load packages
library(tidyverse)
library(afex)
library(effsize)
library(emmeans)
# Step 4: Input 'time_*' values for which blocks to process as repeated measure (ie: beginning, middle, end = 5,12,18)
time_exp = c(5,12,18)
time_pe = c(19,22)

# Training - post exposure t.test -----------------------------------------
# 9/8/22 - The new workspace does not have training data.... I'll just avoid this for now (stats are unchanged)
# t.test(subset(trainData_mbb, trainData_mbb$block %in% c(65:68) & trainData_mbb$group %in% c(1))$ide.bc,
#        subset(trainData_mbb, trainData_mbb$block %in% c(65:68) & trainData_mbb$group %in% c(2))$ide.bc, paired = FALSE)
#

##### Testing ANOVAS #####
# Testing - ide and fbrmse rh ANOVA --------------------------------------------------
aov.rh.ide = aov_ez(subset(rhKinematics_stats, block %in% time_exp), id = 'subjectID', dv = 'ide.bc', between = 'group', within = 'block')
aov.rh.ide
aov.rh.ide$anova_table$`Pr(>F)`

aov.rh.rmse = aov_ez(subset(rhKinematics_stats, block %in% time_exp), id = 'subjectID', dv = 'fbrmse_c', between = 'group', within = 'block')
aov.rh.rmse
aov.rh.rmse$anova_table$`Pr(>F)`
# Post-hocs (you must correct for multiple comparisons after getting p value)
t.test(subset(rhKinematics_stats, block %in% c(18) & group %in% c(2))$ide.bc, 
       subset(rhKinematics_stats, block %in% c(18) & group %in% c(1))$ide.bc, paired = FALSE)
cohen.d(subset(rhKinematics_stats, block %in% c(5) & group %in% c(2))$ide.bc, 
       subset(rhKinematics_stats, block %in% c(5) & group %in% c(1))$ide.bc, paired = FALSE)

t.test(subset(rhKinematics_stats, block %in% c(18) & group %in% c(2))$fbrmse_c, 
       subset(rhKinematics_stats, block %in% c(18) & group %in% c(1))$fbrmse_c, paired = FALSE)
cohen.d(subset(rhKinematics_stats, block %in% c(5) & group %in% c(2))$fbrmse_c, 
       subset(rhKinematics_stats, block %in% c(5) & group %in% c(1))$fbrmse_c, paired = FALSE)

#rh.ide.aov = aov_ez(subset(rhKinematics_stats, block %in% time_exp), id = 'subjectID', dv = 'ide.bc', between = 'group', within = 'block')
#em.rh.ide = emmeans(rh.ide.aov$aov, specs = pairwise ~group:block)
#contrast(em.rh.ide, method = list()) # this is broken
#emmeans(rh.ide.aov$aov, specs = pairwise ~group:block, adjust = "tukey")
#emmeans(rh.ide.aov$aov, specs = pairwise ~group:block, adjust = "bonferroni")
#

# Testing - ide lh ANOVA --------------------------------------------------
aov.lh.ide = aov_ez(subset(lhKinematics_stats, block %in% time_exp), id = 'subjectID', dv = 'ide.bc', between = 'group', within = 'block')
aov.lh.ide
aov.lh.ide$anova_table$`Pr(>F)`
#

# Testing - ede lh ANOVA --------------------------------------------------
aov.lh.ede =  aov_ez(subset(lhKinematics_stats, block %in% time_exp), id = 'subjectID', dv = 'ede.bc', between = 'group', within = 'block')
aov.lh.ede
aov.lh.ede$anova_table$`Pr(>F)`
#

# Testing - epx lh ANOVA --------------------------------------------------
aov_ez(subset(lhKinematics_stats, block %in% time_exp), id = 'subjectID', dv = 'EP_X.bc', between = 'group', within = 'block')
#

# Testing - LFPV lh ANOVA --------------------------------------------------
aov.lh.lfpv =  aov_ez(subset(lhKinematics_stats, block %in% time_exp), id = 'subjectID', dv = 'LFvp.bc', between = 'group', within = 'block')
aov.lh.lfpv
aov.lh.lfpv$anova_table$`Pr(>F)`
#

# Testing - LFOFF lh ANOVA --------------------------------------------------
aov.lh.lfoff =  aov_ez(subset(lhKinematics_stats, block %in% time_exp), id = 'subjectID', dv = 'LFoff.bc', between = 'group', within = 'block')
aov.lh.lfoff
aov.lh.lfoff$anova_table$`Pr(>F)`
#

##### Testing t.test first 30 trials of exposure (unpaired t.tests) ######
# First need to compute the mean of each DV for the first 3 blocks (30 trials) of the exposure phase
early.ex.lh.mean = lhKinematics_stats %>%
        filter(block %in% c(5,6)) %>%
        group_by(subjectID, group) %>%
        summarise_all(.funs = mean, na.rm = T)
# Testing - ide lh t.test -------------------------------------------------
t.test(subset(early.ex.lh.mean, group %in% c(2))$ide.bc, 
       subset(early.ex.lh.mean, group %in% c(1))$ide.bc, paired = FALSE)
cohen.d(subset(early.ex.lh.mean, group %in% c(2))$ide.bc, 
       subset(early.ex.lh.mean, group %in% c(1))$ide.bc, paired = FALSE)
#

# Testing - ede lh t.test -------------------------------------------------
t.test(subset(early.ex.lh.mean, group %in% c(2))$ede.bc, 
       subset(early.ex.lh.mean, group %in% c(1))$ede.bc, paired = FALSE)
cohen.d(subset(early.ex.lh.mean, group %in% c(2))$ede.bc, 
        subset(early.ex.lh.mean, group %in% c(1))$ede.bc, paired = FALSE)
#

# Testing - LFPV lh t.test -------------------------------------------------
t.test(subset(early.ex.lh.mean, group %in% c(2))$LFvp.bc, 
       subset(early.ex.lh.mean, group %in% c(1))$LFvp.bc, paired = FALSE)
cohen.d(subset(early.ex.lh.mean, group %in% c(2))$LFvp.bc, 
        subset(early.ex.lh.mean, group %in% c(1))$LFvp.bc, paired = FALSE)
#

# Testing - LFOFF lh t.test -------------------------------------------------
t.test(subset(early.ex.lh.mean, group %in% c(2))$LFoff.bc, 
       subset(early.ex.lh.mean, group %in% c(1))$LFoff.bc, paired = FALSE)
cohen.d(subset(early.ex.lh.mean, group %in% c(2))$LFoff.bc, 
        subset(early.ex.lh.mean, group %in% c(1))$LFoff.bc, paired = FALSE)
#


# Implicit vs. Explicit ---------------------------------------------------
summary(strint_implicitVexplicit)
wilcox.test(subset(strint_implicitVexplicit, Group %in% 'Control')$Angle,
            subset(strint_implicitVexplicit, Group %in% 'Structure')$Angle,
            alternative = "two.sided")


# Post Exposure -----------------------------------------------------------
# Right Hand
aov_ez(subset(rhKinematics_stats, block %in% time_pe), id = 'subjectID', dv = 'ide.bc', between = 'group', within = 'block')
aov_ez(subset(rhKinematics_stats, block %in% time_pe), id = 'subjectID', dv = 'fbrmse_c', between = 'group', within = 'block')

t.test(subset(rhKinematics_stats, block %in% 19 & group == 1)$fbrmse,
       subset(rhKinematics_stats, block %in% 19 & group == 2)$fbrmse, paried = T)
cohen.d(subset(rhKinematics_stats, block %in% 19 & group == 1)$fbrmse,
       subset(rhKinematics_stats, block %in% 19 & group == 2)$fbrmse, paried = T)

t.test(subset(rhKinematics_stats, block %in% 22 & group == 1)$fbrmse,
       subset(rhKinematics_stats, block %in% 22 & group == 2)$fbrmse, paried = T)
cohen.d(subset(rhKinematics_stats, block %in% 22 & group == 1)$fbrmse,
        subset(rhKinematics_stats, block %in% 22 & group == 2)$fbrmse, paried = T)
#Left Hand

aov.lh.ide.pe =  aov_ez(subset(lhKinematics_stats, block %in% time_pe), id = 'subjectID', dv = 'ide.bc', between = 'group', within = 'block')
aov.lh.ide.pe
aov.lh.ide.pe$anova_table$`Pr(>F)`

aov.lh.ede.pe =  aov_ez(subset(lhKinematics_stats, block %in% time_pe), id = 'subjectID', dv = 'ede.bc', between = 'group', within = 'block')
aov.lh.ede.pe
aov.lh.ede.pe$anova_table$`Pr(>F)`

aov.lh.lfpv.pe =  aov_ez(subset(lhKinematics_stats, block %in% time_pe), id = 'subjectID', dv = 'LFvp.bc', between = 'group', within = 'block')
aov.lh.lfpv.pe
aov.lh.lfpv.pe$anova_table$`Pr(>F)`

aov.lh.lfoff.pe =  aov_ez(subset(lhKinematics_stats, block %in% time_pe), id = 'subjectID', dv = 'LFoff.bc', between = 'group', within = 'block')
aov.lh.lfoff.pe
aov.lh.lfoff.pe$anova_table$`Pr(>F)`
#


# Force Channel -----------------------------------------------------------
# Omnibus
aov.fc = aov_ez(subset(FC_3way, phase != 'kbmean'), id = 'subID', dv = 'value', between = 'group', within = c('sample','phase'))
aov.fc

# FC at peak velocity across phase
aov.fc.pv = aov_ez(subset(FC_3way, phase != 'kbmean' & sample == 278), id = 'subID', dv = 'value', between = 'group', within = c('phase'))
aov.fc.pv

# Group wise t.tests
t.test(value ~ group, data = subset(FC_3way, phase == 'bk1mean' & sample == 278), paired = F)
t.test(value ~ group, data = subset(FC_3way, phase == 'bk2mean' & sample == 278), paired = F)
t.test(value ~ group, data = subset(FC_3way, phase == 'bk3mean' & sample == 278), paired = F)

# Sample wise t.tests
t.test(value ~ sample, data = subset(FC_3way, phase == 'bk1mean'), paired = T)
cohen.d(subset(FC_3way, phase == 'bk1mean' & sample == 278)$value,
        subset(FC_3way, phase == 'bk1mean' & sample == 950)$value, paired = T)

t.test(value ~ sample, data = subset(FC_3way, phase == 'bk2mean'), paired = T)
t.test(value ~ sample, data = subset(FC_3way, phase == 'bk3mean'), paired = T)
