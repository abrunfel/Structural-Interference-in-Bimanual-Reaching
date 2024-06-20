#strint stats
# Step 1: Load workspace (must include lhKinematics_stats, rhKinematics_stats, FC_3way, and FC_3way.ex)
# Step 2:: Load 'afex' package
library(afex)
# Step 3: Load 'emmeans' package
library(emmeans)
# Step 4: Input 'time_*' values for which blocks to process as repeated measure (ie: beginning, middle, end = 5,12,18)
time_exp = c(5,12,18)
time_pe = c(19,22)

# Training - post exposure t.test -----------------------------------------
t.test(subset(trainData_mbb, trainData_mbb$block %in% c(65:68) & trainData_mbb$group %in% c(1))$ide.bc, 
       subset(trainData_mbb, trainData_mbb$block %in% c(65:68) & trainData_mbb$group %in% c(2))$ide.bc, paired = FALSE)
#

##### Testing ANOVAS #####
# Testing - ide rh ANOVA --------------------------------------------------
aov_ez(subset(rhKinematics_stats, block %in% time_exp), id = 'subjectID', dv = 'ide.bc', between = 'group', within = 'block')
aov_ez(subset(rhKinematics_stats, block %in% time_exp), id = 'subjectID', dv = 'rmse_c', between = 'group', within = 'block')

# Post-hocs (you must correct for multiple comparisons after getting p value)
t.test(subset(rhKinematics_stats, block %in% c(19) & group %in% c(2))$ide.bc, 
       subset(rhKinematics_stats, block %in% c(19) & group %in% c(1))$ide.bc, paired = FALSE)

t.test(subset(rhKinematics_stats, block %in% c(19) & group %in% c(2))$rmse_c, 
       subset(rhKinematics_stats, block %in% c(19) & group %in% c(1))$rmse_c, paired = FALSE)

#rh.ide.aov = aov_ez(subset(rhKinematics_stats, block %in% time_exp), id = 'subjectID', dv = 'ide.bc', between = 'group', within = 'block')
#em.rh.ide = emmeans(rh.ide.aov$aov, specs = pairwise ~group:block)
#contrast(em.rh.ide, method = list()) # this is broken
#emmeans(rh.ide.aov$aov, specs = pairwise ~group:block, adjust = "tukey")
#emmeans(rh.ide.aov$aov, specs = pairwise ~group:block, adjust = "bonferroni")
#

# Testing - ide lh ANOVA --------------------------------------------------
aov_ez(subset(lhKinematics_stats, block %in% time_exp), id = 'subjectID', dv = 'ide.bc', between = 'group', within = 'block')
#

# Testing - ede lh ANOVA --------------------------------------------------
aov_ez(subset(lhKinematics_stats, block %in% time_exp), id = 'subjectID', dv = 'ede.bc', between = 'group', within = 'block')
#

# Testing - epx lh ANOVA --------------------------------------------------
aov_ez(subset(lhKinematics_stats, block %in% time_exp), id = 'subjectID', dv = 'EP_X.bc', between = 'group', within = 'block')
#

# Testing - LFPV lh ANOVA --------------------------------------------------
aov_ez(subset(lhKinematics_stats, block %in% time_exp), id = 'subjectID', dv = 'LFvp.bc', between = 'group', within = 'block')
#

# Testing - LFOFF lh ANOVA --------------------------------------------------
aov_ez(subset(lhKinematics_stats, block %in% time_exp), id = 'subjectID', dv = 'LFoff.bc', between = 'group', within = 'block')
#

##### Testing t.test first block of exposure (unpaired t.tests) ######

# Testing - ide lh t.test -------------------------------------------------
t.test(subset(lhKinematics_stats, block %in% c(5,6,7) & group %in% c(2))$ide.bc, 
       subset(lhKinematics_stats, block %in% c(5,6,7) & group %in% c(1))$ide.bc, paired = FALSE)
#

# Testing - ede lh t.test -------------------------------------------------
t.test(subset(lhKinematics_stats, block %in% c(5,6,7) & group %in% c(2))$ede.bc, 
       subset(lhKinematics_stats, block %in% c(5,6,7) & group %in% c(1))$ede.bc, paired = FALSE)
#

# Testing - LFPV lh t.test -------------------------------------------------
t.test(subset(lhKinematics_stats, block %in% c(5,6,7) & group %in% c(2))$LFvp.bc, 
       subset(lhKinematics_stats, block %in% c(5,6,7) & group %in% c(1))$LFvp.bc, paired = FALSE)
#

# Testing - LFOFF lh t.test -------------------------------------------------
t.test(subset(lhKinematics_stats, block %in% c(5,6,7) & group %in% c(2))$LFoff.bc, 
       subset(lhKinematics_stats, block %in% c(5,6,7) & group %in% c(1))$LFoff.bc, paired = FALSE)
#


# Implicit vs. Explicit ---------------------------------------------------
summary(strint_implicitVexplicit)
wilcox.test(subset(strint_implicitVexplicit, Group %in% 'Control')$Angle,
            subset(strint_implicitVexplicit, Group %in% 'Structure')$Angle,
            alternative = "two.sided")


# Post Exposure -----------------------------------------------------------
# Right Hand
aov_ez(subset(rhKinematics_stats, block %in% time_pe), id = 'subjectID', dv = 'ide.bc', between = 'group', within = 'block')
aov_ez(subset(rhKinematics_stats, block %in% time_pe), id = 'subjectID', dv = 'rmse_c', between = 'group', within = 'block')

#Left Hand
aov_ez(subset(lhKinematics_stats, block %in% time_pe), id = 'subjectID', dv = 'ide.bc', between = 'group', within = 'block')
aov_ez(subset(lhKinematics_stats, block %in% time_pe), id = 'subjectID', dv = 'ede.bc', between = 'group', within = 'block')
aov_ez(subset(lhKinematics_stats, block %in% time_pe), id = 'subjectID', dv = 'LFvp.bc', between = 'group', within = 'block')
aov_ez(subset(lhKinematics_stats, block %in% time_pe), id = 'subjectID', dv = 'LFoff.bc', between = 'group', within = 'block')
#
