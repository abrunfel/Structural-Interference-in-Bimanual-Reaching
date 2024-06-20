library(ggplot2)
block_ex = subset(lhData_mbg, lhData_mbg$phase==3)$block
group_ex = subset(lhData_mbg, lhData_mbg$phase==3)$group

block_pe = subset(lhData_mbg, lhData_mbg$phase==4)$block
group_pe = subset(lhData_mbg, lhData_mbg$phase==4)$group


# ide exposure and post, both hands ---------------------------------------------------------------------
siz = 3 # Used to scale the size of the data points
ggplot()+
  #geom_point(data = subset(lhData_mbg, lhData_mbg$phase==3), aes(x = block, y = ide.bc, color = group_ex), size = siz)+
  #geom_point(data = subset(lhData_mbg, lhData_mbg$phase==4), aes(x = block, y = ide.bc, color = group_pe), size = siz)+
  geom_point(data = subset(rhData_mbg, rhData_mbg$phase==3), aes(x = block, y = ide.bc, color = group_ex), size = siz, stroke = 1)+
  geom_point(data = subset(rhData_mbg, rhData_mbg$phase==4), aes(x = block, y = ide.bc, color = group_pe), size = siz, stroke = 1)+
  labs(y = "IDE (deg)")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.title = element_blank())+
  scale_color_discrete(labels = c("Control", "Structure"))+
  geom_vline(xintercept = c(18.5), linetype = "dotted")
ggsave("ide.rh.jpeg", plot = last_plot(), device = "jpeg", path = "//Volumes/mnl/Data/Adaptation/structural_interference/R output files", width = 7.5, height = 4.5, units = "in")
#

# RMSE exposure and post, both hands ---------------------------------------------------------------------
siz = 3 # Used to scale the size of the data points
ggplot()+
  geom_point(data = subset(lhData_mbg, lhData_mbg$phase==3), aes(x = block, y = rmse_c, color = group_ex), size = siz)+
  geom_point(data = subset(lhData_mbg, lhData_mbg$phase==4), aes(x = block, y = rmse_c, color = group_pe), size = siz)+
  geom_point(data = subset(rhData_mbg, rhData_mbg$phase==3), aes(x = block, y = rmse_c, color = group_ex), shape=1, size = siz, stroke = 1)+
  geom_point(data = subset(rhData_mbg, rhData_mbg$phase==4), aes(x = block, y = rmse_c, color = group_pe), shape=1, size = siz, stroke = 1)+
  labs(y = "RMSE (mm)")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.title = element_blank())+
  scale_color_discrete(labels = c("Control", "Structure"))+
  geom_vline(xintercept = c(18.5), linetype = "dotted")
ggsave("rmse.bh.jpeg", plot = last_plot(), device = "jpeg", path = "//Volumes/mnl/Data/Adaptation/structural_interference/R output files", width = 7.5, height = 4.5, units = "in")
#

# ide exposure only, left hand only ---------------------------------------
pd = position_dodge(width = 0.4)
block_ex = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase==3)$block
group_ex = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase==3)$group
ggplot(data = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase==3), aes(x = block, y = ide.bc))+
  geom_point(aes(color = group_ex), position = pd, size = 3)+
  geom_errorbar(data = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase==3), aes(x = block, ymin = ide.bc-ide.bc_sem, ymax = ide.bc+ide.bc_sem, color = group),
                position = pd, width = 0)+
  geom_line(aes(color = group_ex), position = pd, size = 1)+
  scale_y_continuous(limits = c(-1,7))+
  labs(y = "IDE (deg)")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.title = element_blank())+
  scale_color_discrete(labels = c("Control", "Structure"))+
  geom_vline(xintercept = c(5,12,18), linetype = "dotted")+
  scale_x_continuous(name = "block", breaks = c(5,12,18))
ggsave("ide.lh.jpeg", plot = last_plot(), device = "jpeg", path = "//Volumes/mnl/Data/Adaptation/structural_interference/R output files", width = 7.5, height = 4.5, units = "in")
#

# epx ---------------------------------------------------------------------
pd = position_dodge(width = 0.4)
block_ex = subset(lhData_mbg, lhData_mbg$phase==3)$block
group_ex = subset(lhData_mbg, lhData_mbg$phase==3)$group
ggplot(data = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase==3), aes(x = block, y = EP_X.bc))+
  geom_point(aes(color = group_ex), position = pd, size = 3)+
  geom_errorbar(data = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase==3), aes(x = block, ymin = EP_X.bc-EP_X.bc_sem, ymax = EP_X.bc+EP_X.bc_sem, color = group),
                position = pd, width = 0)+
  geom_line(aes(color = group_ex), position = pd, size = 1)+
  scale_y_continuous(limits = c(0,1.25))+
  labs(y = "EPX (cm)")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.title = element_blank())+
  scale_color_discrete(labels = c("Control", "Structure"))+
  geom_vline(xintercept = c(5,12,18), linetype = "dotted")+
  scale_x_continuous(name = "block", breaks = c(5,12,18))
ggsave("epx.lh.jpeg", plot = last_plot(), device = "jpeg", path = "//Volumes/mnl/Data/Adaptation/structural_interference/R output files", width = 7.5, height = 4.5, units = "in")
#


# Force Channel -----------------------------------------------------------
# Which block do you want to plot?
plot.block = block1.fs
ggplot(data = plot.block, aes(x = sample, y = mean, color = group))+
  geom_point()+
  geom_errorbar(aes(x = plot.block$sample, ymin = plot.block$mean-plot.block$se, ymax = plot.block$mean+plot.block$se, color = plot.block$group), alpha = 0.1)+
  coord_cartesian(ylim = c(-0.5, 3.5))+
  labs(y = "Lateral Force (N)")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.title = element_blank())+
  scale_color_discrete(labels = c("Control", "Structure"))+
  geom_vline(xintercept = c(100,900), linetype = "dotted")
ggsave("fc.bk1.jpeg", plot = last_plot(), device = "jpeg", path = "//Volumes/mnl/Data/Adaptation/structural_interference/R output files", width = 7.5, height = 4.5, units = "in")

plot.block = block2.fs
ggplot(data = plot.block, aes(x = sample, y = mean, color = group))+
  geom_point()+
  geom_errorbar(aes(x = plot.block$sample, ymin = plot.block$mean-plot.block$se, ymax = plot.block$mean+plot.block$se, color = plot.block$group), alpha = 0.1)+
  coord_cartesian(ylim = c(-0.5, 3.5))+
  labs(y = "Lateral Force (N)")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.title = element_blank())+
  scale_color_discrete(labels = c("Control", "Structure"))+
  geom_vline(xintercept = c(100,900), linetype = "dotted")
ggsave("fc.bk2.jpeg", plot = last_plot(), device = "jpeg", path = "//Volumes/mnl/Data/Adaptation/structural_interference/R output files", width = 7.5, height = 4.5, units = "in")

plot.block = block3.fs
ggplot(data = plot.block, aes(x = sample, y = mean, color = group))+
  geom_point()+
  geom_errorbar(aes(x = plot.block$sample, ymin = plot.block$mean-plot.block$se, ymax = plot.block$mean+plot.block$se, color = plot.block$group),alpha = 0.1)+
  coord_cartesian(ylim = c(-0.5, 3.5))+
  labs(y = "Lateral Force (N)")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.title = element_blank())+
  scale_color_discrete(labels = c("Control", "Structure"))+
  geom_vline(xintercept = c(100,900), linetype = "dotted")
ggsave("fc.bk3.jpeg", plot = last_plot(), device = "jpeg", path = "//Volumes/mnl/Data/Adaptation/structural_interference/R output files", width = 7.5, height = 4.5, units = "in")
#

# LFvp.bc plotting --------------------------------------------------------
pd = position_dodge(width = 0.4)
block_ex = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase==3)$block
group_ex = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase==3)$group
ggplot(data = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase==3), aes(x = block, y = LFvp.bc))+
  geom_point(aes(color = group_ex), position = pd, size = 3)+
  geom_errorbar(data = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase==3), aes(x = block, ymin = LFvp.bc-LFvp.bc_sem, ymax = LFvp.bc+LFvp.bc_sem, color = group),
                position = pd, width = 0)+
  geom_line(aes(color = group_ex), position = pd, size = 1)+
  scale_y_continuous(limits = c(-0.1,0.5))+
  labs(y = "LFPV (N)")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.title = element_blank())+
  scale_color_discrete(labels = c("Control", "Structure"))+
  geom_vline(xintercept = c(5,12,18), linetype = "dotted")+
  scale_x_continuous(name = "block", breaks = c(5,12,18))
ggsave("LFPV.jpeg", plot = last_plot(), device = "jpeg", path = "//Volumes/mnl/Data/Adaptation/structural_interference/R output files", width = 7.5, height = 4.5, units = "in")
#

# LFoff.bc plotting -----------------------------------------------------
pd = position_dodge(width = 0.4)
block_ex = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase==3)$block
group_ex = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase==3)$group
ggplot(data = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase==3), aes(x = block, y = LFoff.bc))+
  geom_point(aes(color = group_ex), position = pd, size = 3)+
  geom_errorbar(data = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase==3), aes(x = block, ymin = LFoff.bc-LFoff.bc_sem, ymax = LFoff.bc+LFoff.bc_sem, color = group),
                position = pd, width = 0)+
  geom_line(aes(color = group_ex), position = pd, size = 1)+
  scale_y_continuous(limits = c(0,1))+
  labs(y = "LFOFF (N)")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.title = element_blank())+
  scale_color_discrete(labels = c("Control", "Structure"))+
  geom_vline(xintercept = c(5,12,18), linetype = "dotted")+
  scale_x_continuous(name = "block", breaks = c(5,12,18))
ggsave("LFOFF.jpeg", plot = last_plot(), device = "jpeg", path = "//Volumes/mnl/Data/Adaptation/structural_interference/R output files", width = 7.5, height = 4.5, units = "in")
#

# ide full experiemnt RIGHT hand only ---------------------------------------
pd = position_dodge(width = 0.4)
ggplot(data = rhData_MandSEMbg, aes(x = block, y = ide.bc))+
  geom_point(aes(color = group), position = pd, size = 3)+
  geom_errorbar(data = rhData_MandSEMbg, aes(x = block, ymin = ide.bc-ide.bc_sem, ymax = ide.bc+ide.bc_sem, color = group),
                position = pd, width = 0)+
  geom_line(aes(color = group), position = pd, size = 1)+
  scale_y_continuous(limits = c())+
  labs(y = "IDE (deg)")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.title = element_blank())+
  scale_color_discrete(labels = c("Control", "Structure"))+
  geom_vline(xintercept = c(5,12,18), linetype = "dotted")+
  scale_x_continuous(name = "block", breaks = c(5,12,18))
ggsave("ide.rh.jpeg", plot = last_plot(), device = "jpeg", path = "//Volumes/mnl/Data/Adaptation/structural_interference/R output files", width = 7.5, height = 4.5, units = "in")
#

# rmse full experiemnt RIGHT hand only ---------------------------------------
pd = position_dodge(width = 0.4)
ggplot(data = rhData_MandSEMbg, aes(x = block, y = rmse_c))+
  geom_point(aes(color = group), position = pd, size = 3)+
  geom_errorbar(data = rhData_MandSEMbg, aes(x = block, ymin = rmse_c-rmse_c_sem, ymax = rmse_c+rmse_c_sem, color = group),
                position = pd, width = 0)+
  geom_line(aes(color = group), position = pd, size = 1)+
  scale_y_continuous(limits = c())+
  labs(y = "RMSE (mm)")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.title = element_blank())+
  scale_color_discrete(labels = c("Control", "Structure"))+
  geom_vline(xintercept = c(5,12,18), linetype = "dotted")+
  scale_x_continuous(name = "block", breaks = c(5,12,18))
ggsave("rmse.rh.jpeg", plot = last_plot(), device = "jpeg", path = "//Volumes/mnl/Data/Adaptation/structural_interference/R output files", width = 7.5, height = 4.5, units = "in")
#