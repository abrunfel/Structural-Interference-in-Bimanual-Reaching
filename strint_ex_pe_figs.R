block_ex = subset(lhData_mbg, lhData_mbg$phase%in% c(3,4))$block
group_expe = subset(lhData_mbg, lhData_mbg$phase%in% c(3,4))$group


# ide exposure and post-exposure, right hand only ---------------------------------------
pd = position_dodge(width = 0.4)
block_ex = subset(rhData_MandSEMbg, rhData_MandSEMbg$phase%in% c(3,4))$block
group_expe = subset(rhData_MandSEMbg, rhData_MandSEMbg$phase%in% c(3,4))$group
ggplot(data = subset(rhData_MandSEMbg, rhData_MandSEMbg$phase%in% c(3,4)), aes(x = block, y = ide.bc))+
  geom_point(aes(color = group_expe), position = pd, size = 3)+
  geom_errorbar(data = subset(rhData_MandSEMbg, rhData_MandSEMbg$phase%in% c(3,4)), aes(x = block, ymin = ide.bc-ide.bc_sem, ymax = ide.bc+ide.bc_sem, color = group),
                position = pd, width = 0)+
  geom_line(aes(color = group_expe), position = pd, size = 1)+
  #scale_y_continuous(limits = c(-3,7))+
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
ggsave("ide.rh.jpeg", plot = last_plot(), device = "jpeg", path = "Z:\\Data\\Adaptation\\structural_interference\\R output files", width = 7.5, height = 4.5, units = "in")
#

# rmse exposure and post-exposure, right hand only ---------------------------------------
pd = position_dodge(width = 0.4)
block_ex = subset(rhData_MandSEMbg, rhData_MandSEMbg$phase%in% c(3,4))$block
group_expe = subset(rhData_MandSEMbg, rhData_MandSEMbg$phase%in% c(3,4))$group
ggplot(data = subset(rhData_MandSEMbg, rhData_MandSEMbg$phase%in% c(3,4)), aes(x = block, y = rmse_c))+
  geom_point(aes(color = group_expe), position = pd, size = 3)+
  geom_errorbar(data = subset(rhData_MandSEMbg, rhData_MandSEMbg$phase%in% c(3,4)), aes(x = block, ymin = rmse_c-rmse_c_sem, ymax = rmse_c+rmse_c_sem, color = group),
                position = pd, width = 0)+
  geom_line(aes(color = group_expe), position = pd, size = 1)+
  #scale_y_continuous(limits = c(-3,7))+
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
ggsave("rmse.rh.jpeg", plot = last_plot(), device = "jpeg", path = "Z:\\Data\\Adaptation\\structural_interference\\R output files", width = 7.5, height = 4.5, units = "in")
#


# ide exposure only, left hand only ---------------------------------------
#jpeg("ide.bc.jpeg", width = 8.32, height = 5.12, useDingbats = FALSE)
pd = position_dodge(width = 0.4)
block_ex = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase%in% c(3,4))$block
group_expe = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase%in% c(3,4))$group
ggplot(data = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase%in% c(3,4)), aes(x = block, y = ide.bc))+
  geom_point(aes(color = group_expe), position = pd, size = 3)+
  geom_errorbar(data = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase%in% c(3,4)), aes(x = block, ymin = ide.bc-ide.bc_sem, ymax = ide.bc+ide.bc_sem, color = group),
                position = pd, width = 0)+
  geom_line(aes(color = group_expe), position = pd, size = 1)+
  scale_y_continuous(limits = c(-3,7))+
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
ggsave("ide.bc.jpeg", plot = last_plot(), device = "jpeg", path = "Z:\\Data\\Adaptation\\structural_interference\\R output files", width = 7.5, height = 4.5, units = "in")
#dev.off()
#

# ede exposure only, left hand only ---------------------------------------
#jpeg("ede.bc.jpeg", width = 8.32, height = 5.12, useDingbats = FALSE)
pd = position_dodge(width = 0.4)
block_ex = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase%in% c(3,4))$block
group_expe = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase%in% c(3,4))$group
ggplot(data = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase%in% c(3,4)), aes(x = block, y = ede.bc))+
  geom_point(aes(color = group_expe), position = pd, size = 3)+
  geom_errorbar(data = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase%in% c(3,4)), aes(x = block, ymin = ede.bc-ede.bc_sem, ymax = ede.bc+ede.bc_sem, color = group),
                position = pd, width = 0)+
  geom_line(aes(color = group_expe), position = pd, size = 1)+
  scale_y_continuous(limits = c(-2,8))+
  labs(y = "EDE (deg)")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.title = element_blank())+
  scale_color_discrete(labels = c("Control", "Structure"))+
  geom_vline(xintercept = c(5,12,18), linetype = "dotted")+
  scale_x_continuous(name = "block", breaks = c(5,12,18))
ggsave("ede.bc.jpeg", plot = last_plot(), device = "jpeg", path = "Z:\\Data\\Adaptation\\structural_interference\\R output files", width = 7.5, height = 4.5, units = "in")
#dev.off()
#

# epx ---------------------------------------------------------------------
#jpeg("epx.bc.jpeg", width = 8.32, height = 5.12, useDingbats = FALSE)
pd = position_dodge(width = 0.4)
block_ex = subset(lhData_mbg, lhData_mbg$phase%in% c(3,4))$block
group_expe = subset(lhData_mbg, lhData_mbg$phase%in% c(3,4))$group
ggplot(data = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase%in% c(3,4)), aes(x = block, y = EP_X.bc))+
  geom_point(aes(color = group_expe), position = pd, size = 3)+
  geom_errorbar(data = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase%in% c(3,4)), aes(x = block, ymin = EP_X.bc-EP_X.bc_sem, ymax = EP_X.bc+EP_X.bc_sem, color = group),
                position = pd, width = 0)+
  geom_line(aes(color = group_expe), position = pd, size = 1)+
  scale_y_continuous(limits = c(-0.25,1))+
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
ggsave("epx.bc.jpeg", plot = last_plot(), device = "jpeg", path = "Z:\\Data\\Adaptation\\structural_interference\\R output files", width = 7.5, height = 4.5, units = "in")
#dev.off()
#

# Force Channel -----------------------------------------------------------
# Which block do you want to plot?
plot.block = block1.fs
#jpeg("latForce_bk1.jpeg", width = 8.32, height = 5.12, useDingbats = FALSE)
ggplot(data = plot.block, aes(x = sample, y = mean, color = group))+
  geom_line(size = 1.2)+
  geom_ribbon(aes(x = plot.block$sample, ymin = plot.block$mean-plot.block$se, ymax = plot.block$mean+plot.block$se, fill = plot.block$group, alpha = 0.05), linetype = 0, show.legend = FALSE)+
  coord_cartesian(ylim = c(-0.5, 3.5))+
  labs(x = "Movement Extent (%)", y = "Lateral Force (N)")+
  scale_x_continuous(labels = c("0", "25", "50", "75", "100"))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.title = element_blank())+
  scale_color_discrete(labels = c("Control", "Structure"))+
  geom_vline(xintercept = c(950), linetype = "dotted")
#dev.off()
ggsave("latForce.bk1.jpeg", plot = last_plot(), device = "jpeg", path = "Z:\\Data\\Adaptation\\structural_interference\\R output files", width = 7.5, height = 4.5, units = "in")

plot.block = block2.fs
#jpeg("latForce_bk1.jpeg", width = 8.32, height = 5.12, useDingbats = FALSE)
ggplot(data = plot.block, aes(x = sample, y = mean, color = group))+
  geom_line(size = 1.2)+
  geom_ribbon(aes(x = plot.block$sample, ymin = plot.block$mean-plot.block$se, ymax = plot.block$mean+plot.block$se, fill = plot.block$group, alpha = 0.05), linetype = 0, show.legend = FALSE)+
  coord_cartesian(ylim = c(-0.5, 3.5))+
  labs(x = "Movement Extent (%)", y = "Lateral Force (N)")+
  scale_x_continuous(labels = c("0", "25", "50", "75", "100"))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.title = element_blank())+
  scale_color_discrete(labels = c("Control", "Structure"))+
  geom_vline(xintercept = c(950), linetype = "dotted")
#dev.off()
ggsave("latForce.bk2.jpeg", plot = last_plot(), device = "jpeg", path = "Z:\\Data\\Adaptation\\structural_interference\\R output files", width = 7.5, height = 4.5, units = "in")

plot.block = block3.fs
#jpeg("latForce_bk1.jpeg", width = 8.32, height = 5.12, useDingbats = FALSE)
ggplot(data = plot.block, aes(x = sample, y = mean, color = group))+
  geom_line(size = 1.2)+
  geom_ribbon(aes(x = plot.block$sample, ymin = plot.block$mean-plot.block$se, ymax = plot.block$mean+plot.block$se, fill = plot.block$group, alpha = 0.05), linetype = 0, show.legend = FALSE)+
  coord_cartesian(ylim = c(-0.5, 3.5))+
  labs(x = "Movement Extent (%)", y = "Lateral Force (N)")+
  scale_x_continuous(labels = c("0", "25", "50", "75", "100"))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.title = element_blank())+
  scale_color_discrete(labels = c("Control", "Structure"))+
  geom_vline(xintercept = c(950), linetype = "dotted")
#dev.off()
ggsave("latForce.bk3.jpeg", plot = last_plot(), device = "jpeg", path = "Z:\\Data\\Adaptation\\structural_interference\\R output files", width = 7.5, height = 4.5, units = "in")
#

# LFvp.bc plotting --------------------------------------------------------
#jpeg("LFvp.bc.jpeg", width = 8.32, height = 5.12, useDingbats = FALSE)
pd = position_dodge(width = 0.4)
block_ex = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase%in% c(3,4))$block
group_expe = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase%in% c(3,4))$group
ggplot(data = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase%in% c(3,4)), aes(x = block, y = LFvp.bc))+
  geom_point(aes(color = group_expe), position = pd, size = 3)+
  geom_errorbar(data = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase%in% c(3,4)), aes(x = block, ymin = LFvp.bc-LFvp.bc_sem, ymax = LFvp.bc+LFvp.bc_sem, color = group),
                position = pd, width = 0)+
  geom_line(aes(color = group_expe), position = pd, size = 1)+
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
ggsave("LFPV.bc.jpeg", plot = last_plot(), device = "jpeg", path = "Z:\\Data\\Adaptation\\structural_interference\\R output files", width = 7.5, height = 4.5, units = "in")
#dev.off()
#

# LFoff.bc plotting -----------------------------------------------------
#jpeg("LFoff.bc.jpeg", width = 8.32, height = 5.12, useDingbats = FALSE)
pd = position_dodge(width = 0.4)
block_ex = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase%in% c(3,4))$block
group_expe = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase%in% c(3,4))$group
ggplot(data = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase%in% c(3,4)), aes(x = block, y = LFoff.bc))+
  geom_point(aes(color = group_expe), position = pd, size = 3)+
  geom_errorbar(data = subset(lhData_MandSEMbg, lhData_MandSEMbg$phase%in% c(3,4)), aes(x = block, ymin = LFoff.bc-LFoff.bc_sem, ymax = LFoff.bc+LFoff.bc_sem, color = group),
                position = pd, width = 0)+
  geom_line(aes(color = group_expe), position = pd, size = 1)+
  scale_y_continuous(limits = c(-0.1,1))+
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
ggsave("LFOFF.bc.jpeg", plot = last_plot(), device = "jpeg", path = "Z:\\Data\\Adaptation\\structural_interference\\R output files", width = 7.5, height = 4.5, units = "in")
#dev.off()
#


# Implicit vs. Explicit ---------------------------------------------------
# http://rstudio-pubs-static.s3.amazonaws.com/12358_c9e8e1fe26054888b308dd29e0d6d0e1.html
#install.packages("circular")
#library(circular)

# Load in data
#strint_implicitVexplicit <- read_excel("Z:/Data/Adaptation/structural_interference/strint_implicitVexplicit.xlsx")

ggplot(strint_implicitVexplicit, aes(x = Angle, fill = Group)) +
      geom_histogram(aes(y = ..count..), breaks = seq(-90, 90, by = 5))+
      #coord_polar(start = pi)+
      labs(x = "Angle (degrees)", y = "Count") + theme_bw()
      #facet_grid(~Direction)

ggsave("impVSexp.jpeg", plot = last_plot(), device = "jpeg", path = "Z:\\Data\\Adaptation\\structural_interference\\R output files", width = 7.5, height = 4.5, units = "in")
strint_implicitVexplicit %>% group_by(Direction) %>% summarise_at("Angle", list(mean, sd))
#
