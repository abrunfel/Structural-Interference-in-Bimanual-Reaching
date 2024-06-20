#### Structural Interference Project Functions ####

# Timeseries Plotting: ts.plot() ----------
# Dependencies: tidyverse, cowplot, showtext
ts.plot <- function(dat, var, bks, ylab){
  dat$block = rep(rep(1:(nr/blockSize/numSub), each = blockSize), numSub) # Add in the block variable (inherits blockSize from Workspace)
  dat %>% group_by(subjectID, group, block) %>% summarise_all(.funs = mean, na.rm = T) %>% # calculates mean-by-block for each subject
    filter(block %in% c(bks)) %>%
    ggplot(aes(x = block, y=!!ensym(var), color = group))+ # !!ensym() converts string to variable name
    stat_summary(fun.data = 'mean_se', position = pd)+
    stat_summary(geom = "line", position = pd)+
    theme_cowplot()+
    labs(y = ylab)+
    theme(text = element_text(family = "Helvetica"),
          axis.text.y = element_text(size = 16),
          axis.text.x = element_text(size = 12),
          axis.title.y = element_text(size = 20),
          axis.title.x = element_text(size = 20),
          legend.title = element_blank())+
    scale_color_discrete(labels = c("Control", "Structure"))+
    geom_vline(xintercept = c(5,12,18), linetype = "dotted")+
    scale_x_continuous(name = "block", breaks = c(5,12,18))
}
#

# FC Plotting: ts.plot() ----------
# Dependencies: tidyverse, cowplot
# Which block do you want to plot?
fc.plot <- function(plot.block){
  ggplot(data = plot.block, aes(x = sample, y = mean, color = group))+
    geom_line(size = 1.2)+
    geom_ribbon(aes(x = plot.block$sample, ymin = plot.block$mean-plot.block$se, ymax = plot.block$mean+plot.block$se, fill = plot.block$group, alpha = 0.05), linetype = 0, show.legend = FALSE)+
    coord_cartesian(ylim = c(-0.5, 3.5))+
    labs(x = "Movement Extent (%)", y = "Lateral Force (N)")+
    scale_x_continuous(labels = c("0", "25", "50", "75", "100"))+
    theme_cowplot()+
    theme(text = element_text(family = "Helvetica"),
          axis.text.y = element_text(size = 16),
          axis.text.x = element_text(size = 12),
          axis.title.y = element_text(size = 20),
          axis.title.x = element_text(size = 20),
          legend.title = element_blank())+
    scale_color_discrete(labels = c("Control", "Structure"))+
    geom_vline(xintercept = c(278,950), linetype = "dotted", size = 1.2)
}
#