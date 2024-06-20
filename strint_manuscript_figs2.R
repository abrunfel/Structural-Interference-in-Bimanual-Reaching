# Load in library dependencies -----------
library(tidyverse)
library(cowplot)
library(readxl)
#library(Cairo)
#library(showtext)
#showtext_auto()
#library(rlang) # Not sure if I need this... I don't think so

# Set up directories and save path --------------
if(.Platform$OS.type == "unix"){
  setwd("//Volumes//mnl//Data//Adaptation//structural_interference//manuscript//R") # Find project's R folder
  source("strint_functions.R") # Load in custom functions
  load("//Volumes//mnl//Data//Adaptation//structural_interference//manuscript//R//Routput//220907_strint_manuscript_WS.RData") # Load in data (make sure its the most up-to-date)
  strint_implicitVexplicit <- read_excel("get Mac file for implicit vs. explicit!")
  savepath = "//Volumes//mnl//Data//Adaptation//structural_interference//manuscript//R//Routput" # Set save path for plots
} else if (.Platform$OS.type == "windows") {
  setwd("Z://Data//Adaptation//structural_interference//manuscript//R") # Find project's R folder
  source("strint_functions.R") # Load in custom functions
  load("Z://Data//Adaptation//structural_interference//manuscript//R//Routput/220907_strint_manuscript_WS.RData") # Load in data (make sure its the most up-to-date)
  strint_implicitVexplicit <- read_excel("Z://Data//Adaptation//structural_interference//strint_implicitVexplicit.xlsx")
  savepath = "Z://Data//Adaptation//structural_interference//manuscript//R//Routput" # Set save path for plots
}
#

# RH IDE ---------
p.rh.ide = ts.plot(rhData, ide.bc, 1:22, "IDE (deg)") # setting an output "p*" suppresses the plot. This is then passed into the ggsave function
#ggsave("rh.ide.eps", plot = p.rh.ide, device = cairo_ps, path = savepath, width = 7.5, height = 4.5, units = "in")
ggsave("rh.ide.pdf", plot = p.rh.ide, path = savepath, width = 7.5, height = 4.5, units = "in")
#

# RH RMSE ---------
p.rh.rmse = ts.plot(rhData, fbrmse_c, 1:22, "RMSE (mm)") # setting an output "p*" suppresses the plot. This is then passed into the ggsave function
#ggsave("rh.rmse.eps", plot = p.rh.rmse, device = cairo_ps, path = savepath, width = 7.5, height = 4.5, units = "in")
ggsave("rh.rmse.pdf", plot = p.rh.rmse, path = savepath, width = 7.5, height = 4.5, units = "in")
#

# LH IDE ------------
p.lh.ide = ts.plot(lhData, ide.bc, 5:22, "IDE (deg)") # setting an output "p*" suppresses the plot. This is then passed into the ggsave function
#ggsave("lh.ide.eps", plot = p.lh.ide, device = cairo_ps, path = savepath, width = 7.5, height = 4.5, units = "in")
ggsave("lh.ide.pdf", plot = p.lh.ide, path = savepath, width = 7.5, height = 4.5, units = "in")
#

# LH LFPV ------------
p.lh.LFvp = ts.plot(lhData, LFvp.bc, 5:22, "LFPV (N)") # setting an output "p*" suppresses the plot. This is then passed into the ggsave function
#ggsave("lh.lfpv.eps", plot = p.lh.LFvp, device = cairo_ps, path = savepath, width = 7.5, height = 4.5, units = "in")
ggsave("lh.lfpv.pdf", plot = p.lh.LFvp, path = savepath, width = 7.5, height = 4.5, units = "in")
#

# LH EDE ------------
p.lh.ede = ts.plot(lhData, ede.bc, 5:22, "EDE (deg)") # setting an output "p*" suppresses the plot. This is then passed into the ggsave function
#ggsave("lh.ede.eps", plot = p.lh.ede, device = cairo_ps, path = savepath, width = 7.5, height = 4.5, units = "in")
ggsave("lh.ede.pdf", plot = p.lh.ede, path = savepath, width = 7.5, height = 4.5, units = "in")
#

# LH LFoff ------------
p.lh.LFoff = ts.plot(lhData, LFoff.bc, 5:22, "LFOFF (N)") # setting an output "p*" suppresses the plot. This is then passed into the ggsave function
#ggsave("lh.LFoff.eps", plot = p.lh.LFoff, device = cairo_ps, path = savepath, width = 7.5, height = 4.5, units = "in")
ggsave("lh.LFoff.pdf", plot = p.lh.LFoff, path = savepath, width = 7.5, height = 4.5, units = "in")
#

# FC Block 1 ---------
# NOTE: use the force sensor (*.fs) version of FC data!!!
p.fc.bk1 = fc.plot(block1.fs)
#ggsave("fc.bk1.eps", plot = p.fc.bk1, device = cairo_ps, path = savepath, width = 7.5, height = 4.5, units = "in")
ggsave("fc.bk1.pdf", plot = p.fc.bk1, path = savepath, width = 7.5, height = 4.5, units = "in")

# FC Block 2 ---------
# NOTE: use the force sensor (*.fs) version of FC data!!!
p.fc.bk2 = fc.plot(block2.fs)
#ggsave("fc.bk2.eps", plot = p.fc.bk2, device = cairo_ps, path = savepath, width = 7.5, height = 4.5, units = "in")
ggsave("fc.bk2.pdf", plot = p.fc.bk2, path = savepath, width = 7.5, height = 4.5, units = "in")

# FC Block 3 ---------
# NOTE: use the force sensor (*.fs) version of FC data!!!
p.fc.bk3 = fc.plot(block3.fs)
#ggsave("fc.bk3.eps", plot = p.fc.bk3, device = cairo_ps, path = savepath, width = 7.5, height = 4.5, units = "in")
ggsave("fc.bk3.pdf", plot = p.fc.bk3, path = savepath, width = 7.5, height = 4.5, units = "in")

# Implicit vs. Explicit ---------------------------------------------------
# http://rstudio-pubs-static.s3.amazonaws.com/12358_c9e8e1fe26054888b308dd29e0d6d0e1.html
#install.packages("circular")
#library(circular)
ggplot(strint_implicitVexplicit, aes(x = Angle, fill = Group)) +
  geom_histogram(aes(y = ..count..), breaks = seq(-70, 70, by = 5))+
  #coord_polar(start = pi)+
  labs(x = "Angle (degrees)", y = "Count")+
  theme(text = element_text(family = "Helvetica"),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.title = element_blank())+
  theme_cowplot()
#facet_grid(~Direction)

ggsave("impVSexp.pdf", plot = last_plot(), device = "pdf", path = savepath, width = 7.5, height = 4.5, units = "in")
#
