library(ggplot2)
library(sysfonts)
library(Cairo)

setwd("../data/")

x1 <- read.table("gt60-15.csv")
x2 <- read.table("gt60-30.csv")
x3 <- read.table("gt60-60.csv")
x4 <- read.table("gt60-120.csv")

tmp1 <- data.frame(delay = x1, framerate = "15 Hz")
tmp2 <- data.frame(delay = x2, framerate = "30 Hz")
tmp3 <- data.frame(delay = x3, framerate = "60 Hz")
tmp4 <- data.frame(delay = x4, framerate = "120 Hz")

df <- rbind(tmp1, tmp2, tmp3, tmp4)
names(df) <- c("delay", "framerate")


cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


p <- ggplot(df, aes(x=delay, color=framerate, lty=framerate)) + stat_ecdf(lwd=1)
p <- p + xlab("end-to-end lag (ms)") + ylab("CDF")
p <- p + scale_color_manual(values=cbPalette)
p <- p + theme(text = element_text(family="Linux Biolinum O", size=24))
p
ggsave("R-gamesim.pdf", width=12, height=8, device=cairo_pdf)
