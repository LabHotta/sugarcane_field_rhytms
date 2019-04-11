library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)


f1 = "#00441b"
e1 = "#a20021"
e5 = "#fabc3c"
f1_line = "solid"
e1_line = "longdash"
e5_line = "dotdash"

timecourses_LHY = read.csv("FigureS8_LHY_timecourses.txt", sep="\t")

LHY_L1 <- ggplot(data=timecourses_LHY, aes(x=Hour, y=timecourses_LHY$leaf..1)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = 0, ymax = 750, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = 0, ymax = 750, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=f1_line, linetype=f1_line), size = 1.5) +
  #annotate("text", x = 22.2, y = 2.15, label = "italic(LHY)", size = 7, parse = TRUE)+
  scale_colour_manual(values=c(f1))+
  scale_linetype_manual(values=c(f1_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
  scale_y_continuous(breaks=seq(0,750,150), name="Fold-change", limits=c(0, 750))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_text(size=18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        legend.position="none")

LHY_E1 <- ggplot(data=timecourses_LHY, aes(x=Hour, y=timecourses_LHY$internode.1)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = 0, ymax = 750, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = 0, ymax = 750, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=f1_line, linetype=f1_line), size = 1.5) +
  #annotate("text", x = 22.2, y = 2.15, label = "italic(LHY)", size = 7, parse = TRUE)+
  scale_colour_manual(values=c(e1))+
  scale_linetype_manual(values=c(e1_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
  scale_y_continuous(breaks=seq(0,750,150), name="Expression value", limits=c(0, 750))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_text(size=18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        #axis.text.y = element_text(size=18),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none")

LHY_E5 <- ggplot(data=timecourses_LHY, aes(x=Hour, y=timecourses_LHY$internode.5)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = 0, ymax = 750, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = 0, ymax = 750, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=e5_line, linetype=e5_line), size = 1.5) +
  #annotate("text", x = 22.2, y = 2.15, label = "italic(LHY)", size = 7, parse = TRUE)+
  scale_colour_manual(values=c(e5))+
  scale_linetype_manual(values=c(e5_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
  scale_y_continuous(breaks=seq(0,750,150), name="Expression value", limits=c(0, 750))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_text(size=18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        #axis.text.y = element_text(size=18),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none")

timecourses_TOC1 = read.csv("FigureS8_TOC1_timecourses.txt", sep="\t")

TOC1_L1 <- ggplot(data=timecourses_TOC1, aes(x=Hour, y=timecourses_TOC1$leaf..1)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = 0, ymax = 30, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = 0, ymax = 30, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=f1_line, linetype=f1_line), size = 1.5) +
  #annotate("text", x = 22.2, y = 2.15, label = "italic(TOC1)", size = 7, parse = TRUE)+
  scale_colour_manual(values=c(f1))+
  scale_linetype_manual(values=c(f1_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
  scale_y_continuous(breaks=seq(0,30,6), name="Fold-change", limits=c(0, 30))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position="none")

TOC1_E1 <- ggplot(data=timecourses_TOC1, aes(x=Hour, y=timecourses_TOC1$internode.1)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = 0, ymax = 30, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = 0, ymax = 30, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=f1_line, linetype=f1_line), size = 1.5) +
  #annotate("text", x = 22.2, y = 2.15, label = "italic(TOC1)", size = 7, parse = TRUE)+
  scale_colour_manual(values=c(e1))+
  scale_linetype_manual(values=c(e1_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
  scale_y_continuous(breaks=seq(0,30,6), name="Expression value", limits=c(0, 30))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        #axis.text.y = element_text(size=18),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none")

TOC1_E5 <- ggplot(data=timecourses_TOC1, aes(x=Hour, y=timecourses_TOC1$internode.5)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = 0, ymax = 30, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = 0, ymax = 30, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=e5_line, linetype=e5_line), size = 1.5) +
  scale_colour_manual(values=c(e5))+
  scale_linetype_manual(values=c(e5_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
  scale_y_continuous(breaks=seq(0,30,6), name="Expression value", limits=c(0, 30))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        #axis.text.y = element_text(size=18),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none")

plot_grid(LHY_L1, LHY_E1, LHY_E5, TOC1_L1, TOC1_E1, TOC1_E5, labels = c("a", "", "", "b", "", ""), ncol = 3, align = "v",label_size = 20)
