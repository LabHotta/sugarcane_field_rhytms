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

timecourses_LHY = read.csv("Figure3_LHY_timecourses.txt", sep="\t")
names(timecourses_LHY) [names(timecourses_LHY) == "leaf..1"] <- "leaf +1"
names(timecourses_LHY) [names(timecourses_LHY) == "internode.1"] <- "internode1"
names(timecourses_LHY) [names(timecourses_LHY) == "internode.5"] <- "internode5"

timecourses2_LHY <- melt(timecourses_LHY, id.vars="Hour")
names(timecourses2_LHY) [names(timecourses2_LHY) == "value"] <- "Expression"
names(timecourses2_LHY) [names(timecourses2_LHY) == "variable"] <- "Organ"

LHY <- ggplot(data=timecourses2_LHY, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(LHY)", size = 7, parse = TRUE)+
  scale_colour_manual(values=c(f1,e1,e5))+
  scale_linetype_manual(values=c(f1_line, e1_line, e5_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Expression value", limits=c(-3, 3))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        #axis.text.x = element_text(size=18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        legend.position="none")


timecourses_TOC1 = read.csv("Figure3_TOC1_timecourses.txt", sep="\t")
names(timecourses_TOC1) [names(timecourses_TOC1) == "leaf..1"] <- "leaf +1"
names(timecourses_TOC1) [names(timecourses_TOC1) == "internode.1"] <- "internode1"
names(timecourses_TOC1) [names(timecourses_TOC1) == "internode.5"] <- "internode5"

timecourses2_TOC1 <- melt(timecourses_TOC1, id.vars="Hour")
names(timecourses2_TOC1) [names(timecourses2_TOC1) == "value"] <- "Expression"
names(timecourses2_TOC1) [names(timecourses2_TOC1) == "variable"] <- "Organ"

TOC1 <- ggplot(data=timecourses2_TOC1, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(TOC1)", size = 7, parse = TRUE)+
  scale_colour_manual(values=c(f1,e1,e5))+
  scale_linetype_manual(values=c(f1_line, e1_line, e5_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Expression value", limits=c(-3, 3))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position="none")

timecourses_GI = read.csv("Figure3_GI_timecourses.txt", sep="\t")
names(timecourses_GI) [names(timecourses_GI) == "leaf..1"] <- "leaf +1"
names(timecourses_GI) [names(timecourses_GI) == "internode.1"] <- "internode1"
names(timecourses_GI) [names(timecourses_GI) == "internode.5"] <- "internode5"

timecourses2_GI <- melt(timecourses_GI, id.vars="Hour")
names(timecourses2_GI) [names(timecourses2_GI) == "value"] <- "Expression"
names(timecourses2_GI) [names(timecourses2_GI) == "variable"] <- "Organ"


GI <- ggplot(data=timecourses2_GI, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(GI)", size = 7, parse = TRUE)+
  scale_colour_manual(values=c(f1,e1,e5))+
  scale_linetype_manual(values=c(f1_line, e1_line, e5_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Expression value", limits=c(-3, 3))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        #axis.text.y = element_text(size=18),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none")

timecourses_PRR73 = read.csv("Figure3_PRR73_timecourses.txt", sep="\t")
names(timecourses_PRR73) [names(timecourses_PRR73) == "leaf..1"] <- "leaf +1"
names(timecourses_PRR73) [names(timecourses_PRR73) == "internode.1"] <- "internode1"
names(timecourses_PRR73) [names(timecourses_PRR73) == "internode.5"] <- "internode5"

timecourses2_PRR73 <- melt(timecourses_PRR73, id.vars="Hour")
names(timecourses2_PRR73) [names(timecourses2_PRR73) == "value"] <- "Expression"
names(timecourses2_PRR73) [names(timecourses2_PRR73) == "variable"] <- "Organ"

PRR73 <- ggplot(data=timecourses2_PRR73, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(PRR73)", size = 7, parse = TRUE)+
  scale_colour_manual(values=c(f1,e1,e5))+
  scale_linetype_manual(values=c(f1_line, e1_line, e5_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Expression value", limits=c(-3, 3))+
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

timecourses_PRR59 = read.csv("Figure3_PRR59_timecourses.txt", sep="\t")
names(timecourses_PRR59) [names(timecourses_PRR59) == "leaf..1"] <- "leaf +1"
names(timecourses_PRR59) [names(timecourses_PRR59) == "internode.1"] <- "internode1"
names(timecourses_PRR59) [names(timecourses_PRR59) == "internode.5"] <- "internode5"

timecourses2_PRR59 <- melt(timecourses_PRR59, id.vars="Hour")
names(timecourses2_PRR59) [names(timecourses2_PRR59) == "value"] <- "Expression"
names(timecourses2_PRR59) [names(timecourses2_PRR59) == "variable"] <- "Organ"

PRR59 <- ggplot(data=timecourses2_PRR59, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(PRR59)", size = 7, parse = TRUE)+
  scale_colour_manual(values=c(f1,e1,e5))+
  scale_linetype_manual(values=c(f1_line, e1_line, e5_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Expression value", limits=c(-3, 3))+
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

timecourses_PRR95 = read.csv("Figure3_PRR95_timecourses.txt", sep="\t")
names(timecourses_PRR95) [names(timecourses_PRR95) == "leaf..1"] <- "leaf +1"
names(timecourses_PRR95) [names(timecourses_PRR95) == "internode.1"] <- "internode1"
names(timecourses_PRR95) [names(timecourses_PRR95) == "internode.5"] <- "internode5"

timecourses2_PRR95 <- melt(timecourses_PRR95, id.vars="Hour")
names(timecourses2_PRR95) [names(timecourses2_PRR95) == "value"] <- "Expression"
names(timecourses2_PRR95) [names(timecourses2_PRR95) == "variable"] <- "Organ"

PRR95 <- ggplot(data=timecourses2_PRR95, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(PRR95)", size = 7, parse = TRUE)+
  scale_colour_manual(values=c(f1,e1,e5))+
  scale_linetype_manual(values=c(f1_line, e1_line, e5_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Expression value", limits=c(-3, 3))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        #axis.text.y = element_text(size=18),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none")


plot_grid(LHY, PRR59, PRR73, TOC1, PRR95, GI, labels = c("a", "b", "c", "d", "e", "f"), ncol = 3, align = "v",label_size = 20)

