library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
library(png)
library(grid)
library(RColorBrewer)


f1 = "#00441b"
e1 = "#a20021"
e5 = "#fabc3c"
f1_line = "solid"
e1_line = "longdash"
e5_line = "dotdash"

#FigureS7A
timecourses_PHYA = read.csv("FigureS7_PHYA_timecourses.txt", sep="\t")
names(timecourses_PHYA) [names(timecourses_PHYA) == "leaf..1"] <- "leaf +1"
names(timecourses_PHYA) [names(timecourses_PHYA) == "internode.1"] <- "internode1"
names(timecourses_PHYA) [names(timecourses_PHYA) == "internode.5"] <- "internode5"

timecourses2_PHYA <- melt(timecourses_PHYA, id.vars="Hour")
names(timecourses2_PHYA) [names(timecourses2_PHYA) == "value"] <- "Expression"
names(timecourses2_PHYA) [names(timecourses2_PHYA) == "variable"] <- "Organ"

PHYA <- ggplot(data=timecourses2_PHYA, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(PHYA).1", size = 7, parse = TRUE)+
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


#FigureS7B
timecourses_PHYB = read.csv("FigureS7_PHYB_timecourses.txt", sep="\t")
names(timecourses_PHYB) [names(timecourses_PHYB) == "leaf..1"] <- "leaf +1"
names(timecourses_PHYB) [names(timecourses_PHYB) == "internode.1"] <- "internode1"
names(timecourses_PHYB) [names(timecourses_PHYB) == "internode.5"] <- "internode5"

timecourses2_PHYB <- melt(timecourses_PHYB, id.vars="Hour")
names(timecourses2_PHYB) [names(timecourses2_PHYB) == "value"] <- "Expression"
names(timecourses2_PHYB) [names(timecourses2_PHYB) == "variable"] <- "Organ"

PHYB <- ggplot(data=timecourses2_PHYB, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(PHYB)", size = 7, parse = TRUE)+
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


#FigureS7C
timecourses_ELF3 = read.csv("FigureS7_ELF3_timecourses.txt", sep="\t")
names(timecourses_ELF3) [names(timecourses_ELF3) == "leaf..1"] <- "leaf +1"
names(timecourses_ELF3) [names(timecourses_ELF3) == "internode.1"] <- "internode1"
names(timecourses_ELF3) [names(timecourses_ELF3) == "internode.5"] <- "internode5"

timecourses2_ELF3 <- melt(timecourses_ELF3, id.vars="Hour")
names(timecourses2_ELF3) [names(timecourses2_ELF3) == "value"] <- "Expression"
names(timecourses2_ELF3) [names(timecourses2_ELF3) == "variable"] <- "Organ"

ELF3 <- ggplot(data=timecourses2_ELF3, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(ELF3)", size = 7, parse = TRUE)+
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

#FigureS7D
timecourses_CRY1 = read.csv("FigureS7_CRY1_timecourses.txt", sep="\t")
names(timecourses_CRY1) [names(timecourses_CRY1) == "leaf..1"] <- "leaf +1"
names(timecourses_CRY1) [names(timecourses_CRY1) == "internode.1"] <- "internode1"
names(timecourses_CRY1) [names(timecourses_CRY1) == "internode.5"] <- "internode5"

timecourses2_CRY1 <- melt(timecourses_CRY1, id.vars="Hour")
names(timecourses2_CRY1) [names(timecourses2_CRY1) == "value"] <- "Expression"
names(timecourses2_CRY1) [names(timecourses2_CRY1) == "variable"] <- "Organ"

CRY1 <- ggplot(data=timecourses2_CRY1, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(CRY1).1", size = 7, parse = TRUE)+
  scale_colour_manual(values=c(f1,e1,e5))+
  scale_linetype_manual(values=c(f1_line, e1_line, e5_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Expression value", limits=c(-3, 3))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        axis.text.y = element_text(size=18),
        legend.position="none")


#FigureS7E
timecourses_CRY2 = read.csv("FigureS7_CRY2_timecourses.txt", sep="\t")
names(timecourses_CRY2) [names(timecourses_CRY2) == "leaf..1"] <- "leaf +1"
names(timecourses_CRY2) [names(timecourses_CRY2) == "internode.1"] <- "internode1"
names(timecourses_CRY2) [names(timecourses_CRY2) == "internode.5"] <- "internode5"

timecourses2_CRY2 <- melt(timecourses_CRY2, id.vars="Hour")
names(timecourses2_CRY2) [names(timecourses2_CRY2) == "value"] <- "Expression"
names(timecourses2_CRY2) [names(timecourses2_CRY2) == "variable"] <- "Organ"

CRY2 <- ggplot(data=timecourses2_CRY2, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(CRY2).1", size = 7, parse = TRUE)+
  scale_colour_manual(values=c(f1,e1,e5))+
  scale_linetype_manual(values=c(f1_line, e1_line, e5_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Expression value", limits=c(-3, 3))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        #axis.text.y = element_text(size=18),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none")


#FigureS7F
timecourses_ZTL = read.csv("FigureS7_ZTL_timecourses.txt", sep="\t")
names(timecourses_ZTL) [names(timecourses_ZTL) == "leaf..1"] <- "leaf +1"
names(timecourses_ZTL) [names(timecourses_ZTL) == "internode.1"] <- "internode1"
names(timecourses_ZTL) [names(timecourses_ZTL) == "internode.5"] <- "internode5"

timecourses2_ZTL <- melt(timecourses_ZTL, id.vars="Hour")
names(timecourses2_ZTL) [names(timecourses2_ZTL) == "value"] <- "Expression"
names(timecourses2_ZTL) [names(timecourses2_ZTL) == "variable"] <- "Organ"

ZTL <- ggplot(data=timecourses2_ZTL, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(ZTL).1", size = 7, parse = TRUE)+
  scale_colour_manual(values=c(f1,e1,e5))+
  scale_linetype_manual(values=c(f1_line, e1_line, e5_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="Expression value", limits=c(-3, 3))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=18), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none")


plot_grid(PHYA, PHYB, ELF3, CRY1, CRY2, ZTL, labels = c("A", "B", "C","D", "E", "F"), ncol = 3, align = "v", rel_widths = c(1, 1, 1),label_size = 20)

