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

timecourses_JMJD51 = read.csv("FigureS9_JMJD51_timecourses.txt", sep="\t")
names(timecourses_JMJD51) [names(timecourses_JMJD51) == "leaf..1"] <- "leaf +1"
names(timecourses_JMJD51) [names(timecourses_JMJD51) == "internode.1"] <- "internode1"
names(timecourses_JMJD51) [names(timecourses_JMJD51) == "internode.5"] <- "internode5"

timecourses2_JMJD51 <- melt(timecourses_JMJD51, id.vars="Hour")
names(timecourses2_JMJD51) [names(timecourses2_JMJD51) == "value"] <- "Expression"
names(timecourses2_JMJD51) [names(timecourses2_JMJD51) == "variable"] <- "Organ"

JMJD51 <- ggplot(data=timecourses2_JMJD51, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(JMJD5.1)", size = 7, parse = TRUE)+
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
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank(),
        legend.position="none")

timecourses_JMJD52 = read.csv("FigureS9_JMJD52_timecourses.txt", sep="\t")
names(timecourses_JMJD52) [names(timecourses_JMJD52) == "leaf..1"] <- "leaf +1"
names(timecourses_JMJD52) [names(timecourses_JMJD52) == "internode.1"] <- "internode1"
names(timecourses_JMJD52) [names(timecourses_JMJD52) == "internode.5"] <- "internode5"

timecourses2_JMJD52 <- melt(timecourses_JMJD52, id.vars="Hour")
names(timecourses2_JMJD52) [names(timecourses2_JMJD52) == "value"] <- "Expression"
names(timecourses2_JMJD52) [names(timecourses2_JMJD52) == "variable"] <- "Organ"

JMJD52 <- ggplot(data=timecourses2_JMJD52, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(JMJD5.2)", size = 7, parse = TRUE)+
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

timecourses_H2A = read.csv("FigureS9_H2A_timecourses.txt", sep="\t")
names(timecourses_H2A) [names(timecourses_H2A) == "leaf..1"] <- "leaf +1"
names(timecourses_H2A) [names(timecourses_H2A) == "internode.1"] <- "internode1"
names(timecourses_H2A) [names(timecourses_H2A) == "internode.5"] <- "internode5"

timecourses2_H2A <- melt(timecourses_H2A, id.vars="Hour")
names(timecourses2_H2A) [names(timecourses2_H2A) == "value"] <- "Expression"
names(timecourses2_H2A) [names(timecourses2_H2A) == "variable"] <- "Organ"

H2A <- ggplot(data=timecourses2_H2A, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(H2A)", size = 7, parse = TRUE)+
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

timecourses_FBH1 = read.csv("FigureS9_FBH1_timecourses.txt", sep="\t")
names(timecourses_FBH1) [names(timecourses_FBH1) == "leaf..1"] <- "leaf +1"
names(timecourses_FBH1) [names(timecourses_FBH1) == "internode.1"] <- "internode1"
names(timecourses_FBH1) [names(timecourses_FBH1) == "internode.5"] <- "internode5"

timecourses2_FBH1 <- melt(timecourses_FBH1, id.vars="Hour")
names(timecourses2_FBH1) [names(timecourses2_FBH1) == "value"] <- "Expression"
names(timecourses2_FBH1) [names(timecourses2_FBH1) == "variable"] <- "Organ"

FBH1 <- ggplot(data=timecourses2_FBH1, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(FBH1)", size = 7, parse = TRUE)+
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

timecourses_bZIP4 = read.csv("FigureS9_bZIP4_timecourses.txt", sep="\t")
names(timecourses_bZIP4) [names(timecourses_bZIP4) == "leaf..1"] <- "leaf +1"
names(timecourses_bZIP4) [names(timecourses_bZIP4) == "internode.1"] <- "internode1"
names(timecourses_bZIP4) [names(timecourses_bZIP4) == "internode.5"] <- "internode5"

timecourses2_bZIP4 <- melt(timecourses_bZIP4, id.vars="Hour")
names(timecourses2_bZIP4) [names(timecourses2_bZIP4) == "value"] <- "Expression"
names(timecourses2_bZIP4) [names(timecourses2_bZIP4) == "variable"] <- "Organ"

bZIP4 <- ggplot(data=timecourses2_bZIP4, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(bZIP4)", size = 7, parse = TRUE)+
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

timecourses_L27 = read.csv("FigureS9_L27_timecourses.txt", sep="\t")
names(timecourses_L27) [names(timecourses_L27) == "leaf..1"] <- "leaf +1"
names(timecourses_L27) [names(timecourses_L27) == "internode.1"] <- "internode1"
names(timecourses_L27) [names(timecourses_L27) == "internode.5"] <- "internode5"

timecourses2_L27 <- melt(timecourses_L27, id.vars="Hour")
names(timecourses2_L27) [names(timecourses2_L27) == "value"] <- "Expression"
names(timecourses2_L27) [names(timecourses2_L27) == "variable"] <- "Organ"

RPL27 <- ggplot(data=timecourses2_L27, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(RPL27)", size = 7, parse = TRUE)+
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

timecourses_S1 = read.csv("FigureS9_RPS1_timecourses.txt", sep="\t")
names(timecourses_S1) [names(timecourses_S1) == "leaf..1"] <- "leaf +1"
names(timecourses_S1) [names(timecourses_S1) == "internode.1"] <- "internode1"
names(timecourses_S1) [names(timecourses_S1) == "internode.5"] <- "internode5"

timecourses2_S1 <- melt(timecourses_S1, id.vars="Hour")
names(timecourses2_S1) [names(timecourses2_S1) == "value"] <- "Expression"
names(timecourses2_S1) [names(timecourses2_S1) == "variable"] <- "Organ"

RPS1 <- ggplot(data=timecourses2_S1, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(RPS1)", size = 7, parse = TRUE)+
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


#plot_grid(Carbs_expressed, CellWall_expressed, Carbs_rhythmic,  CellWall_rhythmic,  ,  labels = c("a", "b", "c","d", "e", "f", "g", "h", "i"), ncol = 4, align = "v", rel_widths = c(1, 1,1,1),label_size = 20)
plot_grid(JMJD51, FBH1, RPL27, JMJD52, bZIP4, RPS1, labels = c("a", "b", "c","d", "e", "f"), ncol = 3, align = "v", rel_widths = c(1, 1, 1),label_size = 20)

