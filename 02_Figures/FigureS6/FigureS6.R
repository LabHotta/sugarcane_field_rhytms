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

timecourses_SuSy = read.csv("FigureS6_SuSy_timecourses.txt", sep="\t")
names(timecourses_SuSy) [names(timecourses_SuSy) == "leaf..1"] <- "leaf +1"
names(timecourses_SuSy) [names(timecourses_SuSy) == "internode.1"] <- "internode1"
names(timecourses_SuSy) [names(timecourses_SuSy) == "internode.5"] <- "internode5"

timecourses2_SuSy <- melt(timecourses_SuSy, id.vars="Hour")
names(timecourses2_SuSy) [names(timecourses2_SuSy) == "value"] <- "Expression"
names(timecourses2_SuSy) [names(timecourses2_SuSy) == "variable"] <- "Organ"

SuSy <- ggplot(data=timecourses2_SuSy, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(SuSy4)", size = 7, parse = TRUE)+
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

timecourses_SPSII = read.csv("FigureS6_SPSII_timecourses.txt", sep="\t")
names(timecourses_SPSII) [names(timecourses_SPSII) == "leaf..1"] <- "leaf +1"
names(timecourses_SPSII) [names(timecourses_SPSII) == "internode.1"] <- "internode1"
names(timecourses_SPSII) [names(timecourses_SPSII) == "internode.5"] <- "internode5"

timecourses2_SPSII <- melt(timecourses_SPSII, id.vars="Hour")
names(timecourses2_SPSII) [names(timecourses2_SPSII) == "value"] <- "Expression"
names(timecourses2_SPSII) [names(timecourses2_SPSII) == "variable"] <- "Organ"

SPSII <- ggplot(data=timecourses2_SPSII, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(SPSII)", size = 7, parse = TRUE)+
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

timecourses_INV = read.csv("FigureS6_INV_timecourses.txt", sep="\t")
names(timecourses_INV) [names(timecourses_INV) == "leaf..1"] <- "leaf +1"
names(timecourses_INV) [names(timecourses_INV) == "internode.1"] <- "internode1"
names(timecourses_INV) [names(timecourses_INV) == "internode.5"] <- "internode5"

timecourses2_INV <- melt(timecourses_INV, id.vars="Hour")
names(timecourses2_INV) [names(timecourses2_INV) == "value"] <- "Expression"
names(timecourses2_INV) [names(timecourses2_INV) == "variable"] <- "Organ"

INV <- ggplot(data=timecourses2_INV, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(INV)", size = 7, parse = TRUE)+
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

timecourses_SWEET2 = read.csv("FigureS6_SWEET2_timecourses.txt", sep="\t")
names(timecourses_SWEET2) [names(timecourses_SWEET2) == "leaf..1"] <- "leaf +1"
names(timecourses_SWEET2) [names(timecourses_SWEET2) == "internode.1"] <- "internode1"
names(timecourses_SWEET2) [names(timecourses_SWEET2) == "internode.5"] <- "internode5"

timecourses2_SWEET2 <- melt(timecourses_SWEET2, id.vars="Hour")
names(timecourses2_SWEET2) [names(timecourses2_SWEET2) == "value"] <- "Expression"
names(timecourses2_SWEET2) [names(timecourses2_SWEET2) == "variable"] <- "Organ"

SWEET2 <- ggplot(data=timecourses2_SWEET2, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(SWEET2)", size = 7, parse = TRUE)+
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

timecourses_SWEET11 = read.csv("FigureS6_SWEET11_timecourses.txt", sep="\t")
names(timecourses_SWEET11) [names(timecourses_SWEET11) == "leaf..1"] <- "leaf +1"
names(timecourses_SWEET11) [names(timecourses_SWEET11) == "internode.1"] <- "internode1"
names(timecourses_SWEET11) [names(timecourses_SWEET11) == "internode.5"] <- "internode5"

timecourses2_SWEET11 <- melt(timecourses_SWEET11, id.vars="Hour")
names(timecourses2_SWEET11) [names(timecourses2_SWEET11) == "value"] <- "Expression"
names(timecourses2_SWEET11) [names(timecourses2_SWEET11) == "variable"] <- "Organ"

SWEET11 <- ggplot(data=timecourses2_SWEET11, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(SWEET11)", size = 7, parse = TRUE)+
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

timecourses_SUT2 = read.csv("FigureS6_SUT2_timecourses.txt", sep="\t")
names(timecourses_SUT2) [names(timecourses_SUT2) == "leaf..1"] <- "leaf +1"
names(timecourses_SUT2) [names(timecourses_SUT2) == "internode.1"] <- "internode1"
names(timecourses_SUT2) [names(timecourses_SUT2) == "internode.5"] <- "internode5"

timecourses2_SUT2 <- melt(timecourses_SUT2, id.vars="Hour")
names(timecourses2_SUT2) [names(timecourses2_SUT2) == "value"] <- "Expression"
names(timecourses2_SUT2) [names(timecourses2_SUT2) == "variable"] <- "Organ"

SUT2 <- ggplot(data=timecourses2_SUT2, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(SUT2)", size = 7, parse = TRUE)+
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

timecourses_COMT = read.csv("FigureS6_COMT_timecourses.txt", sep="\t")
names(timecourses_COMT) [names(timecourses_COMT) == "leaf..1"] <- "leaf +1"
names(timecourses_COMT) [names(timecourses_COMT) == "internode.1"] <- "internode1"
names(timecourses_COMT) [names(timecourses_COMT) == "internode.5"] <- "internode5"

timecourses2_COMT <- melt(timecourses_COMT, id.vars="Hour")
names(timecourses2_COMT) [names(timecourses2_COMT) == "value"] <- "Expression"
names(timecourses2_COMT) [names(timecourses2_COMT) == "variable"] <- "Organ"

COMT <- ggplot(data=timecourses2_COMT, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(COMT)", size = 7, parse = TRUE)+
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

timecourses_PAL = read.csv("FigureS6_PAL_timecourses.txt", sep="\t")
names(timecourses_PAL) [names(timecourses_PAL) == "leaf..1"] <- "leaf +1"
names(timecourses_PAL) [names(timecourses_PAL) == "internode.1"] <- "internode1"
names(timecourses_PAL) [names(timecourses_PAL) == "internode.5"] <- "internode5"

timecourses2_PAL <- melt(timecourses_PAL, id.vars="Hour")
names(timecourses2_PAL) [names(timecourses2_PAL) == "value"] <- "Expression"
names(timecourses2_PAL) [names(timecourses2_PAL) == "variable"] <- "Organ"

PAL <- ggplot(data=timecourses2_PAL, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(PAL)", size = 7, parse = TRUE)+
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

timecourses_EXP = read.csv("FigureS6_EXP_timecourses.txt", sep="\t")
names(timecourses_EXP) [names(timecourses_EXP) == "leaf..1"] <- "leaf +1"
names(timecourses_EXP) [names(timecourses_EXP) == "internode.1"] <- "internode1"
names(timecourses_EXP) [names(timecourses_EXP) == "internode.5"] <- "internode5"

timecourses2_EXP <- melt(timecourses_EXP, id.vars="Hour")
names(timecourses2_EXP) [names(timecourses2_EXP) == "value"] <- "Expression"
names(timecourses2_EXP) [names(timecourses2_EXP) == "variable"] <- "Organ"

EXP <- ggplot(data=timecourses2_EXP, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(EXP)", size = 7, parse = TRUE)+
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

timecourses_CM = read.csv("FigureS6_CM_timecourses.txt", sep="\t")
names(timecourses_CM) [names(timecourses_CM) == "leaf..1"] <- "leaf +1"
names(timecourses_CM) [names(timecourses_CM) == "internode.1"] <- "internode1"
names(timecourses_CM) [names(timecourses_CM) == "internode.5"] <- "internode5"

timecourses2_CM <- melt(timecourses_CM, id.vars="Hour")
names(timecourses2_CM) [names(timecourses2_CM) == "value"] <- "CMression"
names(timecourses2_CM) [names(timecourses2_CM) == "variable"] <- "Organ"

CM <- ggplot(data=timecourses2_CM, aes(x=Hour, y=CMression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(CM)", size = 7, parse = TRUE)+
  scale_colour_manual(values=c(f1,e1,e5))+
  scale_linetype_manual(values=c(f1_line, e1_line, e5_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="CMression value", limits=c(-3, 3))+
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

timecourses_P4H = read.csv("FigureS6_P4H_timecourses.txt", sep="\t")
names(timecourses_P4H) [names(timecourses_P4H) == "leaf..1"] <- "leaf +1"
names(timecourses_P4H) [names(timecourses_P4H) == "internode.1"] <- "internode1"
names(timecourses_P4H) [names(timecourses_P4H) == "internode.5"] <- "internode5"

timecourses2_P4H <- melt(timecourses_P4H, id.vars="Hour")
names(timecourses2_P4H) [names(timecourses2_P4H) == "value"] <- "P4Hression"
names(timecourses2_P4H) [names(timecourses2_P4H) == "variable"] <- "Organ"

P4H <- ggplot(data=timecourses2_P4H, aes(x=Hour, y=P4Hression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(P4H)", size = 7, parse = TRUE)+
  scale_colour_manual(values=c(f1,e1,e5))+
  scale_linetype_manual(values=c(f1_line, e1_line, e5_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="P4Hression value", limits=c(-3, 3))+
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

timecourses_GDH2 = read.csv("FigureS6_GDH2_timecourses.txt", sep="\t")
names(timecourses_GDH2) [names(timecourses_GDH2) == "leaf..1"] <- "leaf +1"
names(timecourses_GDH2) [names(timecourses_GDH2) == "internode.1"] <- "internode1"
names(timecourses_GDH2) [names(timecourses_GDH2) == "internode.5"] <- "internode5"

timecourses2_GDH2 <- melt(timecourses_GDH2, id.vars="Hour")
names(timecourses2_GDH2) [names(timecourses2_GDH2) == "value"] <- "GDH2ression"
names(timecourses2_GDH2) [names(timecourses2_GDH2) == "variable"] <- "Organ"

GDH2 <- ggplot(data=timecourses2_GDH2, aes(x=Hour, y=GDH2ression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(GDH2)", size = 7, parse = TRUE)+
  scale_colour_manual(values=c(f1,e1,e5))+
  scale_linetype_manual(values=c(f1_line, e1_line, e5_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="GDH2ression value", limits=c(-3, 3))+
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


img <- readPNG("FigureS6_Carbs_expressed.png")
g <- rasterGrob(img, interpolate=TRUE)
Carbs_expressed <- ggplot() + geom_blank() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

img <- readPNG("FigureS6_Carbs_rhythmic.png")
g <- rasterGrob(img, interpolate=TRUE)
Carbs_rhythmic <- ggplot() + geom_blank() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

img <- readPNG("FigureS6_CellWall_expressed.png")
g <- rasterGrob(img, interpolate=TRUE)
CellWall_expressed <- ggplot() + geom_blank() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

img <- readPNG("FigureS6_CellWall_rhythmic.png")
g <- rasterGrob(img, interpolate=TRUE)
CellWall_rhythmic <- ggplot() + geom_blank() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

img <- readPNG("FigureS6_AA_expressed.png")
g <- rasterGrob(img, interpolate=TRUE)
AA_expressed <- ggplot() + geom_blank() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

img <- readPNG("FigureS6_AA_rhythmic.png")
g <- rasterGrob(img, interpolate=TRUE)
AA_rhythmic <- ggplot() + geom_blank() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

img <- readPNG("FigureS6_Transporters_expressed.png")
g <- rasterGrob(img, interpolate=TRUE)
Transporters_expressed <- ggplot() + geom_blank() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

img <- readPNG("FigureS6_Transporters_rhythmic.png")
g <- rasterGrob(img, interpolate=TRUE)
Transporters_rhythmic <- ggplot() + geom_blank() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)


plot_grid(Carbs_expressed, CellWall_expressed, AA_expressed, Transporters_expressed, Carbs_rhythmic,  CellWall_rhythmic, AA_rhythmic, Transporters_rhythmic, SuSy, COMT, CM, SWEET2, SPSII, PAL, P4H, SWEET11, INV, EXP, GDH2, SUT2, labels = c("a", "b", "c","d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t"), ncol = 4, align = "v", rel_widths = c(1, 1, 1,1),label_size = 20)
