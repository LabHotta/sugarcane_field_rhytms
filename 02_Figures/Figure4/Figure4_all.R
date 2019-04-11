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

#Figure4ABC
img <- readPNG("Figure4ABC.png")
g <- rasterGrob(img, interpolate=TRUE)
Circlize_heatmaps <- ggplot() + geom_blank() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

#Figure4D
img <- readPNG("Figure4D_Chromatin_expressed.png")
g <- rasterGrob(img, interpolate=TRUE)
Chromatin_expressed <- ggplot() + geom_blank() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

#Figure4E
img <- readPNG("Figure4E_TF_expressed.png")
g <- rasterGrob(img, interpolate=TRUE)
TF_expressed <- ggplot() + geom_blank() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

#Figure4F
img <- readPNG("Figure4F_Protein_expressed.png")
g <- rasterGrob(img, interpolate=TRUE)
Protein_expressed <- ggplot() + geom_blank() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

#Figure4G
img <- readPNG("Figure4G_Chromatin_rhythmic.png")
g <- rasterGrob(img, interpolate=TRUE)
Chromatin_rhythmic <- ggplot() + geom_blank() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

#Figure4H
img <- readPNG("Figure4H_TF_rhythmic.png")
g <- rasterGrob(img, interpolate=TRUE)
TF_rhythmic <- ggplot() + geom_blank() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

#Figure4I
img <- readPNG("Figure4I_Protein_rhythmic.png")
g <- rasterGrob(img, interpolate=TRUE)
Protein_rhythmic <- ggplot() + geom_blank() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

#Figure4J
timecourses_SMC1 = read.csv("Figure4J_SMC1_timecourses.txt", sep="\t")
names(timecourses_SMC1) [names(timecourses_SMC1) == "leaf..1"] <- "leaf +1"
names(timecourses_SMC1) [names(timecourses_SMC1) == "internode.1"] <- "internode1"
names(timecourses_SMC1) [names(timecourses_SMC1) == "internode.5"] <- "internode5"

timecourses2_SMC1 <- melt(timecourses_SMC1, id.vars="Hour")
names(timecourses2_SMC1) [names(timecourses2_SMC1) == "value"] <- "Expression"
names(timecourses2_SMC1) [names(timecourses2_SMC1) == "variable"] <- "Organ"

SMC1 <- ggplot(data=timecourses2_SMC1, aes(x=Hour, y=Expression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(SMC1)", size = 7, parse = TRUE)+
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
        #axis.text.y = element_blank(),
        #axis.title.y = element_blank(),
        legend.position="none")

#Figure4K
timecourses_HB24 = read.csv("Figure4K_HB24_timecourses.txt", sep="\t")
names(timecourses_HB24) [names(timecourses_HB24) == "leaf..1"] <- "leaf +1"
names(timecourses_HB24) [names(timecourses_HB24) == "internode.1"] <- "internode1"
names(timecourses_HB24) [names(timecourses_HB24) == "internode.5"] <- "internode5"

timecourses2_HB24 <- melt(timecourses_HB24, id.vars="Hour")
names(timecourses2_HB24) [names(timecourses2_HB24) == "value"] <- "HB24ression"
names(timecourses2_HB24) [names(timecourses2_HB24) == "variable"] <- "Organ"

HB24 <- ggplot(data=timecourses2_HB24, aes(x=Hour, y=HB24ression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(HB24)", size = 7, parse = TRUE)+
  scale_colour_manual(values=c(f1,e1,e5))+
  scale_linetype_manual(values=c(f1_line, e1_line, e5_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="HB24ression value", limits=c(-3, 3))+
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

#Figure4L
timecourses_S15A = read.csv("Figure4L_S15A_timecourses.txt", sep="\t")
names(timecourses_S15A) [names(timecourses_S15A) == "leaf..1"] <- "leaf +1"
names(timecourses_S15A) [names(timecourses_S15A) == "internode.1"] <- "internode1"
names(timecourses_S15A) [names(timecourses_S15A) == "internode.5"] <- "internode5"

timecourses2_S15A <- melt(timecourses_S15A, id.vars="Hour")
names(timecourses2_S15A) [names(timecourses2_S15A) == "value"] <- "S15Aression"
names(timecourses2_S15A) [names(timecourses2_S15A) == "variable"] <- "Organ"

S15A <- ggplot(data=timecourses2_S15A, aes(x=Hour, y=S15Aression, group=Organ)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Organ, linetype=Organ), size = 1.5) +
  annotate("text", x = 21, y = 2.7, label = "italic(S15A)", size = 7, parse = TRUE)+
  scale_colour_manual(values=c(f1,e1,e5))+
  scale_linetype_manual(values=c(f1_line, e1_line, e5_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
  scale_y_continuous(breaks=seq(-3,3,1.5), name="S15Aression value", limits=c(-3, 3))+
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


FigureDEF <- plot_grid(Chromatin_expressed, TF_expressed , Protein_expressed, labels = c("d", "e", "f"), ncol = 3, align = "v", rel_widths = c(1, 1, 1),label_size = 20)
FigureGHI <- plot_grid(Chromatin_rhythmic, TF_rhythmic , Protein_rhythmic, labels = c("g", "h", "i"), ncol = 3, align = "v", rel_widths = c(1, 1, 1),label_size = 20)
FigureJKL <- plot_grid(SMC1, HB24, S15A, ncol = 3, align = "v", labels = c("j", "k", "l"), rel_widths = c(1, 1, 1),label_size = 20)



plot_grid(Circlize_heatmaps, FigureDEF, FigureGHI, FigureJKL, ncol = 1, align = "v", label_size = 20)
