library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
library(png)
library(grid)
library(RColorBrewer)

#Figure 1A
ritmicos = read.csv("Figure_1A.txt", sep="\t")
ritmicos$Organ <- factor(ritmicos$Organ, levels = c("leaf +1 circadian", "leaf +1", "internode 1/2", "internode 5"))
names(ritmicos) [names(ritmicos) == "expressed"] <- "expressed  "
names(ritmicos) [names(ritmicos) == "rhythmic"] <- "rhythmic"

rhythmic <- ggplot(data=ritmicos, aes(x=Organ, y=Count, fill=Type)) +
  geom_rect(data = ritmicos, aes(x = Organ, y=Count), xmin = as.numeric(ritmicos$Organ[[1]]) - 0.52,
            xmax = as.numeric(ritmicos$Organ[[1]]) + 0.52,
            ymin = 50, ymax = 14600, fill="transparent", linetype= 2, colour="#969696", size = 1)+
  geom_bar(stat="identity", position=position_dodge(), width=0.7)+ 
  scale_fill_manual(values=c("#383838", "#bdbdbd"))+
  scale_x_discrete(labels=c("leaf +1 circadian" = "L1", "leaf +1" = "L1", "internode 1/2" = "I1","internode 5" = "I5"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,15000), breaks=seq(0,12000, by=2000), name="Counts")+
  theme(text = element_text(size=18), 
        legend.title = element_blank(), 
        legend.position="right", 
        axis.ticks = element_blank(),
        legend.spacing.x = unit(0.05, 'cm'),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        panel.grid.major.y = element_line(colour = "#e5e5e5", size = 0.5),
        panel.grid.major.x = element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,-5,-5),
        axis.title = element_text(face="bold", size=14),
        axis.title.x = element_blank()
  ) +
  annotate("text", x = 1, y = 14000, label = "circadian", size=7)+
  annotate("text", x = 1, y = 13000, label = "32.1%", size=7)+
  annotate("text", x = 2, y = 13000, label = "68.3%", size=7)+
  annotate("text", x = 3, y = 13000, label = "31.2%", size=7)+
  annotate("text", x = 4, y = 13000, label = "28.8%", size=7)


#Figure 1B
img <- readPNG("Figure_1B_euler_expressed_circadian.png")
g <- rasterGrob(img, interpolate=TRUE)
euler_expressed_circadian <- ggplot() + geom_blank() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

#Figure 1C
img <- readPNG("Figure_1C_euler_rhythmic_circadian.png")
g <- rasterGrob(img, interpolate=TRUE)
euler_rhythmic_circadian <- ggplot() + geom_blank() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

#Figure 1D
histograma <- read.csv("Figure_1D.txt", sep="\t")
histograma$column <- factor(histograma$column, levels = c("expressed", "rhythmic", "same phase"))
histograma$category <- factor(histograma$category, levels = c("others","L1 only", "I1 only", "I5 only", "L1I1", "L1I5", "I1I5", "all"))

L1 = "#00441b"
I1 = "#a20021"
I5 = "#fabc3c"
L1I1 = "#5a5096"
L1I5 = "#7D802B"
I1I5 = "#CE5E2E"

comparison <- ggplot(data = histograma, aes(y = Numbers, x = column, fill = category)) + 
  geom_bar(stat="identity", width=0.5)+
  geom_segment(aes(x=as.numeric(histograma$column[[1]])+0.25, y=12501, xend=as.numeric(histograma$column[[1]])+0.75, yend=8514), linetype= 2, colour="#969696", size = 1)+
  geom_segment(aes(x=as.numeric(histograma$column[[1]])+1.25, y=3582, xend=as.numeric(histograma$column[[1]])+1.75, yend=2946), linetype= 2, colour="#969696", size = 1)+
  geom_segment(aes(x=as.numeric(histograma$column[[1]])+0.25, y=9380, xend=as.numeric(histograma$column[[1]])+0.75, yend=7591), linetype= 2, colour="#969696", size = 1)+
  geom_segment(aes(x=as.numeric(histograma$column[[1]])+1.25, y=1413, xend=as.numeric(histograma$column[[1]])+1.75, yend=1254), linetype= 2, colour="#969696", size = 1)+
  geom_segment(aes(x=as.numeric(histograma$column[[1]])+0.25, y=0, xend=as.numeric(histograma$column[[1]])+0.75, yend=0), linetype= 2, colour="#969696", size = 1)+
  geom_segment(aes(x=as.numeric(histograma$column[[1]])+1.25, y=0, xend=as.numeric(histograma$column[[1]])+1.75, yend=0), linetype= 2, colour="#969696", size = 1)+
  scale_fill_manual(values=c("#bdbdbd", L1, I1, I5, L1I1, L1I5, I1I5, "#0081AF"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,15000), breaks=seq(0,12000, by=2000), name="Counts")+
  theme(text = element_text(size=18), 
        legend.title = element_blank(), 
        legend.position="right", 
        axis.ticks = element_blank(),
        legend.spacing.x = unit(0.1, 'cm'),
        axis.line = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,-5,-5),
        panel.grid.major.y = element_line(colour = "#e5e5e5", size = 0.5),
        panel.grid.major.x = element_blank(),
        axis.title = element_text(face="bold"),
        axis.title.x = element_blank()
  )

#Figure 1E
img <- readPNG("Figure_1E_euler_expressed.png")
g <- rasterGrob(img, interpolate=TRUE)
euler_expressed <- ggplot() + geom_blank() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

#Figure 1F
img <- readPNG("Figure_1F_euler_rhythmic.png")
g <- rasterGrob(img, interpolate=TRUE)
euler_rhythmic <- ggplot() + geom_blank() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)


plot_grid(rhythmic, euler_expressed_circadian, euler_rhythmic_circadian, comparison, euler_expressed, euler_rhythmic, labels = c("a", "b", "c","d","e","f" ), ncol = 3, align = "v", rel_widths = c(1, 0.5,0.5),label_size = 20)

