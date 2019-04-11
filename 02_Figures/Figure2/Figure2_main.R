library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
library(png)
library(grid)
library(RColorBrewer)


#Figure 2A
img <- readPNG("Figure2A.png")
g <- rasterGrob(img, interpolate=TRUE)
circular_heatmap <- ggplot() + geom_blank() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

#Figure 2B
ritmicos = read.csv("Figure2B.txt", sep="\t")
ritmicos$organ = factor(ritmicos$organ, levels=c("rhythmic in L1", "rhythmic in I1", "rhythmic in I5"))
ritmicos$group = factor(ritmicos$group, levels=c("L1_all", "L1", "L1_L1I1", "L1_L1I5", "L1_L1I1I5", "I1_all", "I1", "I1_I1I5", "I1_L1I1", "I1_L1I1I5","I5_all", "I5", "I5_I1I5", "I5_L1I5", "I5_L1I1I5"))

overlaps <- ggplot(ritmicos, aes(x=group, y=rhythmic, color=organ)) +
  scale_colour_manual(values=c(L1, I1, I5))+
  geom_segment( aes(x=group, xend=group, y=0, yend=rhythmic), color = "#969696", size = 1) +
  geom_point(size=4) +
  scale_x_discrete(labels=c("I1_L1I1I5" = "L1I1I5", "I1_L1I1" = "L1I1", "I1_I1I5" = "I1I5", "I1_all" = "all", "I5_L1I1I5" = "L1I1I5", "I5_L1I5" = "L1I5", "I5_I1I5" = "I1I5", "I5_all" = "all", "L1_L1I1I5" = "L1I1I5", "L1_L1I1" = "L1I1", "L1_L1I5" = "L1I5", "L1_all" = "all"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,75), breaks=seq(0,70, by=10))+
  labs(y = "% of rhythmic")+
  facet_wrap(~ organ, scales = "free_x", ncol = 3)+
  theme(
    text = element_text(size=16, color="black"), 
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#e5e5e5", size = 0.5),
    panel.border = element_blank(),
    axis.title.x = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    strip.background = element_blank()
  ) 
overlaps 

#Figure 2C
diel_circadian <- read.csv("Figure2C.txt", sep="\t")

f1 = "#00441b"
e1 = "#a20021"
e5 = "#fabc3c"
f1_line = "solid"
e1_line = "longdash"
e5_line = "dotdash"

phase_d_c <- ggplot(data=diel_circadian , aes(x=ZT, y=Timecourses, group=Group)) +
  annotate("rect", xmin = -2, xmax = 0, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  annotate("rect", xmin = 13.5, xmax = 24, ymin = -3, ymax = 3, alpha = .5, fill = "#e3e3e3")+
  geom_line(aes(col=Group, linetype=Group), size = 1.5) +
  scale_colour_manual(values=c("#73827a","#349b5f"))+
  scale_linetype_manual(values=c(f1_line, e1_line, e5_line)) +
  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
  scale_y_continuous(breaks=seq(0,900,150), name="Counts", limits=c(0, 900))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
        text = element_text(size=16), 
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        legend.position = "none"
  )

#Figure 2D
img <- readPNG("Figure2D.png")
g <- rasterGrob(img, interpolate=TRUE)
enrichment_heatmap <- ggplot() + geom_blank() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

#Figure 2E
img <- readPNG("Figure2E.png")
g <- rasterGrob(img, interpolate=TRUE)
annotation_heatmaps <- ggplot() + geom_blank() + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)



Figure2BC <- plot_grid(overlaps, phase_d_c, labels = c("b", "c"), ncol = 1, align = "v", label_size = 20)

plot_grid(circular_heatmap, Figure2BC, enrichment_heatmap, annotation_heatmaps, labels = c("a", "", "d","e"), ncol = 2, align = "v", rel_widths = c(1, 1, 1),label_size = 20)
