library("ComplexHeatmap")
library("circlize")
library(scales)
library(reshape2)
library(cowplot)


heatmap_l10 <- read.csv("FigureS3_heatmap_l1.txt", sep="\t")
heatmap_l1 <- heatmap_l10[,-1]
rownames(heatmap_l1) <- heatmap_l10[,1]

l1 <- Heatmap(heatmap_l1, 
        col = colorRamp2(c(-1.5,0,1.5), c("#d94801","#f7f7f7","#74add1")),
        name = "counts",
        #row_title = "Pathways",
        cluster_columns = FALSE,
        #clustering_distance_rows = "spearman",
        #clustering_distance_columns = "kendall",
        cluster_rows = FALSE,
        show_row_names = FALSE
        )

heatmap_i10 <- read.csv("FigureS3_heatmap_i1.txt", sep="\t")
heatmap_i1 <- heatmap_i10[,-1]
rownames(heatmap_i1) <- heatmap_i10[,1]

i1 <- Heatmap(heatmap_i1, 
        col = colorRamp2(c(-1.5,0,1.5), c("#d94801","#f7f7f7","#74add1")),
        name = "p-value",
        #row_title = "Pathways",
        cluster_columns = FALSE,
        #clustering_distance_rows = "spearman",
        #clustering_distance_columns = "kendall",
        cluster_rows = FALSE,
        show_row_names = FALSE
)

heatmap_i50 <- read.csv("FigureS3_heatmap_i5.txt", sep="\t")
heatmap_i5 <- heatmap_i50[,-1]
rownames(heatmap_i5) <- heatmap_i50[,1]

i5 <- Heatmap(heatmap_i5, 
        col = colorRamp2(c(-1.5,0,1.5), c("#d94801","#f7f7f7","#74add1")),
        name = "p-value",
        #row_title = "Pathways",
        cluster_columns = FALSE,
        #clustering_distance_rows = "spearman",
        #clustering_distance_columns = "kendall",
        cluster_rows = FALSE,
        show_row_names = FALSE
)

