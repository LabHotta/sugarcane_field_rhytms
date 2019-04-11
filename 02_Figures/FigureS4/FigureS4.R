library("ComplexHeatmap")
library("circlize")


zero = "#f7f7f7"
L1I1 = "#5a5096"
L1I5 = "#7D802B"
I1I5 = "#CE5E2E"
DC = "#383838"

#FigureS4A
heatmap_phases0 <- read.csv("FigureS4_d_c_heatmaps.txt", sep="\t")
heatmap_phases_dc <- heatmap_phases0[,-1]
rownames(heatmap_phases_dc) <- heatmap_phases0[,1]
DC <- Heatmap(heatmap_phases_dc, 
                col = colorRamp2(c(0, 115, 450), c(zero, "#979797", DC)),
                name = "time courses",
                cluster_columns = FALSE,
                cluster_rows = FALSE,
                rect_gp = gpar(col= "white"),
                row_title = "diel",
                row_names_side = "left",
                column_title = "circadian",
                column_names_side = "bottom"
                )

#FigureS4B
heatmap_phases0 <- read.csv("FigureS4_f1_e1_heatmaps.txt", sep="\t")
heatmap_phases_f1e1 <- heatmap_phases0[,-1]
rownames(heatmap_phases_f1e1) <- heatmap_phases0[,1]
L1I1 <- Heatmap(heatmap_phases_f1e1, 
        col = colorRamp2(c(0, 60, 240), c(zero, "#A8A3C6", L1I1)),
        name = "time courses",
        cluster_columns = FALSE,
        cluster_rows = FALSE,
        rect_gp = gpar(col= "white"),
        row_title = "L1",
        row_names_side = "left",
        column_title = "I1",
        column_names_side = "bottom"
        )

#FigureS4C
heatmap_phases0 <- read.csv("FigureS4_f1_e5_heatmaps.txt", sep="\t")
heatmap_phases_f1e5 <- heatmap_phases0[,-1]
rownames(heatmap_phases_f1e5) <- heatmap_phases0[,1]
L1I5 <- Heatmap(heatmap_phases_f1e5, 
        col = colorRamp2(c(0, 75, 300), c(zero, "#BABB91", L1I5)),
        name = "time courses",
        cluster_columns = FALSE,
        cluster_rows = FALSE,
        rect_gp = gpar(col= "white"),
        row_title = "L1",
        row_names_side = "left",
        column_title = "I5",
        column_names_side = "bottom"
        )

#FigureS4D
heatmap_phases0 <- read.csv("FigureS4_e1_e5_heatmaps.txt", sep="\t")
heatmap_phases_e1e5 <- heatmap_phases0[,-1]
rownames(heatmap_phases_e1e5) <- heatmap_phases0[,1]

I1I5 <- Heatmap(heatmap_phases_e1e5, 
        col = colorRamp2(c(0, 150, 600), c(zero, "#E2AA92", I1I5)),
        name = "time courses",
        cluster_columns = FALSE,
        cluster_rows = FALSE,
        rect_gp = gpar(col= "white"),
        row_title = "I1",
        row_names_side = "left",
        column_title = "I5",
        column_names_side = "bottom"
        )
