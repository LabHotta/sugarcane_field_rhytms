library(ComplexHeatmap)
library(circlize)

#saved as 600x600

#Figure 2D
heatmap_enrichment0 <- read.csv("Figure2D.txt", sep="\t")
heatmap_enrichment <- heatmap_enrichment0[,-1]
rownames(heatmap_enrichment) <- heatmap_enrichment0[,1]
names(heatmap_enrichment) [names(heatmap_enrichment) == "L1.expressed"] <- "L1"
names(heatmap_enrichment) [names(heatmap_enrichment) == "I1.expressed"] <- "I1"
names(heatmap_enrichment) [names(heatmap_enrichment) == "I5.expressed"] <- "I5"
names(heatmap_enrichment) [names(heatmap_enrichment) == "L1.rhythmic"] <- "L1"
names(heatmap_enrichment) [names(heatmap_enrichment) == "I1.rhythmic"] <- "I1"
names(heatmap_enrichment) [names(heatmap_enrichment) == "I5.rhythmic"] <- "I5"

heatmap_expressed <- heatmap_enrichment[,c(1:3)]
heatmap_rhythmic <- heatmap_enrichment[,c(8:10)]

underepresented = "#d94801"
overrepresented = "#74add1"
not.significant = "#f7f7f7"

colfunc <- colorRampPalette(c(underepresented, not.significant, overrepresented))
colfunc(7)
"#D94801" "#E38252" "#EDBCA4" "#F7F7F7" "#CBDEEA" "#9FC5DD" "#74ADD1"


expressed <- Heatmap(heatmap_expressed, 
                     col = colorRamp2(c(0, 0.075,0.925, 1), c(overrepresented, not.significant, not.significant, underepresented)),
                     name = "p-value",
                     cluster_columns = FALSE,
                     cluster_rows = FALSE,
                     rect_gp = gpar(col= "white"),
                     row_names_side = "left"
)
rhythmic <- Heatmap(heatmap_rhythmic, 
                    col = colorRamp2(c(0, 0.075,0.925, 1), c(overrepresented,not.significant, not.significant, underepresented)),
                    name = "p-value2",
                    row_title = "Pathways",
                    rect_gp = gpar(col= "white"),
                    cluster_columns = FALSE,
                    #clustering_distance_rows = "kendall",
                    #clustering_distance_columns = "kendall",
                    cluster_rows = FALSE,
                    show_row_names = FALSE
)

expressed + rhythmic 