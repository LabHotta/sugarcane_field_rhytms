library("ComplexHeatmap")
library("circlize")

heatmap_enrichment0 <- read.csv("FigureS5_heatmap_enrichment_all.txt", sep="\t")
heatmap_enrichment <- heatmap_enrichment0[,-1]
rownames(heatmap_enrichment) <- heatmap_enrichment0[,1]
names(heatmap_enrichment) [names(heatmap_enrichment) == "L1.expressed"] <- "L1"
names(heatmap_enrichment) [names(heatmap_enrichment) == "I1.expressed"] <- "I1"
names(heatmap_enrichment) [names(heatmap_enrichment) == "I5.expressed"] <- "I5"
names(heatmap_enrichment) [names(heatmap_enrichment) == "f1e1_expressed"] <- "L1I1"
names(heatmap_enrichment) [names(heatmap_enrichment) == "f1e5_expressed"] <- "L1I5"
names(heatmap_enrichment) [names(heatmap_enrichment) == "e1e5_expressed"] <- "I1I5"
names(heatmap_enrichment) [names(heatmap_enrichment) == "f1e1e5_expressed"] <- "L1I1I5"

names(heatmap_enrichment) [names(heatmap_enrichment) == "L1.rhythmic"] <- "L1"
names(heatmap_enrichment) [names(heatmap_enrichment) == "I1.rhythmic"] <- "I1"
names(heatmap_enrichment) [names(heatmap_enrichment) == "I5.rhythmic"] <- "I5"
names(heatmap_enrichment) [names(heatmap_enrichment) == "f1e1_rhythmic"] <- "L1I1"
names(heatmap_enrichment) [names(heatmap_enrichment) == "f1e5_rhythmic"] <- "L1I5"
names(heatmap_enrichment) [names(heatmap_enrichment) == "e1e5_rhythmic"] <- "I1I5"
names(heatmap_enrichment) [names(heatmap_enrichment) == "f1e1e5_rhythmic"] <- "L1I1I5"


heatmap_expressed <- heatmap_enrichment[,c(1:7)]
heatmap_rhythmic <- heatmap_enrichment[,c(8:14)]

underepresented = "#d94801"
overrepresented = "#74add1"
not.significant = "#f7f7f7"

colfunc <- colorRampPalette(c(underepresented, not.significant, overrepresented))
colfunc(7)


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