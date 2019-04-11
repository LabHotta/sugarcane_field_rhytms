library(eulerr)
library(png)
library(cowplot)
library(grid)

#save 600 x600, crop 600 x 500

f1 = "#00441b"
e1 = "#a20021"
e5 = "#fabc3c"
circadian = "#969696"

colfunc <- colorRampPalette(c("white", "#fabc3c"))
colfunc(7)


#f1_fill = "#b2dbc3"
#e1_fill = "#e3b2bc"
#e5_fill = "#feeece"
#circadian_fill = "#e5e5e5"

f1_fill = "#7FA18D"
e1_fill = "#D07F90"
e5_fill = "#FCDD9D"
circadian_fill = "#CACACA"

#Figure 4D
Chromatin_expressed <- euler(c("A" = 2, "B" = 22, "C" = 1,
                               "A&B" = 3, "A&C" = 0, "B&C" = 31,
                               "A&B&C" = 183))
Chromatin_expressed <- plot(Chromatin_expressed, 
                            lex = 4,
                            labels = c("L1", "I1", "I5"), 
                            quantities = TRUE,
                            #main = "rhythmic",
                            col = c(f1, e1, e5),
                            border = c(f1, e1, e5),
                            counts = TRUE,
                            fill = c(f1_fill, e1_fill, e5_fill)
)


#Figure 4E
TF_expressed <- euler(c("A" = 5, "B" = 41, "C" = 6,
                        "A&B" = 2, "A&C" = 4, "B&C" = 54,
                        "A&B&C" = 357))
TF_expressed  <- plot(TF_expressed, 
                      lex = 4,
                      labels = c("L1", "I1", "I5"), 
                      quantities = TRUE,
                      #main = "rhythmic",
                      col = c(f1, e1, e5),
                      border = c(f1, e1, e5),
                      counts = TRUE,
                      fill = c(f1_fill, e1_fill, e5_fill)
)


#Figure 4F
Protein_expressed <- euler(c("A" = 1, "B" = 15, "C" = 0,
                             "A&B" = 1, "A&C" = 2, "B&C" = 8,
                             "A&B&C" = 287))
Protein_expressed <- plot(Protein_expressed , 
                          lex = 4,
                          labels = c("L1", "I1", "I5"), 
                          quantities = TRUE,
                          #main = "rhythmic",
                          col = c(f1, e1, e5),
                          border = c(f1, e1, e5),
                          counts = TRUE,
                          fill = c(f1_fill, e1_fill, e5_fill)
)


#Figure 4G
Chromatin_rhythmic <- euler(c("A" = 77, "B" = 19, "C" = 7,
                              "A&B" = 15, "A&C" = 17, "B&C" = 8,
                              "A&B&C" = 17))
Chromatin_rhythmic <- plot(Chromatin_rhythmic, 
                           lex = 4,
                           labels = c("L1", "I1", "I5"), 
                           quantities = TRUE,
                           #main = "rhythmic",
                           col = c(f1, e1, e5),
                           border = c(f1, e1, e5),
                           counts = TRUE,
                           fill = c(f1_fill, e1_fill, e5_fill)
)


#Figure 4H
TF_rhythmic <- euler(c("A" = 127, "B" = 28, "C" = 15,
                       "A&B" = 31, "A&C" = 39, "B&C" = 24,
                       "A&B&C" = 77))
TF_rhythmic <- plot(TF_rhythmic, 
                    lex = 4,
                    labels = c("L1", "I1", "I5"), 
                    quantities = TRUE,
                    #main = "rhythmic",
                    col = c(f1, e1, e5),
                    border = c(f1, e1, e5),
                    counts = TRUE,
                    fill = c(f1_fill, e1_fill, e5_fill)
)


#Figure 4I
Protein_rhythmic <- euler(c("A" = 147, "B" = 9, "C" = 7,
                            "A&B" = 32, "A&C" = 32, "B&C" = 14,
                            "A&B&C" = 28))
Protein_rhythmic <- plot(Protein_rhythmic, 
                         lex = 4,
                         labels = c("L1", "I1", "I5"), 
                         quantities = TRUE,
                         #main = "rhythmic",
                         col = c(f1, e1, e5),
                         border = c(f1, e1, e5),
                         counts = TRUE,
                         fill = c(f1_fill, e1_fill, e5_fill)
)
