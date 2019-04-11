library(eulerr)
library(png)
library(cowplot)
library(grid)

#save 600 x600, crop 600 x 500

f1 = "#00441b"
e1 = "#a20021"
e5 = "#fabc3c"
circadian = "#969696"

f1_fill = "#7FA18D"
e1_fill = "#D07F90"
e5_fill = "#FCDD9D"
circadian_fill = "#CACACA"

Carbs_rhythmic <- euler(c("A" = 33, "B" = 9, "C" = 2,
                          "A&B" = 13, "A&C" = 11, "B&C" = 9,
                          "A&B&C" = 16))

Carbs_rhythmic <- plot(Carbs_rhythmic, 
                       lex = 4,
                       labels = c("L1", "I1", "I5"), 
                       quantities = TRUE,
                       #main = "rhythmic",
                       col = c(f1, e1, e5),
                       border = c(f1, e1, e5),
                       counts = TRUE,
                       fill = c(f1_fill, e1_fill, e5_fill)
)


Carbs_expressed <- euler(c("A" = 3, "B" = 6, "C" = 1,
                           "A&B" = 3, "A&C" = 2, "B&C" = 19,
                           "A&B&C" = 98))

Carbs_expressed <- plot(Carbs_expressed , 
                        lex = 4,
                        labels = c("L1", "I1", "I5"), 
                        quantities = TRUE,
                        #main = "rhythmic",
                        col = c(f1, e1, e5),
                        border = c(f1, e1, e5),
                        counts = TRUE,
                        fill = c(f1_fill, e1_fill, e5_fill)
)

CellWall_rhythmic <- euler(c("A" = 40, "B" = 14, "C" = 7,
                             "A&B" = 10, "A&C" = 3, "B&C" = 6,
                             "A&B&C" = 8))


CellWall_rhythmic <- plot(CellWall_rhythmic, 
                          lex = 4,
                          labels = c("L1", "I1", "I5"), 
                          quantities = TRUE,
                          #main = "rhythmic",
                          col = c(f1, e1, e5),
                          border = c(f1, e1, e5),
                          counts = TRUE,
                          fill = c(f1_fill, e1_fill, e5_fill)
)


CellWall_expressed <- euler(c("A" = 4, "B" = 3, "C" = 4,
                              "A&B" = 1, "A&C" = 4, "B&C" = 43,
                              "A&B&C" = 70))

CellWall_expressed <- plot(CellWall_expressed , 
                           lex = 4,
                           labels = c("L1", "I1", "I5"), 
                           quantities = TRUE,
                           #main = "rhythmic",
                           col = c(f1, e1, e5),
                           border = c(f1, e1, e5),
                           counts = TRUE,
                           fill = c(f1_fill, e1_fill, e5_fill)
)

Transporters_rhythmic <- euler(c("A" = 34, "B" = 4, "C" = 3,
                                 "A&B" = 11, "A&C" = 7, "B&C" = 0,
                                 "A&B&C" = 18))


Transporters_rhythmic <- plot(Transporters_rhythmic, 
                              lex = 4,
                              labels = c("L1", "I1", "I5"), 
                              quantities = TRUE,
                              #main = "rhythmic",
                              col = c(f1, e1, e5),
                              border = c(f1, e1, e5),
                              counts = TRUE,
                              fill = c(f1_fill, e1_fill, e5_fill)
)


Transporters_expressed <- euler(c("A" = 1, "B" = 0, "C" = 0,
                                  "A&B" = 1, "A&C" = 4, "B&C" = 1,
                                  "A&B&C" = 74))

Transporters_expressed <- plot(Transporters_expressed , 
                               lex = 4,
                               labels = c("L1", "I1", "I5"), 
                               quantities = TRUE,
                               #main = "rhythmic",
                               col = c(f1, e1, e5),
                               border = c(f1, e1, e5),
                               counts = TRUE,
                               fill = c(f1_fill, e1_fill, e5_fill)
)

AA_rhythmic <- euler(c("A" = 46, "B" = 7, "C" = 4,
                       "A&B" = 6, "A&C" = 12, "B&C" = 3,
                       "A&B&C" = 16))


AA_rhythmic <- plot(AA_rhythmic, 
                    lex = 4,
                    labels = c("L1", "I1", "I5"), 
                    quantities = TRUE,
                    #main = "rhythmic",
                    col = c(f1, e1, e5),
                    border = c(f1, e1, e5),
                    counts = TRUE,
                    fill = c(f1_fill, e1_fill, e5_fill)
)


AA_expressed <- euler(c("A" = 3, "B" = 3, "C" = 1,
                        "A&B" = 1, "A&C" = 1, "B&C" = 9,
                        "A&B&C" = 96))

AA_expressed <- plot(AA_expressed , 
                     lex = 4,
                     labels = c("L1", "I1", "I5"), 
                     quantities = TRUE,
                     #main = "rhythmic",
                     col = c(f1, e1, e5),
                     border = c(f1, e1, e5),
                     counts = TRUE,
                     fill = c(f1_fill, e1_fill, e5_fill)
)
