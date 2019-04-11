library(eulerr)
library(png)
library(cowplot)
library(grid)

#save 600x600, crop 600x500

f1 = "#00441b"
e1 = "#a20021"
e5 = "#fabc3c"
circadian = "#969696"

f1_fill = "#7FA18D"
e1_fill = "#D07F90"
e5_fill = "#FCDD9D"
circadian_fill = "#CACACA"

#Figure 1B
euler_expressed_circadian_data  <- euler(c("A" = 562, "B" = 671, 
                                           "A&B" = 9260))
euler_expressed_circadian <- plot(euler_expressed_circadian_data, 
                                  lex = 4,
                                  labels = c("diurnal", "circadian"), 
                                  fill_alpha = 0.5,
                                  #main = "rhythmic",
                                  col = c(f1, circadian),
                                  border = c(f1, circadian),
                                  quantities = TRUE,
                                  fill = c(f1_fill, circadian_fill)
)
euler_expressed_circadian


#Figure 1C
euler_rhythmic_circadian_data <- euler(c("A" = 4076, "B" = 813, 
                                         "A&B" = 2629))
euler_rhythmic_circadian <- plot(euler_rhythmic_circadian_data, 
                                 lex = 4,
                                 labels = c("diurnal", "circadian"), 
                                 fill_alpha = 0.5,
                                 #main = "rhythmic",
                                 col = c(f1, circadian),
                                 border = c(f1, circadian),
                                 quantities = TRUE,
                                 fill = c(f1_fill, circadian_fill)
)
euler_rhythmic_circadian


#Figure 1E
euler_expressed_data <- euler(c("A" = 199, "B" = 933, "C" = 135,
                                "A&B" = 128, "A&C" = 115, "B&C" = 1611,
                                "A&B&C" = 9380))
euler_expressed <- plot(euler_expressed_data, 
                        lex = 4,
                        labels = c("L1", "I1", "I5"), 
                        quantities = TRUE,
                        #main = "rhythmic",
                        col = c(f1, e1, e5),
                        border = c(f1, e1, e5),
                        counts = TRUE,
                        fill = c(f1_fill, e1_fill, e5_fill)
)
euler_expressed 


#Figure 1F
euler_rhythmic_data  <- euler(c("A" = 3531, "B" = 722, "C" = 486,
               "A&B" = 1019, "A&C" = 742, "B&C" = 601,
               "A&B&C" = 1413))
euler_rhythmic <- plot(euler_rhythmic_data, 
                      lex = 4,
                      labels = c("L1", "I1", "I5"), 
                      quantities = TRUE,
                      #main = "rhythmic",
                      col = c(f1, e1, e5),
                      border = c(f1, e1, e5),
                      counts = TRUE,
                      fill = c(f1_fill, e1_fill, e5_fill)
)
euler_rhythmic