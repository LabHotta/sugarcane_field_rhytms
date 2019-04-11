library("circlize")
library("ComplexHeatmap")

#save 1440 x 600

f1 = "#00441b"
e1 = "#a20021"
e5 = "#fabc3c"

colfunc <- colorRampPalette(c("white", "#00441b"))
colfunc(9)

#Figure 4A - Chromatin
phase_chromatin0 <- read.csv("Figure4A_phases_chromatin.txt", sep="\t")
phase_chromatin <- t(phase_chromatin0[,-1])
colnames(phase_chromatin) <- phase_chromatin0[,1]
phase_chromatin <- as.data.frame(t(phase_chromatin))

phase_chromatin1 <- t(cbind(phase_chromatin$f1,phase_chromatin$f1))
phase_chromatin2 <- t(cbind(phase_chromatin$e1,phase_chromatin$e1))
phase_chromatin3 <- t(cbind(phase_chromatin$e5,phase_chromatin$e5))

col_fun1 = colorRamp2(c(0, 5, 25, 100), c("#D4DFD9", "#6D947C", "#245E3B", "#00441B"))
factors1 = rep(letters[1:2], times = c(12, 0))
phase_chromatin_list1 = list(a = phase_chromatin1[, factors1 == "a"])

circos.par(cell.padding = c(0, 0, 0, 0), track.margin = c(0,0), start.degree = 80, gap.after = 20)
circos.initialize(factors1, xlim = cbind(c(0), table(factors1)))

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  #circos.axis(h = "top", major.tick = 8)
  m1 = phase_chromatin_list1[[sector.index]]
  col_phase_chromatin1 = col_fun1(m1)
  nr = nrow(m1)
  nc = ncol(m1)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_chromatin1[i, ], col = col_phase_chromatin1[i, ])
  }
})


col_fun2 = colorRamp2(c(0, 5, 25, 100), c("#F1DADF", "#C96D80", "#AF2440", "#A20021"))
factors2 = rep(letters[1:2], times = c(12, 0))
phase_chromatin_list2 = list(a = phase_chromatin2[, factors2 == "a"])

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  #circos.axis(h = "top", major.tick = 8)
  m2 = phase_chromatin_list2[[sector.index]]
  col_phase_chromatin2 = col_fun2(m2)
  nr = nrow(m2)
  nc = ncol(m2)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_chromatin2[i, ], col = col_phase_chromatin2[i, ])
  }
})

col_fun3 = colorRamp2(c(0, 5, 25, 100), c("#FEF5E3", "#FCD88F", "#FAC557", "#FABC3C"))
factors3 = rep(letters[1:2], times = c(12, 0))
phase_chromatin_list3 = list(a = phase_chromatin3[, factors3 == "a"])

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  m3 = phase_chromatin_list3[[sector.index]]
  col_phase_chromatin3 = col_fun3(m3)
  nr = nrow(m3)
  nc = ncol(m3)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_chromatin3[i, ], col = col_phase_chromatin3[i, ])
  }
})
title("a Chromatin Remodeling")

ZT_f1 = 12
ZT_e1 = 0
ZT_e5 = -0.5

ZT.degree = 80 - (ZT_f1)/24 * 340 - 30/60 * 340/12 
arrows(0, 0, cos(ZT.degree/180*pi)*0.7, sin(ZT.degree/180*pi)*0.7, lwd = 4,  col="#00441b")


ZT.degree = 80 - (ZT_e1)/24 * 340 - 30/60 * 340/12 
arrows(0, 0, cos(ZT.degree/180*pi)*0.7, sin(ZT.degree/180*pi)*0.7, lwd = 4,  col="#a20021")

ZT.degree = 80 - (ZT_e5)/24 * 340 - 30/60 * 340/12 
arrows(0, 0, cos(ZT.degree/180*pi)*0.7, sin(ZT.degree/180*pi)*0.7, lwd = 4,  col="#fabc3c")

#Figure 4B - transcription factors

phase_TF0 <- read.csv("Figure4B_phases_TF.txt", sep="\t")
phase_TF <- t(phase_TF0[,-1])
colnames(phase_TF) <- phase_TF0[,1]
phase_TF <- as.data.frame(t(phase_TF))

phase_TF1 <- t(cbind(phase_TF$f1,phase_TF$f1))
phase_TF2 <- t(cbind(phase_TF$e1,phase_TF$e1))
phase_TF3 <- t(cbind(phase_TF$e5,phase_TF$e5))

#mat = matrix(rnorm(13*3), nrow = 3, ncol = 13)
#col_fun1 = colorRamp2(c(0, 400, 1200), c("#c7e9c0", "#a1d99b", "#00441b"))
#col_fun1 = colorRamp2(c(0, 700, 1200), c("#deebf7", "#4292c6", "#08306b"))
col_fun1 = colorRamp2(c(0, 5, 25, 100), c("#D4DFD9", "#6D947C", "#245E3B", "#00441B"))
factors1 = rep(letters[1:2], times = c(12, 0))
phase_TF_list1 = list(a = phase_TF1[, factors1 == "a"])

circos.par(cell.padding = c(0, 0, 0, 0), track.margin = c(0,0), start.degree = 80, gap.after = 20)
circos.initialize(factors1, xlim = cbind(c(0), table(factors1)))

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  #circos.axis(h = "top", major.tick = 8)
  m1 = phase_TF_list1[[sector.index]]
  col_phase_TF1 = col_fun1(m1)
  nr = nrow(m1)
  nc = ncol(m1)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_TF1[i, ], col = col_phase_TF1[i, ])
  }
})


col_fun2 = colorRamp2(c(0, 5, 25, 100), c("#F1DADF", "#C96D80", "#AF2440", "#A20021"))
factors2 = rep(letters[1:2], times = c(12, 0))
phase_TF_list2 = list(a = phase_TF2[, factors2 == "a"])

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  m2 = phase_TF_list2[[sector.index]]
  col_phase_TF2 = col_fun2(m2)
  nr = nrow(m2)
  nc = ncol(m2)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_TF2[i, ], col = col_phase_TF2[i, ])
  }
})

col_fun3 = colorRamp2(c(0, 5, 25, 100), c("#FEF5E3", "#FCD88F", "#FAC557", "#FABC3C"))
factors3 = rep(letters[1:2], times = c(12, 0))
phase_TF_list3 = list(a = phase_TF3[, factors3 == "a"])

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  m3 = phase_TF_list3[[sector.index]]
  col_phase_TF3 = col_fun3(m3)
  nr = nrow(m3)
  nc = ncol(m3)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_TF3[i, ], col = col_phase_TF3[i, ])
  }
})
title("b Transcription Factors")

ZT_f1 = 1
ZT_e1 = 0
ZT_e5 = -0.5

ZT.degree = 80 - (ZT_f1)/24 * 340 - 30/60 * 340/12 
#arrows(0, 0, cos(ZT.degree/180*pi)*0.55, sin(ZT.degree/180*pi)*0.55, lwd = 4,  col="#00441b")
arrows(0, 0, cos(ZT.degree/180*pi)*0.7, sin(ZT.degree/180*pi)*0.7, lwd = 4,  col="#00441b")


ZT.degree = 80 - (ZT_e1)/24 * 340 - 30/60 * 340/12 
#arrows(0, 0, cos(ZT.degree/180*pi)*0.55, sin(ZT.degree/180*pi)*0.55, lwd = 4,  col="#a20021")
arrows(0, 0, cos(ZT.degree/180*pi)*0.7, sin(ZT.degree/180*pi)*0.7, lwd = 4,  col="#a20021")

ZT.degree = 80 - (ZT_e5)/24 * 340 - 30/60 * 340/12 
#arrows(0, 0, cos(ZT.degree/180*pi)*0.55, sin(ZT.degree/180*pi)*0.55, lwd = 4,  col="#fabc3c")
arrows(0, 0, cos(ZT.degree/180*pi)*0.7, sin(ZT.degree/180*pi)*0.7, lwd = 4,  col="#fabc3c")


#Figure 4C - Protein Synthesis

phase_protein0 <- read.csv("Figure4C_phases_protein.txt", sep="\t")
phase_protein <- t(phase_protein0[,-1])
colnames(phase_protein) <- phase_protein0[,1]
phase_protein <- as.data.frame(t(phase_protein))

phase_protein1 <- t(cbind(phase_protein$f1,phase_protein$f1))
phase_protein2 <- t(cbind(phase_protein$e1,phase_protein$e1))
phase_protein3 <- t(cbind(phase_protein$e5,phase_protein$e5))

#mat = matrix(rnorm(13*3), nrow = 3, ncol = 13)
#col_fun1 = colorRamp2(c(0, 400, 1200), c("#c7e9c0", "#a1d99b", "#00441b"))
#col_fun1 = colorRamp2(c(0, 700, 1200), c("#deebf7", "#4292c6", "#08306b"))
col_fun1 = colorRamp2(c(0, 5, 25, 100), c("#D4DFD9", "#6D947C", "#245E3B", "#00441B"))
factors1 = rep(letters[1:2], times = c(12, 0))
phase_protein_list1 = list(a = phase_protein1[, factors1 == "a"])

circos.par(cell.padding = c(0, 0, 0, 0), track.margin = c(0,0), start.degree = 80, gap.after = 20)
circos.initialize(factors1, xlim = cbind(c(0), table(factors1)))

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  #circos.axis(h = "top", major.tick = 8)
  m1 = phase_protein_list1[[sector.index]]
  col_phase_protein1 = col_fun1(m1)
  nr = nrow(m1)
  nc = ncol(m1)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_protein1[i, ], col = col_phase_protein1[i, ])
  }
})


col_fun2 = colorRamp2(c(0, 5, 25, 100), c("#F1DADF", "#C96D80", "#AF2440", "#A20021"))
factors2 = rep(letters[1:2], times = c(12, 0))
phase_protein_list2 = list(a = phase_protein2[, factors2 == "a"])

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  m2 = phase_protein_list2[[sector.index]]
  col_phase_protein2 = col_fun2(m2)
  nr = nrow(m2)
  nc = ncol(m2)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_protein2[i, ], col = col_phase_protein2[i, ])
  }
})

col_fun3 = colorRamp2(c(0, 5, 25, 100), c("#FEF5E3", "#FCD88F", "#FAC557", "#FABC3C"))
factors3 = rep(letters[1:2], times = c(12, 0))
phase_protein_list3 = list(a = phase_protein3[, factors3 == "a"])

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  #circos.axis(h = "top", major.tick = 8)
  m3 = phase_protein_list3[[sector.index]]
  col_phase_protein3 = col_fun3(m3)
  nr = nrow(m3)
  nc = ncol(m3)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_protein3[i, ], col = col_phase_protein3[i, ])
  }
})
title("c Protein Synthesis")

ZT_f1 = 12
ZT_e1 = 0
ZT_e5 = 0.5

ZT.degree = 80 - (ZT_f1)/24 * 340 - 30/60 * 340/12 
arrows(0, 0, cos(ZT.degree/180*pi)*0.7, sin(ZT.degree/180*pi)*0.7, lwd = 4,  col="#00441b")


ZT.degree = 80 - (ZT_e1)/24 * 340 - 30/60 * 340/12 
arrows(0, 0, cos(ZT.degree/180*pi)*0.7, sin(ZT.degree/180*pi)*0.7, lwd = 4,  col="#a20021")

ZT.degree = 80 - (ZT_e5)/24 * 340 - 30/60 * 340/12 
arrows(0, 0, cos(ZT.degree/180*pi)*0.7, sin(ZT.degree/180*pi)*0.7, lwd = 4,  col="#fabc3c")



