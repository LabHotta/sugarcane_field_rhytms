library("circlize")
library("ComplexHeatmap")

#save 600x600

layout(matrix(c(1, 2,
                3, 4), nrow=2, byrow=TRUE))
layout.show(n=4)

#Carbs
phase_carbs0 <- read.csv("Figure2e_phases_carbs.txt", sep="\t")
phase_carbs <- t(phase_carbs0[,-1])
colnames(phase_carbs) <- phase_carbs0[,1]
phase_carbs <- as.data.frame(t(phase_carbs))

phase_carbs1 <- t(cbind(phase_carbs$f1,phase_carbs$f1))
phase_carbs2 <- t(cbind(phase_carbs$e1,phase_carbs$e1))
phase_carbs3 <- t(cbind(phase_carbs$e5,phase_carbs$e5))

col_fun1 = colorRamp2(c(0, 3, 19), c("#D4DFD9", "#6D947C", "#00441B"))
factors1 = rep(letters[1:2], times = c(12, 0))
phase_carbs_list1 = list(a = phase_carbs1[, factors1 == "a"])

circos.par(cell.padding = c(0, 0, 0, 0), track.margin = c(0,0), start.degree = 80, gap.after = 20)
circos.initialize(factors1, xlim = cbind(c(0), table(factors1)))

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  m1 = phase_carbs_list1[[sector.index]]
  col_phase_carbs1 = col_fun1(m1)
  nr = nrow(m1)
  nc = ncol(m1)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_carbs1[i, ], col = col_phase_carbs1[i, ])
  }
})


col_fun2 = colorRamp2(c(0, 3, 19), c("#F1DADF", "#C96D80", "#A20021"))
factors2 = rep(letters[1:2], times = c(12, 0))
phase_carbs_list2 = list(a = phase_carbs2[, factors2 == "a"])

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  m2 = phase_carbs_list2[[sector.index]]
  col_phase_carbs2 = col_fun2(m2)
  nr = nrow(m2)
  nc = ncol(m2)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_carbs2[i, ], col = col_phase_carbs2[i, ])
  }
})

col_fun3 = colorRamp2(c(0, 3, 19), c("#FEF5E3", "#FCD88F", "#FABC3C"))
factors3 = rep(letters[1:2], times = c(12, 0))
phase_carbs_list3 = list(a = phase_carbs3[, factors3 == "a"])

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  m3 = phase_carbs_list3[[sector.index]]
  col_phase_carbs3 = col_fun3(m3)
  nr = nrow(m3)
  nc = ncol(m3)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_carbs3[i, ], col = col_phase_carbs3[i, ])
  }
})

title("Carbohydrate Metabolism")

ZT_f1 = 1
ZT_e1 = 0
ZT_e5 = -0.5

ZT.degree = 80 - (ZT_f1)/24 * 340 - 30/60 * 340/12 
arrows(0, 0, cos(ZT.degree/180*pi)*0.7, sin(ZT.degree/180*pi)*0.7, lwd = 4,  col="#00441b")

ZT.degree = 80 - (ZT_e1)/24 * 340 - 30/60 * 340/12 
arrows(0, 0, cos(ZT.degree/180*pi)*0.7, sin(ZT.degree/180*pi)*0.7, lwd = 4,  col="#a20021")

ZT.degree = 80 - (ZT_e5)/24 * 340 - 30/60 * 340/12 
arrows(0, 0, cos(ZT.degree/180*pi)*0.7, sin(ZT.degree/180*pi)*0.7, lwd = 4,  col="#fabc3c")


#cell wall
phase_cellwall0 <- read.csv("Figure2e_phases_cellwall.txt", sep="\t")
phase_cellwall <- t(phase_cellwall0[,-1])
colnames(phase_cellwall) <- phase_cellwall0[,1]
phase_cellwall <- as.data.frame(t(phase_cellwall))

phase_cellwall1 <- t(cbind(phase_cellwall$f1,phase_cellwall$f1))
phase_cellwall2 <- t(cbind(phase_cellwall$e1,phase_cellwall$e1))
phase_cellwall3 <- t(cbind(phase_cellwall$e5,phase_cellwall$e5))

col_fun1 = colorRamp2(c(0, 2, 18), c("#D4DFD9", "#6D947C", "#00441B"))
factors1 = rep(letters[1:2], times = c(12, 0))
phase_cellwall_list1 = list(a = phase_cellwall1[, factors1 == "a"])

circos.par(cell.padding = c(0, 0, 0, 0), track.margin = c(0,0), start.degree = 80, gap.after = 20)
circos.initialize(factors1, xlim = cbind(c(0), table(factors1)))

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  m1 = phase_cellwall_list1[[sector.index]]
  col_phase_cellwall1 = col_fun1(m1)
  nr = nrow(m1)
  nc = ncol(m1)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_cellwall1[i, ], col = col_phase_cellwall1[i, ])
  }
})

col_fun2 = colorRamp2(c(0, 2, 18), c("#F1DADF", "#C96D80", "#A20021"))
factors2 = rep(letters[1:2], times = c(12, 0))
phase_cellwall_list2 = list(a = phase_cellwall2[, factors2 == "a"])

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  m2 = phase_cellwall_list2[[sector.index]]
  col_phase_cellwall2 = col_fun2(m2)
  nr = nrow(m2)
  nc = ncol(m2)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_cellwall2[i, ], col = col_phase_cellwall2[i, ])
  }
})

col_fun3 = colorRamp2(c(0, 2, 18), c("#FEF5E3", "#FCD88F", "#FABC3C"))
factors3 = rep(letters[1:2], times = c(12, 0))
phase_cellwall_list3 = list(a = phase_cellwall3[, factors3 == "a"])

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  m3 = phase_cellwall_list3[[sector.index]]
  col_phase_cellwall3 = col_fun3(m3)
  nr = nrow(m3)
  nc = ncol(m3)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_cellwall3[i, ], col = col_phase_cellwall3[i, ])
  }
})
title("Cell Wall Metabolism & Elongation")

ZT_f1 = 2
ZT_e1 = 6
ZT_e5 = 0

ZT.degree = 80 - (ZT_f1)/24 * 340 - 30/60 * 340/12 
arrows(0, 0, cos(ZT.degree/180*pi)*0.7, sin(ZT.degree/180*pi)*0.7, lwd = 4,  col="#00441b")

ZT.degree = 80 - (ZT_e1)/24 * 340 - 30/60 * 340/12 
arrows(0, 0, cos(ZT.degree/180*pi)*0.7, sin(ZT.degree/180*pi)*0.7, lwd = 4,  col="#a20021")

ZT.degree = 80 - (ZT_e5)/24 * 340 - 30/60 * 340/12 
arrows(0, 0, cos(ZT.degree/180*pi)*0.7, sin(ZT.degree/180*pi)*0.7, lwd = 4,  col="#fabc3c")


#aa
phase_aa0 <- read.csv("Figure2e_phases_aa.txt", sep="\t")
phase_aa <- t(phase_aa0[,-1])
colnames(phase_aa) <- phase_aa0[,1]
phase_aa <- as.data.frame(t(phase_aa))

phase_aa1 <- t(cbind(phase_aa$f1,phase_aa$f1))
phase_aa2 <- t(cbind(phase_aa$e1,phase_aa$e1))
phase_aa3 <- t(cbind(phase_aa$e5,phase_aa$e5))

col_fun1 = colorRamp2(c(0, 2, 20), c("#D4DFD9", "#6D947C", "#00441B"))
factors1 = rep(letters[1:2], times = c(12, 0))
phase_aa_list1 = list(a = phase_aa1[, factors1 == "a"])

circos.par(cell.padding = c(0, 0, 0, 0), track.margin = c(0,0), start.degree = 80, gap.after = 20)
circos.initialize(factors1, xlim = cbind(c(0), table(factors1)))

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  m1 = phase_aa_list1[[sector.index]]
  col_phase_aa1 = col_fun1(m1)
  nr = nrow(m1)
  nc = ncol(m1)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_aa1[i, ], col = col_phase_aa1[i, ])
  }
})

col_fun2 = colorRamp2(c(0, 2, 20), c("#F1DADF", "#C96D80", "#A20021"))
factors2 = rep(letters[1:2], times = c(12, 0))
phase_aa_list2 = list(a = phase_aa2[, factors2 == "a"])

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  m2 = phase_aa_list2[[sector.index]]
  col_phase_aa2 = col_fun2(m2)
  nr = nrow(m2)
  nc = ncol(m2)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_aa2[i, ], col = col_phase_aa2[i, ])
  }
})

col_fun3 = colorRamp2(c(0, 2, 20), c("#FEF5E3", "#FCD88F", "#FABC3C"))
factors3 = rep(letters[1:2], times = c(12, 0))
phase_aa_list3 = list(a = phase_aa3[, factors3 == "a"])

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  m3 = phase_aa_list3[[sector.index]]
  col_phase_aa3 = col_fun3(m3)
  nr = nrow(m3)
  nc = ncol(m3)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_aa3[i, ], col = col_phase_aa3[i, ])
  }
})
title("Amino Acid Metabolism")

ZT_f1 = 13
ZT_e1 = 2
ZT_e5 = 0

ZT.degree = 80 - (ZT_f1)/24 * 340 - 30/60 * 340/12 
arrows(0, 0, cos(ZT.degree/180*pi)*0.7, sin(ZT.degree/180*pi)*0.7, lwd = 4,  col="#00441b")

ZT.degree = 80 - (ZT_e1)/24 * 340 - 30/60 * 340/12 
arrows(0, 0, cos(ZT.degree/180*pi)*0.7, sin(ZT.degree/180*pi)*0.7, lwd = 4,  col="#a20021")

ZT.degree = 80 - (ZT_e5)/24 * 340 - 30/60 * 340/12 
arrows(0, 0, cos(ZT.degree/180*pi)*0.7, sin(ZT.degree/180*pi)*0.7, lwd = 4,  col="#fabc3c")


#transporters
phase_transporters0 <- read.csv("Figure2e_phases_transporters.txt", sep="\t")
phase_transporters <- t(phase_transporters0[,-1])
colnames(phase_transporters) <- phase_transporters0[,1]
phase_transporters <- as.data.frame(t(phase_transporters))

phase_transporters1 <- t(cbind(phase_transporters$f1,phase_transporters$f1))
phase_transporters2 <- t(cbind(phase_transporters$e1,phase_transporters$e1))
phase_transporters3 <- t(cbind(phase_transporters$e5,phase_transporters$e5))

col_fun1 = colorRamp2(c(0, 2, 20), c("#D4DFD9", "#6D947C", "#00441B"))
factors1 = rep(letters[1:2], times = c(12, 0))
phase_transporters_list1 = list(a = phase_transporters1[, factors1 == "a"])

circos.par(cell.padding = c(0, 0, 0, 0), track.margin = c(0,0), start.degree = 80, gap.after = 20)
circos.initialize(factors1, xlim = cbind(c(0), table(factors1)))

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  m1 = phase_transporters_list1[[sector.index]]
  col_phase_transporters1 = col_fun1(m1)
  nr = nrow(m1)
  nc = ncol(m1)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_transporters1[i, ], col = col_phase_transporters1[i, ])
  }
})


col_fun2 = colorRamp2(c(0, 2, 20), c("#F1DADF", "#C96D80", "#A20021"))
factors2 = rep(letters[1:2], times = c(12, 0))
phase_transporters_list2 = list(a = phase_transporters2[, factors2 == "a"])

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  m2 = phase_transporters_list2[[sector.index]]
  col_phase_transporters2 = col_fun2(m2)
  nr = nrow(m2)
  nc = ncol(m2)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_transporters2[i, ], col = col_phase_transporters2[i, ])
  }
})

col_fun3 = colorRamp2(c(0, 2, 20), c("#FEF5E3", "#FCD88F", "#FABC3C"))
factors3 = rep(letters[1:2], times = c(12, 0))
phase_transporters_list3 = list(a = phase_transporters3[, factors3 == "a"])

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  m3 = phase_transporters_list3[[sector.index]]
  col_phase_transporters3 = col_fun3(m3)
  nr = nrow(m3)
  nc = ncol(m3)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_transporters3[i, ], col = col_phase_transporters3[i, ])
  }
})
title("Transporters")

ZT_f1 = 2
ZT_e1 = 8
ZT_e5 = 1

ZT.degree = 80 - (ZT_f1)/24 * 340 - 30/60 * 340/12 
arrows(0, 0, cos(ZT.degree/180*pi)*0.7, sin(ZT.degree/180*pi)*0.7, lwd = 4,  col="#00441b")

ZT.degree = 80 - (ZT_e1)/24 * 340 - 30/60 * 340/12 
arrows(0, 0, cos(ZT.degree/180*pi)*0.7, sin(ZT.degree/180*pi)*0.7, lwd = 4,  col="#a20021")

ZT.degree = 80 - (ZT_e5)/24 * 340 - 30/60 * 340/12 
arrows(0, 0, cos(ZT.degree/180*pi)*0.7, sin(ZT.degree/180*pi)*0.7, lwd = 4,  col="#fabc3c")
