library("circlize")
library("ComplexHeatmap")

f1 = "#00441b"
e1 = "#a20021"
e5 = "#fabc3c"

#Figure 2A
phase_heat0 <- read.csv("Figure3_phases_circular.txt", sep="\t")
phase_heat <- t(phase_heat0[,-1])
colnames(phase_heat) <- phase_heat0[,1]
phase_heat <- as.data.frame(t(phase_heat))

phase_heat1 <- t(cbind(phase_heat$f1,phase_heat$f1))
phase_heat2 <- t(cbind(phase_heat$e1,phase_heat$e1))
phase_heat3 <- t(cbind(phase_heat$e5,phase_heat$e5))

col_fun1 = colorRamp2(c(0, 200, 1400), c("#D4DFD9", "#6D947C", "#00441B"))
factors1 = rep(letters[1:2], times = c(12, 0))
phase_heat_list1 = list(a = phase_heat1[, factors1 == "a"])

circos.par(cell.padding = c(0, 0, 0, 0), track.margin = c(0,0), start.degree = 80, gap.after = 20)
circos.initialize(factors1, xlim = cbind(c(0), table(factors1)))

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  #circos.axis(h = "top", major.tick = 8)
  m1 = phase_heat_list1[[sector.index]]
  col_phase_heat1 = col_fun1(m1)
  nr = nrow(m1)
  nc = ncol(m1)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_heat1[i, ], col = col_phase_heat1[i, ])
  }
})

col_fun2 = colorRamp2(c(0, 200, 1400), c("#F1DADF", "#C96D80", "#A20021"))
factors2 = rep(letters[1:2], times = c(12, 0))
phase_heat_list2 = list(a = phase_heat2[, factors2 == "a"])

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  #circos.axis(h = "top", major.tick = 8)
  m2 = phase_heat_list2[[sector.index]]
  col_phase_heat2 = col_fun2(m2)
  nr = nrow(m2)
  nc = ncol(m2)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_heat2[i, ], col = col_phase_heat2[i, ])
  }
})

col_fun3 = colorRamp2(c(0, 200, 1400), c("#FEF5E3", "#FCD88F", "#FABC3C"))
factors3 = rep(letters[1:2], times = c(12, 0))
phase_heat_list3 = list(a = phase_heat3[, factors3 == "a"])

circos.track(ylim = c(0, 2), track.height = 0.07, bg.border = NA, track.margin = c(0,0), panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  #circos.axis(h = "top", major.tick = 8)
  m3 = phase_heat_list3[[sector.index]]
  col_phase_heat3 = col_fun3(m3)
  nr = nrow(m3)
  nc = ncol(m3)
  for(i in 1:nr) {
    circos.rect(1:nc - 1, rep(nr - i, nc), 
                1:nc, rep(nr - i + 1, nc), 
                border = col_phase_heat3[i, ], col = col_phase_heat3[i, ])
  }
})

ZT_f1 = 1
ZT_e1 = 0
ZT_e5 = -0.5

ZT.degree = 80 - (ZT_f1)/24 * 340 - 30/60 * 340/12 
arrows(0, 0, cos(ZT.degree/180*pi)*0.5, sin(ZT.degree/180*pi)*0.5, lwd = 4,  col="#00441b")

ZT.degree = 80 - (ZT_e1)/24 * 340 - 30/60 * 340/12 
arrows(0, 0, cos(ZT.degree/180*pi)*0.5, sin(ZT.degree/180*pi)*0.5, lwd = 4,  col="#a20021")

ZT.degree = 80 - (ZT_e5)/24 * 340 - 30/60 * 340/12 
arrows(0, 0, cos(ZT.degree/180*pi)*0.5, sin(ZT.degree/180*pi)*0.5, lwd = 4,  col="#fabc3c")
