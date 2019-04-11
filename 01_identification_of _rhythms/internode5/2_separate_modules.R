#set your working directory first

library(reshape)
library(ggplot2)

input_file <- "internode5_modules.txt"
timecourses_file <- "internode5_expressed_timecourses.txt" 
output_file <- "outputs/i5_rhythmic_timecourses"


#import data and organize the dataset
nodes <- read.table(input_file)
nodes1 <- nodes[-1,-2]
colnames(nodes1) <- c("ProbeName", "module")
timecourse <- read.table(timecourses_file)
timecourse1 <- timecourse[-1,]
colnames(timecourse1) <- c("ProbeName")
caneData0 <- merge(nodes1,timecourse1,by="ProbeName")
caneData0 <- as.matrix(caneData0)
colnames(caneData0) <- NULL
caneData0 <- as.data.frame(caneData0)
caneData0[,-c(1:2)] <- lapply(caneData0[,-c(1:2)], function(x) as.numeric(as.character(x)))

#Z-score normalization
caneData0$means <- rowMeans(caneData0[,-c(1:2)])
caneData0$sd <- apply(caneData0[,-c(1:2)],1,sd)
caneData0 <- data.frame(caneData0[1:2], caneData0["means"], caneData0["sd"], caneData0[3:16])
caneData <- apply(caneData0[,-c(1:4)],2,function(x){(x-caneData0$means)/caneData0$sd})
caneData <- data.frame(caneData0[,c(1:2)], caneData)

#sort modules
caneData <- data.frame(caneData)
modules <- levels(caneData$V2)
modules <-sub("^", "_", modules)
modules <-sub("$", "_", modules)
modules <-as.data.frame(modules)
caneData$V2 <- sub("^", "_", caneData$V2 )
caneData$V2 <- sub("$", "_", caneData$V2 )
separado <- apply(modules,1,function(x) {caneData[grep(x,caneData$V2,fixed = TRUE),]})
modules2 <- t(modules)
modules2 <- as.vector(modules2)
names(separado) <- modules2

#generate timecourses
Time <- seq(-2,24,by=2)
Time <- as.matrix(Time)
Time <- as.data.frame(Time)
colnames(Time) <- "Time"
write.table(Time, "temp/modules_medians.txt", quote = FALSE, row.names = FALSE)


#select the time course with the best rhythm in each module
apply(modules,1,function(x) {
  caneData<-separado[[paste(x)]]
  nomes <- caneData[,1]
  caneData <- caneData[,3:ncol(caneData)]
  caneData <- t(caneData) 
  caneData <- as.data.frame(caneData)
  colnames(caneData) <-nomes
  caneData <- as.matrix(caneData) 
  
  caneData2 <- t(caneData) 
  annot <- seq(1,nrow(caneData2),by=1)
  annot <- as.matrix(annot) 
  nomes2 <- as.matrix(nomes) 
  colnames(annot) = "Probe"
  annot2 <- cbind(annot, nomes2)
  nomes2 <- as.matrix(nomes2) 
  annot3 <- as.data.frame(annot2)
  data <- cbind(annot, caneData2)
  row.names(data) <- NULL 
  teste <- c("Probe", seq(1,27,by=2))
  colnames(data) <- teste
  data <- as.data.frame(data)
  
  source("JTK_CYCLE.R")
  
  project <- "internode5_1cycle"
  options(stringsAsFactors=FALSE)
  
  rownames(data) <- data[,1]
  data <- data[,-1]
  jtkdist(ncol(data))
  
  periods <- 12; interval <- 2;
  jtk.init(periods,interval)
  
  cat("JTK analysis started on",date(),"\n")
  flush.console()
  
  st <- system.time({
    res <- apply(data,1,function(z) {
      jtkx(z)
      c(JTK.ADJP,JTK.PERIOD,JTK.LAG,JTK.AMP)
    })
    res <- as.data.frame(t(res))
    bhq <- p.adjust(unlist(res[,1]),"BH")
    res <- cbind(bhq,res)
    colnames(res) <- c("BH.Q","ADJ.P","PER","LAG","AMP")
    results <- cbind(annot,res,data)
    results <- results[order(res$ADJ.P,-res$AMP),]
  })
  print(st)
  
best_rhythm <- t(results[1,7:20])
  

#compare each time course to the best rhythm of each module 
  caneSpearman <- apply(caneData,2,function(caneData) cor(best_rhythm,caneData, method="spearman"))
  caneSpearman2 <- as.matrix(caneSpearman)
  caneSpearman2 <- as.data.frame(caneSpearman2)
  
  
#separate positive correlations from negative correlations in the modules
  selection_positive <-subset(caneSpearman2, V1>=0.3)
  #selection_positive <-subset(caneSpearman2, V1>=0.6)
  selection_positive2 <- rownames(selection_positive)
  selection_positive3 <- as.data.frame(selection_positive2)
  if (nrow(selection_positive3) == 1) {
    graphs_positive <-as.data.frame(caneData[,selection_positive2])
    colnames(graphs_positive) <- selection_positive2
  } else {
    graphs_positive <-as.data.frame(caneData[,selection_positive2])
  }
  graphs_positive2 <- t(graphs_positive)
  graphs_positive3 <- graphs_positive2
  module_name <-sub("_", "", x)
  graphs_positive4 <- rep(paste("i5_",module_name,"01", sep = ""),nrow(graphs_positive2))
  graphs_positive5 <- cbind(graphs_positive3, graphs_positive4)
  write.table(graphs_positive5, "temp/all_probes_modules.txt", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
  write.table(graphs_positive2, paste("modules/",module_name,"01",".txt", sep=""), quote = FALSE, row.names = TRUE)
  graphs_positive <- as.data.frame(graphs_positive)
  if (nrow(graphs_positive) == 1) {
    positiveMedian <- graphs_positive[,1]
  } else {
    positiveMedian <- apply(graphs_positive,1,median)
  }
  positiveMedian <- as.matrix(positiveMedian)
  positiveMedian <- as.data.frame(positiveMedian)
  colnames(positiveMedian) <- paste(module_name,"01",sep="")
  write.table(positiveMedian, "temp/modules_medians.txt", append = TRUE, quote = FALSE, row.names = FALSE)
  positiveMedian <- as.matrix(positiveMedian)  
  colnames(Time) <- "Time"
  graphs_positive<-cbind.data.frame(Time,graphs_positive)
  
  test <- melt.data.frame(graphs_positive, id="Time")
  test[,2]=as.character(test[,2])
  test[,1]=as.character(test[,1])
  test[,1]=as.numeric(test[,1])
  test[,3]=as.character(test[,3])
  test[,3]=as.numeric(test[,3])
  png(filename=paste("modules/",module_name,"01",".png", sep=""))
  print (qplot(Time, value, data = test, main=paste(x,"01", sep=""), ylim = c(-3,3), xlab = "Time (h)", ylab = "Normalized Expression")+ theme(text = element_text(size=16))+ stat_summary(fun.y=median, fun.ymin=median, fun.ymax=median, colour="red", geom="line", size = 1)+geom_vline(xintercept = c(0, 13.5), linetype="longdash"))
  dev.off()
  
  selection_negative <-subset(caneSpearman2, V1<=-0.3)
  if (nrow(selection_negative) > 0) {
  selection_negative2 <- rownames(selection_negative)
  selection_negative3 <- as.data.frame(selection_negative2)
  if (nrow(selection_negative3) == 1) {
    graphs_negative <-as.data.frame(caneData[,selection_negative2])
    colnames(graphs_negative) <- selection_negative2
  } else {
    graphs_negative <-as.data.frame(caneData[,selection_negative2])
  }
  graphs_negative2 <- t(graphs_negative)
  graphs_negative3 <- graphs_negative2
  graphs_negative4 <- rep(paste("i5_",module_name, "02", sep = ""),nrow(graphs_negative2))
  graphs_negative5 <- cbind(graphs_negative3, graphs_negative4)
  write.table(graphs_negative5, "temp/all_probes_modules.txt", append = TRUE, quote = FALSE, row.names = TRUE, col.names = FALSE)
  write.table(graphs_negative2, paste("modules/",module_name,"02",".txt", sep=""), quote = FALSE, row.names = TRUE)
  graphs_negative <- as.data.frame(graphs_negative)
  if (nrow(graphs_negative) == 1) {
    negativeMedian <- graphs_negative[,1]
  } else {
    negativeMedian <- apply(graphs_negative,1,median)
  }
  negativeMedian <- as.matrix(negativeMedian)
  negativeMedian <- as.data.frame(negativeMedian)
  colnames(negativeMedian) <- paste(module_name,"02",sep="")
  write.table(negativeMedian, "temp/modules_medians.txt", append = TRUE, quote = FALSE, row.names = FALSE)
  negativeMedian <- as.matrix(negativeMedian)
  colnames(Time) <- "Time"
  graphs_negative<-cbind.data.frame(Time,graphs_negative)
  test2 <- melt.data.frame(graphs_negative, id="Time")
  test2[,2]=as.character(test2[,2])
  test2[,1]=as.character(test2[,1])
  test2[,1]=as.numeric(test2[,1])
  test2[,3]=as.character(test2[,3])
  test2[,3]=as.numeric(test2[,3])
  png(filename=paste("modules/",module_name,"02",".png", sep=""))
  #qplot(Time, value, data = test2, geom="line", ylim = c(-3,3), xlab = "Time (h)", ylab = "Normalized Expression", color = variable)+ theme(text = element_text(size=16))+geom_vline(xintercept = c(0, 13.5), linetype="longdash")
  print (qplot(Time, value, data = test2, main=paste(x,"02", sep=""), ylim = c(-3,3), xlab = "Time (h)", ylab = "Normalized Expression")+ theme(text = element_text(size=16))+ stat_summary(fun.y=median, fun.ymin=median, fun.ymax=median, colour="red", geom="line", size = 1)+geom_vline(xintercept = c(0, 13.5), linetype="longdash"))
  dev.off()
  }
      })


#generate the median of each module
teste <- read.table("temp/modules_medians.txt")
teste2 <- t(teste)
teste3 <- matrix(teste2, nrow=15, ncol=nrow(teste)/15)
teste4 <- as.data.frame(teste3)
teste5 <- t(teste4[2:nrow(teste4),])
teste6 <- t(teste4[1,])
rownames(teste5) <- teste6
teste7 <- t(teste5)
teste8 <- as.data.frame(teste7)
write.table("temp/modules_medians2.txt")
write.table(teste8,"temp/modules_medians2.txt", quote = FALSE, row.names = FALSE)

#analizes modules for rhythmicity
teste9 <- as.data.frame(t(teste8[,2:ncol(teste8)]))
rownames(teste9) = NULL
bla <- as.matrix(teste9)
bla2 <- as.numeric(bla)
bla3 <- 2^bla2
bla4 <- matrix(bla3, nrow=nrow(teste9))
bla5 <-as.data.frame(bla4)
teste10 <- rbind(seq(1,27, by=2),bla5)
teste11 <- as.matrix(seq(1:(nrow(teste10)-1)))
teste12 <- rbind("Probe",teste11)
teste13 <- data.frame(teste12, teste10)
write.table(teste13, "temp/input_modules_i5.txt", sep = "\t",quote = FALSE, row.names=FALSE, col.names = FALSE)
teste14 <- as.data.frame(colnames(teste8))
teste15 <- as.data.frame(c("Probe", seq(1,nrow(teste14)-1)))
teste16 <- data.frame(teste15, teste14)
teste16[1,2] <- "module_name"
write.table(teste16, "temp/annot_modules_i5.txt", sep = "\t", quote = FALSE, row.names=FALSE, col.names = FALSE)

source("run_JTK_CYCLE.R")
final_JTK <-subset(results, ADJ.P<0.75)

all_probes <- read.table("temp/all_probes_modules.txt")
all_probes <- as.data.frame(all_probes)
chama <- as.data.frame(final_JTK$module_name)
chama2 <- as.data.frame(final_JTK[8:21])
chama3 <- as.data.frame(c(chama,chama2))
colnames(chama3) <- NULL
write.table(chama, "temp/i5_rhythmic_modules.txt", sep = "\t",append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
write.table(chama3, "temp/i5_rhythmic_modules_timecourses.txt", sep = "\t",append = TRUE, quote = FALSE, row.names = FALSE)


rhythmic_modules <- read.table("temp/i5_rhythmic_modules.txt")
timecourses_modules <- read.table("temp/all_probes_modules.txt")
timecourses_modules <- timecourses_modules[,c(1, 16, 2:15)]
header <- t(c("ProbeName", "Module", "-02", "00", "02", "04", "06", "08", "10", "12", "16", "18", "20", "22", "24", "26"))
write.table(header, "outputs/i5_rhythmic_timecourses.txt", sep = "\t",append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
apply(rhythmic_modules,1,function(x) {
                        prov <- timecourses_modules[grep(x,timecourses_modules$V16,fixed = TRUE),]
                        write.table(prov, "outputs/i5_rhythmic_timecourses.txt", sep = "\t",append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
                        })
