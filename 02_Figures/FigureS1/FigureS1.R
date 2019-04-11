library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)

Sys.setlocale("LC_ALL","English")


#FigureS1A
campo = read.csv("FigureS1A.txt", sep="\t")

campo$Date = as.character(campo$Date)
campo$Date = as.Date(campo$Date, format = "%d/%m/%Y")

campo2 <- melt(campo, id.vars="Date")
names(campo2) [names(campo2) == "value"] <- "Temperature"

seasonal_temperature <- ggplot(campo, aes(Date))+
  geom_ribbon(aes(ymin = minimum, ymax = maximum), fill = "#969696") +
  geom_line(aes(y = average), size = 0.75)+
  scale_x_date(name="Month", date_labels = "%b %Y") +
  geom_segment(mapping=aes(x=as.Date("2013-01-22", "%Y-%m-%d"), y=36, xend=as.Date("2013-01-22", "%Y-%m-%d"), yend=31), size = 1, arrow=arrow(type = "closed", angle = 30, length = unit(0.3, "cm"))) + 
  geom_segment(mapping=aes(x=as.Date("2012-04-20", "%Y-%m-%d"), y=36, xend=as.Date("2012-04-20", "%Y-%m-%d"), yend=31), size = 1, arrow=arrow(type = "closed", angle = 30, length = unit(0.3, "cm"))) + 
  scale_y_continuous(breaks=seq(0,36,6), name="Temperature ("~degree~"C )", limits=c(0, 36))+
  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75),
      text = element_text(size=18), 
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.text.x = element_text(size=18),
      axis.text.y = element_text(size=18),
      legend.position="none")


#FigureS1C
campo_hidrico = read.csv("FigureS1C.txt", sep="\t")
campo_hidrico$Month = as.character(campo_hidrico$Month)
campo_hidrico$Month = as.Date(campo_hidrico$Month, format = "%d/%m/%Y")
campo_hidrico2 <- melt(campo_hidrico, id.vars="Month")
names(campo_hidrico2) [names(campo_hidrico2) == "value"] <- "Water.status"

seasonal_water <- ggplot(data=campo_hidrico2, aes(x=Month, y=Water.status, group=variable)) +
  geom_area(data=subset(campo_hidrico2, Water.status <= 0), aes(col=variable), fill="#e41a1c", linetype=0)  +
  geom_area(data=subset(campo_hidrico2, Water.status >= 0), aes(col=variable), fill="#377eb8", linetype=0)  +
  scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(name="Water balance (mm)", breaks=seq(-60,180,60), limits=c(-65, 185))+
  geom_segment(mapping=aes(x=as.Date("2013-01-22", "%Y-%m-%d"), y=150, xend=as.Date("2013-01-22", "%Y-%m-%d"), yend=110), size = 1, arrow=arrow(type = "closed", angle = 30, length = unit(0.3, "cm"))) + 
  geom_segment(mapping=aes(x=as.Date("2012-04-20", "%Y-%m-%d"), y=150, xend=as.Date("2012-04-20", "%Y-%m-%d"), yend=110), size = 1, arrow=arrow(type = "closed", angle = 30, length = unit(0.3, "cm"))) + 
    theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75),
      text = element_text(size=18), 
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.text.x = element_text(size=18),
      axis.text.y = element_text(size=18),
      legend.position="none")


#FigureS1BD
diaria = read.csv("FigureS1BD.txt", sep="\t")

temperature_daily_field <- ggplot(data=diaria, aes(x=Hour, y=Temperature)) +
                        annotate("rect", xmin = -2, xmax = 0, ymin = 15, ymax = 30, alpha = .5, fill = "#e3e3e3")+
                        annotate("rect", xmin = 13.5, xmax = 24, ymin = 15, ymax = 30, alpha = .5, fill = "#e3e3e3")+
                        geom_line(size = 1.2) +
                        scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
                        scale_y_continuous(name="Temperature ("~degree~"C )", breaks=seq(15,30,3), limits=c(15, 30))+
                        theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75),
                              text = element_text(size=18), 
                              axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              axis.text.x = element_text(size=18),
                              axis.text.y = element_text(size=18),
                              legend.position="none")


light_daily_field <- ggplot(data=diaria, aes(x=Hour, y=Light)) +
                  annotate("rect", xmin = -2, xmax = 0, ymin = 0, ymax = 3, alpha = .5, fill = "#e3e3e3")+
                  annotate("rect", xmin = 13.5, xmax = 24, ymin = 0, ymax = 3, alpha = .5, fill = "#e3e3e3")+
                  geom_line(size = 1.2) +
                  scale_x_continuous(breaks=seq(0,24,6), name="Time from dawn (h)", limits=c(-2, 24))+
                  scale_y_continuous(name="Light Intensity (MJ/" ~ m^{2}~")", breaks=seq(0,3,0.6), limits=c(0, 3))+
                  theme(panel.grid.major = element_line(colour = "#efefef", size = 0.75), 
                  text = element_text(size=18), 
                  axis.ticks = element_blank(),
                  axis.line = element_blank(),
                  axis.text.x = element_text(size=18),
                  axis.text.y = element_text(size=18),
                  legend.position="none")

plot_grid(seasonal_temperature, temperature_daily_field, seasonal_water, light_daily_field, labels = c("a", "b", "c", "d"), ncol= 2, align = "v",label_size = 20)
