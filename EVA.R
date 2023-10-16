## Exposure variation analysis for time series.
##
Power <- read.csv("time serie vector")
Power=as.matrix(Power)
Time=seq(from = 1, to = length(Power), by = 1)

#Power=Power[apply(Power, 1, function(row) all(row !=0 )), ]

APO=0
IPOh=0
REGh=0
T_1=0
IPOl=0
REGl=0

Mean=mean(Power)

for (j in 1:length(Power == TRUE)) {
  if ((0.95*Mean)<=Power[j] & Power[j]<=(1.05*Mean)) {
    APO=APO+1
    T_1=0}
  else {
    T_1=T_1+1
  }
  
  if ((1.05*Mean)<Power[j] & T_1>=1 & T_1<=10) {
    REGh=REGh+1
  }
  
  if ((0.95*Mean)>Power[j] & T_1>=1 & T_1<=10) {
    REGl=REGl+1
  }
  
  
  if (Power[j]<(0.95*Mean) & T_1>10) {
    IPOl=IPOl+1
  }
  
  if ((1.05*Mean)<Power[j]  & T_1>10 ) {
    IPOh=IPOh+1
  }
}


L=length(Power)
IPOh=round(IPOh/L*100)
REGh=round(REGh/L*100)
IPOl=round(IPOl/L*100)
REGl=round(REGl/L*100)
APO=round(APO/L*100)


#Mean across time
MP=Power
for (o in 1:length(Power == TRUE))
  MP[o]=mean(Power[1:o])

T=seq(1,L,1)/60

print(paste0("IPOh = ", IPOh, " %"))
print(paste0("IPOl = ", IPOl, " %"))
print(paste0("REGh = ", REGh, " %"))
print(paste0("REGl = ", REGl, " %"))
print(paste0("APO = ", APO, " %"))
print(paste0("Temps = ", L, " sec"))

library(gridExtra)
library(ggplot2)

min=as.data.frame(replicate(length(Power), round(0.95*Mean)))
min[,2]=Time
max=replicate(length(Power), round(1.05*Mean))

p<-qplot(T,Power,geom=c("point", "line"))
p<-p+geom_area(aes(T,min[,1]),colour="black",fill = "yellow")
p<-p+geom_ribbon(aes(ymin=round(1.05*Mean), ymax=max(Power)),colour="black",fill = "orange")+scale_x_continuous(breaks=round(seq(0, max(T),max(T)/10),2))



p<-p+geom_ribbon(aes(ymin=round(1.05*Mean), ymax=round(0.95*Mean)),colour="black",fill = "green")

p<-p+geom_point(aes(T,MP), color="red", lwd=2)

p<-p+geom_line(aes(T,Power),size=1)
p<-p+ theme(
  plot.background = element_rect(fill = "white"), 
  panel.background = element_rect(fill = "grey", colour="black"))+
  labs(title="EVA analysis",x ="Time (min)")+theme(axis.text.x = element_text(face="bold", color="black", size=16),
                                                   axis.text.y = element_text(face="bold", color="black", 
                                                    size=16, angle=0, margin = margin(t = -5, r = 5, b = 5, l = 5)),
                                                   axis.title.y = element_blank())

p<-p + theme(
  plot.title = element_text(color="red", size=20, face="bold"),
  axis.title.x = element_text(color="blue", size=18, face="bold")
)

b=T

p<-p +annotate(geom="text", x=b[length(Power)/2], y=round(1.2*Mean), label="105% of mean PO", size=7, color="cyan")+annotate(geom="text", x=b[length(Power)/2], y=round(0.7*Mean), label="95% of mean PO", size=7, color="cyan")+theme(
  plot.title = element_text(hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5)
)


EVA=as.data.frame(REGl)
EVA[2,1]=as.data.frame(REGh)
EVA[3,1]=as.data.frame(APO)
EVA[4,1]=as.data.frame(IPOl)
EVA[5,1]=as.data.frame(IPOh)

EVA[1:2,2]="Short time regulations (REG)"
EVA[3,2]="Acurate PO (APO)"
EVA[4:5,2]="Inaccurate PO (IPO)"

EVA[1,3]="- 95%"
EVA[4,3]="- 95%"
EVA[2,3]="+ 105%"
EVA[5,3]="+ 105%"
EVA[3,3]=" 95 to 105 %"

c<-c("Percentage of total TT duration","Intensity regulation indices", "PO (% of mean PO)")
colnames(EVA)<-c

h<-ggplot(data=EVA, aes(x=EVA[,3], y=EVA[,1], fill=EVA[,2]))+     geom_bar(stat="identity", color="black")+scale_fill_manual(values=c("green", "red", "dodgerblue"))+labs(title="",x ="PO (% of mean PO)", y = "Percentage of total TT duration")

h<-h + labs(title = paste("Puissance Moyenne :",round(Mean)," W","     Duree :", paste(length(Power),"sec.")), x = "PO (% of mean PO)", y = "Percentage of total TT duration")+theme(axis.text.x = element_text(face="bold", color="black", 
                                                                                                                                                                                                                size=16),
                                                                                                                                                                                     axis.text.y = element_text(face="bold", color="black", 
                                                                                                                                                                                                                size=16, angle=0, margin = margin(r = 20)),
                                                                                                                                                                                     axis.title.y    = element_text(hjust = -140,vjust = -1 ),
                                                                                                                                                                                     legend.title = element_blank(),
                                                                                                                                                                                     legend.text = element_text(size=20,  face="bold")
)

h<-h+theme(
  plot.title = element_text(color="red", size=20, face="bold"),
  axis.title.x = element_text(color="blue", size=18, face="bold"),
  axis.title.y = element_text(color="#993333", size=18, face="bold")
)


if (REGl>0) {
  h<-h +annotate(geom="text", x=1, y=REGl/2, label=paste(REGl, "%"), size=7, color="black")
}

if (REGh>0) {
  h<-h+annotate(geom="text", x=3, y=REGh/2, label=paste(REGh, "%"), size=7, color="black")
}

h<- h+annotate(geom="text", x=2, y=APO/2, label=paste(APO, "%"), size=10, color="black")

if (IPOh>0) {
  h<-h+annotate(geom="text", x=3, y=(IPOh/2+REGh), label=paste(IPOh, "%"), size=7, color="black")
}

if (IPOl>0) {
  h<-h+annotate(geom="text", x=1, y=(IPOl/2+REGl), label=paste(IPOl, "%"), size=7, color="black")
}











grid.arrange(p, h, ncol = 1, nrow=2, heights=c(2.5,3))

