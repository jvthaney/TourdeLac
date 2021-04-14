#### Packages #######
library(ggplot2)
library(colorspace)
library(car)
library(scales)
library(reshape2)
library(GGally)
library(grid)
library(gridExtra)
library(nlme)
library(stringr)
library(dplyr)
library(RColorBrewer)
library(wesanderson)
library(gtable)
library(ggallin)
library(gdata)
library(data.table)
library(mltools)
library(caret)
library(tidyverse)
library(broom)
library(lubridate)
library(rcompanion)
library(ggpmisc)


#### Define Colors ###########################
mycolors=c("#332288","#88CCEE","#44AA99","#117733",
           "#999933","#DDCC77","#CC6677","#882255","#AA4499","#DDDDDD",
           "#CC3311","#777777","#EE8866","#EE3377","#000000","#EAECCC")

mycolors2=c("#4477AA","#228833","#EE6677","#CCBB44",
            "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF")


# Paul tol's 21 color rainbow
tol14rainbow=c("#882E72", "#B178A6", "#D6C1DE", "#1965B0", "#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C", "#DC050C")
tol15rainbow=c("#114477", "#4477AA", "#77AADD", "#117755", "#44AA88", "#99CCBB", "#777711", "#AAAA44", "#DDDD77", "#771111", "#AA4444", "#DD7777", "#771144", "#AA4477", "#DD77AA")
tol18rainbow=c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
# ...and finally, the Paul Tol 21-color salute
tol21rainbow= c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")


### Read SAMPLE POINT data for Multibuffer Analysis ###################
# Read GEE output files (5 day window, buoy location on Lake Sunapee)
buff3<-read.csv("3m_5day_buoy.csv",header=TRUE)
buff3<-data.frame(append(buff3,c(BufferSize=3),after=1))
buff30<-read.csv("30m_5day_buoy.csv",header=TRUE)
buff30<-data.frame(append(buff30,c(BufferSize=30),after=1))
buff60<-read.csv("60m_5day_buoy.csv",header=TRUE)
buff60<-data.frame(append(buff60,c(BufferSize=60),after=1))
buff90<-read.csv("90m_5day_buoy.csv",header=TRUE)
buff90<-data.frame(append(buff90,c(BufferSize=90),after=1))
buff120<-read.csv("120m_5day_buoy.csv",header=TRUE)
buff120<-data.frame(append(buff120,c(BufferSize=120),after=1))
buff150<-read.csv("150m_5day_buoy.csv",header=TRUE)
buff150<-data.frame(append(buff150,c(BufferSize=150),after=1))
buff180<-read.csv("180m_5day_buoy.csv",header=TRUE)
buff180<-data.frame(append(buff180,c(BufferSize=180),after=1))
buff210<-read.csv("210m_5day_buoy.csv",header=TRUE)
buff210<-data.frame(append(buff210,c(BufferSize=210),after=1))
buff240<-read.csv("240m_5day_buoy.csv",header=TRUE)
buff240<-data.frame(append(buff240,c(BufferSize=240),after=1))
buff270<-read.csv("270m_5day_buoy.csv",header=TRUE)
buff270<-data.frame(append(buff270,c(BufferSize=270),after=1))
buff300<-read.csv("300m_5day_buoy.csv",header=TRUE)
buff300<-data.frame(append(buff300,c(BufferSize=300),after=1))
buff330<-read.csv("330m_5day_buoy.csv",header=TRUE)
buff330<-data.frame(append(buff330,c(BufferSize=330),after=1))
buff360<-read.csv("360m_5day_buoy.csv",header=TRUE)
buff360<-data.frame(append(buff360,c(BufferSize=360),after=1))
buff390<-read.csv("390m_5day_buoy.csv",header=TRUE)
buff390<-data.frame(append(buff390,c(BufferSize=390),after=1))
buff420<-read.csv("420m_5day_buoy.csv",header=TRUE)
buff420<-data.frame(append(buff420,c(BufferSize=420),after=1))
buff450<-read.csv("450m_5day_buoy.csv",header=TRUE)
buff450<-data.frame(append(buff450,c(BufferSize=450),after=1))
buff480<-read.csv("480m_5day_buoy.csv",header=TRUE)
buff480<-data.frame(append(buff480,c(BufferSize=480),after=1))
buff510<-read.csv("510m_5day_buoy.csv",header=TRUE)
buff510<-data.frame(append(buff510,c(BufferSize=510),after=1))


# Remove Lakes from buffer sizes that touch land
b3<-subset(buff3,ï..Lake=="Morey"|ï..Lake=="Mascoma"|ï..Lake=="Messalonskee"|ï..Lake=="Auburn"|ï..Lake=="Sebago"|ï..Lake=="Sabattus"|
             ï..Lake=="Panther"|ï..Lake=="Fairlee"|ï..Lake=="Androscoggin"|ï..Lake=="Thomas"|ï..Lake=="Goose"|ï..Lake=="Long Pond"|
             ï..Lake=="Long ï..Lake"|ï..Lake=="Crescent"|ï..Lake=="Sunapee")

b30<-subset(buff30,ï..Lake=="Morey"|ï..Lake=="Mascoma"|ï..Lake=="Messalonskee"|ï..Lake=="Auburn"|ï..Lake=="Sebago"|ï..Lake=="Sabattus"|
              ï..Lake=="Panther"|ï..Lake=="Fairlee"|ï..Lake=="Androscoggin"|ï..Lake=="Thomas"|ï..Lake=="Goose"|ï..Lake=="Long Pond"|
              ï..Lake=="Long ï..Lake"|ï..Lake=="Crescent"|ï..Lake=="Sunapee")

b60<-subset(buff60,ï..Lake=="Morey"|ï..Lake=="Mascoma"|ï..Lake=="Messalonskee"|ï..Lake=="Auburn"|ï..Lake=="Sebago"|ï..Lake=="Sabattus"|
              ï..Lake=="Panther"|ï..Lake=="Fairlee"|ï..Lake=="Androscoggin"|ï..Lake=="Thomas"|ï..Lake=="Goose"|ï..Lake=="Long Pond"|
              ï..Lake=="Long ï..Lake"|ï..Lake=="Crescent"|ï..Lake=="Sunapee")

b90<-subset(buff90,ï..Lake=="Morey"|ï..Lake=="Mascoma"|ï..Lake=="Messalonskee"|ï..Lake=="Auburn"|ï..Lake=="Sebago"|ï..Lake=="Sabattus"|
              ï..Lake=="Panther"|ï..Lake=="Fairlee"|ï..Lake=="Androscoggin"|ï..Lake=="Thomas"|ï..Lake=="Goose"|ï..Lake=="Long Pond"|
              ï..Lake=="Long ï..Lake"|ï..Lake=="Crescent"|ï..Lake=="Sunapee")

b120<-subset(buff120,ï..Lake=="Morey"|ï..Lake=="Mascoma"|ï..Lake=="Messalonskee"|ï..Lake=="Auburn"|ï..Lake=="Sebago"|ï..Lake=="Sabattus"|
               ï..Lake=="Panther"|ï..Lake=="Fairlee"|ï..Lake=="Androscoggin"|ï..Lake=="Thomas"|ï..Lake=="Goose"|ï..Lake=="Long Pond"|
               ï..Lake=="Long ï..Lake"|ï..Lake=="Crescent"|ï..Lake=="Sunapee")

b150<-subset(buff150,ï..Lake=="Morey"|ï..Lake=="Mascoma"|ï..Lake=="Messalonskee"|ï..Lake=="Auburn"|ï..Lake=="Sebago"|ï..Lake=="Sabattus"|
               ï..Lake=="Panther"|ï..Lake=="Fairlee"|ï..Lake=="Androscoggin"|ï..Lake=="Thomas"|ï..Lake=="Goose"|ï..Lake=="Long Pond"|
               ï..Lake=="Long ï..Lake"|ï..Lake=="Sunapee")

b180<-subset(buff180,ï..Lake=="Morey"|ï..Lake=="Mascoma"|ï..Lake=="Messalonskee"|ï..Lake=="Auburn"|ï..Lake=="Sebago"|ï..Lake=="Sabattus"|
               ï..Lake=="Panther"|ï..Lake=="Fairlee"|ï..Lake=="Androscoggin"|ï..Lake=="Thomas"|ï..Lake=="Goose"|ï..Lake=="Long Pond"|
               ï..Lake=="Long ï..Lake"|ï..Lake=="Sunapee")

b210<-subset(buff210,ï..Lake=="Morey"|ï..Lake=="Mascoma"|ï..Lake=="Messalonskee"|ï..Lake=="Auburn"|ï..Lake=="Sebago"|ï..Lake=="Sabattus"|
               ï..Lake=="Panther"|ï..Lake=="Fairlee"|ï..Lake=="Androscoggin"|ï..Lake=="Thomas"|ï..Lake=="Goose"|ï..Lake=="Long Pond"|ï..Lake=="Sunapee")

b240<-subset(buff240,ï..Lake=="Morey"|ï..Lake=="Mascoma"|ï..Lake=="Messalonskee"|ï..Lake=="Auburn"|ï..Lake=="Sebago"|ï..Lake=="Sabattus"|
               ï..Lake=="Panther"|ï..Lake=="Fairlee"|ï..Lake=="Androscoggin"|ï..Lake=="Thomas"|ï..Lake=="Goose"|ï..Lake=="Long Pond"|ï..Lake=="Sunapee")

b270<-subset(buff270,ï..Lake=="Morey"|ï..Lake=="Mascoma"|ï..Lake=="Messalonskee"|ï..Lake=="Auburn"|ï..Lake=="Sebago"|ï..Lake=="Sabattus"|
               ï..Lake=="Panther"|ï..Lake=="Fairlee"|ï..Lake=="Androscoggin"|ï..Lake=="Thomas"|ï..Lake=="Goose"|ï..Lake=="Long Pond"|ï..Lake=="Sunapee")

b300<-subset(buff300,ï..Lake=="Morey"|ï..Lake=="Mascoma"|ï..Lake=="Messalonskee"|ï..Lake=="Auburn"|ï..Lake=="Sebago"|ï..Lake=="Sabattus"|
               ï..Lake=="Panther"|ï..Lake=="Fairlee"|ï..Lake=="Androscoggin"|ï..Lake=="Thomas"|ï..Lake=="Sunapee")

b330<-subset(buff330,ï..Lake=="Morey"|ï..Lake=="Mascoma"|ï..Lake=="Messalonskee"|ï..Lake=="Auburn"|ï..Lake=="Sebago"|
               ï..Lake=="Sabattus"|ï..Lake=="Sunapee")

b360<-subset(buff360,ï..Lake=="Morey"|ï..Lake=="Mascoma"|ï..Lake=="Messalonskee"|ï..Lake=="Auburn"|ï..Lake=="Sebago"|ï..Lake=="Sunapee")

b390<-subset(buff390,ï..Lake=="Morey"|ï..Lake=="Mascoma"|ï..Lake=="Messalonskee"|ï..Lake=="Auburn"|ï..Lake=="Sebago"|ï..Lake=="Sunapee")

b420<-subset(buff420,ï..Lake=="Morey"|ï..Lake=="Mascoma"|ï..Lake=="Messalonskee"|ï..Lake=="Auburn"|ï..Lake=="Sunapee")

b450<-subset(buff450,ï..Lake=="Morey"|ï..Lake=="Mascoma"|ï..Lake=="Messalonskee"|ï..Lake=="Sunapee")

b480<-subset(buff480,ï..Lake=="Morey"|ï..Lake=="Sunapee")

b510<-subset(buff510,ï..Lake=="Sunapee")

# Merge buffer subsets into one dataset
buffers<-rbind(b3,b30,b60,b90,b120,b150,b180,b210,b240,b270,b300,b330,b360,b390,b420,b450,b480,b510)
buffers<-rename(buffers,Lake = ï..Lake)
buffers$time_zone<-"EDT"
buffers$state<-""

# Merge buffer subsets with whole lake polygons
buffwhole<-read.csv("WholeLakeStats.csv",header=TRUE) ##Same band stats for whole lake polygons
buffersW<-rbind(buffers,buffwhole)

# Add column called "Landsat" that takes first four characters of imgname
buffers$Landsat<-substr(buffers$imgname, 1, 4)
buffersW$Landsat<-substr(buffersW$imgname, 1, 4)

# Band 7 (swir2) check cutoff - remove values >200 and count = 0 for nir band (no GEE image) n = 133 no image
sum(is.na(buffersW$nir_median)) # 142 for just reg. bands
sum(is.na(buffersW$swir2_median)) # 142 - more missing values


# Check how many SWIR2 (band 7) values are above 250
sum(buffersW$swir2_median>=250,na.rm=TRUE) #62
sum(buffersW$swir2_mean>=250,na.rm=TRUE) #99

# Histogram of swir2 values before filtering 
buffersW%>%
  ggplot(aes(swir2_median))+theme_classic()+
  geom_histogram(binwidth=10,fill="grey", color="black", alpha=0.9)+labs(title="Sampling point")

# Plot swir2 values per lake before filtering 
buffersW%>%
  ggplot(aes(Lake,swir2_median,shape=Landsat,color=Landsat))+theme_classic()+geom_hline(yintercept=c(200,250))+
  geom_point(size=3,position=position_dodge(width=.2))+scale_color_manual(values=mycolors)+labs(title="Sampling point")
#theme(axis.text.x=element_text(size=14,angle=300,hjust=0))

buffersW%>%
  ggplot(aes(Lake,nir_median,shape=Landsat,color=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=.2))+scale_color_manual(values=mycolors)+labs(title="Sampling point")
#theme(axis.text.x=element_text(size=14,angle=300,hjust=0))

# Band 7 (swir2) cutoff - remove values >200
is.na(buffersW$swir2_mean) <- buffers$swir2_mean >=200         
is.na(buffersW$swir2_median) <- buffers$swir2_median >=200         
is.na(buffersW$swir2_mode) <- buffers$swir2_mode >=200         
is.na(buffers$swir2_mean) <- buffers$swir2_mean >=200         
is.na(buffers$swir2_median) <- buffers$swir2_median >=200         
is.na(buffers$swir2_mode) <- buffers$swir2_mode >=200         


## Filter for 5 day only
#buffersW<-filter(buffersW,MaxDayDiff=='5')
#buffersW$MaxDayDiff

## Remove na rows
#buffersW<-buffersW%>%filter(!is.na(swir1_median))
#buffersW<-buffersW%>%filter(!is.na(swir2_median))


# Write table
write.table(buffersW,"buffersW.csv",sep=" ,",col.names = NA)
#write.table(buffersCW,"buffersCW.csv",sep=" ,",col.names = NA)
# Check Sunapee
#Sunapee<-filter(buffersW,Lake=="Sunapee")

############### Add column for day difference #####
library(lubridate)
buffersW$acq.date.string<-substr(buffersW$imgname,start=18,stop=26)
buffersW$acq.date.string
buffersW$acq.date<-as_date(as.character(buffersW$acq.date.string))
buffersW$acq.date
buffersW$sample.date<-substr(buffersW$datetime,start=1,stop=10)
buffersW$sample.date<-as_date(buffersW$sample.date)
buffersW$DayDiff<-as_date(buffersW$sample.date,format="%Y-%m-%d")-as_date(buffersW$acq.date,format="%Y-%m-%d")
buffersW$DayDiff<-as.numeric(abs(buffersW$DayDiff))


############### Summary table to check stuff ######
toursummary_s<-buffersW%>%group_by(Lake,BufferSize,acq.date)%>%
  summarise(satellite = Landsat,
            pixcount = (blue_count),
            red = (red_median),
            green = (green_median),
            blue = (blue_median),
            nir = (nir_median),
            swir1 = (swir1_median),
            swir2 = (swir2_median),
            sample.date = sample.date,
            daydiff = DayDiff,
            nimages = n(),
            imgname = imgname)

write.table(toursummary_s,"toursummary_s.csv",sep=" ,",col.names=NA)

## Remove na rows
buffersW<-buffersW%>%filter(!is.na(swir1_median))

##
### RESHAPE sample pt WITH polygons ######
## Reshape data
buffersW<-as.data.table(buffersW)
tdl1sw<-
  melt(buffersW,id.variables=c("blue_count","green_count","red_count","swir1_count","swir2_count","nir_count",
                               "blue_median","green_median","red_median","swir1_median","swir2_median","nir_median",
                               "blue_stdDev","green_stdDev","red_stdDev","swir1_stdDev","swir2_stdDev","nir_stdDev",
                               "MaxDayDiff","BufferSize","datetime",
                               "Landsat","Lake","state"),
       variable.name=c("Band.Mean"),
       value.name=c("mean"),
       measure.vars=c("blue_mean","green_mean","red_mean","swir1_mean","swir2_mean","nir_mean"))

tdl2sw<-
  melt(tdl1sw,id.variables=c("Band.Mean","mean",
                             "blue_median","green_median","red_median","swir1_median","swir2_median","nir_median",
                             "blue_stdDev","green_stdDev","red_stdDev","swir1_stdDev","swir2_stdDev","nir_stdDev",
                             "MaxDayDiff","BufferSize","datetime",
                             "Landsat","Lake","state"),
       variable.name=c("Band.Count"),
       value.name=c("count"),
       measure.vars=c("blue_count","green_count","red_count","swir1_count","swir2_count","nir_count"))

tdl3sw<-
  melt(tdl2sw,id.variables=c("Band.Mean","mean","Band.Count","count",
                             "blue_stdDev","green_stdDev","red_stdDev","swir1_stdDev","swir2_stdDev","nir_stdDev",
                             "MaxDayDiff","BufferSize","datetime",
                             "Landsat","Lake","state"),
       variable.name=c("Band.Median"),
       value.name=c("median"),
       measure.vars=c( "blue_median","green_median","red_median","swir1_median","swir2_median","nir_median"))

tdlsw<-
  melt(tdl3sw,id.variables=c("Band.Mean","mean","Band.Count","count",
                             "Band Median","median",
                             "MaxDayDiff","BufferSize","datetime",
                             "Landsat","Lake","state"),
       variable.name=c("Band.std"),
       value.name=c("std"),
       measure.vars=c("blue_stdDev","green_stdDev","red_stdDev","swir1_stdDev","swir2_stdDev","nir_stdDev"),
       na.rm = TRUE)

## Make Time Window categorical
tdlsw$MaxDayDiff<-as.factor(tdlsw$MaxDayDiff)

## Rename factor levels
levels(tdlsw$Band.Mean)
levels(tdlsw$Band.Mean)<-c("blue","green","red","swir1","swir2","nir")

levels(tdlsw$Band.Median)
levels(tdlsw$Band.Median)<-c("blue","green","red","swir1","swir2","nir")

levels(tdlsw$Band.Count)
levels(tdlsw$Band.Count)<-c("blue","green","red","swir1","swir2","nir")

levels(tdlsw$Band.std)
levels(tdlsw$Band.std)<-c("blue","green","red","swir1","swir2","nir")


####### Plot
## Categorical
tdlsw$BufferSize<-as.factor(tdlsw$BufferSize)

## Order X axis
tdlsw$BufferSize <- factor(tdlsw$BufferSize, levels=c("3","30","60","90","120","150","180","210","240","270","300",
                                                      "330","360","390","420","450","480","Whole"))



############### Plot Summary Stats (sample point) ####
# Std
StdSW<-
  ggplot(tdlsw,aes(BufferSize,std,color=Band.std,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  labs(title="5 days, center of lake",x="Buffer Size (m)")
a <- annotation_logticks(sides='l')
BuffCWStd<-StdSW+a
ggsave("BuffCWStd.png",BuffCWStd,height=6,width=12,dpi=100)

# Mean
BuffMeanSW<-
  ggplot(tdlsw,aes(BufferSize,mean,color=Band.Mean,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=18,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(1,1500))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  labs(title="5 days, center of lake",x="Buffer size (m)")
ggsave("BuffMeanSW.png",BuffMeanC,height=6,width=12,dpi=100)

# Median
BuffMedianSW<-
  ggplot(tdlsw,aes(BufferSize,median,color=Band.Median,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=18,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(1,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  labs(title="5 days, center of lake",x="Buffer size (m)")
ggsave("BuffMedianSW.png",BuffMedianSW,height=6,width=12,dpi=100)

# Count
BuffCountSW<-
  ggplot(tdlsw,aes(BufferSize,count,color=Band.Count,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=18,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(1,200000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  labs(title="5 days, center of lake",x="Buffer size (m)")
ggsave("BuffCountSW.png",BuffCountSW,height=6,width=12,dpi=100)

## Plot together
BuffFigs_5dSW <- arrangeGrob(BuffCountSW,StdSW,BuffMeanSW,BuffMedianSW,nrow=2, ncol=2)

ggsave("BuffFigs_5dCW.png",BuffFigs_5dSW,height=15,width=20,dpi=100)




### Read CENTER POINT data for Multibuffer Analysis ###################
# Read GEE output files (5 day window only)
buff3c<-read.csv("3m_5day_clag.csv",header=TRUE)
buff3c<-data.frame(append(buff3c,c(BufferSize=3),after=1))
buff30c<-read.csv("30m_5day_clag.csv",header=TRUE)
buff30c<-data.frame(append(buff30c,c(BufferSize=30),after=1))
buff60c<-read.csv("60m_5day_clag.csv",header=TRUE)
buff60c<-data.frame(append(buff60c,c(BufferSize=60),after=1))
buff90c<-read.csv("90m_5day_clag.csv",header=TRUE)
buff90c<-data.frame(append(buff90c,c(BufferSize=90),after=1))
buff120c<-read.csv("120m_5day_clag.csv",header=TRUE)
buff120c<-data.frame(append(buff120c,c(BufferSize=120),after=1))
buff150c<-read.csv("150m_5day_clag.csv",header=TRUE)
buff150c<-data.frame(append(buff150c,c(BufferSize=150),after=1))
buff180c<-read.csv("180m_5day_clag.csv",header=TRUE)
buff180c<-data.frame(append(buff180c,c(BufferSize=180),after=1))
buff210c<-read.csv("210m_5day_clag.csv",header=TRUE)
buff210c<-data.frame(append(buff210c,c(BufferSize=210),after=1))
buff240c<-read.csv("240m_5day_clag.csv",header=TRUE)
buff240c<-data.frame(append(buff240c,c(BufferSize=240),after=1))
buff270c<-read.csv("270m_5day_clag.csv",header=TRUE)
buff270c<-data.frame(append(buff270c,c(BufferSize=270),after=1))
buff300c<-read.csv("300m_5day_clag.csv",header=TRUE)
buff300c<-data.frame(append(buff300c,c(BufferSize=300),after=1))
buff330c<-read.csv("330m_5day_clag.csv",header=TRUE)
buff330c<-data.frame(append(buff330c,c(BufferSize=330),after=1))
buff360c<-read.csv("360m_5day_clag.csv",header=TRUE)
buff360c<-data.frame(append(buff360c,c(BufferSize=360),after=1))
buff390c<-read.csv("390m_5day_clag.csv",header=TRUE)
buff390c<-data.frame(append(buff390c,c(BufferSize=390),after=1))
buff420c<-read.csv("420m_5day_clag.csv",header=TRUE)
buff420c<-data.frame(append(buff420c,c(BufferSize=420),after=1))
buff450c<-read.csv("450m_5day_clag.csv",header=TRUE)
buff450c<-data.frame(append(buff450c,c(BufferSize=450),after=1))
buff480c<-read.csv("480m_5day_clag.csv",header=TRUE)
buff480c<-data.frame(append(buff480c,c(BufferSize=480),after=1))
buff510c<-read.csv("510m_5day_clag.csv",header=TRUE)
buff510c<-data.frame(append(buff510c,c(BufferSize=510),after=1))
buff540c<-read.csv("540m_5day_clag.csv",header=TRUE)
buff540c<-data.frame(append(buff540c,c(BufferSize=540),after=1))
buff570c<-read.csv("570m_5day_clag.csv",header=TRUE)
buff570c<-data.frame(append(buff570c,c(BufferSize=570),after=1))
buff600c<-read.csv("600m_5day_clag.csv",header=TRUE)
buff600c<-data.frame(append(buff600c,c(BufferSize=600),after=1))
buff630c<-read.csv("630m_5day_clag.csv",header=TRUE)
buff630c<-data.frame(append(buff630c,c(BufferSize=630),after=1))
buff660c<-read.csv("660m_5day_clag.csv",header=TRUE)
buff660c<-data.frame(append(buff660c,c(BufferSize=660),after=1))
buff690c<-read.csv("690m_5day_clag.csv",header=TRUE)
buff690c<-data.frame(append(buff690c,c(BufferSize=690),after=1))
buff720c<-read.csv("720m_5day_clag.csv",header=TRUE)
buff720c<-data.frame(append(buff720c,c(BufferSize=720),after=1))
buff750c<-read.csv("750m_5day_clag.csv",header=TRUE)
buff750c<-data.frame(append(buff750c,c(BufferSize=750),after=1))
buff780c<-read.csv("780m_5day_clag.csv",header=TRUE)
buff780c<-data.frame(append(buff780c,c(BufferSize=780),after=1))
buff810c<-read.csv("810m_5day_clag.csv",header=TRUE)
buff810c<-data.frame(append(buff810c,c(BufferSize=810),after=1))
buff840c<-read.csv("840m_5day_clag.csv",header=TRUE)
buff840c<-data.frame(append(buff840c,c(BufferSize=840),after=1))
buff870c<-read.csv("870m_5day_clag.csv",header=TRUE)
buff870c<-data.frame(append(buff870c,c(BufferSize=870),after=1))
buff900c<-read.csv("900m_5day_clag.csv",header=TRUE)
buff900c<-data.frame(append(buff900c,c(BufferSize=900),after=1))
buff930c<-read.csv("930m_5day_clag.csv",header=TRUE)
buff930c<-data.frame(append(buff930c,c(BufferSize=930),after=1))
buff960c<-read.csv("960m_5day_clag.csv",header=TRUE)
buff960c<-data.frame(append(buff960c,c(BufferSize=960),after=1))
buff990c<-read.csv("990m_5day_clag.csv",header=TRUE)
buff990c<-data.frame(append(buff990c,c(BufferSize=990),after=1))
buff1020c<-read.csv("1020m_5day_clag.csv",header=TRUE)
buff1020c<-data.frame(append(buff1020c,c(BufferSize=1020),after=1))
buff1050c<-read.csv("1050m_5day_clag.csv",header=TRUE)
buff1050c<-data.frame(append(buff1050c,c(BufferSize=1050),after=1))
buff1080c<-read.csv("1080m_5day_clag.csv",header=TRUE)
buff1080c<-data.frame(append(buff1080c,c(BufferSize=1080),after=1))

# Remove org_org_lake_name_names from buffer sizes that touch land
b3c<-subset(buff3c,org_lake_name=="Fairlee"|org_lake_name=="Crescent"|org_lake_name=="Thomas"|org_lake_name=="Long Pond"|org_lake_name=="Goose"|org_lake_name=="Mascoma"|
              org_lake_name=="Morey"|org_lake_name=="Long Lake"|org_lake_name=="Sabattus"|org_lake_name=="Panther"|org_lake_name=="Androscoggin"|org_lake_name=="Sunapee"|
              org_lake_name=="Auburn"|org_lake_name=="Messalonskee"|org_lake_name=="Sebago")
b30c<-subset(buff30c,org_lake_name=="Fairlee"|org_lake_name=="Crescent"|org_lake_name=="Thomas"|org_lake_name=="Long Pond"|org_lake_name=="Goose"|org_lake_name=="Mascoma"|
               org_lake_name=="Morey"|org_lake_name=="Long Lake"|org_lake_name=="Sabattus"|org_lake_name=="Panther"|org_lake_name=="Androscoggin"|org_lake_name=="Sunapee"|
               org_lake_name=="Auburn"|org_lake_name=="Messalonskee"|org_lake_name=="Sebago")
b60c<-subset(buff60c,org_lake_name=="Fairlee"|org_lake_name=="Crescent"|org_lake_name=="Thomas"|org_lake_name=="Long Pond"|org_lake_name=="Goose"|org_lake_name=="Mascoma"|
               org_lake_name=="Morey"|org_lake_name=="Long Lake"|org_lake_name=="Sabattus"|org_lake_name=="Panther"|org_lake_name=="Androscoggin"|org_lake_name=="Sunapee"|
               org_lake_name=="Auburn"|org_lake_name=="Messalonskee"|org_lake_name=="Sebago")
b90c<-subset(buff90c,org_lake_name=="Fairlee"|org_lake_name=="Crescent"|org_lake_name=="Thomas"|org_lake_name=="Long Pond"|org_lake_name=="Goose"|org_lake_name=="Mascoma"|
               org_lake_name=="Morey"|org_lake_name=="Long Lake"|org_lake_name=="Sabattus"|org_lake_name=="Panther"|org_lake_name=="Androscoggin"|org_lake_name=="Sunapee"|
               org_lake_name=="Auburn"|org_lake_name=="Messalonskee"|org_lake_name=="Sebago")
b120c<-subset(buff120c,org_lake_name=="Fairlee"|org_lake_name=="Crescent"|org_lake_name=="Thomas"|org_lake_name=="Long Pond"|org_lake_name=="Goose"|org_lake_name=="Mascoma"|
                org_lake_name=="Morey"|org_lake_name=="Long Lake"|org_lake_name=="Sabattus"|org_lake_name=="Panther"|org_lake_name=="Androscoggin"|org_lake_name=="Sunapee"|
                org_lake_name=="Auburn"|org_lake_name=="Messalonskee"|org_lake_name=="Sebago")
b150c<-subset(buff150c,org_lake_name=="Crescent"|org_lake_name=="Thomas"|org_lake_name=="Long Pond"|org_lake_name=="Goose"|org_lake_name=="Mascoma"|
                org_lake_name=="Morey"|org_lake_name=="Long Lake"|org_lake_name=="Sabattus"|org_lake_name=="Panther"|org_lake_name=="Androscoggin"|org_lake_name=="Sunapee"|
                org_lake_name=="Auburn"|org_lake_name=="Messalonskee"|org_lake_name=="Sebago")
b180c<-subset(buff180c,org_lake_name=="Crescent"|org_lake_name=="Thomas"|org_lake_name=="Long Pond"|org_lake_name=="Goose"|org_lake_name=="Mascoma"|
                org_lake_name=="Morey"|org_lake_name=="Long Lake"|org_lake_name=="Sabattus"|org_lake_name=="Panther"|org_lake_name=="Androscoggin"|org_lake_name=="Sunapee"|
                org_lake_name=="Auburn"|org_lake_name=="Messalonskee"|org_lake_name=="Sebago")
b210c<-subset(buff210c,org_lake_name=="Thomas"|org_lake_name=="Long Pond"|org_lake_name=="Goose"|org_lake_name=="Mascoma"|
                org_lake_name=="Morey"|org_lake_name=="Long Lake"|org_lake_name=="Sabattus"|org_lake_name=="Androscoggin"|org_lake_name=="Sunapee"|
                org_lake_name=="Auburn"|org_lake_name=="Messalonskee"|org_lake_name=="Sebago")
b240c<-subset(buff240c,org_lake_name=="Long Pond"|org_lake_name=="Goose"|org_lake_name=="Mascoma"|
                org_lake_name=="Morey"|org_lake_name=="Long Lake"|org_lake_name=="Sabattus"|org_lake_name=="Androscoggin"|org_lake_name=="Sunapee"|
                org_lake_name=="Auburn"|org_lake_name=="Messalonskee"|org_lake_name=="Sebago")
b270c<-subset(buff270c,org_lake_name=="Long Pond"|org_lake_name=="Mascoma"|
                org_lake_name=="Morey"|org_lake_name=="Long Lake"|org_lake_name=="Sabattus"|org_lake_name=="Androscoggin"|org_lake_name=="Sunapee"|
                org_lake_name=="Auburn"|org_lake_name=="Messalonskee"|org_lake_name=="Sebago")
b300c<-subset(buff300c,org_lake_name=="Long Pond"|org_lake_name=="Mascoma"|
                org_lake_name=="Long Lake"|org_lake_name=="Sabattus"|org_lake_name=="Androscoggin"|org_lake_name=="Sunapee"|
                org_lake_name=="Auburn"|org_lake_name=="Messalonskee"|org_lake_name=="Sebago")
b330c<-subset(buff330c,org_lake_name=="Long Pond"|org_lake_name=="Mascoma"|
                org_lake_name=="Long Lake"|org_lake_name=="Sabattus"|org_lake_name=="Androscoggin"|org_lake_name=="Sunapee"|
                org_lake_name=="Auburn"|org_lake_name=="Messalonskee"|org_lake_name=="Sebago")
b360c<-subset(buff360c,org_lake_name=="Long Lake"|org_lake_name=="Sabattus"|org_lake_name=="Androscoggin"|org_lake_name=="Sunapee"|
                org_lake_name=="Auburn"|org_lake_name=="Messalonskee"|org_lake_name=="Sebago")
b390c<-subset(buff390c,org_lake_name=="Long Lake"|org_lake_name=="Sabattus"|org_lake_name=="Androscoggin"|org_lake_name=="Sunapee"|
                org_lake_name=="Auburn"|org_lake_name=="Messalonskee"|org_lake_name=="Sebago")
b420c<-subset(buff420c,org_lake_name=="Long Lake"|org_lake_name=="Sabattus"|org_lake_name=="Androscoggin"|
                org_lake_name=="Auburn"|org_lake_name=="Messalonskee"|org_lake_name=="Sebago")
b450c<-subset(buff450c,org_lake_name=="Long Lake"|org_lake_name=="Sabattus"|org_lake_name=="Androscoggin"|
                org_lake_name=="Auburn"|org_lake_name=="Sebago")
b480c<-subset(buff480c,org_lake_name=="Long Lake"|org_lake_name=="Sabattus"|org_lake_name=="Androscoggin"|
                org_lake_name=="Auburn"|org_lake_name=="Sebago")
b510c<-subset(buff510c,org_lake_name=="Long Lake"|org_lake_name=="Sabattus"|org_lake_name=="Androscoggin"|
                org_lake_name=="Auburn"|org_lake_name=="Sebago")
b540c<-subset(buff540c,org_lake_name=="Long Lake"|org_lake_name=="Sabattus"|org_lake_name=="Androscoggin"|
                org_lake_name=="Auburn"|org_lake_name=="Sebago")
b570c<-subset(buff570c,org_lake_name=="Long Lake"|org_lake_name=="Sabattus"|org_lake_name=="Androscoggin"|
                org_lake_name=="Auburn"|org_lake_name=="Sebago")
b600c<-subset(buff600c,org_lake_name=="Androscoggin"|org_lake_name=="Auburn"|org_lake_name=="Sebago")
b630c<-subset(buff630c,org_lake_name=="Androscoggin"|org_lake_name=="Sebago")
b660c<-subset(buff660c,org_lake_name=="Sebago")
b690c<-subset(buff690c,org_lake_name=="Sebago")
b720c<-subset(buff720c,org_lake_name=="Sebago")
b750c<-subset(buff750c,org_lake_name=="Sebago")
b780c<-subset(buff780c,org_lake_name=="Sebago")
b810c<-subset(buff810c,org_lake_name=="Sebago")
b840c<-subset(buff840c,org_lake_name=="Sebago")
b870c<-subset(buff870c,org_lake_name=="Sebago")
b900c<-subset(buff900c,org_lake_name=="Sebago")
b930c<-subset(buff930c,org_lake_name=="Sebago")
b960c<-subset(buff960c,org_lake_name=="Sebago")
b990c<-subset(buff990c,org_lake_name=="Sebago")
b1020c<-subset(buff1020c,org_lake_name=="Sebago")
b1050c<-subset(buff1050c,org_lake_name=="Sebago")
b1080c<-subset(buff1080c,org_lake_name=="Sebago")

# Merge buffer subsets into one dataset
buffersC<-rbind(b3c,b30c,b60c,b90c,b120c,b150c,b180c,b210c,b240c,b270c,b300c,b330c,b360c,b390c,b420c,b450c,b480c,
                b510c,b540c,b570c,b600c,b630c,b660c,b690c,b720c,b750c,b780c,b810c,b840c,b870c,b900c,b930c,b960c,b990c,
                b1020c,b1050c,b1080c)
buffersC<-select(buffersC,-Notes)

# Merge buffer subsets with whole lake polygons
buffwholeC<-read.csv("WholeLakeStats.csv",header=TRUE) ##Same band stats for whole lake polygons
buffwholeC<-rename(buffwholeC,org_lake_name = Lake)

buffersCW<-rbind(buffersC,buffwholeC)

# Change name of lake column to match buffer subsets above
buffersCW<-rename(buffersCW,Lake = org_lake_name)
buffersC<-rename(buffersC,Lake = org_lake_name)

# Add column called "Landsat" that takes first four characters of imgname
buffersC$Landsat<-substr(buffersC$imgname, 1, 4)
buffersCW$Landsat<-substr(buffersCW$imgname, 1, 4)


# Check how many SWIR2 (band 7) values are above 250
sum(buffersCW$swir2_median>=250,na.rm=TRUE) #46
sum(buffersCW$swir2_mean>=250,na.rm=TRUE) #46

# Histogram of swir2 values before filtering 
buffersCW%>%
  ggplot(aes(swir2_median))+theme_classic()+
  geom_histogram(binwidth=10,fill="grey", color="black", alpha=0.9)+labs(title="Center point")

# Plot swir2 values per lake before filtering 
buffersCW%>%
  ggplot(aes(Lake,swir2_median,shape=Landsat,color=Landsat))+theme_classic()+geom_hline(yintercept=c(200,250))+
  geom_point(size=3,position=position_dodge(width=.2))+scale_color_manual(values=mycolors)+labs(title="Center point")
  #theme(axis.text.x=element_text(size=14,angle=300,hjust=0))

buffersCW%>%
  ggplot(aes(Lake,nir_median,shape=Landsat,color=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=.2))+scale_color_manual(values=mycolors)+labs(title="Center point")
  #theme(axis.text.x=element_text(size=14,angle=300,hjust=0))


# Remove SWIR2 values >250 (for mean, median, mode)
is.na(buffersCW$swir2_mean) <- buffersCW$swir2_mean >=250         
is.na(buffersCW$swir2_median) <- buffersCW$swir2_median >=250         
is.na(buffersCW$swir2_mode) <- buffersCW$swir2_mode >=250         
is.na(buffersC$swir2_mean) <- buffersC$swir2_mean >=250         
is.na(buffersC$swir2_median) <- buffersC$swir2_median >=250         
is.na(buffersC$swir2_mode) <- buffersC$swir2_mode >=250         

# Filter to Landsat 7 only
#buffersCW<-subset(buffersCW,Landsat=="LE07")

# Check na's
sum(is.na(buffersCW$nir_median)) # 131 for nir
sum(is.na(buffersCW$swir2_median)) # 177 - more missing values for swir2

## Remove na rows
#buffersCW<-buffersCW%>%filter(!is.na(swir1_median))
#buffersCW<-buffersCW%>%filter(!is.na(swir2_median))


#SunapeeCW<-filter(buffersCW,Lake=="Sunapee")
# Write table
#write.table(buffersC,"buffersC.csv",sep=" ,",col.names = NA)
#write.table(buffersCW,"buffersCW.csv",sep=" ,",col.names = NA)

############### Add column for day difference #####
library(lubridate)
buffersCW$acq.date.string<-substr(buffersCW$imgname,start=18,stop=26)
buffersCW$acq.date.string
buffersCW$acq.date<-as_date(as.character(buffersCW$acq.date.string))
buffersCW$acq.date
buffersCW$sample.date<-substr(buffersCW$datetime,start=1,stop=10)
buffersCW$sample.date<-as_date(buffersCW$sample.date)
buffersCW$DayDiff<-as_date(buffersCW$sample.date,format="%Y-%m-%d")-as_date(buffersCW$acq.date,format="%Y-%m-%d")
buffersCW$DayDiff<-as.numeric(abs(buffersCW$DayDiff))

############### Summary table to check stuff ######
toursummary_c<-buffersCW%>%group_by(Lake,BufferSize,acq.date)%>%
  summarise(satellite = Landsat,
            pixcount = (blue_count),
            red = (red_median),
            green = (green_median),
            blue = (blue_median),
            nir = (nir_median),
            swir1 = (swir1_median),
            swir2 = (swir2_median),
            sample.date = sample.date,
            daydiff = DayDiff,
            nimages = n(),
            imgname = imgname)

write.table(toursummary_c,"toursummary_c.csv",sep=" ,",col.names=NA)

## Remove na rows
buffersCW<-buffersCW%>%filter(!is.na(swir1_median))

### RESHAPE center point WITH polygons ######
## Reshape data
buffersCW<-data.table(buffersCW)
tdl1cw<-
  melt(buffersCW,id.variables=c("blue_count","green_count","red_count","swir1_count","swir2_count","nir_count",
                               "blue_median","green_median","red_median","swir1_median","swir2_median","nir_median",
                               "blue_stdDev","green_stdDev","red_stdDev","swir1_stdDev","swir2_stdDev","nir_stdDev",
                               "MaxDayDiff","BufferSize","datetime",
                               "Landsat","Lake","state"),
       variable.name=c("Band.Mean"),
       value.name=c("mean"),
       measure.vars=c("blue_mean","green_mean","red_mean","swir1_mean","swir2_mean","nir_mean"))

tdl2cw<-
  melt(tdl1cw,id.variables=c("Band.Mean","mean",
                             "blue_median","green_median","red_median","swir1_median","swir2_median","nir_median",
                             "blue_stdDev","green_stdDev","red_stdDev","swir1_stdDev","swir2_stdDev","nir_stdDev",
                             "MaxDayDiff","BufferSize","datetime",
                             "Landsat","Lake","state"),
       variable.name=c("Band.Count"),
       value.name=c("count"),
       measure.vars=c("blue_count","green_count","red_count","swir1_count","swir2_count","nir_count"))

tdl3cw<-
  melt(tdl2cw,id.variables=c("Band.Mean","mean","Band.Count","count",
                             "blue_stdDev","green_stdDev","red_stdDev","swir1_stdDev","swir2_stdDev","nir_stdDev",
                             "MaxDayDiff","BufferSize","datetime",
                             "Landsat","Lake","state"),
       variable.name=c("Band.Median"),
       value.name=c("median"),
       measure.vars=c( "blue_median","green_median","red_median","swir1_median","swir2_median","nir_median"))

tdlcw<-
  melt(tdl3cw,id.variables=c("Band.Mean","mean","Band.Count","count",
                             "Band Median","median",
                             "MaxDayDiff","BufferSize","datetime",
                             "Landsat","Lake","state"),
       variable.name=c("Band.std"),
       value.name=c("std"),
       measure.vars=c("blue_stdDev","green_stdDev","red_stdDev","swir1_stdDev","swir2_stdDev","nir_stdDev"),
       na.rm = TRUE)

## Make Time Window categorical
tdlcw$MaxDayDiff<-as.factor(tdlcw$MaxDayDiff)

## Rename factor levels
levels(tdlsw$Band.Mean)
levels(tdlsw$Band.Mean)<-c("blue","green","red","swir1","swir2","nir")

levels(tdlsw$Band.Median)
levels(tdlsw$Band.Median)<-c("blue","green","red","swir1","swir2","nir")

levels(tdlsw$Band.Count)
levels(tdlsw$Band.Count)<-c("blue","green","red","swir1","swir2","nir")

levels(tdlsw$Band.std)
levels(tdlsw$Band.std)<-c("blue","green","red","swir1","swir2","nir")


####### Plot
## Categorical
tdlcw$BufferSize<-as.factor(tdlcw$BufferSize)

## Order X axis
tdlcw$BufferSize <- factor(tdlcw$BufferSize, levels=c("3","30","60","90","120","150","180","210","240","270","300",
                                                      "330","360","390","420","450","480","510","540","570","600","630",
                                                      "660","690","720","750","780","810","840","870","900","930","960",
                                                      "990","1020","1050","1080","Whole"))
############### Plot Summary Stats (center of lake) ####
# Std
StdCW<-
  ggplot(tdlcw,aes(BufferSize,std,color=Band.std,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=.3),na.rm=TRUE)+
  theme(axis.text.x=element_text(angle=300,size=14,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  labs(title="5 days, center of lake",x="Buffer Size (m)")
a <- annotation_logticks(sides='l')
BuffCWStd<-StdCW+a
ggsave("BuffCWStd.png",BuffCWStd,height=6,width=15,dpi=100)

# Mean
BuffMeanCW<-
  ggplot(tdlcw,aes(BufferSize,mean,color=Band.Mean,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=18,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(1,1500))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  labs(title="5 days, center of lake",x="Buffer size (m)")
ggsave("BuffMeanCW.png",BuffMeanC,height=6,width=12,dpi=100)

# Median
BuffMedianCW<-
  ggplot(tdlcw,aes(BufferSize,median,color=Band.Median,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=18,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(1,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  labs(title="5 days, center of lake",x="Buffer size (m)")
ggsave("BuffMedianCW.png",BuffMedianCW,height=6,width=12,dpi=100)

# Count
BuffCountCW<-
  ggplot(tdlcw,aes(BufferSize,count,color=Band.Count,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=18,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(1,200000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  labs(title="5 days, center of lake",x="Buffer size (m)")
ggsave("BuffCountCW.png",BuffCountCW,height=6,width=12,dpi=100)

## Plot together
BuffFigs_5dCW <- arrangeGrob(BuffCountCW,StdCW,BuffMeanCW,BuffMedianCW,nrow=2, ncol=2)

ggsave("BuffFigs_5dCW.png",BuffFigs_5dCW,height=15,width=20,dpi=100)


### Make Subsets based on lake size - ALL LAKES VS. LARGE LAKES (long format) #################
####### Sample point
## Subset to buffer sizes that work for all 15 lakes (up to 60 m buffer)
small.s<-filter(tdlsw,BufferSize=="3"|BufferSize=="30"|BufferSize=="60"|BufferSize=="90"|BufferSize=="120"|BufferSize=="Whole")
small.s$BufferSize<-factor(small.s$BufferSize)

## Subset large lakes (up to 420 m buffer, N=5 lakes)
large.s1<-filter(tdlsw,Lake=="Auburn"|Lake=="Mascoma"|Lake=="Messalonskee"|Lake=="Morey"|Lake=="Sunapee")
large.s1$Lake<-factor(large.s1$Lake)
large.s<-filter(large.s1,BufferSize=="3"|BufferSize=="30"|BufferSize=="60"|BufferSize=="90"|BufferSize=="120"|BufferSize=="150"|
                  BufferSize=="180"|BufferSize=="210"|BufferSize=="240"|BufferSize=="270"|BufferSize=="300"|BufferSize=="330"|
                  BufferSize=="360"|BufferSize=="390"|BufferSize=="420"|BufferSize=="Whole")
large.s$BufferSize<-factor(large.s$BufferSize)

####### Center of lake 
## Subset to buffer sizes that work for all 15 lakes (up to 150 m buffer)
small.c<-filter(tdlcw,BufferSize=="3"|BufferSize=="30"|BufferSize=="60"|BufferSize=="90"|BufferSize=="120"|BufferSize=="Whole")
small.c$BufferSize<-factor(small.c$BufferSize)

## Subset large lakes (up to 570 m buffer, N=5 lakes)
large.c1<-filter(tdlcw,Lake=="Sabattus"|Lake=="Long Lake"|Lake=="Auburn"|Lake=="Androscoggin"|Lake=="Sebago")
large.c1$Lake<-factor(large.c1$Lake)

large.c<-filter(large.c1,BufferSize=="3"|BufferSize=="30"|BufferSize=="60"|BufferSize=="90"|BufferSize=="120"|BufferSize=="150"|
                  BufferSize=="180"|BufferSize=="210"|BufferSize=="240"|BufferSize=="270"|BufferSize=="300"|BufferSize=="330"|
                  BufferSize=="360"|BufferSize=="390"|BufferSize=="420"|BufferSize=="450"|BufferSize=="480"|BufferSize=="510"|
                  BufferSize=="540"|BufferSize=="570"|BufferSize=="Whole")
large.c$BufferSize<-factor(large.c$BufferSize)


############### Plot for sample point ########
###################### All 15 lakes (up to 60 m)
## Plot std
stdS.sm<-
  ggplot(small.s,aes(BufferSize,std,color=Band.std,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1000),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="All 15 lakes, Sample point",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
stdS.sm<-stdS.sm+a
ggsave("stdS.sm.png",stdS.sm,height=6,width=12,dpi=100)

## Count
countS.sm<-
  ggplot(small.s,aes(BufferSize,count,color=Band.Count,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,200000),breaks=c(1,10,100,1000,10000,100000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="All 15 lakes, Sample point",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
countS.sm<-countS.sm+a
ggsave("countS.sm.png",countS.sm,height=6,width=12,dpi=100)

## Mean
meanS.sm<-
  ggplot(small.s,aes(BufferSize,mean,color=Band.Mean,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1000),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="All 15 lakes, Sample point",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
meanS.sm<-meanS.sm+a
ggsave("meanS.sm.png",meanS.sm,height=6,width=12,dpi=100)


## Median
medianS.sm<-
  ggplot(small.s,aes(BufferSize,median,color=Band.Median,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1000),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="All 15 lakes, Sample point",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
medianS.sm<-medianS.sm+a
ggsave("medianS.sm.png",medianS.sm,height=6,width=12,dpi=100)

## Arrange together
Allstats.s.sm<-arrangeGrob(countS.sm,stdS.sm,meanS.sm,medianS.sm,nrow=2,ncol=2)
ggsave("Allstats.s.sm.png",Allstats.s.sm,height=10,width=15,dpi=100)

###################### Medium sized lakes (up to 270 m buffer, N=12 lakes)
## Plot std
stdS.med<-
  ggplot(medium.s,aes(BufferSize,std,color=Band.std,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14,,angle = 300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1000),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="Medium sized lakes (N=12), Sample point",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
stdS.med<-stdS.med+a
ggsave("stdS.med.png",stdS.med,height=6,width=12,dpi=100)

## Count
countS.med<-
  ggplot(medium.s,aes(BufferSize,count,color=Band.Count,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14,angle = 300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,200000),breaks=c(1,10,100,1000,10000,100000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="Medium sized lakes (N=12), Sample point",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
countS.med<-countS.med+a
ggsave("countS.med.png",countS.med,height=6,width=12,dpi=100)

## Mean
meanS.med<-
  ggplot(medium.s,aes(BufferSize,mean,color=Band.Mean,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14,angle = 300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1000),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="Medium sized lakes (N=12), Sample point",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
meanS.med<-meanS.med+a
ggsave("meanS.med.png",meanS.med,height=6,width=12,dpi=100)


## Median
medianS.med<-
  ggplot(medium.s,aes(BufferSize,median,color=Band.Median,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14,angle = 300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1000),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="Medium sized lakes (N=12), Sample point",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
medianS.med<-medianS.med+a
ggsave("medianS.med.png",medianS.med,height=6,width=12,dpi=100)

## Arrange together
Allstats.S.med<-arrangeGrob(countS.med,stdS.med,meanS.med,medianS.med,nrow=2,ncol=2)
ggsave("Allstats.S.med.png",Allstats.S.med,height=10,width=15,dpi=100)


###################### Large lakes (up to 390 m buffer)
## Plot std
stdS.lg<-
  ggplot(large.s,aes(BufferSize,std,color=Band.std,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14,angle = 300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1000),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="Large lakes (N=5), Sample point",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
stdS.lg<-stdS.lg+a
ggsave("stdS.lg.png",stdS.lg,height=6,width=12,dpi=100)

## Count
countS.lg<-
  ggplot(large.s,aes(BufferSize,count,color=Band.Count,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14,angle = 300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,20000),breaks=c(1,10,100,1000,10000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="Large lakes (N=5), Sample point",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
countS.lg<-countS.lg+a
ggsave("countS.lg.png",countS.lg,height=6,width=12,dpi=100)

## Mean
meanS.lg<-
  ggplot(large.s,aes(BufferSize,mean,color=Band.Mean,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14,angle = 300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1000),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="Large lakes (N=5), Sample point",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
meanS.lg<-meanS.lg+a
ggsave("meanS.lg.png",meanS.lg,height=6,width=12,dpi=100)


## Median
medianS.lg<-
  ggplot(large.s,aes(BufferSize,median,color=Band.Median,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14,angle = 300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1000),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="Large lakes (N=5), Sample point",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
medianS.lg<-medianS.lg+a
ggsave("medianS.lg.png",medianS.lg,height=6,width=12,dpi=100)

## Arrange together
Allstats.S.lg<-arrangeGrob(countS.lg,stdS.lg,meanS.lg,medianS.lg,nrow=2,ncol=2)
ggsave("Allstats.S.lg.png",Allstats.S.lg,height=10,width=17,dpi=100)

############### Plot for center of lake ########
###################### All 15 lakes
## Plot std
stdC.sm<-
  ggplot(small.c,aes(BufferSize,std,color=Band.std,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1000),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="All 15 lakes, Center of lake",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
stdC.sm<-stdC.sm+a
ggsave("stdC.sm.png",stdC.sm,height=6,width=12,dpi=100)

## Count
countC.sm<-
  ggplot(small.c,aes(BufferSize,count,color=Band.Count,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,200000),breaks=c(1,10,100,1000,10000,100000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="All 15 lakes, Center of lake",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
countC.sm<-countC.sm+a
ggsave("countC.sm.png",countC.sm,height=6,width=12,dpi=100)

## Mean
meanC.sm<-
  ggplot(small.c,aes(BufferSize,mean,color=Band.Mean,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1000),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="All 15 lakes, Center of lake",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
meanC.sm<-meanC.sm+a
ggsave("meanC.sm.png",meanC.sm,height=6,width=12,dpi=100)


## Median
medianC.sm<-
  ggplot(small.c,aes(BufferSize,median,color=Band.Median,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1000),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="All 15 lakes, Center of lake",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
medianC.sm<-medianC.sm+a
ggsave("medianC.sm.png",medianC.sm,height=6,width=12,dpi=100)

## Arrange together
Allstats.c.sm<-arrangeGrob(countC.sm,stdC.sm,meanC.sm,medianC.sm,nrow=2,ncol=2)
ggsave("Allstats.c.sm.png",Allstats.c.sm,height=10,width=15,dpi=100)

###################### Medium sized lakes (up to 390 m buffer)
## Plot std
stdC.med<-
  ggplot(medium.c,aes(BufferSize,std,color=Band.std,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14,,angle = 300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1000),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="Medium sized lakes (N=11), Center of lake",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
stdC.med<-stdC.med+a
ggsave("stdC.med.png",stdC.med,height=6,width=12,dpi=100)

## Count
countC.med<-
  ggplot(medium.c,aes(BufferSize,count,color=Band.Count,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14,angle = 300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,200000),breaks=c(1,10,100,1000,10000,100000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="Medium sized lakes (N=11), Center of lake",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
countC.med<-countC.med+a
ggsave("countC.med.png",countC.med,height=6,width=12,dpi=100)

## Mean
meanC.med<-
  ggplot(medium.c,aes(BufferSize,mean,color=Band.Mean,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14,angle = 300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1000),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="Medium sized lakes (N=11), Center of lake",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
meanC.med<-meanC.med+a
ggsave("meanC.med.png",meanC.med,height=6,width=12,dpi=100)


## Median
medianC.med<-
  ggplot(medium.c,aes(BufferSize,median,color=Band.Median,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14,angle = 300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1000),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="Medium sized lakes (N=11), Center of lake",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
medianC.med<-medianC.med+a
ggsave("medianC.med.png",medianC.med,height=6,width=12,dpi=100)

## Arrange together
Allstats.c.med<-arrangeGrob(countC.med,stdC.med,meanC.med,medianC.med,nrow=2,ncol=2)
ggsave("Allstats.c.med.png",Allstats.c.med,height=10,width=15,dpi=100)


###################### Large lakes (up to 810 m buffer)
## Plot std
stdC.lg<-
  ggplot(large.c,aes(BufferSize,std,color=Band.std,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14,angle = 300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1000),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="Large lakes (N=4), Center of lake",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
stdC.lg<-stdC.lg+a
ggsave("stdC.lg.png",stdC.lg,height=6,width=12,dpi=100)

## Count
countC.lg<-
  ggplot(large.c,aes(BufferSize,count,color=Band.Count,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14,angle = 300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,20000),breaks=c(1,10,100,1000,10000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="Large lakes (N=4), Center of lake",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
countC.lg<-countC.lg+a
ggsave("countC.lg.png",countC.lg,height=6,width=12,dpi=100)

## Mean
meanC.lg<-
  ggplot(large.c,aes(BufferSize,mean,color=Band.Mean,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14,angle = 300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1000),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="Large lakes (N=4), Center of lake",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
meanC.lg<-meanC.lg+a
ggsave("meanC.lg.png",meanC.lg,height=6,width=12,dpi=100)


## Median
medianC.lg<-
  ggplot(large.c,aes(BufferSize,median,color=Band.Median,shape=Landsat))+theme_classic()+
  geom_point(size=3,position = position_dodge(width=.3))+
  theme(axis.text.x=element_text(size=14,angle = 300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1000),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=mycolors2)+
  labs(title="Large lakes (N=4), Center of lake",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
medianC.lg<-medianC.lg+a
ggsave("medianC.lg.png",medianC.lg,height=6,width=12,dpi=100)

## Arrange together
Allstats.c.lg<-arrangeGrob(countC.lg,stdC.lg,meanC.lg,medianC.lg,nrow=2,ncol=2)
ggsave("Allstats.c.lg.png",Allstats.c.lg,height=10,width=17,dpi=100)




### Remake lake size subsets for use w/ limno data (wide format) ######
####### Sample point
## Subset to buffer sizes that work for all 15 lakes (up to 120 m buffer)
small.s<-filter(buffersW,BufferSize=="3"|BufferSize=="30"|BufferSize=="60"|BufferSize=="90"|BufferSize=="120"|BufferSize=="Whole")
small.s$BufferSize<-factor(small.s$BufferSize,levels=c("3","30","60","90","120","Whole"))

## Subset large lakes (up to 420 m buffer, N=5 lakes)
large.s1<-filter(buffersW,Lake=="Auburn"|Lake=="Mascoma"|Lake=="Messalonskee"|Lake=="Morey"|Lake=="Sunapee")
large.s1$Lake<-factor(large.s1$Lake)
large.s<-filter(large.s1,BufferSize=="3"|BufferSize=="30"|BufferSize=="60"|BufferSize=="90"|BufferSize=="120"|BufferSize=="150"|
                  BufferSize=="180"|BufferSize=="210"|BufferSize=="240"|BufferSize=="270"|BufferSize=="300"|BufferSize=="330"|
                  BufferSize=="360"|BufferSize=="390"|BufferSize=="420"|BufferSize=="Whole")
large.s$BufferSize<-factor(large.s$BufferSize,levels=c("3","30","60","90","120","150","180","210",
                                                       "240","270","300","330","360","390","420","Whole"))

####### Center of lake 
## Subset to buffer sizes that work for all 15 lakes (up to 120 m buffer)
small.c<-filter(buffersCW,BufferSize=="3"|BufferSize=="30"|BufferSize=="60"|BufferSize=="90"|BufferSize=="120"|BufferSize=="Whole")
small.c$BufferSize<-factor(small.c$BufferSize,levels=c("3","30","60","90","120","Whole"))

## Subset large lakes (up to 570 m buffer, 5 lakes)
large.c1<-subset(buffersCW,Lake=="Sabattus"|Lake=="Long Lake"|Lake=="Auburn"|Lake=="Androscoggin"|Lake=="Sebago")
large.c1$Lake<-factor(large.c1$Lake)

large.c<-subset(large.c1,BufferSize=="3"|BufferSize=="30"|BufferSize=="60"|BufferSize=="90"|BufferSize=="120"|BufferSize=="150"|
                  BufferSize=="180"|BufferSize=="210"|BufferSize=="240"|BufferSize=="270"|BufferSize=="300"|BufferSize=="330"|
                  BufferSize=="360"|BufferSize=="390"|BufferSize=="420"|BufferSize=="450"|BufferSize=="480"|BufferSize=="510"|
                  BufferSize=="540"|BufferSize=="570"|BufferSize=="Whole")
large.c$BufferSize<-factor(large.c$BufferSize,levels=c("3","30","60","90","120","150","180","210","240","270","300",
                                                       "330","360","390","420","450","480","510","540","570","Whole"))

## Remove na rows
buffersW<-buffersW%>%filter(!is.na(nir_mean))
buffersCW<-buffersCW%>%filter(!is.na(nir_mean))


## Read limno data
nasa<-read.csv("nasalakes.csv",header=TRUE)
limno<-read.csv("LimnoCombinedData.csv",header=TRUE)

## Take average of Sunapee chl and color values
limno_avg <- limno %>% 
  group_by(Lake, State, Depth.m) %>% 
  summarize(Secchi.m = mean(Secchi.m),
            Chl = mean(Chl,na.rm=TRUE),
            Color_254_nm = mean(Color_254_nm,na.rm=TRUE),
            Color_320_nm = mean(Color_320_nm,na.rm=TRUE),
            Color_440_nm = mean(Color_440_nm,na.rm=TRUE),
            DOC.mg.L = mean(DOC.mg.L,na.rm=TRUE),
            Gloeo.L = mean(Gloeo.L,na.rm=TRUE))


## Merge sample point multibuffer data with limno data
tdls.limno1<-merge(buffers,nasa,by.x =c("Lake"),by.y =c("Lake"),na.rm=TRUE,,allow.cartesian = TRUE)
limno.s<-merge(limno_avg,tdls.limno1,by.x=c("Lake"),by.y=c("Lake"),na.rm=TRUE)

## Merge center of lake multibuffer data with limno data
tdlc.limno1<-merge(buffersC,nasa,by.x =c("Lake"),by.y =c("Lake"),na.rm=TRUE)
limno.c<-merge(limno_avg,tdlc.limno1,by.x=c("Lake"),by.y=c("Lake"),na.rm=TRUE)

## Merge size categories with limno data
lsmall.s1<-merge(small.s,nasa,by.x=c("Lake"),by.y=c("Lake"),na.rm=TRUE,allow.cartesian = TRUE)
lsmall.s<-merge(limno_avg,lsmall.s1,by.x=c("Lake"),by.y=c("Lake"),na.rm=TRUE,allow.cartesian = TRUE)
llarge.s1<-merge(large.s,nasa,by.x=c("Lake"),by.y=c("Lake"),na.rm=TRUE,allow.cartesian = TRUE)
llarge.s<-merge(limno_avg,llarge.s1,by.x=c("Lake"),by.y=c("Lake"),na.rm=TRUE,allow.cartesian = TRUE)

lsmall.c1<-merge(small.c,nasa,by.x=c("Lake"),by.y=c("Lake"),na.rm=TRUE,allow.cartesian = TRUE)
lsmall.c<-merge(limno_avg,lsmall.c1,by.x=c("Lake"),by.y=c("Lake"),na.rm=TRUE,allow.cartesian = TRUE)
llarge.c1<-merge(large.c,nasa,by.x=c("Lake"),by.y=c("Lake"),na.rm=TRUE,allow.cartesian = TRUE)
llarge.c<-merge(limno_avg,llarge.c1,by.x=c("Lake"),by.y=c("Lake"),na.rm=TRUE,allow.cartesian = TRUE)

### Write table to R folder
#write.table(tdl2,"tdl2.csv")

### CHLOROPHYLL ALGORITHMS BELOW: ################
### Giardino et al. (2001) #####
# Lake Iseo, Italy
# ChlA = 11.18*B1 -8.96*B2 - 3.28 mg/m^3, Landsat 5
# B1 + B2 = nir + green
# Note: paper says band values are "TM per-band reflectance ??TMi (in percentage)"
# See lines below to fit this algorithm to the six Subsets 

################# ALL 15 LAKES, SAMPLE POINT
## Tell it what variables to group by (i.e., buffer size)
#by_buffersize<-group_by(lsmall.s,BufferSize)
## Run algorithm over the grouped data
#do(by_buffersize,glance(lm(Chl~nir_median+green_median,data=.)))
## Look at the grouped parameter-level data
#do(by_buffersize,tidy(lm(Chl~nir_median+green_median,data=.)))
## Add predicted (fitted) values to the lm table
pred.small.s<-
  lsmall.s%>%group_by(BufferSize)%>%
  do(augment(lm(Chl~nir_median+red_median,data=.)))
## Calculate nrmse (normalized by standard deviation, as calculated by HydroGOF package)
nrmse.small.s<-
  pred.small.s%>%
  select(BufferSize,Chl,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Chl)^2))/sd(Chl))%>% # adds columns with nrmse values
  mutate(Subset="All 15")%>%
  mutate(Centroid="S") 
# Regressions
reg.small.s<-
nrmse.small.s%>%
ggplot(aes(Chl,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=4)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual Chl (ug/L)",y="Predicted Chl (ug/L)",title="All lakes: Sample point")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.small.s.png",reg.small.s,width=12,height=5,dpi=100)

################# LARGE LAKES, SAMPLE POINT
#by_buffersize<-group_by(llarge.s,BufferSize)
#do(by_buffersize,glance(lm(Chl~nir_median+green_median,data=.)))
#do(by_buffersize,tidy(lm(Chl~nir_median+green_median,data=.)))
pred.large.s<-
  llarge.s%>%group_by(BufferSize)%>%
  do(augment(lm(Chl~nir_median+red_median,data=.)))
nrmse.large.s<-
  pred.large.s%>%
  select(BufferSize,Chl,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Chl)^2))/sd(Chl)) %>%
  mutate(Subset="Large")%>%
  mutate(Centroid="S")
# Regressions
reg.large.s<-
  nrmse.large.s%>%
  ggplot(aes(Chl,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=5)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual Chl (ug/L)",y="Predicted Chl (ug/L)",title="Large lakes: Sample point")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=1))
ggsave("reg.large.s.png",reg.large.s,width=12,height=8,dpi=100)

################# ALL 15 LAKES, CENTER OF LAKE
#by_buffersize<-group_by(lsmall.c,BufferSize)
#do(by_buffersize,glance(lm(Chl~nir_median+green_median,data=.)))
#do(by_buffersize,tidy(lm(Chl~nir_median+green_median,data=.)))
pred.small.c<-
  lsmall.c%>%group_by(BufferSize)%>%
  do(augment(lm(Chl~nir_median+red_median,data=.)))
nrmse.small.c<-
  pred.small.c%>%
  select(BufferSize,Chl,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Chl)^2))/sd(Chl)) %>%
  mutate(Subset="All 15")%>%
  mutate(Centroid="C")
# Regressions
reg.small.c<-
  nrmse.small.c%>%
  ggplot(aes(Chl,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=4)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual Chl (ug/L)",y="Predicted Chl (ug/L)",title="All lakes: Center")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.small.c.png",reg.small.c,width=12,height=5,dpi=100)


################# LARGE LAKES, CENTER OF LAKE
#by_buffersize<-group_by(llarge.c,BufferSize)
#do(by_buffersize,glance(lm(Chl~nir_median+green_median,data=.)))
#do(by_buffersize,tidy(lm(Chl~nir_median+green_median,data=.)))
pred.large.c<-
  llarge.c%>%group_by(BufferSize)%>%
  do(augment(lm(Chl~nir_median+red_median,data=.)))
nrmse.large.c<-
  pred.large.c%>%
  select(BufferSize,Chl,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Chl)^2))/sd(Chl)) %>%
  mutate(Subset="Large")%>%
  mutate(Centroid="C")
# Regressions
reg.large.c<-
  nrmse.large.c%>%
  ggplot(aes(Chl,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=6)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual Chl (ug/L)",y="Predicted Chl (ug/L)",title="Large lakes: Center")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.large.c.png",reg.large.c,width=16,height=12,dpi=100)

######## Create nrmse table #####
mergeCols<-c("Centroid","Subset","BufferSize","nrmse","Chl",".fitted")

Fit.5d.S<-merge(nrmse.small.s,nrmse.large.s,by=mergeCols,all=TRUE)%>%
  merge(.,nrmse.small.c,by=mergeCols,all=TRUE)%>%
  merge(.,nrmse.large.c,by=mergeCols,all=TRUE)


## Condense table to just the single nrmse for each buffer size and subset
FitSum<-summarise(group_by(Fit.5d.S,BufferSize,Subset,Centroid),
               nrmse=median(nrmse))

######## Plot ####
# Reorder
FitSum$BufferSize <-
  factor(FitSum$BufferSize,levels=c("3","30","60","90","120","150","180","210","240","270","300",
                                "330","360","390","420","450","480","510","540","570","600","630",
                                "660","690","720","750","780","810","Whole"))

Giardino.nrmse.s.size<-
  FitSum%>%
  filter(Centroid=="S")%>%
  ggplot(aes(BufferSize,nrmse,fill=Subset))+theme_classic()+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=14,angle=300,hjust=0),
        axis.text.y=element_text(size=14),
        plot.title = element_text(size=22),
        legend.text=element_text(size=16),
        legend.title=element_text(size=18),legend.position=c(.9,.9))+
  scale_y_continuous(lim=c(0,1))+
  scale_fill_manual(values=mycolors)+
  labs(title="Giardino, Sample point",x="")

Giardino.nrmse.c.size<-
  FitSum%>%
  filter(Centroid=="C")%>%
  ggplot(aes(BufferSize,nrmse,fill=Subset))+theme_classic()+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=14,angle=300,hjust=0),
        axis.text.y=element_text(size=14),
        plot.title = element_text(size=22),
        legend.text=element_text(size=16),
        legend.title=element_text(size=18),legend.position=c(.9,.9))+
  scale_y_continuous(lim=c(0,1))+
  scale_fill_manual(values=mycolors)+
  labs(title="Giardino, Center of lake",x="")

#nrmse.small.s%>%
#  ggplot(aes(Chl,.fitted))+theme_classic()+geom_point(aes(color=Lake))

# Plot regressions together
regs.Giardino<-arrangeGrob(reg.small.s,reg.large.s,reg.small.c,reg.large.c,ncol=2,nrow=2)
ggsave("regs.Giardino.png",regs.Giardino,width=15,height=8,dpi=100)


### Mancino et al. (2009) ##############
# ChlA = -47.515 + 9.516(B3/B2) + 20.952(B1/B2) - 873(B2) + 34.889(B2/B1)
# They started w/DNs and did radiometric calibration and atomspheric correction
# Monticchio Lakes, Italy
# See lines below to run this algorithm on the six subsets

################# ALL 15 LAKES, SAMPLE POINT
#by_buffersize<-group_by(lsmall.s,BufferSize)
#do(by_buffersize,glance(lm(Chl~(red_median/green_median)+(nir_median/green_median)+green_median+(green_median/nir_median),data=.)))))
#do(by_buffersize,tidy(lm(Chl~(red_median/green_median)+(nir_median/green_median)+green_median+(green_median/nir_median),data=.)))))
## Add predicted (fitted) values to the lm table
pred.small.s<-
  lsmall.s%>%group_by(BufferSize)%>%
  do(augment(lm(
    Chl~(red_median/green_median)+(nir_median/green_median)+green_median+(green_median/nir_median),data=.)))
## Calculate nrmse (normalized by standard deviation, as calculated by HydroGOF package)
nrmse.small.s<-
  pred.small.s%>%
  select(BufferSize,Chl,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Chl)^2))/sd(Chl))%>% # adds columns with nrmse values
  mutate(Subset="All 15")%>%
  mutate(Centroid="S") 
# Regressions
reg.small.s<-
  nrmse.small.s%>%
  ggplot(aes(Chl,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=4)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual Chl (ug/L)",y="Predicted Chl (ug/L)",title="All lakes: Sample point")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.small.s.png",reg.small.s,width=12,height=5,dpi=100)

################# LARGE LAKES, SAMPLE POINT
#by_buffersize<-group_by(llarge.s,BufferSize)
#do(by_buffersize,glance(lm(Chl~(red_median/green_median)+(nir_median/green_median)+green_median+(green_median/nir_median),data=.)))))
#do(by_buffersize,tidy(lm(Chl~(red_median/green_median)+(nir_median/green_median)+green_median+(green_median/nir_median),data=.)))))
pred.large.s<-
  llarge.s%>%group_by(BufferSize)%>%
    do(augment(lm(Chl~(red_median/green_median)+(nir_median/green_median)+green_median+(green_median/nir_median),data=.)))
nrmse.large.s<-
  pred.large.s%>%
  select(BufferSize,Chl,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Chl)^2))/sd(Chl)) %>%
  mutate(Subset="Large")%>%
  mutate(Centroid="S")
# Regressions
reg.large.s<-
  nrmse.large.s%>%
  ggplot(aes(Chl,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=5)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual Chl (ug/L)",y="Predicted Chl (ug/L)",title="Large lakes: Sample point")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=1))
ggsave("reg.large.s.png",reg.large.s,width=12,height=8,dpi=100)


################# ALL 15 LAKES, CENTER OF LAKE
#by_buffersize<-group_by(lsmall.c,BufferSize)
#do(by_buffersize,glance(lm(Chl~(red_median/green_median)+(nir_median/green_median)+green_median+(green_median/nir_median),data=.)))))
#do(by_buffersize,tidy(lm(Chl~(red_median/green_median)+(nir_median/green_median)+green_median+(green_median/nir_median),data=.)))))
pred.small.c<-
  lsmall.c%>%group_by(BufferSize)%>%
    do(augment(lm(Chl~(red_median/green_median)+(nir_median/green_median)+green_median+(green_median/nir_median),data=.)))
nrmse.small.c<-
  pred.small.c%>%
  select(BufferSize,Chl,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Chl)^2))/sd(Chl)) %>%
  mutate(Subset="All 15")%>%
  mutate(Centroid="C")
# Regressions
reg.small.c<-
  nrmse.small.c%>%
  ggplot(aes(Chl,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=4)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual Chl (ug/L)",y="Predicted Chl (ug/L)",title="All lakes: Center")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.small.c.png",reg.small.c,width=12,height=5,dpi=100)

################# LARGE LAKES, CENTER OF LAKE
#by_buffersize<-group_by(llarge.c,BufferSize)
#do(by_buffersize,glance(lm(Chl~(red_median/green_median)+(nir_median/green_median)+green_median+(green_median/nir_median),data=.)))))
#do(by_buffersize,tidy(lm(Chl~(red_median/green_median)+(nir_median/green_median)+green_median+(green_median/nir_median),data=.)))))
pred.large.c<-
  llarge.c%>%group_by(BufferSize)%>%
    do(augment(lm(Chl~(red_median/green_median)+(nir_median/green_median)+green_median+(green_median/nir_median),data=.)))
nrmse.large.c<-
  pred.large.c%>%
  select(BufferSize,Chl,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Chl)^2))/sd(Chl)) %>%
  mutate(Subset="Large")%>%
  mutate(Centroid="C")
# Regressions
reg.large.c<-
  nrmse.large.c%>%
  ggplot(aes(Chl,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=6)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual Chl (ug/L)",y="Predicted Chl (ug/L)",title="Large lakes: Center")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.large.c.png",reg.large.c,width=16,height=12,dpi=100)

######## Create nrmse table #####
mergeCols<-c("Centroid","Subset","BufferSize","nrmse","Chl",".fitted")

Fit.5d.S<-merge(nrmse.small.s,nrmse.large.s,by=mergeCols,all=TRUE)%>%
  merge(.,nrmse.small.c,by=mergeCols,all=TRUE)%>%
  merge(.,nrmse.large.c,by=mergeCols,all=TRUE)

## Condense table to just the single nrmse for each buffer size and subset
FitSum<-summarise(group_by(Fit.5d.S,BufferSize,Subset,Centroid),
                  nrmse=median(nrmse))

######## Plot ####
# Reorder
FitSum$BufferSize <-
  factor(FitSum$BufferSize,levels=c("3","30","60","90","120","150","180","210","240","270","300",
                                    "330","360","390","420","450","480","510","540","570","600","630",
                                    "660","690","720","750","780","810","Whole"))

Mancino.nrmse.s.size<-
  FitSum%>%
  filter(Centroid=="S")%>%
  ggplot(aes(BufferSize,nrmse,fill=Subset))+theme_classic()+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=14,angle=300,hjust=0),
        axis.text.y=element_text(size=14),
        plot.title = element_text(size=22),
        legend.text=element_text(size=16),
        legend.title=element_text(size=18),legend.position=c(.9,.9))+
  scale_y_continuous(lim=c(0,1))+
  scale_fill_manual(values=mycolors)+
  labs(title="Mancino, Sample point",x="")

Mancino.nrmse.c.size<-
  FitSum%>%
  filter(Centroid=="C")%>%
  ggplot(aes(BufferSize,nrmse,fill=Subset))+theme_classic()+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=14,angle=300,hjust=0),
        axis.text.y=element_text(size=14),
        plot.title = element_text(size=22),
        legend.text=element_text(size=16),
        legend.title=element_text(size=18),legend.position=c(.9,.9))+
  scale_y_continuous(lim=c(0,1))+
  scale_fill_manual(values=mycolors)+
  labs(title="Mancino, Center of lake",x="")






### Yip et al. (2015) ##############
# ChlA = -115.95*B4 + 19.31*B3 + 4.56
# Lake Diefenbaker in Saskatchewan, CA, LS 5
# See lines below to run this alogorithm on the six subsets

################# ALL 15 LAKES, SAMPLE POINT
#by_buffersize<-group_by(lsmall.s,BufferSize)
#do(by_buffersize,glance(lm(Chl~nir_median+red_median,data=.)))
#do(by_buffersize,tidy(lm(Chl~nir_median+red_median,data=.)))
## Add predicted (fitted) values to the lm table
pred.small.s<-
  lsmall.s%>%group_by(BufferSize)%>%
  do(augment(lm(Chl~nir_median+red_median,data=.)))
## Calculate nrmse (normalized by standard deviation, as calculated by HydroGOF package)
nrmse.small.s<-
  pred.small.s%>%
  select(BufferSize,Chl,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Chl)^2))/sd(Chl))%>% # adds columns with nrmse values
  mutate(Subset="All 15")%>%
  mutate(Centroid="S") 
# Regressions
reg.small.s<-
  nrmse.small.s%>%
  ggplot(aes(Chl,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=4)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual Chl (ug/L)",y="Predicted Chl (ug/L)",title="All lakes: Sample point")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.small.s.png",reg.small.s,width=12,height=5,dpi=100)

################# LARGE LAKES, SAMPLE POINT
#by_buffersize<-group_by(llarge.s,BufferSize)
#do(by_buffersize,glance(lm(Chl~nir_median+red_median,data=.)))
#do(by_buffersize,tidy(lm(Chl~nir_median+red_median,data=.)))
pred.large.s<-
  llarge.s%>%group_by(BufferSize)%>%
 do(augment(lm(Chl~nir_median+red_median,data=.)))
nrmse.large.s<-
  pred.large.s%>%
  select(BufferSize,Chl,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Chl)^2))/sd(Chl)) %>%
  mutate(Subset="Large")%>%
  mutate(Centroid="S")
# Regressions
reg.large.s<-
  nrmse.large.s%>%
  ggplot(aes(Chl,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=5)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual Chl (ug/L)",y="Predicted Chl (ug/L)",title="Large lakes: Sample point")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=1))
ggsave("reg.large.s.png",reg.large.s,width=12,height=8,dpi=100)


################# ALL 15 LAKES, CENTER OF LAKE
#by_buffersize<-group_by(lsmall.c,BufferSize)
#do(by_buffersize,glance(lm(Chl~nir_median+red_median,data=.)))
#do(by_buffersize,tidy(lm(Chl~nir_median+red_median,data=.)))
pred.small.c<-
  lsmall.c%>%group_by(BufferSize)%>%
 do(augment(lm(Chl~nir_median+red_median,data=.)))
nrmse.small.c<-
  pred.small.c%>%
  select(BufferSize,Chl,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Chl)^2))/sd(Chl)) %>%
  mutate(Subset="All 15")%>%
  mutate(Centroid="C")
# Regressions
reg.small.c<-
  nrmse.small.c%>%
  ggplot(aes(Chl,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=4)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual Chl (ug/L)",y="Predicted Chl (ug/L)",title="All lakes: Center")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.small.c.png",reg.small.c,width=12,height=5,dpi=100)

################# LARGE LAKES, CENTER OF LAKE
#by_buffersize<-group_by(llarge.c,BufferSize)
#do(by_buffersize,glance(lm(Chl~nir_median+red_median,data=.)))
#do(by_buffersize,tidy(lm(Chl~nir_median+red_median,data=.)))
pred.large.c<-
  llarge.c%>%group_by(BufferSize)%>%
 do(augment(lm(Chl~nir_median+red_median,data=.)))
nrmse.large.c<-
  pred.large.c%>%
  select(BufferSize,Chl,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Chl)^2))/sd(Chl)) %>%
  mutate(Subset="Large")%>%
  mutate(Centroid="C")
# Regressions
reg.large.c<-
  nrmse.large.c%>%
  ggplot(aes(Chl,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=6)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual Chl (ug/L)",y="Predicted Chl (ug/L)",title="Large lakes: Center")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.large.c.png",reg.large.c,width=16,height=12,dpi=100)

######## Create nrmse table #####
mergeCols<-c("Centroid","Subset","BufferSize","nrmse","Chl",".fitted")

Fit.5d.S<-merge(nrmse.small.s,nrmse.large.s,by=mergeCols,all=TRUE)%>%
  merge(.,nrmse.small.c,by=mergeCols,all=TRUE)%>%
  merge(.,nrmse.large.c,by=mergeCols,all=TRUE)

## Condense table to just the single nrmse for each buffer size and subset
FitSum<-summarise(group_by(Fit.5d.S,BufferSize,Subset,Centroid),
                  nrmse=median(nrmse))

######## Plot ####
# Reorder
FitSum$BufferSize <-
  factor(FitSum$BufferSize,levels=c("3","30","60","90","120","150","180","210","240","270","300",
                                    "330","360","390","420","450","480","510","540","570","600","630",
                                    "660","690","720","750","780","810","Whole"))

Yip.nrmse.s.size<-
  FitSum%>%
  filter(Centroid=="S")%>%
  ggplot(aes(BufferSize,nrmse,fill=Subset))+theme_classic()+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=14,angle=300,hjust=0),
        axis.text.y=element_text(size=14),
        plot.title = element_text(size=22),
        legend.text=element_text(size=16),
        legend.title=element_text(size=18),legend.position=c(.9,.9))+
  scale_y_continuous(lim=c(0,1))+
  scale_fill_manual(values=mycolors)+
  labs(title="Yip, Sample point",x="")

Yip.nrmse.c.size<-
  FitSum%>%
  filter(Centroid=="C")%>%
  ggplot(aes(BufferSize,nrmse,fill=Subset))+theme_classic()+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=14,angle=300,hjust=0),
        axis.text.y=element_text(size=14),
        plot.title = element_text(size=22),
        legend.text=element_text(size=16),
        legend.title=element_text(size=18),legend.position=c(.9,.9))+
  scale_y_continuous(lim=c(0,1))+
  scale_fill_manual(values=mycolors)+
  labs(title="Yip, Center of lake",x="")







### Brezonik et al. (2005) ##############
# ChlA = B1+(B1/B3)
# 13 lakes in east-central Minnesota
# See lines below to run this alogorithm on the six subsets

################# ALL 15 LAKES, SAMPLE POINT
#by_buffersize<-group_by(lsmall.s,BufferSize)
#do(by_buffersize,glance(lm(Chl~nir_median+(nir_median/red_median),data=.)))
#do(by_buffersize,tidy(lm(Chl~nir_median+(nir_median/red_median),data=.)))
## Add predicted (fitted) values to the lm table
pred.small.s<-
  lsmall.s%>%group_by(BufferSize)%>%
  do(augment(lm(Chl~nir_median+(nir_median/red_median),data=.)))
## Calculate nrmse (normalized by standard deviation, as calculated by HydroGOF package)
nrmse.small.s<-
  pred.small.s%>%
  select(BufferSize,Chl,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Chl)^2))/sd(Chl))%>% # adds columns with nrmse values
  mutate(r2=summary(lm(Chl~nir_median+(nir_median/red_median),data=lsmall.s))$r.squared)%>%
  mutate(Subset="All 15")%>%
  mutate(Centroid="S") 
# Regressions
reg.small.s<-
  nrmse.small.s%>%
  ggplot(aes(Chl,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=4)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual Chl (ug/L)",y="Predicted Chl (ug/L)",title="All lakes: Sample point")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.small.s.png",reg.small.s,width=12,height=5,dpi=100)

################# LARGE LAKES, SAMPLE POINT
#by_buffersize<-group_by(llarge.s,BufferSize)
#do(by_buffersize,glance(lm(Chl~nir_median+(nir_median/red_median),data=.))))
#do(by_buffersize,tidy(lm(Chl~nir_median+(nir_median/red_median),data=.))))
pred.large.s<-
  llarge.s%>%group_by(BufferSize)%>%
  do(augment(lm(Chl~nir_median+(nir_median/red_median),data=.)))
nrmse.large.s<-
  pred.large.s%>%
  select(BufferSize,Chl,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Chl)^2))/sd(Chl)) %>%
  mutate(Subset="Large")%>%
  mutate(Centroid="S")
# Regressions
reg.large.s<-
  nrmse.large.s%>%
  ggplot(aes(Chl,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=5)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual Chl (ug/L)",y="Predicted Chl (ug/L)",title="Large lakes: Sample point")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=1))
ggsave("reg.large.s.png",reg.large.s,width=12,height=8,dpi=100)

################# ALL 15 LAKES, CENTER OF LAKE
#by_buffersize<-group_by(lsmall.c,BufferSize)
#do(by_buffersize,glance(lm(Chl~nir_median+(nir_median/red_median),data=.))))
#do(by_buffersize,tidy(lm(Chl~nir_median+(nir_median/red_median),data=.))))
pred.small.c<-
  lsmall.c%>%group_by(BufferSize)%>%
  do(augment(lm(Chl~nir_median+(nir_median/red_median),data=.)))
nrmse.small.c<-
  pred.small.c%>%
  select(BufferSize,Chl,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Chl)^2))/sd(Chl)) %>%
  mutate(Subset="All 15")%>%
  mutate(Centroid="C")
# Regressions
reg.small.c<-
  nrmse.small.c%>%
  ggplot(aes(Chl,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=4)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual Chl (ug/L)",y="Predicted Chl (ug/L)",title="All lakes: Center")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.small.c.png",reg.small.c,width=12,height=5,dpi=100)

################# LARGE LAKES, CENTER OF LAKE
#by_buffersize<-group_by(llarge.c,BufferSize)
#do(by_buffersize,glance(lm(Chl~nir_median+(nir_median/red_median),data=.))))
#do(by_buffersize,tidy(lm(Chl~nir_median+(nir_median/red_median),data=.))))
pred.large.c<-
  llarge.c%>%group_by(BufferSize)%>%
  do(augment(lm(Chl~nir_median+(nir_median/red_median),data=.)))
nrmse.large.c<-
  pred.large.c%>%
  select(BufferSize,Chl,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Chl)^2))/sd(Chl)) %>%
  mutate(Subset="Large")%>%
  mutate(Centroid="C")
# Regressions
reg.large.c<-
  nrmse.large.c%>%
  ggplot(aes(Chl,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=6)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual Chl (ug/L)",y="Predicted Chl (ug/L)",title="Large lakes: Center")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.large.c.png",reg.large.c,width=16,height=12,dpi=100)

######## Create nrmse table #####
mergeCols<-c("Centroid","Subset","BufferSize","nrmse","Chl",".fitted")

Fit.5d.S<-merge(nrmse.small.s,nrmse.large.s,by=mergeCols,all=TRUE)%>%
  merge(.,nrmse.small.c,by=mergeCols,all=TRUE)%>%
  merge(.,nrmse.large.c,by=mergeCols,all=TRUE)

## Condense table to just the single nrmse for each buffer size and subset
FitSum<-summarise(group_by(Fit.5d.S,BufferSize,Subset,Centroid),
                  nrmse=median(nrmse))

######## Plot ####
# Reorder
FitSum$BufferSize <-
  factor(FitSum$BufferSize,levels=c("3","30","60","90","120","150","180","210","240","270","300",
                                    "330","360","390","420","450","480","510","540","570","600","630",
                                    "660","690","720","750","780","810","Whole"))

Brezonik.nrmse.s.size<-
  FitSum%>%
  filter(Centroid=="S")%>%
  ggplot(aes(BufferSize,nrmse,fill=Subset))+theme_classic()+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=14,angle=300,hjust=0),
        axis.text.y=element_text(size=14),
        plot.title = element_text(size=22),
        legend.text=element_text(size=16),
        legend.title=element_text(size=18),legend.position=c(.9,.9))+
  scale_y_continuous(lim=c(0,1))+
  scale_fill_manual(values=mycolors)+
  labs(title="Brezonik, Sample point",x="")

Brezonik.nrmse.c.size<-
  FitSum%>%
  filter(Centroid=="C")%>%
  ggplot(aes(BufferSize,nrmse,fill=Subset))+theme_classic()+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=14,angle=300,hjust=0),
        axis.text.y=element_text(size=14),
        plot.title = element_text(size=22),
        legend.text=element_text(size=16),
        legend.title=element_text(size=18),legend.position=c(.9,.9))+
  scale_y_continuous(lim=c(0,1))+
  scale_fill_manual(values=mycolors)+
  labs(title="Brezonik, Center of lake",x="")







### SABI (2020) ##############
# ChlA = (B4-B3)/(B1+B2)
# SABI (Surface algal bloom index) developed by Alwadi (2010) and used by 
# Empirical algorithm developed in order to detect water floating biomass that has a NIR response similar to that of land vegetation
# Specific inclusion of ocean-sensitive spectral bands (nir - characteristic of clear water, and green - characteristic of water column bloom)
# See lines below to run this alogorithm on the six subsets

################# ALL 15 LAKES, SAMPLE POINT
#by_buffersize<-group_by(lsmall.s,BufferSize)
#do(by_buffersize,glance(lm(Chl~(nir_median-red_median)/(nir_median+green_median),data=.)))
#do(by_buffersize,tidy(lm(Chl~(nir_median-red_median)/(nir_median+green_median),data=.)))
## Add predicted (fitted) values to the lm table
pred.small.s<-
  lsmall.s%>%group_by(BufferSize)%>%
  do(augment(lm(Chl~(nir_median-red_median)/(nir_median+green_median),data=.)))
## Calculate nrmse (normalized by standard deviation, as calculated by HydroGOF package)
nrmse.small.s<-
  pred.small.s%>%
  select(BufferSize,Chl,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Chl)^2))/sd(Chl))%>% # adds columns with nrmse values
  mutate(r2=summary(lm(Chl~(nir_median-red_median)/(nir_median+green_median),data=lsmall.s))$r.squared)%>%
  mutate(Subset="All 15")%>%
  mutate(Centroid="S") 
# Regressions
reg.small.s<-
  nrmse.small.s%>%
  ggplot(aes(Chl,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=4)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual Chl (ug/L)",y="Predicted Chl (ug/L)",title="All lakes: Sample point")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.small.s.png",reg.small.s,width=12,height=5,dpi=100)

################# LARGE LAKES, SAMPLE POINT
#by_buffersize<-group_by(llarge.s,BufferSize)
#do(by_buffersize,glance(lm(Chl~(nir_median-red_median)/(nir_median+green_median),data=.))))
#do(by_buffersize,tidy(lm(Chl~(nir_median-red_median)/(nir_median+green_median),data=.))))
pred.large.s<-
  llarge.s%>%group_by(BufferSize)%>%
  do(augment(lm(Chl~(nir_median-red_median)/(nir_median+green_median),data=.)))
nrmse.large.s<-
  pred.large.s%>%
  select(BufferSize,Chl,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Chl)^2))/sd(Chl)) %>%
  mutate(Subset="Large")%>%
  mutate(Centroid="S")
# Regressions
reg.large.s<-
  nrmse.large.s%>%
  ggplot(aes(Chl,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=5)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual Chl (ug/L)",y="Predicted Chl (ug/L)",title="Large lakes: Sample point")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=1))
ggsave("reg.large.s.png",reg.large.s,width=12,height=8,dpi=100)

################# ALL 15 LAKES, CENTER OF LAKE
#by_buffersize<-group_by(lsmall.c,BufferSize)
#do(by_buffersize,glance(lm(Chl~(nir_median-red_median)/(nir_median+green_median),data=.))))
#do(by_buffersize,tidy(lm(Chl~(nir_median-red_median)/(nir_median+green_median),data=.))))
pred.small.c<-
  lsmall.c%>%group_by(BufferSize)%>%
  do(augment(lm(Chl~(nir_median-red_median)/(nir_median+green_median),data=.)))
nrmse.small.c<-
  pred.small.c%>%
  select(BufferSize,Chl,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Chl)^2))/sd(Chl)) %>%
  mutate(Subset="All 15")%>%
  mutate(Centroid="C")
# Regressions
reg.small.c<-
  nrmse.small.c%>%
  ggplot(aes(Chl,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=4)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual Chl (ug/L)",y="Predicted Chl (ug/L)",title="All lakes: Center")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.small.c.png",reg.small.c,width=12,height=5,dpi=100)

################# LARGE LAKES, CENTER OF LAKE
#by_buffersize<-group_by(llarge.c,BufferSize)
#do(by_buffersize,glance(lm(Chl~(nir_median-red_median)/(nir_median+green_median),data=.))))
#do(by_buffersize,tidy(lm(Chl~(nir_median-red_median)/(nir_median+green_median),data=.))))
pred.large.c<-
  llarge.c%>%group_by(BufferSize)%>%
  do(augment(lm(Chl~(nir_median-red_median)/(nir_median+green_median),data=.)))
nrmse.large.c<-
  pred.large.c%>%
  select(BufferSize,Chl,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Chl)^2))/sd(Chl)) %>%
  mutate(Subset="Large")%>%
  mutate(Centroid="C")
# Regressions
reg.large.c<-
  nrmse.large.c%>%
  ggplot(aes(Chl,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=6)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual Chl (ug/L)",y="Predicted Chl (ug/L)",title="Large lakes: Center")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.large.c.png",reg.large.c,width=16,height=12,dpi=100)

######## Create nrmse table #####
mergeCols<-c("Centroid","Subset","BufferSize","nrmse","Chl",".fitted")

Fit.5d.S<-merge(nrmse.small.s,nrmse.large.s,by=mergeCols,all=TRUE)%>%
  merge(.,nrmse.small.c,by=mergeCols,all=TRUE)%>%
  merge(.,nrmse.large.c,by=mergeCols,all=TRUE)

## Condense table to just the single nrmse for each buffer size and subset
FitSum<-summarise(group_by(Fit.5d.S,BufferSize,Subset,Centroid),
                  nrmse=median(nrmse))

######## Plot ####
# Reorder
FitSum$BufferSize <-
  factor(FitSum$BufferSize,levels=c("3","30","60","90","120","150","180","210","240","270","300",
                                    "330","360","390","420","450","480","510","540","570","600","630",
                                    "660","690","720","750","780","810","Whole"))

SABI.nrmse.s.size<-
  FitSum%>%
  filter(Centroid=="S")%>%
  ggplot(aes(BufferSize,nrmse,fill=Subset))+theme_classic()+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=14,angle=300,hjust=0),
        axis.text.y=element_text(size=14),
        plot.title = element_text(size=22),
        legend.text=element_text(size=16),
        legend.title=element_text(size=18),legend.position=c(.9,.9))+
  scale_y_continuous(lim=c(0,1))+
  scale_fill_manual(values=mycolors)+
  labs(title="SABI, Sample point",x="")

SABI.nrmse.c.size<-
  FitSum%>%
  filter(Centroid=="C")%>%
  ggplot(aes(BufferSize,nrmse,fill=Subset))+theme_classic()+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=14,angle=300,hjust=0),
        axis.text.y=element_text(size=14),
        plot.title = element_text(size=22),
        legend.text=element_text(size=16),
        legend.title=element_text(size=18),legend.position=c(.9,.9))+
  scale_y_continuous(lim=c(0,1))+
  scale_fill_manual(values=mycolors)+
  labs(title="SABI, Center of lake",x="")








### Plot the Chl algorithms together ####
chlAlgs<-arrangeGrob(Giardino.nrmse.s.size,Giardino.nrmse.c.size,Mancino.nrmse.s.size,
                     Mancino.nrmse.c.size,Yip.nrmse.s.size,Yip.nrmse.c.size,
                     Brezonik.nrmse.s.size,Brezonik.nrmse.c.size,SABI.nrmse.s.size,SABI.nrmse.c.size,ncol=2,nrow=5)
ggsave("chlAlgs.jpeg",chlAlgs,width=25,height=20)
ggsave("chlAlgs.png",chlAlgs,width=25,height=20,dpi=200)


####### CDOM ALGORITHMS BELOW: ################
### Brezonik et al. (2005) ##############
# ln(a440nm) = B2+(B1/B4)
# 13 lakes in east-central Minnesota
# See lines below to run this alogorithm on the six subsets

################# ALL 15 LAKES, SAMPLE POINT
#by_buffersize<-group_by(lsmall.s,BufferSize)
#do(by_buffersize,glance(lm(Color_440_nm~green_median+(nir_median/nir_median),data=.))))
#do(by_buffersize,tidy(lm(Color_440_nm~green_median+(nir_median/nir_median),data=.))))
## Add predicted (fitted) values to the lm table
pred.small.s<-
  lsmall.s%>%group_by(BufferSize)%>%
  do(augment(lm(Color_440_nm~green_median+(nir_median/nir_median),data=.)))
## Calculate nrmse (normalized by standard deviation, as calculated by HydroGOF package)
nrmse.small.s<-
  pred.small.s%>%
  select(BufferSize,Color_440_nm,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Color_440_nm)^2))/sd(Color_440_nm))%>% # adds columns with nrmse values
  mutate(Subset="All 15")%>%
  mutate(Centroid="S") 
# Regressions
reg.small.s<-
  nrmse.small.s%>%
  ggplot(aes(Color_440_nm,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=4)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual cdom (440 nm)",y="Predicted cdom",title="All lakes: Sample point")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.small.s.png",reg.small.s,width=12,height=5,dpi=100)

################# LARGE LAKES, SAMPLE POINT
#by_buffersize<-group_by(llarge.s,BufferSize)
#do(by_buffersize,glance(lm(Color_440_nm~green_median+(nir_median/nir_median),data=.))))
#do(by_buffersize,tidy(lm(Color_440_nm~green_median+(nir_median/nir_median),data=.))))
pred.large.s<-
  llarge.s%>%group_by(BufferSize)%>%
  do(augment(lm(Color_440_nm~green_median+(nir_median/nir_median),data=.)))
nrmse.large.s<-
  pred.large.s%>%
  select(BufferSize,Color_440_nm,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Color_440_nm)^2))/sd(Color_440_nm)) %>%
  mutate(Subset="Large")%>%
  mutate(Centroid="S")
# Regressions
reg.large.s<-
  nrmse.large.s%>%
  ggplot(aes(Color_440_nm,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=5)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual cdom (440 nm)",y="Predicted cdom",title="Large lakes: Sample point")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=1))
ggsave("reg.large.s.png",reg.large.s,width=12,height=8,dpi=100)

################# ALL 15 LAKES, CENTER OF LAKE
#by_buffersize<-group_by(lsmall.c,BufferSize)
#do(by_buffersize,glance(lm(Color_440_nm~green_median+(nir_median/nir_median),data=.))))
#do(by_buffersize,tidy(lm(Color_440_nm~green_median+(nir_median/nir_median),data=.))))
pred.small.c<-
  lsmall.c%>%group_by(BufferSize)%>%
  do(augment(lm(Color_440_nm~green_median+(nir_median/nir_median),data=.)))
nrmse.small.c<-
  pred.small.c%>%
  select(BufferSize,Color_440_nm,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Color_440_nm)^2))/sd(Color_440_nm)) %>%
  mutate(Subset="All 15")%>%
  mutate(Centroid="C")
# Regressions
reg.small.c<-
  nrmse.small.c%>%
  ggplot(aes(Color_440_nm,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=4)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual cdom (440 nm)",y="Predicted cdom",title="All lakes: Center")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.small.c.png",reg.small.c,width=12,height=5,dpi=100)


################# LARGE LAKES, CENTER OF LAKE
#by_buffersize<-group_by(llarge.c,BufferSize)
#do(by_buffersize,glance(lm(Color_440_nm~green_median+(nir_median/nir_median),data=.))))
#do(by_buffersize,tidy(lm(Color_440_nm~green_median+(nir_median/nir_median),data=.))))
pred.large.c<-
  llarge.c%>%group_by(BufferSize)%>%
  do(augment(lm(Color_440_nm~green_median+(nir_median/nir_median),data=.)))
nrmse.large.c<-
  pred.large.c%>%
  select(BufferSize,Color_440_nm,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Color_440_nm)^2))/sd(Color_440_nm)) %>%
  mutate(Subset="Large")%>%
  mutate(Centroid="C")
# Regressions
reg.large.c<-
  nrmse.large.c%>%
  ggplot(aes(Color_440_nm,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=6)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual cdom (440 nm)",y="Predicted cdom",title="Large lakes: Center")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.large.c.png",reg.large.c,width=16,height=12,dpi=100)


######## Create nrmse table #####
mergeCols<-c("Centroid","Subset","BufferSize","nrmse","Color_440_nm",".fitted")

Fit.5d.S<-merge(nrmse.small.s,nrmse.large.s,by=mergeCols,all=TRUE)%>%
  merge(.,nrmse.small.c,by=mergeCols,all=TRUE)%>%
  merge(.,nrmse.large.c,by=mergeCols,all=TRUE)

  
## Condense table to just the single nrmse for each buffer size and subset
FitSum<-summarise(group_by(Fit.5d.S,BufferSize,Subset,Centroid),
                  nrmse=median(nrmse))

######## Plot ####
# Reorder
FitSum$BufferSize <-
  factor(FitSum$BufferSize,levels=c("3","30","60","90","120","150","180","210","240","270","300",
                                    "330","360","390","420","450","480","510","540","570","600","630",
                                    "660","690","720","750","780","810","Whole"))

Brezonik.cdom.nrmse.s.size<-
  FitSum%>%
  filter(Centroid=="S")%>%
  ggplot(aes(BufferSize,nrmse,fill=Subset))+theme_classic()+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=14,angle=300,hjust=0),
        axis.text.y=element_text(size=14),
        plot.title = element_text(size=22),
        legend.text=element_text(size=16),
        legend.title=element_text(size=18),legend.position=c(.9,.9))+
  scale_y_continuous(lim=c(0,1))+
  scale_fill_manual(values=mycolors)+
  labs(title="Brezonik, Sample point",x="")

Brezonik.cdom.nrmse.c.size<-
  FitSum%>%
  filter(Centroid=="C")%>%
  ggplot(aes(BufferSize,nrmse,fill=Subset))+theme_classic()+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=14,angle=300,hjust=0),
        axis.text.y=element_text(size=14),
        plot.title = element_text(size=22),
        legend.text=element_text(size=16),
        legend.title=element_text(size=18),legend.position=c(.9,.9))+
  scale_y_continuous(lim=c(0,1))+
  scale_fill_manual(values=mycolors)+
  labs(title="Brezonik, Center of lake",x="")









### Griffin et al. (2018) ##############
# a375=B1+B2+B3 -- note absorbance @ 375 nm
# 6 Arctic rivers 
# See lines below to run this alogorithm on the six subsets

################# ALL 15 LAKES, SAMPLE POINT
#by_buffersize<-group_by(lsmall.s,BufferSize)
#do(by_buffersize,glance(lm(Color_320_nm~nir_median+green_median+red_median,data=.))))
#do(by_buffersize,tidy(lm(Color_320_nm~nir_median+green_median+red_median,data=.))))
## Add predicted (fitted) values to the lm table
pred.small.s<-
  lsmall.s%>%group_by(BufferSize)%>%
  do(augment(lm(Color_320_nm~nir_median+green_median+red_median,data=.)))
## Calculate nrmse (normalized by standard deviation, as calculated by HydroGOF package)
nrmse.small.s<-
  pred.small.s%>%
  select(BufferSize,Color_320_nm,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Color_320_nm)^2))/sd(Color_320_nm))%>% # adds columns with nrmse values
  mutate(Subset="All 15")%>%
  mutate(Centroid="S")
# Regressions
reg.small.s<-
  nrmse.small.s%>%
  ggplot(aes(Color_320_nm,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=4)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual cdom (320 nm)",y="Predicted cdom",title="All lakes: Sample point")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.small.s.png",reg.small.s,width=12,height=5,dpi=100)


################# LARGE LAKES, SAMPLE POINT
#by_buffersize<-group_by(llarge.s,BufferSize)
#do(by_buffersize,glance(lm(Color_320_nm~nir_median+green_median+red_median,data=.))))
#do(by_buffersize,tidy(lm(Color_320_nm~nir_median+green_median+red_median,data=.))))
pred.large.s<-
  llarge.s%>%group_by(BufferSize)%>%
  do(augment(lm(Color_320_nm~nir_median+green_median+red_median,data=.)))
nrmse.large.s<-
  pred.large.s%>%
  select(BufferSize,Color_320_nm,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Color_320_nm)^2))/sd(Color_320_nm)) %>%
  mutate(Subset="Large")%>%
  mutate(Centroid="S")
# Regressions
reg.large.s<-
  nrmse.large.s%>%
  ggplot(aes(Color_320_nm,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=5)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual cdom (320 nm)",y="Predicted Chl",title="Large lakes: Sample point")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=1))
ggsave("reg.large.s.png",reg.large.s,width=12,height=8,dpi=100)

################# ALL 15 LAKES, CENTER OF LAKE
#by_buffersize<-group_by(lsmall.c,BufferSize)
#do(by_buffersize,glance(lm(Color_320_nm~nir_median+green_median+red_median,data=.))))
#do(by_buffersize,tidy(lm(Color_320_nm~nir_median+green_median+red_median,data=.))))
pred.small.c<-
  lsmall.c%>%group_by(BufferSize)%>%
  do(augment(lm(Color_320_nm~nir_median+green_median+red_median,data=.)))
nrmse.small.c<-
  pred.small.c%>%
  select(BufferSize,Color_320_nm,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Color_320_nm)^2))/sd(Color_320_nm)) %>%
  mutate(Subset="All 15")%>%
  mutate(Centroid="C")
# Regressions
reg.small.c<-
  nrmse.small.c%>%
  ggplot(aes(Color_320_nm,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=4)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual cdom (320 nm)",y="Predicted cdom",title="All lakes: Center")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.small.c.png",reg.small.c,width=12,height=5,dpi=100)

################# LARGE LAKES, CENTER OF LAKE
#by_buffersize<-group_by(llarge.c,BufferSize)
#do(by_buffersize,glance(lm(Color_320_nm~nir_median+green_median+red_median,data=.))))
#do(by_buffersize,tidy(lm(Color_320_nm~nir_median+green_median+red_median,data=.))))
pred.large.c<-
  llarge.c%>%group_by(BufferSize)%>%
  do(augment(lm(Color_320_nm~nir_median+green_median+red_median,data=.)))
nrmse.large.c<-
  pred.large.c%>%
  select(BufferSize,Color_320_nm,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Color_320_nm)^2))/sd(Color_320_nm)) %>%
  mutate(Subset="Large")%>%
  mutate(Centroid="C")
# Regressions
reg.large.c<-
  nrmse.large.c%>%
  ggplot(aes(Color_320_nm,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=6)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual cdom (320 nm)",y="Predicted cdom",title="Large lakes: Center")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.large.c.png",reg.large.c,width=16,height=12,dpi=100)


######## Create nrmse table #####
mergeCols<-c("Centroid","Subset","BufferSize","nrmse","Color_320_nm",".fitted")

Fit.5d.S<-merge(nrmse.small.s,nrmse.large.s,by=mergeCols,all=TRUE)%>%
  merge(.,nrmse.small.c,by=mergeCols,all=TRUE)%>%
  merge(.,nrmse.large.c,by=mergeCols,all=TRUE)

## Condense table to just the single nrmse for each buffer size and subset
FitSum<-summarise(group_by(Fit.5d.S,BufferSize,Subset,Centroid),
                  nrmse=median(nrmse))

######## Plot ####
# Reorder
FitSum$BufferSize <-
  factor(FitSum$BufferSize,levels=c("3","30","60","90","120","150","180","210","240","270","300",
                                    "330","360","390","420","450","480","510","540","570","600","630",
                                    "660","690","720","750","780","810","Whole"))

Griffin.nrmse.s.size<-
  FitSum%>%
  filter(Centroid=="S")%>%
  ggplot(aes(BufferSize,nrmse,fill=Subset))+theme_classic()+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=14,angle=300,hjust=0),
        axis.text.y=element_text(size=14),
        plot.title = element_text(size=22),
        legend.text=element_text(size=16),
        legend.title=element_text(size=18),legend.position=c(.9,.9))+
  scale_y_continuous(lim=c(0,1))+
  scale_fill_manual(values=mycolors)+
  labs(title="Griffin, Sample point",x="")

Griffin.nrmse.c.size<-
  FitSum%>%
  filter(Centroid=="C")%>%
  ggplot(aes(BufferSize,nrmse,fill=Subset))+theme_classic()+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=14,angle=300,hjust=0),
        axis.text.y=element_text(size=14),
        plot.title = element_text(size=22),
        legend.text=element_text(size=16),
        legend.title=element_text(size=18),legend.position=c(.9,.9))+
  scale_y_continuous(lim=c(0,1))+
  scale_fill_manual(values=mycolors)+
  labs(title="Griffin, Center of lake",x="")










### Olmanson et al. (2016) ##############
# ln(a440) = (B2/B3) + (B3/B4)
# 61 surface water sites (lakes and rivers) in Minnesota and northern Wisconsin 
# See lines below to run this alogorithm on the six subsets

################# ALL 15 LAKES, SAMPLE POINT
#by_buffersize<-group_by(lsmall.s,BufferSize)
#do(by_buffersize,glance(lm(Color_420_nm~green_median/red_median,data=.))))
#do(by_buffersize,tidy(lm(Color_440_nm~(green_median/red_median)+(red_median/nir_median),data=.))))
## Add predicted (fitted) values to the lm table
pred.small.s<-
  lsmall.s%>%group_by(BufferSize)%>%
  do(augment(lm(Color_440_nm~(green_median/red_median)+(red_median/nir_median),data=.)))
## Calculate nrmse (normalized by standard deviation, as calculated by HydroGOF package)
nrmse.small.s<-
  pred.small.s%>%
  select(BufferSize,Color_440_nm,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Color_440_nm)^2))/sd(Color_440_nm))%>% # adds columns with nrmse values
  mutate(Subset="All 15")%>%
  mutate(Centroid="S")
# Regressions
reg.small.s<-
  nrmse.small.s%>%
  ggplot(aes(Color_440_nm,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=4)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual cdom (440 nm)",y="Predicted cdom",title="All lakes: Sample point")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.small.s.png",reg.small.s,width=12,height=5,dpi=100)


################# LARGE LAKES, SAMPLE POINT
#by_buffersize<-group_by(llarge.s,BufferSize)
#do(by_buffersize,glance(lm(Color_440_nm~(green_median/red_median)+(red_median/nir_median),data=.))))
#do(by_buffersize,tidy(lm(Color_440_nm~(green_median/red_median)+(red_median/nir_median),data=.))))
pred.large.s<-
  llarge.s%>%group_by(BufferSize)%>%
  do(augment(lm(Color_440_nm~(green_median/red_median)+(red_median/nir_median),data=.)))
nrmse.large.s<-
  pred.large.s%>%
  select(BufferSize,Color_440_nm,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Color_440_nm)^2))/sd(Color_440_nm)) %>%
  mutate(Subset="Large")%>%
  mutate(Centroid="S")
# Regressions
reg.large.s<-
  nrmse.large.s%>%
  ggplot(aes(Color_440_nm,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=5)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual cdom (440 nm)",y="Predicted cdom",title="Large lakes: Sample point")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=1))
ggsave("reg.large.s.png",reg.large.s,width=12,height=8,dpi=100)

################# ALL 15 LAKES, CENTER OF LAKE
#by_buffersize<-group_by(lsmall.c,BufferSize)
#do(by_buffersize,glance(lm(Color_440_nm~(green_median/red_median)+(red_median/nir_median),data=.))))
#do(by_buffersize,tidy(lm(Color_440_nm~(green_median/red_median)+(red_median/nir_median),data=.))))
pred.small.c<-
  lsmall.c%>%group_by(BufferSize)%>%
  do(augment(lm(Color_440_nm~(green_median/red_median)+(red_median/nir_median),data=.)))
nrmse.small.c<-
  pred.small.c%>%
  select(BufferSize,Color_440_nm,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Color_440_nm)^2))/sd(Color_440_nm)) %>%
  mutate(Subset="All 15")%>%
  mutate(Centroid="C")
# Regressions
reg.small.c<-
  nrmse.small.c%>%
  ggplot(aes(Color_440_nm,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=4)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual cdom (440 nm)",y="Predicted cdom",title="All lakes: Center")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.small.c.png",reg.small.c,width=12,height=5,dpi=100)

################# LARGE LAKES, CENTER OF LAKE
#by_buffersize<-group_by(llarge.c,BufferSize)
#do(by_buffersize,glance(lm(Color_440_nm~(green_median/red_median)+(red_median/nir_median),data=.))))
#do(by_buffersize,tidy(lm(Color_440_nm~(green_median/red_median)+(red_median/nir_median),data=.))))
pred.large.c<-
  llarge.c%>%group_by(BufferSize)%>%
  do(augment(lm(Color_440_nm~(green_median/red_median)+(red_median/nir_median),data=.)))
nrmse.large.c<-
  pred.large.c%>%
  select(BufferSize,Color_440_nm,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Color_440_nm)^2))/sd(Color_440_nm)) %>%
  mutate(Subset="Large")%>%
  mutate(Centroid="C")
# Regressions
reg.large.c<-
  nrmse.large.c%>%
  ggplot(aes(Color_440_nm,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=6)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual cdom (440 nm)",y="Predicted cdom",title="Large lakes: Center")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.large.c.png",reg.large.c,width=16,height=12,dpi=100)


######## Create nrmse table #####
mergeCols<-c("Centroid","Subset","BufferSize","nrmse","Color_420_nm",".fitted")

Fit.5d.S<-merge(nrmse.small.s,nrmse.large.s,by=mergeCols,all=TRUE)%>%
  merge(.,nrmse.small.c,by=mergeCols,all=TRUE)%>%
  merge(.,nrmse.large.c,by=mergeCols,all=TRUE)

## Condense table to just the single nrmse for each buffer size and subset
FitSum<-summarise(group_by(Fit.5d.S,BufferSize,Subset,Centroid),
                  nrmse=median(nrmse))

######## Plot ####
# Reorder
FitSum$BufferSize <-
  factor(FitSum$BufferSize,levels=c("3","30","60","90","120","150","180","210","240","270","300",
                                    "330","360","390","420","450","480","510","540","570","600","630",
                                    "660","690","720","750","780","810","Whole"))

Olmanson.nrmse.s.size<-
  FitSum%>%
  filter(Centroid=="S")%>%
  ggplot(aes(BufferSize,nrmse,fill=Subset))+theme_classic()+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=14,angle=300,hjust=0),
        axis.text.y=element_text(size=14),
        plot.title = element_text(size=22),
        legend.text=element_text(size=16),
        legend.title=element_text(size=18),legend.position=c(.9,.9))+
  scale_y_continuous(lim=c(0,1))+
  scale_fill_manual(values=mycolors)+
  labs(title="Olmanson, Sample point",x="")

Olmanson.nrmse.c.size<-
  FitSum%>%
  filter(Centroid=="C")%>%
  ggplot(aes(BufferSize,nrmse,fill=Subset))+theme_classic()+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=14,angle=300,hjust=0),
        axis.text.y=element_text(size=14),
        plot.title = element_text(size=22),
        legend.text=element_text(size=16),
        legend.title=element_text(size=18),legend.position=c(.9,.9))+
  scale_y_continuous(lim=c(0,1))+
  scale_fill_manual(values=mycolors)+
  labs(title="Olmanson, Center of lake",x="")











### Kutser et al. (2012) ##############
# a320nm = B2/B3
# Two sites, one central one southern Sweden
# LS4, 5, and 7
# See lines below to run this alogorithm on the six subsets

################# ALL 15 LAKES, SAMPLE POINT
#by_buffersize<-group_by(lsmall.s,BufferSize)
#do(by_buffersize,glance(lm(Color_320_nm~green_median/red_median,data=.))))
#do(by_buffersize,tidy(lm(Color_320_nm~green_median/red_median,data=.))))
## Add predicted (fitted) values to the lm table
pred.small.s<-
  lsmall.s%>%group_by(BufferSize)%>%
  do(augment(lm(Color_320_nm~green_median/red_median,data=.)))
## Calculate nrmse (normalized by standard deviation, as calculated by HydroGOF package)
nrmse.small.s<-
  pred.small.s%>%
  select(BufferSize,Color_320_nm,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Color_320_nm)^2))/sd(Color_320_nm))%>% # adds columns with nrmse values
  mutate(Subset="All 15")%>%
  mutate(Centroid="S")
# Regressions
reg.small.s<-
  nrmse.small.s%>%
  ggplot(aes(Color_320_nm,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=4)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual cdom (440 nm)",y="Predicted cdom",title="All lakes: Sample point")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.small.s.png",reg.small.s,width=12,height=5,dpi=100)


################# LARGE LAKES, SAMPLE POINT
#by_buffersize<-group_by(llarge.s,BufferSize)
#do(by_buffersize,glance(lm(Color_320_nm~green_median/red_median,data=.))))
#do(by_buffersize,tidy(lm(Color_320_nm~green_median/red_median,data=.))))
pred.large.s<-
  llarge.s%>%group_by(BufferSize)%>%
  do(augment(lm(Color_320_nm~green_median/red_median,data=.)))
nrmse.large.s<-
  pred.large.s%>%
  select(BufferSize,Color_320_nm,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Color_320_nm)^2))/sd(Color_320_nm)) %>%
  mutate(Subset="Large")%>%
  mutate(Centroid="S")
# Regressions
reg.large.s<-
  nrmse.large.s%>%
  ggplot(aes(Color_320_nm,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=5)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual cdom (440 nm)",y="Predicted cdom",title="Large lakes: Sample point")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=1))
ggsave("reg.large.s.png",reg.large.s,width=12,height=8,dpi=100)

################# ALL 15 LAKES, CENTER OF LAKE
#by_buffersize<-group_by(lsmall.c,BufferSize)
#do(by_buffersize,glance(lm(Color_320_nm~green_median/red_median,data=.))))
#do(by_buffersize,tidy(lm(Color_320_nm~green_median/red_median,data=.))))
pred.small.c<-
  lsmall.c%>%group_by(BufferSize)%>%
  do(augment(lm(Color_320_nm~green_median/red_median,data=.)))
nrmse.small.c<-
  pred.small.c%>%
  select(BufferSize,Color_320_nm,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Color_320_nm)^2))/sd(Color_320_nm)) %>%
  mutate(Subset="All 15")%>%
  mutate(Centroid="C")
# Regressions
reg.small.c<-
  nrmse.small.c%>%
  ggplot(aes(Color_320_nm,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=4)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual cdom (440 nm)",y="Predicted cdom",title="All lakes: Center")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.small.c.png",reg.small.c,width=12,height=5,dpi=100)


################# LARGE LAKES, CENTER OF LAKE
#by_buffersize<-group_by(llarge.c,BufferSize)
#do(by_buffersize,glance(lm(Color_320_nm~green_median/red_median,data=.))))
#do(by_buffersize,tidy(lm(Color_320_nm~green_median/red_median,data=.))))
pred.large.c<-
  llarge.c%>%group_by(BufferSize)%>%
  do(augment(lm(Color_320_nm~green_median/red_median,data=.)))
nrmse.large.c<-
  pred.large.c%>%
  select(BufferSize,Color_320_nm,.fitted)%>%
  group_by(BufferSize)%>%
  mutate(nrmse=sqrt(median((.fitted - Color_320_nm)^2))/sd(Color_320_nm)) %>%
  mutate(Subset="Large")%>%
  mutate(Centroid="C")
# Regressions
reg.large.c<-
  nrmse.large.c%>%
  ggplot(aes(Color_320_nm,.fitted))+theme_classic()+
  geom_point(size=3,color="darkgreen")+geom_smooth(method="lm",color="black")+
  facet_wrap(~BufferSize,ncol=6)+
  scale_color_manual(values=mycolors)+geom_abline(slope=1,linetype="dashed")+
  labs(x="Actual cdom (440 nm)",y="Predicted cdom",title="Large lakes: Center")+
  stat_poly_eq(formula=y~x,label.x = "left",label.y="top",parse=TRUE,
               aes(label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))
ggsave("reg.large.c.png",reg.large.c,width=16,height=12,dpi=100)

######## Create nrmse table ######
mergeCols<-c("Centroid","Subset","BufferSize","nrmse","Color_320_nm",".fitted")

Fit.5d.S<-merge(nrmse.small.s,nrmse.large.s,by=mergeCols,all=TRUE)%>%
  merge(.,nrmse.small.c,by=mergeCols,all=TRUE)%>%
  merge(.,nrmse.large.c,by=mergeCols,all=TRUE)

## Condense table to just the single nrmse for each buffer size and subset
FitSum<-summarise(group_by(Fit.5d.S,BufferSize,Subset,Centroid),
                  nrmse=median(nrmse))

######## Plot ####
# Reorder
FitSum$BufferSize <-
  factor(FitSum$BufferSize,levels=c("3","30","60","90","120","150","180","210","240","270","300",
                                    "330","360","390","420","450","480","510","540","570","600","630",
                                    "660","690","720","750","780","810","Whole"))

Kutser.nrmse.s.size<-
  FitSum%>%
  filter(Centroid=="S")%>%
  ggplot(aes(BufferSize,nrmse,fill=Subset))+theme_classic()+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=14,angle=300,hjust=0),
        axis.text.y=element_text(size=14),
        plot.title = element_text(size=22),
        legend.text=element_text(size=16),
        legend.title=element_text(size=18),legend.position=c(.9,.9))+
  scale_y_continuous(lim=c(0,1))+
  scale_fill_manual(values=mycolors)+
  labs(title="Kutser, Sample point",x="")

Kutser.nrmse.c.size<-
  FitSum%>%
  filter(Centroid=="C")%>%
  ggplot(aes(BufferSize,nrmse,fill=Subset))+theme_classic()+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=14,angle=300,hjust=0),
        axis.text.y=element_text(size=14),
        plot.title = element_text(size=22),
        legend.text=element_text(size=16),
        legend.title=element_text(size=18),legend.position=c(.9,.9))+
  scale_y_continuous(lim=c(0,1))+
  scale_fill_manual(values=mycolors)+
  labs(title="Kutser, Center of lake",x="")




######### Plot 4 CDOM algorithms together ####
cdomAlgs<-arrangeGrob(Brezonik.cdom.nrmse.s.size,Brezonik.cdom.nrmse.c.size,
                      Griffin.nrmse.s.size,Griffin.nrmse.c.size,Olmanson.nrmse.s.size,
                      Olmanson.nrmse.c.size,Kutser.nrmse.s.size,Kutser.nrmse.c.size,ncol=2,nrow=4)
ggsave("cdomAlgs.jpeg",cdomAlgs,width=20,height=20)
ggsave("cdomAlgs.png",cdomAlgs,width=20,height=20,dpi=200)




######### Histograms of limno variables ######
(max(limno_avg$Chl,na.rm=TRUE) - min(limno_avg$Chl,na.rm=TRUE))/sqrt(15) # binwidths
(max(limno_avg$Color_440_nm,na.rm=TRUE) - min(limno_avg$Color_440_nm,na.rm=TRUE))/sqrt(15) # binwidths
(max(limno_avg$Color_320_nm,na.rm=TRUE) - min(limno_avg$Color_320_nm,na.rm=TRUE))/sqrt(15) # binwidths

Chl_histo<-
  limno_avg%>%
  ggplot(aes(Chl))+theme_classic()+
  geom_histogram(binwidth=2,fill="grey", color="black", alpha=0.9)

CDOM440_histo<-
  limno_avg%>%
  ggplot(aes(Color_440_nm))+theme_classic()+
  geom_histogram(binwidth=.017,fill="grey", color="black", alpha=0.9,na.rm=TRUE)

CDOM320_histo<-
  limno_avg%>%
  ggplot(aes(Color_320_nm))+theme_classic()+
  geom_histogram(binwidth=.12,fill="grey", color="black", alpha=0.9,na.rm=TRUE)



### Plot differences btw Center & Sample points - no subsets ####
# Change column names in center of lake data table to make them distinctive for the join
buffersCW<-
  buffersCW %>% 
  rename(
    blue_stdDevC = blue_stdDev,
    blue_meanC = blue_mean,
    blue_medianC = blue_median,
    green_stdDevC = green_stdDev,
    green_meanC = green_mean,
    green_medianC = green_median,
    red_stdDevC = red_stdDev,
    red_meanC = red_mean,
    red_medianC = red_median,
    nir_stdDevC = nir_stdDev,
    nir_meanC = nir_mean,
    nir_medianC = nir_median    
  )

## Merge the new data tables
mergeCols3<-c("BufferSize","Lake")
newtable<-full_join(buffersW,buffersCW,by=mergeCols3)

diffTable<-
  newtable%>%select(Lake,BufferSize,blue_mean,blue_meanC,blue_median,blue_medianC,blue_stdDev,blue_stdDevC,
                    green_stdDev,green_stdDevC,green_mean,green_meanC,green_median,green_medianC,
                    green_stdDev,green_stdDevC,red_mean,red_meanC,red_median,red_medianC,
                    red_stdDev,red_stdDevC,nir_mean,nir_meanC,nir_median,nir_medianC,
                    nir_stdDev,nir_stdDevC,)%>%
mutate(diff.bluemean = (blue_meanC-blue_mean),
       diff.bluemedian = (blue_medianC-blue_median),
       diff.bluestd = (blue_stdDevC-blue_stdDev),
       diff.greenmean = (green_meanC-green_mean),
       diff.greenmedian = (green_medianC-green_median),
       diff.greenstd = (green_stdDevC-green_stdDev),
       diff.redmean = (red_meanC-red_mean),
       diff.redmedian = (red_medianC-red_median),
       diff.redstd = (red_stdDevC-red_stdDev),
       diff.nirmean = (nir_meanC-nir_mean),
       diff.nirmedian = (nir_medianC-nir_median),
       diff.nirstd = (nir_stdDevC-nir_stdDev)
       )

# Plot differences
diffTable$BufferSize <- factor(diffTable$BufferSize, levels=c("3","30","60","90","120","150","180",
                                                              "210","240","270","300","330","360",
                                                              "390","420","450","480","510","Whole"))
## Remove na rows
diffTable<-diffTable%>%filter(!is.na(BufferSize))

# Blue mean
diffTable%>%  
ggplot(aes(BufferSize,diff.bluemean,color=Lake))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=18))+
  scale_color_manual(values=mycolors)+
  labs(x="",y="Difference in blue means \n(center - sample point)")

# Blue median
diffTable%>%  
  ggplot(aes(BufferSize,diff.bluemedian,color=Lake))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=18))+
  scale_color_manual(values=mycolors)+
  labs(x="",y="Difference in blue medians \n(center - sample point)")

# Blue stdev
diffTable%>%  
  ggplot(aes(BufferSize,diff.bluestd,color=Lake))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=18))+
  scale_color_manual(values=mycolors)+
  labs(x="",y="Difference in blue stdev \n(center - sample point)")


# green mean
diffTable%>%  
  ggplot(aes(BufferSize,diff.greenmean,color=Lake))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=18))+
  scale_color_manual(values=mycolors)+
  labs(x="")

# green median
diffTable%>%  
  ggplot(aes(BufferSize,diff.greenmedian,color=Lake))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=18))+
  scale_color_manual(values=mycolors)+
  labs(x="",y="Difference in green median \n(center - sample point)")

# green stdev
diffTable%>%  
  ggplot(aes(BufferSize,diff.greenstd,color=Lake))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=18))+
  scale_color_manual(values=mycolors)+
  labs(x="",y="Difference in green Stdev \n(center - sample point)")

# red mean
diffTable%>%  
  ggplot(aes(BufferSize,diff.redmean,color=Lake))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=18))+
  scale_color_manual(values=mycolors)+
  labs(x="",y="Difference in red means \n(center - sample point)")

# red median
diffTable%>%  
  ggplot(aes(BufferSize,diff.redmedian,color=Lake))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=18))+
  scale_color_manual(values=mycolors)+
  labs(x="",y="Difference in red medians \n(center - sample point)")

# red stdev
diffTable%>%  
  ggplot(aes(BufferSize,diff.redstd,color=Lake))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=18))+
  scale_color_manual(values=mycolors)+
  labs(x="",y="Difference in blue stdev \n(center - sample point)")

# nir mean
diffTable%>%  
  ggplot(aes(BufferSize,diff.nirmean,color=Lake))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=18))+
  scale_color_manual(values=mycolors)+
  labs(x="",y="Difference in nir means \n(center - sample point)")

# nir median
diffTable%>%  
  ggplot(aes(BufferSize,diff.nirmedian,color=Lake))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=18))+
  scale_color_manual(values=mycolors)+
  labs(x="",y="Difference in nir medians \n(center - sample point)")

# nir stdev
diffTable%>%  
  ggplot(aes(BufferSize,diff.nirstd,color=Lake))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=18))+
  scale_color_manual(values=mycolors)+
  labs(x="",y="Difference in nir stdev \n(center - sample point)")

## Reshape into long form
longtable1<-
  melt(diffTable,id.variables=c("Lake","BufferSize","diff.bluemedian","diff.greenmedian","diff.redmedian",
                                "diff.nirmedian","diff.bluestd","diff.greenstd","diff.redstd","diff.nirstd"),
       variable.name=c("Band.Mean"),
       value.name=c("diff.mean"),
       measure.vars=c("diff.bluemean","diff.greenmean","diff.redmean","diff.nirmean"))
  
longtable2<-
  melt(longtable1,id.variables=c("Lake","BufferSize","diff.bluestd","diff.greenstd","diff.redstd",
                                 "diff.nirstd"),
       variable.name=c("Band.Median"),
       value.name=c("diff.median"),
       measure.vars=c("diff.bluemedian","diff.greenmedian","diff.redmedian","diff.nirmedian"))

longtable3<-
  melt(longtable2,id.variables=c("Lake","BufferSize"),
       variable.name=c("Band.Std"),
       value.name=c("diff.std"),
       measure.vars=c("diff.bluestd","diff.greenstd","diff.redstd","diff.nirstd"),na.rm=TRUE)

## PLOT
#longtable3$BufferSize <- factor(longtable3$BufferSize, levels=c("3","30","60","90","120","150","180","210","240","270","300",
#                                                              "330","360","390","420","450","480","Whole"))

# Rename factor levels
levels(longtable3$Band.Mean)
levels(longtable3$Band.Mean)<-c("blue","green","red","nir")
levels(longtable3$Band.Median)
levels(longtable3$Band.Median)<-c("blue","green","red","nir")
levels(longtable3$Band.Std)
levels(longtable3$Band.Std)<-c("blue","green","red","nir")


means<-
  longtable3%>%
  ggplot(aes(BufferSize,diff.mean,color=Band.Mean))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=18),
        axis.title.y=element_text(size=18),
        legend.text=element_text(size=16),
        legend.title = element_blank())+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  labs(x="",y="Diff in means (center - sample point)")

medians<-
  longtable3%>%
  ggplot(aes(BufferSize,diff.median,color=Band.Median))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=18),
        axis.title.y=element_text(size=18),
        legend.text=element_text(size=16),
        legend.title = element_blank())+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  labs(x="",y="Diff in medians (center - sample point)")

stds<-
  longtable3%>%
  ggplot(aes(BufferSize,diff.std,color=Band.Std))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=18),
        axis.title.y=element_text(size=18),
        legend.text=element_text(size=16),
        legend.title = element_blank())+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  labs(x="",y="Diff in stdev (center - sample point)")

diffs<-arrangeGrob(means,medians,stds,ncol=1,nrow=3)
ggsave("diffs.png",diffs,width=15,height=18,dpi=200)

### Plot differences with All 15 lake subset #####
# Change column names in center of lake data table to make them distinctive for the join
small.c<-
  small.c %>% 
  rename(
    blue_stdDevC = blue_stdDev,
    blue_meanC = blue_mean,
    blue_medianC = blue_median,
    green_stdDevC = green_stdDev,
    green_meanC = green_mean,
    green_medianC = green_median,
    red_stdDevC = red_stdDev,
    red_meanC = red_mean,
    red_medianC = red_median,
    nir_stdDevC = nir_stdDev,
    nir_meanC = nir_mean,
    nir_medianC = nir_median    
  )

## Merge the new data tables
mergeCols3<-c("BufferSize","Lake")
newtable<-full_join(small.s,small.c,by=mergeCols3)

diffTable<-
  newtable%>%select(Lake,BufferSize,blue_mean,blue_meanC,blue_median,blue_medianC,blue_stdDev,blue_stdDevC,
                    green_stdDev,green_stdDevC,green_mean,green_meanC,green_median,green_medianC,
                    green_stdDev,green_stdDevC,red_mean,red_meanC,red_median,red_medianC,
                    red_stdDev,red_stdDevC,nir_mean,nir_meanC,nir_median,nir_medianC,
                    nir_stdDev,nir_stdDevC,)%>%
  mutate(diff.bluemean = (blue_meanC-blue_mean),
         diff.bluemedian = (blue_medianC-blue_median),
         diff.bluestd = (blue_stdDevC-blue_stdDev),
         diff.greenmean = (green_meanC-green_mean),
         diff.greenmedian = (green_medianC-green_median),
         diff.greenstd = (green_stdDevC-green_stdDev),
         diff.redmean = (red_meanC-red_mean),
         diff.redmedian = (red_medianC-red_median),
         diff.redstd = (red_stdDevC-red_stdDev),
         diff.nirmean = (nir_meanC-nir_mean),
         diff.nirmedian = (nir_medianC-nir_median),
         diff.nirstd = (nir_stdDevC-nir_stdDev)
  )

## Reshape into long form
longtable1<-
  melt(diffTable,id.variables=c("Lake","BufferSize","diff.bluemedian","diff.greenmedian","diff.redmedian",
                                "diff.nirmedian","diff.bluestd","diff.greenstd","diff.redstd","diff.nirstd"),
       variable.name=c("Band.Mean"),
       value.name=c("diff.mean"),
       measure.vars=c("diff.bluemean","diff.greenmean","diff.redmean","diff.nirmean"))

longtable2<-
  melt(longtable1,id.variables=c("Lake","BufferSize","diff.bluestd","diff.greenstd","diff.redstd",
                                 "diff.nirstd"),
       variable.name=c("Band.Median"),
       value.name=c("diff.median"),
       measure.vars=c("diff.bluemedian","diff.greenmedian","diff.redmedian","diff.nirmedian"))

longtable3<-
  melt(longtable2,id.variables=c("Lake","BufferSize"),
       variable.name=c("Band.Std"),
       value.name=c("diff.std"),
       measure.vars=c("diff.bluestd","diff.greenstd","diff.redstd","diff.nirstd"),na.rm=TRUE)

longtable3$BufferSize <- factor(longtable3$BufferSize, levels=c("3","30","60","90","120","150","180","210","240","270","300",
                                                                "330","360","390","420","450","480","Whole"))

# Rename factor levels
levels(longtable3$Band.Mean)
levels(longtable3$Band.Mean)<-c("blue","green","red","nir")
levels(longtable3$Band.Median)
levels(longtable3$Band.Median)<-c("blue","green","red","nir")
levels(longtable3$Band.Std)
levels(longtable3$Band.Std)<-c("blue","green","red","nir")

## PLOT
means<-
  longtable3%>%
  ggplot(aes(BufferSize,diff.mean,color=Band.Mean))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+geom_hline(yintercept=0)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=18),
        axis.title.y=element_text(size=20),
        legend.text=element_text(size=20),
        legend.position = "none",
        legend.title = element_blank(),
        plot.title = element_text(hjust=0.5,size=20))+
    scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  scale_y_continuous(lim=c(-400,400))+
  labs(x="",y="Difference in means \n(center - sample point)",title="All lakes")

medians<-
  longtable3%>%
  ggplot(aes(BufferSize,diff.median,color=Band.Median))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+geom_hline(yintercept=0)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=18),
        axis.title.y=element_text(size=20),
        legend.text=element_text(size=20),
        legend.position = "none",
        legend.title = element_blank())+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  scale_y_continuous(lim=c(-400,400))+
  labs(x="",y="Difference in medians \n(center - sample point)")

stds<-
  longtable3%>%
  ggplot(aes(BufferSize,diff.std,color=Band.Std))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+geom_hline(yintercept=0)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=18),
        axis.title.y=element_text(size=20),
        legend.text=element_text(size=20),
        legend.position = "none",
        legend.title = element_blank())+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  scale_y_continuous(lim=c(-400,400))+
  labs(x="",y="Difference in stdev \n(center - sample point)")

diffs.small<-arrangeGrob(means,medians,stds,ncol=1,nrow=3)
ggsave("diffs.small.png",diffs.small,width=10,height=18,dpi=200)

### Plot differences with Large lake subset #####
# Change column names in center of lake data table to make them distinctive for the join
large.c<-
  large.c %>% 
  rename(
    blue_stdDevC = blue_stdDev,
    blue_meanC = blue_mean,
    blue_medianC = blue_median,
    green_stdDevC = green_stdDev,
    green_meanC = green_mean,
    green_medianC = green_median,
    red_stdDevC = red_stdDev,
    red_meanC = red_mean,
    red_medianC = red_median,
    nir_stdDevC = nir_stdDev,
    nir_meanC = nir_mean,
    nir_medianC = nir_median    
  )

## Merge the new data tables
mergeCols3<-c("BufferSize","Lake")
newtable<-full_join(large.s,large.c,by=mergeCols3)

diffTable<-
  newtable%>%select(Lake,BufferSize,blue_mean,blue_meanC,blue_median,blue_medianC,blue_stdDev,blue_stdDevC,
                    green_stdDev,green_stdDevC,green_mean,green_meanC,green_median,green_medianC,
                    green_stdDev,green_stdDevC,red_mean,red_meanC,red_median,red_medianC,
                    red_stdDev,red_stdDevC,nir_mean,nir_meanC,nir_median,nir_medianC,
                    nir_stdDev,nir_stdDevC,)%>%
  mutate(diff.bluemean = (blue_meanC-blue_mean),
         diff.bluemedian = (blue_medianC-blue_median),
         diff.bluestd = (blue_stdDevC-blue_stdDev),
         diff.greenmean = (green_meanC-green_mean),
         diff.greenmedian = (green_medianC-green_median),
         diff.greenstd = (green_stdDevC-green_stdDev),
         diff.redmean = (red_meanC-red_mean),
         diff.redmedian = (red_medianC-red_median),
         diff.redstd = (red_stdDevC-red_stdDev),
         diff.nirmean = (nir_meanC-nir_mean),
         diff.nirmedian = (nir_medianC-nir_median),
         diff.nirstd = (nir_stdDevC-nir_stdDev)
  )

## Reshape into long form
longtable1<-
  melt(diffTable,id.variables=c("Lake","BufferSize","diff.bluemedian","diff.greenmedian","diff.redmedian",
                                "diff.nirmedian","diff.bluestd","diff.greenstd","diff.redstd","diff.nirstd"),
       variable.name=c("Band.Mean"),
       value.name=c("diff.mean"),
       measure.vars=c("diff.bluemean","diff.greenmean","diff.redmean","diff.nirmean"))

longtable2<-
  melt(longtable1,id.variables=c("Lake","BufferSize","diff.bluestd","diff.greenstd","diff.redstd",
                                 "diff.nirstd"),
       variable.name=c("Band.Median"),
       value.name=c("diff.median"),
       measure.vars=c("diff.bluemedian","diff.greenmedian","diff.redmedian","diff.nirmedian"))

longtable3<-
  melt(longtable2,id.variables=c("Lake","BufferSize"),
       variable.name=c("Band.Std"),
       value.name=c("diff.std"),
       measure.vars=c("diff.bluestd","diff.greenstd","diff.redstd","diff.nirstd"),na.rm=TRUE)
## PLOT
longtable3$BufferSize <- factor(longtable3$BufferSize, levels=c("3","30","60","90","120","150","180","210","240","270","300",
                                                                "330","360","390","420","450","480","Whole"))

# Rename factor levels
levels(longtable3$Band.Mean)
levels(longtable3$Band.Mean)<-c("blue","green","red","nir")
levels(longtable3$Band.Median)
levels(longtable3$Band.Median)<-c("blue","green","red","nir")
levels(longtable3$Band.Std)
levels(longtable3$Band.Std)<-c("blue","green","red","nir")


means<-
  longtable3%>%
  ggplot(aes(BufferSize,diff.mean,color=Band.Mean))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+geom_hline(yintercept=0)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=18),
        axis.title.y=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_blank(),
        plot.title = element_text(hjust=0.5,size=20))+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  scale_y_continuous(lim=c(-400,400))+
  labs(x="",y="",title="Large lakes")

medians<-
  longtable3%>%
  ggplot(aes(BufferSize,diff.median,color=Band.Median))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+geom_hline(yintercept=0)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=18),
        axis.title.y=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_blank())+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  scale_y_continuous(lim=c(-400,400))+
  labs(x="",y="")

stds<-
  longtable3%>%
  ggplot(aes(BufferSize,diff.std,color=Band.Std))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+geom_hline(yintercept=0)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=18),
        axis.title.y=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_blank())+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  scale_y_continuous(lim=c(-400,400))+
  labs(x="",y="")

diffs.large<-arrangeGrob(means,medians,stds,ncol=1,nrow=3)
ggsave("diffs.large.png",diffs.large,width=10,height=18,dpi=200)

######### PLOT ALL LAKE AND LARGE LAKE SETS TOGETHER ############
differences<-arrangeGrob(diffs.small,diffs.large,ncol=2,nrow=1)
ggsave("differences.png",differences,width=20,height=15,dpi=200)
### Plot differences with All lake subset (lake=color,shape=band) #######
# Change column names in center of lake data table to make them distinctive for the join

## Merge the new data tables
mergeCols3<-c("BufferSize","Lake")
newtable<-full_join(small.s,small.c,by=mergeCols3)

diffTable<-
  newtable%>%select(Lake,BufferSize,blue_mean,blue_meanC,blue_median,blue_medianC,blue_stdDev,blue_stdDevC,
                    green_stdDev,green_stdDevC,green_mean,green_meanC,green_median,green_medianC,
                    green_stdDev,green_stdDevC,red_mean,red_meanC,red_median,red_medianC,
                    red_stdDev,red_stdDevC,nir_mean,nir_meanC,nir_median,nir_medianC,
                    nir_stdDev,nir_stdDevC,)%>%
  mutate(diff.bluemean = (blue_meanC-blue_mean),
         diff.bluemedian = (blue_medianC-blue_median),
         diff.bluestd = (blue_stdDevC-blue_stdDev),
         diff.greenmean = (green_meanC-green_mean),
         diff.greenmedian = (green_medianC-green_median),
         diff.greenstd = (green_stdDevC-green_stdDev),
         diff.redmean = (red_meanC-red_mean),
         diff.redmedian = (red_medianC-red_median),
         diff.redstd = (red_stdDevC-red_stdDev),
         diff.nirmean = (nir_meanC-nir_mean),
         diff.nirmedian = (nir_medianC-nir_median),
         diff.nirstd = (nir_stdDevC-nir_stdDev)
  )

## Reshape into long form
longtable1<-
  melt(diffTable,id.variables=c("Lake","BufferSize","diff.bluemedian","diff.greenmedian","diff.redmedian",
                                "diff.nirmedian","diff.bluestd","diff.greenstd","diff.redstd","diff.nirstd"),
       variable.name=c("Band.Mean"),
       value.name=c("diff.mean"),
       measure.vars=c("diff.bluemean","diff.greenmean","diff.redmean","diff.nirmean"))

longtable2<-
  melt(longtable1,id.variables=c("Lake","BufferSize","diff.bluestd","diff.greenstd","diff.redstd",
                                 "diff.nirstd"),
       variable.name=c("Band.Median"),
       value.name=c("diff.median"),
       measure.vars=c("diff.bluemedian","diff.greenmedian","diff.redmedian","diff.nirmedian"))

longtable3<-
  melt(longtable2,id.variables=c("Lake","BufferSize"),
       variable.name=c("Band.Std"),
       value.name=c("diff.std"),
       measure.vars=c("diff.bluestd","diff.greenstd","diff.redstd","diff.nirstd"),na.rm=TRUE)

longtable3$BufferSize <- factor(longtable3$BufferSize, levels=c("3","30","60","90","120","150","180","210","240","270","300",
                                                                "330","360","390","420","450","480","Whole"))

# Rename factor levels
levels(longtable3$Band.Mean)
levels(longtable3$Band.Mean)<-c("blue","green","red","nir")
levels(longtable3$Band.Median)
levels(longtable3$Band.Median)<-c("blue","green","red","nir")
levels(longtable3$Band.Std)
levels(longtable3$Band.Std)<-c("blue","green","red","nir")

## PLOT
means2<- 
longtable3%>%
  ggplot(aes(BufferSize,diff.mean,color=Lake,shape=Band.Mean))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+geom_hline(yintercept=0)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=18),
        axis.title.y=element_text(size=20),
        legend.text=element_text(size=20),
        legend.position = "none",
        legend.title = element_blank(),
        plot.title = element_text(hjust=0.5,size=20))+
  scale_color_manual(values=c(mycolors))+
  scale_shape_manual(values=c(19,18,17,15))+
  scale_y_continuous(lim=c(-400,400))+
  labs(x="",y="Difference in means \n(center - sample point)",title="All lakes")

medians2<-
  longtable3%>%
  ggplot(aes(BufferSize,diff.median,color=Lake,shape=Band.Median))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+geom_hline(yintercept=0)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=18),
        axis.title.y=element_text(size=20),
        legend.text=element_text(size=20),
        legend.position = "none",
        legend.title = element_blank())+
  scale_color_manual(values=c(mycolors))+
  scale_shape_manual(values=c(19,18,17,15))+
  scale_y_continuous(lim=c(-400,400))+
  labs(x="",y="Difference in medians \n(center - sample point)")

stds2<-
  longtable3%>%
  ggplot(aes(BufferSize,diff.std,color=Lake,shape=Band.Std))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+geom_hline(yintercept=0)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=18),
        axis.title.y=element_text(size=20),
        legend.text=element_text(size=20),
        legend.position = "none",
        legend.title = element_blank())+
  scale_color_manual(values=c(mycolors))+
  scale_shape_manual(values=c(19,18,17,15))+
  scale_y_continuous(lim=c(-400,400))+
  labs(x="",y="Difference in stdev \n(center - sample point)")

diffs.small2<-arrangeGrob(means2,medians2,stds2,ncol=1,nrow=3)
ggsave("diffs.small2.png",diffs.small2,width=10,height=18,dpi=200)

## Save one with the LEGENDS
means.legend<- 
  longtable3%>%
  ggplot(aes(BufferSize,diff.mean,color=Lake,shape=Band.Mean))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+geom_hline(yintercept=0)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=18),
        axis.title.y=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_blank(),
        plot.title = element_text(hjust=0.5,size=20))+
  scale_color_manual(values=c(mycolors))+
  scale_shape_manual(values=c(19,18,17,15))+
  scale_y_continuous(lim=c(-400,400))+
  labs(x="",y="Difference in means \n(center - sample point)",title="All lakes")
ggsave("means.legend.png",means.legend,width=10,height=10,dpi=200)


### Plot differences with Large lake lsubset (lake=color,shape=band) #####
# Change column names in center of lake ldata table to make them distinctive for the join
## Merge the new data tables
mergeCols3<-c("BufferSize","Lake")
newtable<-full_join(large.s,large.c,by=mergeCols3)

diffTable<-
  newtable%>%select(Lake,BufferSize,blue_mean,blue_meanC,blue_median,blue_medianC,blue_stdDev,blue_stdDevC,
                    green_stdDev,green_stdDevC,green_mean,green_meanC,green_median,green_medianC,
                    green_stdDev,green_stdDevC,red_mean,red_meanC,red_median,red_medianC,
                    red_stdDev,red_stdDevC,nir_mean,nir_meanC,nir_median,nir_medianC,
                    nir_stdDev,nir_stdDevC,)%>%
  mutate(diff.bluemean = (blue_meanC-blue_mean),
         diff.bluemedian = (blue_medianC-blue_median),
         diff.bluestd = (blue_stdDevC-blue_stdDev),
         diff.greenmean = (green_meanC-green_mean),
         diff.greenmedian = (green_medianC-green_median),
         diff.greenstd = (green_stdDevC-green_stdDev),
         diff.redmean = (red_meanC-red_mean),
         diff.redmedian = (red_medianC-red_median),
         diff.redstd = (red_stdDevC-red_stdDev),
         diff.nirmean = (nir_meanC-nir_mean),
         diff.nirmedian = (nir_medianC-nir_median),
         diff.nirstd = (nir_stdDevC-nir_stdDev)
  )

## Reshape into long form
longtable1<-
  melt(diffTable,id.variables=c("Lake","BufferSize","diff.bluemedian","diff.greenmedian","diff.redmedian",
                                "diff.nirmedian","diff.bluestd","diff.greenstd","diff.redstd","diff.nirstd"),
       variable.name=c("Band.Mean"),
       value.name=c("diff.mean"),
       measure.vars=c("diff.bluemean","diff.greenmean","diff.redmean","diff.nirmean"))

longtable2<-
  melt(longtable1,id.variables=c("Lake","BufferSize","diff.bluestd","diff.greenstd","diff.redstd",
                                 "diff.nirstd"),
       variable.name=c("Band.Median"),
       value.name=c("diff.median"),
       measure.vars=c("diff.bluemedian","diff.greenmedian","diff.redmedian","diff.nirmedian"))

longtable3<-
  melt(longtable2,id.variables=c("Lake","BufferSize"),
       variable.name=c("Band.Std"),
       value.name=c("diff.std"),
       measure.vars=c("diff.bluestd","diff.greenstd","diff.redstd","diff.nirstd"),na.rm=TRUE)

longtable3$BufferSize <- factor(longtable3$BufferSize, levels=c("3","30","60","90","120","150","180","210","240","270","300",
                                                                "330","360","390","420","450","480","Whole"))

# Rename factor levels
levels(longtable3$Band.Mean)
levels(longtable3$Band.Mean)<-c("blue","green","red","nir")
levels(longtable3$Band.Median)
levels(longtable3$Band.Median)<-c("blue","green","red","nir")
levels(longtable3$Band.Std)
levels(longtable3$Band.Std)<-c("blue","green","red","nir")


## Make sure colors work
mycolors2=c("#4477AA","#228833","#EE6677","#CCBB44","#AA3377",
            "#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF")
mycolors=c("#332288","#88CCEE","#44AA99","#117733",
           "#999933","#DDCC77","#CC6677","#882255","#AA4499","#DDDDDD",
           "#CC3311","#777777","#EE8866","#EE3377","#000000","#EAECCC")
x<-1:16
x<-as.character(x)
y<-1:16
table<-data.table(cbind(x,y))
#table$x<-factor(table$x,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))
table%>%ggplot(aes(x,y,color=x))+geom_point(size=12)+scale_color_manual(values=c(mycolors))+theme_classic()

######## PLOT
means2<-
  longtable3%>%
  ggplot(aes(BufferSize,diff.mean,color=Lake,shape=Band.Mean))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+geom_hline(yintercept=0)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=18),
        axis.title.y=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_blank(),
        plot.title = element_text(hjust=0.5,size=20),
        legend.direction = "vertical")+
  scale_color_manual(values=c("#88CCEE","#AA4499","#EE8866"))+
  scale_shape_manual(values=c(19,18,17,15))+
  scale_y_continuous(lim=c(-400,400))+
  labs(x="",y="",title="Large lakes")+guides(color=F,shape=F)

medians2<-
  longtable3%>%
  ggplot(aes(BufferSize,diff.median,color=Lake,shape=Band.Median))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+geom_hline(yintercept=0)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=18),
        axis.title.y=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_blank())+
  scale_color_manual(values=c("#88CCEE","#AA4499","#EE8866"))+
  scale_shape_manual(values=c(19,18,17,15))+
  scale_y_continuous(lim=c(-400,400))+
  labs(x="",y="")+guides(color=F,shape=F)

stds2<-
  longtable3%>%
  ggplot(aes(BufferSize,diff.std,color=Lake,shape=Band.Std))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+geom_hline(yintercept=0)+
  theme(axis.text.x=element_text(angle=300,size=18,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=18),
        axis.title.y=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_blank())+
  scale_color_manual(values=c("#88CCEE","#AA4499","#EE8866"))+
  scale_shape_manual(values=c(19,18,17,15))+
  scale_y_continuous(lim=c(-400,400))+
  labs(x="",y="")+guides(color=F,shape=F)


diffs.large2<-arrangeGrob(means2,medians2,stds2,ncol=1,nrow=3)
ggsave("diffs.large2.png",diffs.large2,width=10,height=18,dpi=200)


######### PLOT ALL LAKE AND LARGE LAKE SETS TOGETHER (lake=color,shape=band) #####
differences2<-arrangeGrob(diffs.small2,diffs.large2,ncol=2,nrow=1)
ggsave("differences2.png",differences2,width=20,height=15,dpi=200)

### Plot Multibuffer summary stats - CENTER PT - w/ whole lake ######
## Reshape data
tdl1cw<-
  melt(buffersCW,id.variables=c("blue_count","green_count","red_count","swir1_count","swir2_count","nir_count",
                                "blue_median","green_median","red_median","swir1_median","swir2_median","nir_median",
                                "blue_stdDev","green_stdDev","red_stdDev","swir1_stdDev","swir2_stdDev","nir_stdDev",
                                "MaxDayDiff","BufferSize","datetime",
                                "Landsat","Lake","state"),
       variable.name=c("Band.Mean"),
       value.name=c("mean"),
       measure.vars=c("blue_mean","green_mean","red_mean","swir1_mean","swir2_mean","nir_mean"))

tdl2cw<-
  melt(tdl1cw,id.variables=c("Band.Mean","mean",
                             "blue_median","green_median","red_median","swir1_median","swir2_median","nir_median",
                             "blue_stdDev","green_stdDev","red_stdDev","swir1_stdDev","swir2_stdDev","nir_stdDev",
                             "MaxDayDiff","BufferSize","datetime",
                             "Landsat","Lake","state"),
       variable.name=c("Band.Count"),
       value.name=c("count"),
       measure.vars=c("blue_count","green_count","red_count","swir1_count","swir2_count","nir_count"))

tdl3cw<-
  melt(tdl2cw,id.variables=c("Band.Mean","mean","Band.Count","count",
                             "blue_stdDev","green_stdDev","red_stdDev","swir1_stdDev","swir2_stdDev","nir_stdDev",
                             "MaxDayDiff","BufferSize","datetime",
                             "Landsat","Lake","state"),
       variable.name=c("Band.Median"),
       value.name=c("median"),
       measure.vars=c( "blue_median","green_median","red_median","swir1_median","swir2_median","nir_median"))

tdlcw<-
  melt(tdl3cw,id.variables=c("Band.Mean","mean","Band.Count","count",
                             "Band Median","median",
                             "MaxDayDiff","BufferSize","datetime",
                             "Landsat","Lake","state"),
       variable.name=c("Band.std"),
       value.name=c("std"),
       measure.vars=c("blue_stdDev","green_stdDev","red_stdDev","swir1_stdDev","swir2_stdDev","nir_stdDev"),
       na.rm = TRUE)

## Make Time Window categorical
tdlcw$MaxDayDiff<-as.factor(tdlcw$MaxDayDiff)

## Rename factor levels
levels(tdlcw$Band.Mean)
levels(tdlcw$Band.Mean)<-c("blue","green","red","swir1","swir2","nir")

levels(tdlcw$Band.Median)
levels(tdlcw$Band.Median)<-c("blue","green","red","swir1","swir2","nir")

levels(tdlcw$Band.Count)
levels(tdlcw$Band.Count)<-c("blue","green","red","swir1","swir2","nir")

levels(tdlcw$Band.std)
levels(tdlcw$Band.std)<-c("blue","green","red","swir1","swir2","nir")


####### Plot
## Categorical
tdlcw$BufferSize<-as.factor(tdlcw$BufferSize)

## Order X axis
tdlcw$BufferSize <- factor(tdlcw$BufferSize, levels=c("3","30","60","90","120","150","180","210","240","270","300",
                                                      "330","360","390","420","450","480","510","540","570","600","630",
                                                      "660","690","720","750","780","810","840","870","900","930","960",
                                                      "990","1020","1050","1080","Whole"))

# Std
#StdCW<-
  ggplot(tdlcw,aes(BufferSize,std,color=Band.std,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+geom_line(color="grey")
  theme(axis.text.x=element_text(angle=300,size=12,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1500),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  labs(title="5 days, Center of lake",x="Buffer Size (m)")
a <- annotation_logticks(sides='l')
BuffCWStd<-StdCW+a
ggsave("BuffCWStd.png",BuffCWStd,height=6,width=12,dpi=100)

# Mean
BuffMeanCW<-
  ggplot(tdlcw,aes(BufferSize,mean,color=Band.Mean,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=12,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1500),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  labs(title="5 days, Center of lake",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
BuffMeanCW<-BuffMeanCW+a
ggsave("BuffMeanCW.png",BuffMeanCW,height=6,width=12,dpi=100)

# Median
BuffMedianCW<-
  ggplot(tdlcw,aes(BufferSize,median,color=Band.Median,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=12,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1500),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  labs(title="5 days, center of lake",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
BuffMedianCW<-BuffMedianCW+a
ggsave("BuffMedianCW.png",BuffMedianCW,height=6,width=12,dpi=100)

# Count
BuffCountCW<-
  ggplot(tdlcw,aes(BufferSize,count,color=Band.Count,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=12,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,200000),breaks=c(1,10,100,1000,10000,100000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  labs(title="5 days, center of lake",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
BuffCountCW<-BuffCountCW+a
ggsave("BuffCountCW.png",BuffCountCW,height=6,width=12,dpi=100)

## Plot together
BuffFigs_5dCW <- arrangeGrob(BuffCountCW,BuffCWStd,BuffMeanCW,BuffMedianCW,nrow=2, ncol=2)
ggsave("BuffFigs_5dCW.png",BuffFigs_5dCW,height=10,width=18,dpi=100)



########## Band by band STD ############
mycolors2=c("#332288","#88CCEE","#44AA99","#117733",
            "#999933","#DDCC77","#CC6677","#882255","#AA4499","#DDDDDD",
            "#CC3311","#777777","#EE8866","#EE3377","#000000","#EAECCC")

## Subset bands
blueCW<-filter(tdlcw,Band.std=="blue"&Band.Mean=="blue"&Band.Median=="blue"&Band.Count=="blue")
greenCW<-filter(tdlcw,Band.std=="green"&Band.Mean=="green"&Band.Median=="green"&Band.Count=="green")
redCW<-filter(tdlcw,Band.std=="red"&Band.Mean=="red"&Band.Median=="red"&Band.Count=="red")
swir1CW<-filter(tdlcw,Band.std=="swir1"&Band.Mean=="swir1"&Band.Median=="swir1"&Band.Count=="swir1")
swir2CW<-filter(tdlcw,Band.std=="swir2"&Band.Mean=="swir2"&Band.Median=="swir2"&Band.Count=="swir2")
nirCW<-filter(tdlcw,Band.std=="nir"&Band.Mean=="nir"&Band.Median=="nir"&Band.Count=="nir")


## Blue band
blueCWstd<-
  ggplot(blueCW,aes(BufferSize,std,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="blue std")
ggsave("blueCWstd.png",blueCWstd,height=6,width=12,dpi=100)

## Green band
greenCWstd<-
  ggplot(greenCW,aes(BufferSize,std,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="green std")
ggsave("greenCWstd.png",greenCWstd,height=6,width=12,dpi=100)

## Red band
redCWstd<-
  ggplot(redCW,aes(BufferSize,std,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="red std")
ggsave("redCWstd.png",redCWstd,height=6,width=12,dpi=100)

## swir1 band
swir1CWstd<-
  ggplot(swir1CW,aes(BufferSize,std,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="swir1 std")
ggsave("swir1CWstd.png",swir1CWstd,height=6,width=12,dpi=100)

## swir2 band
swir2CWstd<-
  ggplot(swir2CW,aes(BufferSize,std,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="swir2 std")
ggsave("swir2CWstd.png",swir2CWstd,height=6,width=12,dpi=100)

## nir band
nirCWstd<-
  ggplot(nirCW,aes(BufferSize,std,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="nir std")
ggsave("nirCWstd.png",nirCWstd,height=6,width=12,dpi=100)

##Arrange together
bands.stdCW<-arrangeGrob(blueCWstd,greenCWstd,redCWstd,swir1CWstd,swir2CWstd,nirCWstd,nrow=3,ncol=2)
ggsave("bands.stdCW.png",bands.stdCW,height=18,width=25,dpi=100)



########## Band by band MEAN ############
## Blue band
bluemeanCW<-
  ggplot(blueCW,aes(BufferSize,mean,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="blue mean")
ggsave("bluemeanCW.png",bluemeanCW,height=6,width=12,dpi=100)

## Green band
greenmeanCW<-
  ggplot(greenCW,aes(BufferSize,mean,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="green mean")
ggsave("greenmeanCW.png",greenmeanCW,height=6,width=12,dpi=100)

## Red band
redmeanCW<-
  ggplot(redCW,aes(BufferSize,mean,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="red mean")
ggsave("redmeanCW.png",redmeanCW,height=6,width=12,dpi=100)

## swir1 band
swir1meanCW<-
  ggplot(swir1CW,aes(BufferSize,mean,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="swir1 mean")
ggsave("swir1meanCW.png",swir1meanCW,height=6,width=12,dpi=100)

## swir2 band
swir2meanCW<-
  ggplot(swir2CW,aes(BufferSize,mean,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="swir2 mean")
ggsave("swir2meanCW.png",swir2meanCW,height=6,width=12,dpi=100)

## nir band
nirmeanCW<-
  ggplot(nirCW,aes(BufferSize,mean,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="nir mean")
ggsave("nirmeanCW.png",nirmeanCW,height=6,width=12,dpi=100)

##Arrange together
bands.meanCW<-arrangeGrob(bluemeanCW,greenmeanCW,redmeanCW,swir1meanCW,swir2meanCW,nirmeanCW,nrow=3,ncol=2)
ggsave("bands.meanCW.png",bands.meanCW,height=18,width=25,dpi=100)



########## Band by band MEDIAN ###########l#
## Blue band
bluemedianCW<-
  ggplot(blueCW,aes(BufferSize,median,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="blue median")
ggsave("bluemedianCW.png",bluemedianCW,height=6,width=12,dpi=100)

## Green band
greenmedianCW<-
  ggplot(greenCW,aes(BufferSize,median,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="green median")
ggsave("greenmedianCW.png",greenmedianCW,height=6,width=12,dpi=100)

## Red band
redmedianCW<-
  ggplot(redCW,aes(BufferSize,median,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="red median")
ggsave("redmedianCW.png",redmedianCW,height=6,width=12,dpi=100)

## swir1 band
swir1medianCW<-
  ggplot(swir1CW,aes(BufferSize,median,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="swir1 median")
ggsave("swir1medianCW.png",swir1medianCW,height=6,width=12,dpi=100)

## swir2 band
swir2medianCW<-
  ggplot(swir2CW,aes(BufferSize,median,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="swir2 median")
ggsave("swir2medianCW.png",swir2medianCW,height=6,width=12,dpi=100)

## nir band
nirmedianCW<-
  ggplot(nirCW,aes(BufferSize,median,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="nir median")
ggsave("nirmedianCW.png",nirmedianCW,height=6,width=12,dpi=100)

##Arrange together
bands.medianCW<-arrangeGrob(bluemedianCW,greenmedianCW,redmedianCW,swir1medianCW,swir2medianCW,nirmedianCW,nrow=3,ncol=2)
ggsave("bands.medianCW.png",bands.medianCW,height=18,width=25,dpi=100)

########## Band by band COUNT ############
## Blue band
bluecountCW<-
  ggplot(blueCW,aes(BufferSize,count,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,200000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="blue count")
ggsave("bluecountCW.png",bluecountCW,height=6,width=12,dpi=100)

## Green band
greencountCW<-
  ggplot(greenCW,aes(BufferSize,count,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,200000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="green count")
ggsave("greencountCW.png",greencountCW,height=6,width=12,dpi=100)

## Red band
redcountCW<-
  ggplot(redCW,aes(BufferSize,count,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,200000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="red count")
ggsave("redcountCW.png",redcountCW,height=6,width=12,dpi=100)

## swir1 band
swir1countCW<-
  ggplot(swir1CW,aes(BufferSize,count,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,200000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="swir1 count")
ggsave("swir1countCW.png",swir1countCW,height=6,width=12,dpi=100)

## swir2 band
swir2countCW<-
  ggplot(swir2CW,aes(BufferSize,count,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,200000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="swir2 count")
ggsave("swir2countCW.png",swir2countCW,height=6,width=12,dpi=100)

## nir band
nircountCW<-
  ggplot(nirCW,aes(BufferSize,count,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,200000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Center of lake",x="Buffer size (m)",y="nir count")
ggsave("nircountCW.png",nircountCW,height=6,width=12,dpi=100)

##Arrange together
bands.countCW<-arrangeGrob(bluecountCW,greencountCW,redcountCW,swir1countCW,swir2countCW,nircountCW,nrow=3,ncol=2)
ggsave("bands.countCW.png",bands.countCW,height=18,width=25,dpi=100)



### Plot Multibuffer summary stats - SAMPLE PT - w/ whole lake ######
## Reshape data
buffersW<-data.table(buffersW)
tdl1sw<-
  melt(buffersW,id.variables=c("blue_count","green_count","red_count","swir1_count","swir2_count","nir_count",
                               "blue_median","green_median","red_median","swir1_median","swir2_median","nir_median",
                               "blue_stdDev","green_stdDev","red_stdDev","swir1_stdDev","swir2_stdDev","nir_stdDev",
                               "MaxDayDiff","BufferSize","datetime",
                               "Landsat","Lake","state"),
       variable.name=c("Band.Mean"),
       value.name=c("mean"),
       measure.vars=c("blue_mean","green_mean","red_mean","swir1_mean","swir2_mean","nir_mean"))

tdl2sw<-
  melt(tdl1sw,id.variables=c("Band.Mean","mean",
                             "blue_median","green_median","red_median","swir1_median","swir2_median","nir_median",
                             "blue_stdDev","green_stdDev","red_stdDev","swir1_stdDev","swir2_stdDev","nir_stdDev",
                             "MaxDayDiff","BufferSize","datetime",
                             "Landsat","Lake","state"),
       variable.name=c("Band.Count"),
       value.name=c("count"),
       measure.vars=c("blue_count","green_count","red_count","swir1_count","swir2_count","nir_count"))

tdl3sw<-
  melt(tdl2sw,id.variables=c("Band.Mean","mean","Band.Count","count",
                             "blue_stdDev","green_stdDev","red_stdDev","swir1_stdDev","swir2_stdDev","nir_stdDev",
                             "MaxDayDiff","BufferSize","datetime",
                             "Landsat","Lake","state"),
       variable.name=c("Band.Median"),
       value.name=c("median"),
       measure.vars=c( "blue_median","green_median","red_median","swir1_median","swir2_median","nir_median"))

tdlsw<-
  melt(tdl3sw,id.variables=c("Band.Mean","mean","Band.Count","count",
                             "Band Median","median",
                             "MaxDayDiff","BufferSize","datetime",
                             "Landsat","Lake","state"),
       variable.name=c("Band.std"),
       value.name=c("std"),
       measure.vars=c("blue_stdDev","green_stdDev","red_stdDev","swir1_stdDev","swir2_stdDev","nir_stdDev"),
       na.rm = TRUE)

## Make Time Window categorical
tdlsw$MaxDayDiff<-as.factor(tdlsw$MaxDayDiff)

## Rename factor levels
levels(tdlsw$Band.Mean)
levels(tdlsw$Band.Mean)<-c("blue","green","red","swir1","swir2","nir")

levels(tdlsw$Band.Median)
levels(tdlsw$Band.Median)<-c("blue","green","red","swir1","swir2","nir")

levels(tdlsw$Band.Count)
levels(tdlsw$Band.Count)<-c("blue","green","red","swir1","swir2","nir")

levels(tdlsw$Band.std)
levels(tdlsw$Band.std)<-c("blue","green","red","swir1","swir2","nir")


####### Plot
## Categorical
tdlsw$BufferSize<-as.factor(tdlsw$BufferSize)

## Order X axis
tdlsw$BufferSize <- factor(tdlsw$BufferSize, levels=c("3","30","60","90","120","150","180","210","240","270","300",
                                                      "330","360","390","420","450","480","Whole"))

# Std
Stdsw<-
  ggplot(tdlsw,aes(BufferSize,std,color=Band.std,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=.3),na.rm=TRUE)+
  theme(axis.text.x=element_text(angle=300,size=14,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1000),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  labs(title="5 days, Sample point",x="Buffer Size (m)")
a <- annotation_logticks(sides='l')
BuffswStd<-Stdsw+a
ggsave("BuffswStd.png",BuffswStd,height=6,width=12,dpi=100)

# Mean
BuffMeansw<-
  ggplot(tdlsw,aes(BufferSize,mean,color=Band.Mean,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=14,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1500),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  labs(title="5 days, Sample point",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
BuffMeansw<-BuffMeansw+a
ggsave("BuffMeansw.png",BuffMeansw,height=6,width=12,dpi=100)

# Median
BuffMediansw<-
  ggplot(tdlsw,aes(BufferSize,median,color=Band.Median,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=14,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,1000),breaks=c(1,10,100,1000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  labs(title="5 days, Sample point",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
BuffMediansw<-BuffMediansw+a
ggsave("BuffMediansw.png",BuffMediansw,height=6,width=12,dpi=100)

# Count
BuffCountsw<-
  ggplot(tdlsw,aes(BufferSize,count,color=Band.Count,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=14,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,200000),breaks=c(1,10,100,1000,10000,100000))+
  scale_shape_manual(values=c(2,16))+
  scale_color_manual(values=c("#4477AA","#228833","#EE6677","#CCBB44",
                              "#AA3377","#BBBBBB","#EE8866","#FFAABB","#AAAA00","#99DDFF"))+
  labs(title="5 days, Sample point",x="Buffer size (m)")
a <- annotation_logticks(sides='l')
BuffCountsw<-BuffCountsw+a
ggsave("BuffCountsw.png",BuffCountsw,height=6,width=12,dpi=100)

## Plot together
BuffFigs_5dsw <- arrangeGrob(BuffCountsw,BuffswStd,BuffMeansw,BuffMediansw,nrow=2, ncol=2)
ggsave("BuffFigs_5dsw.png",BuffFigs_5dsw,height=10,width=15,dpi=100)


########## Band by band STD ############
mycolors2=c("#332288","#88CCEE","#44AA99","#117733",
            "#999933","#DDCC77","#CC6677","#882255","#AA4499","#DDDDDD",
            "#CC3311","#777777","#EE8866","#EE3377","#000000","#EAECCC")

## Subset bands
blueSW<-filter(tdlsw,Band.std=="blue"&Band.Mean=="blue"&Band.Median=="blue"&Band.Count=="blue")
greenSW<-filter(tdlsw,Band.std=="green"&Band.Mean=="green"&Band.Median=="green"&Band.Count=="green")
redSW<-filter(tdlsw,Band.std=="red"&Band.Mean=="red"&Band.Median=="red"&Band.Count=="red")
swir1SW<-filter(tdlsw,Band.std=="swir1"&Band.Mean=="swir1"&Band.Median=="swir1"&Band.Count=="swir1")
swir2SW<-filter(tdlsw,Band.std=="swir2"&Band.Mean=="swir2"&Band.Median=="swir2"&Band.Count=="swir2")
nirSW<-filter(tdlsw,Band.std=="nir"&Band.Mean=="nir"&Band.Median=="nir"&Band.Count=="nir")


## Blue band
blueSWstd<-
  ggplot(blueSW,aes(BufferSize,std,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="blue std")
ggsave("blueSWstd.png",blueSWstd,height=6,width=12,dpi=100)

## Green band
greenSWstd<-
  ggplot(greenSW,aes(BufferSize,std,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="green std")
ggsave("greenSWstd.png",greenSWstd,height=6,width=12,dpi=100)

## Red band
redSWstd<-
  ggplot(redSW,aes(BufferSize,std,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="red std")
ggsave("redSWstd.png",redSWstd,height=6,width=12,dpi=100)

## swir1 band
swir1SWstd<-
  ggplot(swir1SW,aes(BufferSize,std,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="swir1 std")
ggsave("swir1SWstd.png",swir1SWstd,height=6,width=12,dpi=100)

## swir2 band
swir2SWstd<-
  ggplot(swir2SW,aes(BufferSize,std,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="swir2 std")
ggsave("swir2SWstd.png",swir2SWstd,height=6,width=12,dpi=100)

## nir band
nirSWstd<-
  ggplot(nirSW,aes(BufferSize,std,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=4,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="nir std")
ggsave("nirSWstd.png",nirSWstd,height=6,width=12,dpi=100)

##Arrange together
bands.stdSW<-arrangeGrob(blueSWstd,greenSWstd,redSWstd,swir1SWstd,swir2SWstd,nirSWstd,nrow=3,ncol=2)
ggsave("bands.stdSW.png",bands.stdSW,height=18,width=25,dpi=100)



########## Band by band MEAN ############
## Blue band
bluemeanSW<-
  ggplot(blueSW,aes(BufferSize,mean,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="blue mean")
ggsave("bluemeanSW.png",bluemeanSW,height=6,width=12,dpi=100)

## Green band
greenmeanSW<-
  ggplot(greenSW,aes(BufferSize,mean,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="green mean")
ggsave("greenmeanSW.png",greenmeanSW,height=6,width=12,dpi=100)

## Red band
redmeanSW<-
  ggplot(redSW,aes(BufferSize,mean,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="red mean")
ggsave("redmeanSW.png",redmeanSW,height=6,width=12,dpi=100)

## swir1 band
swir1meanSW<-
  ggplot(swir1SW,aes(BufferSize,mean,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="swir1 mean")
ggsave("swir1meanSW.png",swir1meanSW,height=6,width=12,dpi=100)

## swir2 band
swir2meanSW<-
  ggplot(swir2SW,aes(BufferSize,mean,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="swir2 mean")
ggsave("swir2meanSW.png",swir2meanSW,height=6,width=12,dpi=100)

## nir band
nirmeanSW<-
  ggplot(nirSW,aes(BufferSize,mean,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="nir mean")
ggsave("nirmeanSW.png",nirmeanSW,height=6,width=12,dpi=100)

##Arrange together
bands.meanSW<-arrangeGrob(bluemeanSW,greenmeanSW,redmeanSW,swir1meanSW,swir2meanSW,nirmeanSW,nrow=3,ncol=2)
ggsave("bands.meanSW.png",bands.meanSW,height=18,width=25,dpi=100)



########## Band by band MEDIAN ############
## Blue band
bluemedianSW<-
  ggplot(blueSW,aes(BufferSize,median,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="blue median")
ggsave("bluemedianSW.png",bluemedianSW,height=6,width=12,dpi=100)

## Green band
greenmedianSW<-
  ggplot(greenSW,aes(BufferSize,median,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="green median")
ggsave("greenmedianSW.png",greenmedianSW,height=6,width=12,dpi=100)

## Red band
redmedianSW<-
  ggplot(redSW,aes(BufferSize,median,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="red median")
ggsave("redmedianSW.png",redmedianSW,height=6,width=12,dpi=100)

## swir1 band
swir1medianSW<-
  ggplot(swir1SW,aes(BufferSize,median,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="swir1 median")
ggsave("swir1medianSW.png",swir1medianSW,height=6,width=12,dpi=100)

## swir2 band
swir2medianSW<-
  ggplot(swir2SW,aes(BufferSize,median,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="swir2 median")
ggsave("swir2medianSW.png",swir2medianSW,height=6,width=12,dpi=100)

## nir band
nirmedianSW<-
  ggplot(nirSW,aes(BufferSize,median,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_continuous(labels=comma,lim=c(0,1000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="nir median")
ggsave("nirmedianSW.png",nirmedianSW,height=6,width=12,dpi=100)

##Arrange together
bands.medianSW<-arrangeGrob(bluemedianSW,greenmedianSW,redmedianSW,swir1medianSW,swir2medianSW,nirmedianSW,nrow=3,ncol=2)
ggsave("bands.medianSW.png",bands.medianSW,height=18,width=25,dpi=100)

########## Band by band COUNT ############
## Blue band
bluecountSW<-
  ggplot(blueSW,aes(BufferSize,count,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,200000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="blue count")
ggsave("bluecountSW.png",bluecountSW,height=6,width=12,dpi=100)

## Green band
greencountSW<-
  ggplot(greenSW,aes(BufferSize,count,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,200000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="green count")
ggsave("greencountSW.png",greencountSW,height=6,width=12,dpi=100)

## Red band
redcountSW<-
  ggplot(redSW,aes(BufferSize,count,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,200000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="red count")
ggsave("redcountSW.png",redcountSW,height=6,width=12,dpi=100)

## swir1 band
swir1countSW<-
  ggplot(swir1SW,aes(BufferSize,count,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,200000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="swir1 count")
ggsave("swir1countSW.png",swir1countSW,height=6,width=12,dpi=100)

## swir2 band
swir2countSW<-
  ggplot(swir2SW,aes(BufferSize,count,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,200000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="swir2 count")
ggsave("swir2countSW.png",swir2countSW,height=6,width=12,dpi=100)

## nir band
nircountSW<-
  ggplot(nirSW,aes(BufferSize,count,color=Lake,shape=Landsat))+theme_classic()+
  geom_point(size=3,position=position_dodge(width=0.3))+
  theme(axis.text.x=element_text(size=16,angle=300,hjust=0),
        axis.title.x = element_text(size=18),
        axis.text.y=element_text(size=16),
        axis.title.y=element_text(size=18))+
  scale_y_log10(labels=comma,lim=c(1,200000))+
  #scale_x_continuous(lim=c(0,500))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=mycolors)+
  labs(title="5 days, Sample point",x="Buffer size (m)",y="nir count")
ggsave("nircountSW.png",nircountSW,height=6,width=12,dpi=100)

##Arrange together
bands.countSW<-arrangeGrob(bluecountSW,greencountSW,redcountSW,swir1countSW,swir2countSW,nircountSW,nrow=3,ncol=2)
ggsave("bands.countSW.png",bands.countSW,height=18,width=25,dpi=100)

