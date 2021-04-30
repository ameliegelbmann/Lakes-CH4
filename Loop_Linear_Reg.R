
# libraries ---------------------------------------------------------------

library(ggplot2)
library(gridExtra)



# Read Excel --------------------------------------------------------------




LoopyDF <- readxl::read_xlsx(path = "Classeur11.xlsx")

# exes <- LoopyDF[["xtext"]]
# for (x in exes) {
#   print(eval(parse(text=x)))
# }
# 



# I. Loop -----------------------------------------------------------------

# for testing Loop
rownum=17

#for (rownum in 1:nrow(LoopyDF)) {
# for (rownum in 14:19) {

# Inside the Loop ----
  
eachrow=LoopyDF[rownum,]
  
# colnames(LoopyDF)

Name=eachrow[["Name"]]
Filename=eachrow[["Filename"]]

xcol=eachrow[["x"]]
logx=eachrow[["logx"]]
xtext=parse(text=eachrow[["xtext"]])

ycol=eachrow[["y"]]
ytext=parse(text=eachrow[["ytext"]])
logy=eachrow[["logy"]]

atable<-read.table(Filename,header = TRUE) ## 

if (logx){
  # atable[xcol]<-log10(atable[xcol]+1) # ADD +1 to TP
  atable[xcol]<-atable[xcol]+1 # ADD +1 to TP
}

# if (logy){
#   atable[ycol]<-log10(atable[ycol]) 
# }

meansd1<-aggregate(.~factor(lake),atable,function(x) c(mean = mean(x),sd = sd(x))) #x = lake 
colnames(meansd1)[1] <- "lake1"

xmean=paste0(xcol,"[,1]")
xsd=paste0(xcol,"[,2]")
xuppererror=paste0(xmean, "+", xsd)
xlowererror=paste0(xmean, "-", xsd)

ymean=paste0(ycol,"[,1]")
ysd=paste0(ycol,"[,2]")
yuppererror=paste0(ymean, "+", ysd)
ylowererror=paste0(ymean, "-", ysd)
  
factorlake1string="factor(lake1)"

logsumry=summary(lm(atable[[ycol]] ~ atable[[xcol]]))
r2label=paste0("R2 = ",round(logsumry$adj.r.squared,3))
pvallabel=paste0("P = ",round(logsumry$coefficients[2,4], 5))
gglabel=paste0(r2label, ", ", pvallabel)

logsumryavg=summary(lm(meansd1[[ycol]][,1] ~ meansd1[[xcol]][,1]))
r2labelavg=paste0("R2 = ",round(logsumryavg$adj.r.squared,3))
pvallabelavg=paste0("P = ",round(logsumryavg$coefficients[2,4], 5))
ggavlabel=paste0(r2labelavg, ", ", pvallabelavg)


# I.2 AVERAGE ----

ggAv <- ggplot(meansd1, aes_string(xmean, ymean)) +
  geom_point(
    aes_string(color=factorlake1string), size=3
    )

if (logy){
  ggAv <- ggAv +
    scale_y_log10()
}

if (logx){
  ggAv <- ggAv +
    scale_x_log10()
}

ggAv <- ggAv +
  # geom_text(aes_string(x=r2labelpos[1], y=r2labelpos[2]), label=r2label) +
  labs(subtitle = ggavlabel) +
  # scale_shape_manual(values = c(15, 16, 17)) + # filled square, circle and triangle
  theme_bw() +
  #geom_smooth(method = 'lm', se=F) + #add ', se = FALSE' to get rid of confidence interval shading
  ggtitle("")+
  xlab(eval(xtext)) +
  ylab(eval(ytext)) +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank()) #title in the middle

errorbarheads <- c(
  diff(layer_scales(ggAv)$x$range$range)*0.02,
  diff(layer_scales(ggAv)$y$range$range)*0.02
)

ggAv <- ggAv +
  geom_errorbar(
    aes_string(ymin=ylowererror, ymax=yuppererror,color=factorlake1string), width=errorbarheads[1],
    position=position_dodge(.9)
  ) +  # minimal y value (bottom of the errorbar)
  geom_errorbarh(
    aes_string(xmin=xlowererror, xmax=xuppererror, color=factorlake1string, height=errorbarheads[2]),
    position=position_dodge(.9)
  ) # minimal y value (bottom of the errorbar)


# errorbarheads=c(
#   # changes width of errorbar heads parallel to x axis
#   paste0(resizeF,"*0.02*(", paste0("max(",ymean, ", na.rm=T)"), " - ", paste0("min(",ymean, ", na.rm=T)"), ")"),
#   # changes width of errorbar heads parallel to y axis
#   paste0(1,"*0.02*(", paste0("max(",xmean, ", na.rm=T)"), " - ", paste0("min(",xmean, ", na.rm=T)"), ")")
# )


# I.1 PAS AVERAGE ----

ggPas <- ggplot(atable, aes_string(xcol, ycol)) +
  geom_point(aes(color=factor(lake), shape=factor(date, levels=c("D1", "D2", "D3"))), size=3)

if (logy){
  ggPas <- ggPas +
    scale_y_log10()
}

if (logx){
  ggPas <- ggPas +
    scale_x_log10()  #lTP<-log10(a$TP+1) a voir
}
ggPas <- ggPas +
  # geom_text(aes_string(x=r2labelpos2[1], y=r2labelpos2[2]),label=r2label) +
  labs(subtitle = gglabel) +
  scale_shape_manual(values = c(15, 16, 17)) + # filled square, circle and triangle
  theme_bw() +
  #geom_smooth(method = 'lm', se=F)+ #add ', se = FALSE' to get rid of confidence interval shading
  ggtitle("")+
  xlab(eval(xtext))+
  ylab(eval(ytext))+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank()) #title in the middle



# II. Finished ------------------------------------------------------------

gridExtra::grid.arrange(ggAv, ggPas, ncol=2) #, bottom=paste0(r2label, ", ", pvallabel))

}

######### Chart correlation matrix ############ ----

#########Biological and chemical (water quality) Epilimnion
a<-read.table("R_bio_chem_27.04_epi.txt",header = TRUE)
summary(a)
attach(a)

#1. Log function apply on TP and CH4 and CH4S
#lDN<-log10(a$NO3+1)
lTP<-log10(a$TP+1)
lCH4e<-log10(a$CH4e)
#hist(log10(a$CH4o))
lCH4o<-log10(a$CH4o)
lCH4F<-log10(a$CH4F)

b<-cbind(a,lTP,lCH4e,lCH4o,lCH4F) #add lTPH, lCH4
c<-subset(b,select=-c(3,14,15,16)) #drop column

c <- c[, c(1,2,13,3,4,5,6,7,8,9,10,11,12,14,12,15,16)]

d<-subset(c,select=-c(15))

#2. chart corr without qualitative variables 
library(PerformanceAnalytics)
install.packages("PerformanceAnalytics")
chart.Correlation(d[,-c(1:2)],pch=20,histogram=F)

###########Physical variables
a<-read.table("PhysicalData_24April2020.txt",header = TRUE)
summary(a)
attach(a)

#1. Log function apply on TP and CH4 and CH4S
lCH4e<-log10(a$CH4e)
lCH4h<-log10(a$CH4h)
lCH4F<-log10(a$CH4F)
lCH4o<-log10(a$CH4o)

b<-cbind(a,lCH4h,lCH4e,lCH4o,lCH4F) #add lTPH, lCH4
c<-subset(b,select=-c(8:11)) #drop column
d<-subset(c,select=-c(3,5))

#2. chart corr without qualitative variables and TP (use of ITP instead)
library(PerformanceAnalytics)
install.packages("PerformanceAnalytics")
chart.Correlation(c[,-c(1:2)],pch=20,histogram=F)
chart.Correlation(d[,-c(1:2)],pch=20,histogram=F)

#########Biological and chemical (water quality) Hypolimnion

a<-read.table("R_bio_chem_27.04_epi.txt",header = TRUE)
summary(a)
attach(a)

#1. Log function apply on TP and CH4 and CH4S

lTP<-log10(a$TP+1)
lCH4e<-log10(a$CH4e)
hist(log10(a$CH4o))
lCH4o<-log10(a$CH4o)
lCH4F<-log10(a$CH4F)

b<-cbind(a,lTP,lCH4e,lCH4o,lCH4F) #add lTPH, lCH4

c<-subset(b,select=-c(3,14,15,16)) #drop column

c <- c[, c(1,2,13,3,4,5,6,7,8,9,10,11,12,14,12,15,16)]

#2. chart corr without qualitative variables and TP (use of ITP instead)
library(PerformanceAnalytics)
install.packages("PerformanceAnalytics")
chart.Correlation(c[,-c(1:2,8:13,15)],pch=20,histogram=F)


##### Kd and Alegae ####
DF<-read.table("Kd_alg.txt",header = TRUE)
attach(DF)
lm1<-lm(Kd~AlgaeE)
summary(lm1)

cor.test(Kd,AlgaeE)  # cor 0.13 p value 0.6

lm2<-lm(Kd~AlgaeH)
summary(lm2)
cor.test(Kd,AlgaeH) #

DFhypo<-read.table("R_bio_chem_27.04_hypo.txt",header = TRUE)
attach(DFhypo)
lTP<-log10(TP+1)
lCH4h<-log10(CH4h)
hist(log10(CH4o))
lCH4o<-log10(CH4o)
lCH4F<-log10(CH4F)
DFhypo<-cbind(DFhypo,lTP,lCH4h,lCH4o,lCH4F) #

lm2<-lm(Plank~lCH4h)
summary(lm2)

lm3<-lm(Plank~lTP)
summary(lm3)

lm4<-lm(Plank~NO3)
summary(lm4)
