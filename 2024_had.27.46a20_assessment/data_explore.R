##

## plot up input data

fn.prefix <- paste0("had.27.46a.20 - WGNSSK ",ay)
col.pal <- c(brewer.pal(n = 8, name = "Dark2"),brewer.pal(n=6,name="Set2")[6],brewer.pal(n=6,name="Accent"))
col.pal9 <- c(brewer.pal(n = 8, name = "Dark2"),brewer.pal(n=6,name="Set2")[4])


# load stock data
load("data/stockData.RData")

#-----------------------------------------------------------##
# Maturity ####

# plot number of samples per year - dataG
dataG <- read.taf(paste0("data/Maturity/dataG_individual_sexes_no_sexratio.csv"))
tmp <- tapply(dataG$nb.x,list(dataG$year,dataG$sex),sum)

png(paste0(output.dir,fn.prefix," - Maturity ogive - number of samples.png"),width = 10, height = 7, units = "in", res = 600)
plot(as.numeric(rownames(tmp)),tmp[,"F"], xlab="",ylab="Number of samples",type="o",pch=16)
dev.off()


# plot raw vs smoothed ogives
mo_raw <- read.csv("data/Maturity/raw_matogive_NShelf_individual_sexes_no_sexratio_Female.csv")
mo_raw <- FLQuant(t(mo_raw[,-1]),dimnames=list(age=1:8,year=1991:ay))
mo_smo <- stock.data@mat

mo_raw <- expand(mo_raw, age=0:8)
mo_smo <- trim(mo_smo, age=0:8)

png(paste0(output.dir,fn.prefix," - Maturity ogive - raw vs smoothed.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=FLQuants(raw=mo_raw,smoothed=mo_smo), aes(year, data)) + 
  geom_line(aes(colour=factor(age),linetype=as.factor(qname))) + theme_bw()+ labs(y="Proportion mature",colour="Age",x="",linetype="") +
  scale_colour_manual(values=col.pal9)+
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)
dev.off()

# plot old and new ogives
mo_old <- readVPAFile("boot/data/FLR files - WGNSSK 2023/nor_had_mo.txt")
mo_new <- read.csv("data/Maturity/had.27.46a20 - Maturity ogive.csv")
names(mo_new) <- c("Year",0:8)

mo_new <- FLQuant(t(mo_new[,-1]),dimnames=list(age=0:8,year=1965:ay))

# adjust final year value
mo_new[,ac(ay)] <- mo_new[,ac(ay-1)]
mo_old <- window(trim(mo_old, age=0:8),start=1972)
mo_new <- window(trim(mo_new, age=0:8),start=1972)

png(paste0(output.dir,fn.prefix," - Maturity ogive - old vs new.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=FLQuants(WGNSSK_2024=mo_new,WGNSSK_2023=mo_old), aes(year, data)) + 
  geom_line(aes(colour=factor(age),linetype=as.factor(qname))) + theme_bw()+ labs(y="Proportion mature",colour="Age",x="",linetype="") +
  scale_colour_manual(values=col.pal9)+
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)
dev.off()

# plot new ogives
mo_new <- read.csv("data/Maturity/had.27.46a20 - Maturity ogive.csv")
names(mo_new) <- c("Year",0:8)
mo_new <- FLQuant(t(mo_new[,-1]),dimnames=list(age=0:8,year=1965:ay))
mo_new <- window(trim(mo_new, age=0:8),start=1972)
mo_new[,ac(ay)] <- mo_new[,ac(ay-1)]

png(paste0(output.dir,fn.prefix," - Maturity ogive.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=FLQuants(WGNSSK_2024=mo_new), aes(year, data)) + 
  geom_line(aes(colour=factor(age))) + theme_bw()+ labs(y="Proportion mature",colour="Age",x="") +
  scale_colour_manual(values=col.pal9)+
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)
dev.off()


# plot retro results
# create table
ret <- matrix(NA,nrow=length(1991:ay),ncol=6,dimnames=list(1991:ay,(ay-5):ay))

# read in files
for (fyr in (ay-5):(ay)){
  if(fyr ==ay){
    tmp <- read.taf(paste0("data/Maturity/Smoothed_mat_ogive_plusg8_NShelf_individual_sexes_no_sexratio_Female.csv"))
  }else{
    tmp <- read.taf(paste0("data/Maturity/Retro_Smoothed_mat_ogive_plusg8_NShelf_individual_sexes_no_sexratio_Female_",fyr,".csv"))
  }
  
  for (ag in 1:5){
    if(fyr == ay-5){
      assign("dat",ret)
      dat[as.character(1991:fyr),as.character(fyr)] <- tmp[,paste0("Age",as.character(ag))]
      assign(paste0("ret",ag),dat)
    }else{
      assign("dat",get(paste0("ret",ag)))
      dat[as.character(1991:fyr),as.character(fyr)] <- tmp[,paste0("Age",as.character(ag))]
      assign(paste0("ret",ag),dat)
    }
  }
}

# calc mohn's rho
rhos <- numeric(length(1:5))
for (ag in 1:5) {
  assign("dta",get(paste0("ret",ag)))
  windows()
  rhos[ag] <- icesAdvice::mohn(dta,peels=5)
  
  # plot
  ylims <- range(dta,na.rm=T)
  ylims[1] <- ylims[1]*0.9
  ylims[2] <- ylims[2]*1.1
  icesAdvice::mohn(dta, peels = 5, details = FALSE, plot = TRUE,ylim=ylims)
  title(main=paste0("Age ",ag,": Mohn's rho = ",round(rhos[ag],3)))
  savePlot(filename = paste0(output.dir,fn.prefix," - Maturity ogive -  Mohn's rho - age ",ag,".png"),type = "png")
}
graphics.off()

#-----------------------------------------------------------##
# natural mortality ####

 #plot NM
nm <- readVPAFile(paste0(output.flr,"nor_had_nm.txt"))
nm <- trim(nm, age=0:8)

png(paste0(output.dir,fn.prefix," - Natural mortality.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=nm, aes(year, data)) + 
  geom_line(aes(colour=factor(age))) + theme_bw()+ labs(y="Proportion mature",colour="Age",x="") +
  scale_colour_manual(values=col.pal9)+
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)

dev.off()  

# plot new NM compared to old NM
nm_new <- window(nm,start="1972")
nm_old <- readVPAFile("boot/data/FLR files - WGNSSK 2023/nor_had_nm.txt")
nm_old <- window(trim(nm_old, age=0:8),start="1972")

png(paste0(output.dir,fn.prefix," - Natural mortality - old vs new - scaled.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=FLQuants(WGNSSK_2024=nm_new,WGNSSK_2023 = nm_old), aes(x=year, y=data)) + 
  geom_line(aes(colour=as.factor(age),linetype=as.factor(qname))) + theme_bw()+ labs(y="M",colour="Age",x="",linetype="") +
  facet_wrap(~age,nrow=3,scales="free_y")+
  scale_colour_manual(values=col.pal9)+
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)
dev.off()

png(paste0(output.dir,fn.prefix," - Natural mortality - old vs new.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=FLQuants(WGNSSK_2024=nm_new,WGNSSK_2023 = nm_old), aes(x=year, y=data)) + 
  geom_line(aes(colour=as.factor(age),linetype=as.factor(qname))) + theme_bw()+ labs(y="M",colour="Age",x="",linetype="") +
  facet_wrap(~age,nrow=3)+
  scale_colour_manual(values=col.pal9)+
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9))  
print(p1)
dev.off()

#-----------------------------------------------------------##
# stock weights ####
# plot sw vs catch weights
# add smoother line to stock weights
png(paste0(output.dir,fn.prefix," - Stock weights.png"),width = 11, height = 7, units = "in", res = 600)

datFL <- FLQuants(stock=stock.data.pg@stock.wt)

p1 <- ggplot(data=datFL, aes(year, data)) + xlim(c(1972,(ay-1)))+
  geom_point(aes(group=age,colour=factor(age))) + geom_smooth(span=1,se=F,aes(group=age,colour=factor(age)))+
  facet_wrap(~qname)+
  theme_bw()+ labs(y="Mean weight-at-age (kg)",colour="",x="") +
  scale_colour_manual(values=col.pal9)+
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)
dev.off()

#-----------------------------------------------------------##
# Catch data ####

# yield
stock.data@catch <- computeCatch(stock.data)
stock.data@landings <- computeLandings(stock.data)
stock.data@discards <- computeDiscards(stock.data)

png(paste0(output.dir,fn.prefix," - Yield.png"),width = 11, height = 7, units = "in", res = 600)

datFL <- FLQuants(catch=stock.data@catch, landings=stock.data@landings, discards=stock.data@discards,
                  BMS=quantSums(bmsn*bmsw),IBC=quantSums(ibcn*ibcw))

p1 <- ggplot(data=datFL, aes(year, data)) + xlim(c(1972,(ay-1)))+ylim(c(0,max(window(stock.data,start=1972)@catch,na.rm=T)))+
  geom_line(aes(group=qname,colour=factor(qname)))+ theme_bw()+ labs(y="Yield (tonnes)",colour="",x="") +
  scale_colour_manual(values=col.pal)+
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)
dev.off()


# trends in mean weights by catch component
png(paste0(output.dir,fn.prefix," - Mean weight at age.png"),width = 12, height = 8, units = "in", res = 600)

datFL <- FLQuants(catch=stock.data.pg@catch.wt, landings=stock.data.pg@landings.wt, discards=stock.data.pg@discards.wt,
                  BMS=bmsw.pg,IBC=ibcw.pg)
datFL[["discards"]][datFL$discards ==0] <- NA
datFL[["BMS"]][bmsw.pg ==0] <- NA
datFL[["IBC"]][ibcw.pg ==0] <- NA
datFL[["landings"]]["0",] <- NA

p1 <- ggplot(data=datFL, aes(year, data)) + xlim(c(1972,(ay-1)))+
  geom_point(aes(group=age,colour=factor(age)),size=0.7) + geom_smooth(span=1,se=F,aes(group=age,colour=factor(age)),linewidth=0.5)+
  facet_wrap(~qname,nrow=3)+
  theme_bw()+ labs(y="Mean weight-at-age (kg)",colour="",x="") +
  scale_colour_manual(values=col.pal9)+
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)
dev.off()

# proportion discarded
dis.prop <- stock.data.pg@discards.n/stock.data.pg@catch.n
datFL <- FLQuants(dat = dis.prop)

png(paste0(output.dir,fn.prefix," - Proportion discarded at age.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=datFL, aes(year, data)) + xlim(c(1972,(ay-1)))+
  geom_line() + 
  facet_wrap(~age)+
  theme_bw()+ labs(y="Proportion discarded",x="") +
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)
dev.off()

# catch proportion 
cat.prop <- stock.data.pg@discards.n %/% quantSums(stock.data.pg@catch.n)
datFL <- FLQuants(dat = cat.prop)

png(paste0(output.dir,fn.prefix," - Proportion caught at age.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=datFL, aes(year, data)) + xlim(c(1972,(ay-1)))+
  geom_line() + 
  facet_wrap(~age)+
  theme_bw()+ labs(y="Proportion caught",x="") +
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)
dev.off()

# commerical catch curves
windows(width = 10, height = 7)
par(mfrow = c(1,1), mar=c(3,4,3,3))
plot.catch.curve.and.grads(stock.data.pg, wk.ptype = "c", wk.ages = 2:4, 
                           wk.main = "", wk.yrs = 1980:(ay-1))
mtext("Northern Shelf haddock (had.27.46a20). Log commercial CPUE", side = 3, 
      line = -1.5, cex = 0.75, outer = TRUE)
savePlot(filename = paste0(output.dir,fn.prefix," - Commercial catch curves.png"),type = "png")

# Commercial catch curve gradients
windows(width = 10, height = 7)
par(mfrow = c(1,1), mar = c(3,4,3,3))
plot.catch.curve.and.grads(stock.data.pg, wk.ptype = "g", wk.ages = 2:4, 
                           wk.main = "", wk.yrs = 1980:(ay-1))
mtext("Northern Shelf haddock (had.27.46a20). Commercial catch curve gradients", side = 3, line = -1, cex = 1.0, outer = TRUE)
savePlot(filename = paste0(output.dir,fn.prefix," - Commercial catch curve gradients.png"),type = "png")

# Commercial catch correlations
windows(width = 10, height = 7)
plot.index.corr(list(FLIndex(catch.n = stock.data@catch.n, name = "Northern Shelf haddock (had.27.46a20). Commercial catch correlations")), 
                wk.type = "FLR",wk.name="Commerical catch")
savePlot(filename = paste0(output.dir,fn.prefix," - Commercial catch correlations.png"),type = "png")

windows(width = 10, height = 7)
plot.index.corr(list(FLIndex(catch.n = stock.data.pg@catch.n, name = "Northern Shelf haddock (had.27.46a20). Commercial catch correlations")), 
                wk.type = "FLR",wk.name="Commerical catch")
savePlot(filename = paste0(output.dir,fn.prefix," - Commercial catch correlations pg.png"),type = "tiff")

graphics.off()

#-----------------------------------------------------------##
# Survey indices ####

load("data/indices.RData")

png(paste0(output.dir,fn.prefix," - Survey indices.png"),width = 11, height = 7, units = "in", res = 600)

datFL <- FLQuants(Q1=x.idx[[1]]@index, Q3Q4=x.idx[[2]]@index)

p1 <- ggplot(data=datFL, aes(year, data)) + 
  geom_line(aes(group=qname,colour=factor(qname)))+ theme_bw()+ labs(y="",colour="",x="") +
  facet_wrap(~age,scales="free")+
  scale_colour_manual(values=col.pal)+
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)
dev.off()

# splom plot

png(paste0(output.dir,fn.prefix," - Q1 survey splom plot.png"),width = 6, height = 6, pointsize=10, res=300, units="in")
survey.idx.splom(x.idx=x.idx[[1]], with.plus.pg = TRUE)
dev.off()

png(paste0(output.dir,fn.prefix," - Q3+Q4 survey splom plot.png"),width = 6, height = 6, pointsize=10, res=300, units="in")
survey.idx.splom(x.idx=x.idx[[2]], with.plus.pg = TRUE)
dev.off()

# Plot survey CPUE data #
cpue_dir <- "boot/data/DATRAS CPUE/"
load("boot/data/DATRAS CPUE/gmt3.RData")

### North Sea
# filename for North Sea  CPUE csv
csv_files <- dir(cpue_dir)[intersect(grep("NSQ",dir(cpue_dir)),grep(".csv",dir(cpue_dir)))]
file_nameQ1 <- csv_files[1]
file_nameQ3 <- csv_files[2]

# plot file name - basemap plots the last 5 years 
plot_filenameQ1<-paste0("Fig XXa HAD Q1_",ay-4,"-",ay,".png")
plot_filenameQ3<-paste0("Fig XXa HAD Q3_",ay-5,"-",ay-1,".png")

### IBTS Q1
cpue <- read.csv(paste0(cpue_dir,file_nameQ1), head=TRUE)
cpue <- cpue[cpue$Species=="Melanogrammus aeglefinus",]
names(cpue)[names(cpue)=="SubArea"] <- "Subarea"
cpue <- cpue[cpue$Area!=9,]
lonlat <- xy(cpue$Subarea)
gscale = 0.15

# Q1 Change species name for different species
png(paste0(output.dir,plot_filenameQ1), width=3000, height=2000) # don't forget to change fig name
par(mar=c(0,0.5,5,.5), lwd=2)
lay <- cbind(c(1,2,2),3:5,6:8,9:11,12:14,15:17)
layout(lay)

basemap(-4,13,51,62)
points(lonlat,pch='+', cex=2)
title('Survey Positions', line = 2, cex.main = 4)

plot(rep(.5,5),(1:5/5)-0.1, ylim=c(0,1), xlim=c(0,1), pch = 16, col = "lightblue",
     cex = gscale*sqrt(c(1000,2000,5000,10000,20000)), xlab = '', ylab = '', axes=FALSE)
points(rep(.5,5),(1:5/5)-0.1, cex = gscale*sqrt(c(1000,2000,5000,10000,20000)))
text(rep(.75,5),(1:5/5)-0.1, c(1000,2000,5000,10000,20000), cex=4.5)

# plot CPUE
for(y in (ay-4):ay){
  for(a in c('1','2','3+')){
    plotone(a,y,1)
    title(paste('Q1 ',y,': haddock age ',a,sep=''), 
          cex.main = 4, line = 2)
  }
}
dev.off()

### IBTS Q3
# read in data for Q3
cpue <- read.csv(paste0(cpue_dir,file_nameQ3), head=TRUE)
cpue<-cpue[cpue$Species=="Melanogrammus aeglefinus",]
names(cpue)[names(cpue)=="SubArea"] <- "Subarea"
cpue<-cpue[cpue$Area!=9,]
lonlat<-xy(cpue$Subarea)
gscale=0.15

png(paste0(output.dir,plot_filenameQ3), width=3000, height=2000) # don't forget to change fig name
par(mar=c(1,0.5,9,.5), lwd=2)
lay <- cbind(c(1,2,2,2),3:6,7:10,11:14,15:18,19:22)
layout(lay)

basemap(-4,13,51,62)
points(lonlat,pch='+', cex=2)
title('Survey Positions', line = 2, cex.main = 4)

plot(rep(.5,5),1:5/5, ylim=c(0,1), xlim=c(0,1), pch = 16, col = "lightblue",
     cex = gscale*sqrt(c(1000,2000,5000,10000,20000)), xlab = '', ylab = '', axes=FALSE)
points(rep(.5,5),1:5/5, cex = gscale*sqrt(c(1000,2000,5000,10000,20000)))
text(rep(.9,5),1:5/5, c(1000,2000,5000,10000,20000), cex=5)

# plot CPUE
for(y in (ay-5):(ay-1)){
  for(a in c('0','1','2','3+')){
    plotone(a,y,3)
    title(paste('Q3 ',y,': haddock age ',a,sep=''), 
          cex.main = 4, line = 2)
  }
}
dev.off()

# plot CVs
cv <- read.table("data/Survey indices/survey-haddock-Q1-1-8plus_CV.dat",sep="\t",skip=5)
cv[,1] <- 1983:ay
colnames(cv) <- c("Year", 1:8)
cv <- pivot_longer(cv,cols=ac(1:8),names_to="Age",values_to="CV")

png(paste0(output.dir,fn.prefix," - Survey indices CV.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=cv, aes(x=Year, y=CV)) + 
  geom_line()+ theme_bw()+ labs(y="",x="") +
  facet_wrap(~Age)+
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)
dev.off()

p1 <- ggplot(data=cv, aes(x=Age, y=CV,group=Year)) + 
  geom_line()+ theme_bw()+ labs(y="",x="") +
  facet_wrap(~Year)+
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)

#-----------------------------------------------------------##
# Survey diagnostics Q1 ####

###!!! The input files needed for these diagnostic plots are too large for GitHub to upload !!!!###
###!!! Please get in touch if you would like access to these data files !!!###
if(0){
  library(surveyIndex)
  datafile <- paste0("boot/data/Survey indices/haddock-Q1-", ay, ".RData")
  load(datafile)
  
  cmSize = 1
  agesQ1 = 1:8
  years = c(1983:ay) 
  
  dQ1 = addSpectrum(dQ1, by=cmSize)
  ##### Impute missing depths
  dQ1[[2]][!is.na(dQ1$Depth) & dQ1$Depth==0, "Depth"] <- NA
  
  dmodel = mgcv::gam(log(Depth) ~ s(lon, lat, k=200), data=dQ1[[2]])
  sel = subset(dQ1, is.na(Depth))
  sel$Depth = 0; ##### Guard against NA-error
  dQ1$Depth[is.na(dQ1$Depth)] = exp(predict(dmodel, newdata=sel[[2]]))
  dmodel = NULL
  sel = NULL
  gc()
  
  dQ1 = subset(dQ1, Month >= 1, Month <= 4)
  
  table(dQ1[[2]]$ICESAREA)
  sel.areas <- c("IIIa20", "IVa", "IVb", "IVc", "VIa")
  dQ1 <- subset(dQ1, ICESAREA %in% as.character(sel.areas))
  
  dQ1 = addWeightByHaul(dQ1)
  
  #  windows(8, 6)
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q1 survey map.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  par(mar=c(3.8,3.8,0.5,1),  mgp=c(2.2, 0.8, 0))
  mybubblePlot(dQ1, xlim=c(-10, 15), ylim=c(51, 62), col.zero=grey(0.7), scale=0.0075, col.cty=as.numeric(dQ1[[2]]$Survey))
  legend("bottomright", levels(dQ1[[2]]$Survey), col=1:length(levels(dQ1[[2]]$Survey)), pch=16, cex=0.9)
  dev.off()
  
  # windows(8, 6)
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q1 gear map.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  par(mar=c(3.8,3.8,0.5,1),  mgp=c(2.2, 0.8, 0))
  mybubblePlot(dQ1, xlim=c(-10, 15), ylim=c(51, 62), col.zero=grey(0.7), scale=0.0075, col.cty=as.numeric(dQ1[[2]]$Gear))
  legend("bottomright", levels(dQ1[[2]]$Gear), col=1:length(levels(dQ1[[2]]$Gear)), pch=16)
  dev.off()
  
  # windows(8, 6)
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q1 country map.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  par(mar=c(3.8,3.8,0.5,1),  mgp=c(2.2, 0.8, 0))
  mybubblePlot(dQ1, xlim=c(-10, 15), ylim=c(51, 62), col.zero=grey(0.7), scale=0.0075, col.cty=as.numeric(dQ1[[2]]$Country))
  legend("bottomright", levels(dQ1[[2]]$Country), col=1:length(levels(dQ1[[2]]$Country)), pch=16)
  dev.off()
  
  #might need to reload library?
  #library(surveyIndex)
  load("boot/data/Survey indices/dQ1withALK.RData")
  grid = getGrid(dQ1, nLon=40)
  tryCatch.W.E <- DATRAS:::tryCatch.W.E 
  SI.alt = getSurveyIdxStratMean(dQ1, agesQ1)
  
  load("boot/data/Survey indices/Q1.model.rdata")
  internalCons(SI$idx)
  
  # png(filename="figure.png", height=4.4, width=6, bg="white", pointsize=12, res=300, units="in")
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q1 compare to SMM.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots.mod(SI, dQ1, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.1,3.1,1,1), mgp=c(1.9, 0.8, 0)), select=c("index"), plotByAge=FALSE)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q1 diag plot effects.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots.mod(SI, dQ1, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3.1,1,1), mgp=c(1.9, 0.8, 0)), select=c("1"), plotByAge=FALSE, scheme=1)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q1 diag plot spatial.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots.mod(SI, dQ1, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3,1,1), mgp=c(1.9, 0.8, 0)), select=c("2"), plotByAge=FALSE, scheme=2, residuals=F, rug=T)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q1 diag plot depth.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots.mod(SI, dQ1, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3,1,1), mgp=c(1.9, 0.8, 0)), select=c("3"), plotByAge=FALSE, scheme=1, residuals=F, rug=T)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q1 diag plot timeShotHour.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots.mod(SI, dQ1, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3,1,1), mgp=c(1.9, 0.8, 0)), select=c("4"), plotByAge=FALSE, scheme=1, residuals=F, rug=T)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q1 diag plot time of day.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots.mod(SI, dQ1, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3,1,1), mgp=c(1.9, 0.8, 0)), select=c("5"), plotByAge=FALSE, scheme=1, residuals=F, rug=T)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q1 diag plot timeShotHour 2.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3,1,1), mgp=c(1.9, 0.8, 0)), select=c("6"), plotByAge=FALSE, scheme=1, residuals=F, rug=T)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q1 diag plot residuals histograms.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3.1,1,1), mgp=c(1.9, 0.8, 0)), select=c("residuals"), plotByAge=FALSE)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q1 relative abundance map.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3.1,1,1), mgp=c(1.9, 0.8, 0)), select=c("map"), plotByAge=FALSE, colors=rev(heat.colors(5)), legend=FALSE)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q1 relative abundance map ",ay,".png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3.1,1,1), mgp=c(1.9, 0.8, 0)), select=c("map"), year=ay,plotByAge=FALSE, colors=rev(heat.colors(5)), legend=FALSE)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q1 diag plot spatial residuals ",ay,".png"),height=12, width=19, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3.1,1,1), mgp=c(1.9, 0.8, 0)), select=c("spatialResiduals"), year=ay, plotByAge=FALSE, colors=rev(heat.colors(5)), legend=FALSE)
  dev.off()
  
  png(filename=paste0("output/input data/had.27.46a.20 - Survey indices - Q1 abundance map ",ay,".png"), height=7, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3,3,0.5,1), mgp=c(1, 0.8, 0)), select=c("absolutemap"), year=ay, plotByAge=FALSE,legend=FALSE, colors=rev(heat.colors(5)))
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q1 diag plot residuals by age and year.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3,1,1), mgp=c(1.9, 0.8, 0)), select=c("resVsYear"), plotByAge=FALSE)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q1 diag plot fitvsres.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3,1,1), mgp=c(1.9, 0.8, 0)), select=c("fitVsRes"), plotByAge=FALSE)
  dev.off()
  
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q1 diag plot1.png"), height=5, width=9, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots.mod(SI, dQ1, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(2,4), mar=c(3.5,3,1,1), mgp=c(1.9, 0.8, 0)), select=2, plotByAge=FALSE, scheme=2, residuals=F, rug=T)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q1 diag plot2.png"), height=5, width=9, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots.mod(SI, dQ1, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(2,4), mar=c(3.5,3,1,1), mgp=c(1.9, 0.8, 0)), select=3, plotByAge=FALSE, scheme=1, residuals=F, rug=T)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q1 diag plot3.png"), height=5, width=9, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots.mod(SI, dQ1, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(2,4), mar=c(3.5,3,1,1), mgp=c(1.9, 0.8, 0)), select=4, plotByAge=FALSE, scheme=1, residuals=F, rug=T)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q1 diag plot4.png"), height=5, width=9, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots.mod(SI, dQ1, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(2,4), mar=c(3.5,3,1,1), mgp=c(1.9, 0.8, 0)), select=5, plotByAge=FALSE, scheme=1, residuals=F, rug=T)
  dev.off()
  
  # retro
  
  load("boot/data/Survey indices/Q1.retro.model.rdata")
 
  npeels <- 5
  n.non.peeled <- 3
  years.inc <- 1983:ay
  last.yr <- rev(tail(years.inc, npeels+1)) 
  ret.peels <- ret[1:(1+npeels)]

  for (i in 1:length(last.yr)) ret[[i]]=2*ret[[i]]/length(grid[[3]])
  colfunc <- colorRampPalette(c("red2", "blue"))
  
  rhos <- numeric(length(agesQ1))
  for (a in 1:length(agesQ1)) {
    dta <- as.data.frame(matrix(nrow=npeels+1+n.non.peeled, ncol=npeels+1, dimnames=list(as.character(tail(years.inc, npeels+1+n.non.peeled)), c("base", -(1:npeels)) ))) 
    for (i in 1:(npeels+1)) 
      dta[1:(npeels-i+2+n.non.peeled), i] <- tail(ret.peels[[i]][, a], npeels-i+2+n.non.peeled)
    rhos[a] <- icesRound(icesAdvice::mohn(dta))
    # mohn(dta, plot=TRUE, details=TRUE)
  }
  
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q1 Retros.png"), height=7, width=9, bg="white", pointsize=12, res=300, units="in")
  par(mfrow=c(3, 3), mar=c(3.1, 3.1, 1, 1), mgp=c(1.9, 0.8, 0))
  for (a in 1:length(agesQ1)) {
    maxy <- NULL
    for (i in 1:length(ret.peels)) maxy <- c(maxy, ret.peels[[i]][, a])
    maxy <- max(maxy)
    plot(NA, NA, type="l", lwd=1, xlab="Index", ylab="Year", main=paste("Age group", colnames(dQ1$Nage)[a]), xlim=range(years.inc), ylim=c(0, maxy))
    for (i in length(ret.peels):1)
      lines(as.numeric(rownames(ret.peels[[i]])), ret.peels[[i]][, a], lty=1, lwd=1, col=adjustcolor(colfunc(length(ret.peels)))[i])
    legend("topright", legend=paste0("rho=", rhos[a]), bty="n", inset=0.0)
  }
  dev.off()
}

#-----------------------------------------------------------------------------------##
# Survey Q3Q4 diagnostics ####

###!!! The input files needed for these diagnistic plots are too large for GitHub to upload !!!!###
###!!! Please get in touch if you would like access to these data files !!!###
if(0){
  library(surveyIndex)
  cmSize = 1
  agesQ3Q4 = 0:8
  years = c(1991:(ay-1))
  
  datafile <- paste0("boot/data/Survey indices/haddock-Q3Q4-", max(years),".RData")
  load(datafile)
  
  dQ3Q4 = addSpectrum(dQ3Q4, by=cmSize)
  
  ##### Impute missing depths
  dmodel = gam(log(Depth) ~ s(lon, lat, k=200), data=dQ3Q4[[2]])
  sel = subset(dQ3Q4, is.na(Depth))
  sel$Depth = 0; ##### Guard against NA-error
  dQ3Q4$Depth[is.na(dQ3Q4$Depth)] = exp(predict(dmodel, newdata=sel[[2]]))
  dmodel = NULL
  sel = NULL
  gc()
  
  ##### Q3 and Q4 survey run in end Jun-Dec
  dQ3Q4 = subset(dQ3Q4, Month >= 6, Month <= 12)
  
  table(dQ3Q4[[2]]$ICESAREA)
  sel.areas <- c("IIIa20", "IVa", "IVb", "IVc", "VIa")
  dQ3Q4 <- subset(dQ3Q4, ICESAREA %in% as.character(sel.areas))
  
  dQ3Q4 = addWeightByHaul(dQ3Q4)
  
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q3Q4 survey map.png"), height=4, width=6, bg="white", pointsize=12, res=300, units="in")
  par(mar=c(3.8, 3.8, 0.5, 1),  mgp=c(2.2, 0.8, 0))
  mybubblePlot(dQ3Q4, xlim=c(-10, 15), ylim=c(51, 62), col.zero=grey(0.7), scale=0.0075, col.cty=as.numeric(dQ3Q4[[2]]$Survey))
  legend("bottomright", levels(dQ3Q4[[2]]$Survey), col=1:length(levels(dQ3Q4[[2]]$Survey)), pch=16, cex=0.9)
  dev.off()
  
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q3Q4 gear map.png"), height=4, width=6, bg="white", pointsize=12, res=300, units="in")
  par(mar=c(3.8, 3.8, 0.5, 1),  mgp=c(2.2, 0.8, 0))
  mybubblePlot(dQ3Q4, xlim=c(-10, 15), ylim=c(51, 62), col.zero=grey(0.7), scale=0.0075, col.cty=as.numeric(dQ3Q4[[2]]$Gear))
  legend("bottomright", levels(dQ3Q4[[2]]$Gear), col=1:length(levels(dQ3Q4[[2]]$Gear)), pch=16)
  dev.off()
  
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q3Q4 country map.png"), height=4, width=6, bg="white", pointsize=12, res=300, units="in")
  par(mar=c(3.8, 3.8, 0.5, 1),  mgp=c(2.2, 0.8, 0))
  mybubblePlot(dQ3Q4, xlim=c(-10, 15), ylim=c(51, 62), col.zero=grey(0.7), scale=0.0075, col.cty=as.numeric(dQ3Q4[[2]]$Country))
  legend("bottomright", levels(dQ3Q4[[2]]$Country), col=1:length(levels(dQ3Q4[[2]]$Country)), pch=16)
  dev.off()
  
  load("boot/data/Survey indices/dQ3Q4withALK.RData")
  grid = getGrid(dQ3Q4, nLon=40)
  tryCatch.W.E <- DATRAS:::tryCatch.W.E 
  SI.alt = getSurveyIdxStratMean(dQ3Q4, agesQ3Q4+1)
  
  load(file="boot/data/Survey indices/Q3Q4.model.rdata")
  
  
  # compare modelled index to old SMM index
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q3Q4 compare to SMM.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots.mod(SI, dQ3Q4, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.1,3.1,1,1), mgp=c(1.9, 0.8, 0)), select=c("index"), plotByAge=FALSE)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q3Q4 diag plot spatial.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots.mod(SI, dQ3Q4, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3.1,1,1), mgp=c(1.9, 0.8, 0)), select=c("1"), plotByAge=FALSE, scheme=2, residuals=F, rug=T)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q3Q4 diag plot depth.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots.mod(SI, dQ3Q4, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3,1,1), mgp=c(1.9, 0.8, 0)), select=c("2"), plotByAge=FALSE, scheme=1, residuals=F, rug=T)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q3Q4 diag plot effects.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots.mod(SI, dQ3Q4, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3,1,1), mgp=c(1.9, 0.8, 0)), select=c("3"), plotByAge=FALSE, scheme=1, residuals=F, rug=T)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q3Q4 diag plot timeOfYear.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots.mod(SI, dQ3Q4, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3,1,1), mgp=c(1.9, 0.8, 0)), select=c("4"), plotByAge=FALSE, scheme=1, residuals=F, rug=T)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q3Q4 diag plot timeShotHour.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots.mod(SI, dQ3Q4, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3,1,1), mgp=c(1.9, 0.8, 0)), select=c("5"), plotByAge=FALSE, scheme=1, residuals=F, rug=T)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q3Q4 diag plot Quarter-timeShotHour.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots.mod(SI, dQ3Q4, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3,1,1), mgp=c(1.9, 0.8, 0)), select=c("6"), plotByAge=FALSE, scheme=1, residuals=F, rug=F)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q3Q4 diag plot residuals histograms.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots(SI, dQ3Q4, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3.1,1,1), mgp=c(1.9, 0.8, 0)), select=c("residuals"), plotByAge=FALSE)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q3Q4 relative abundance map.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots(SI, dQ3Q4, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3.1,1,1), mgp=c(1.9, 0.8, 0)), select=c("map"), plotByAge=FALSE, colors=rev(heat.colors(5)), legend=FALSE)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q3Q4 relative abundance map ",ay-1,".png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots(SI, dQ3Q4, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3.1,1,1), mgp=c(1.9, 0.8, 0)), select=c("map"), year=ay-1,plotByAge=FALSE, colors=rev(heat.colors(5)), legend=FALSE)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q3Q4 diag plot spatial residuals ",ay-1,".png"), height=12, width=19, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots(SI, dQ3Q4, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3.1,1,1), mgp=c(1.9, 0.8, 0)), select=c("spatialResiduals"), year=ay-1, plotByAge=FALSE, colors=rev(heat.colors(5)), legend=FALSE)
  dev.off()
  
  # png(filename=paste0("output/input data/had.27.46a.20 - Survey indices - Q3Q4 abundance map ",ay-1,".png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  # surveyIdxPlots(SI, dQ3Q4, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3,3,0.5,1), mgp=c(1, 0.8, 0)), select=c("absolutemap"), year=ay-1,colors=rev(heat.colors(5)), plotByAge=FALSE,legend=FALSE)
  # dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q3Q4 diag plot residuals by age and year.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots(SI, dQ3Q4, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3,1,1), mgp=c(1.9, 0.8, 0)), select=c("resVsYear"), plotByAge=FALSE)
  dev.off()
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q3Q4 diag plot fitvsres.png"), height=6.2, width=8, bg="white", pointsize=12, res=300, units="in")
  surveyIdxPlots(SI, dQ3Q4, alt.idx=SI.alt, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(3.5,3,1,1), mgp=c(1.9, 0.8, 0)), select=c("fitVsRes"), plotByAge=FALSE)
  dev.off()
  
  
  # retro
  load("boot/data/Survey indices/Q3Q4.retro.model.rdata")
  npeels <- 5
  n.non.peeled <- 3
  years.inc <- 1991:(ay-1)
  last.yr <- rev(tail(years.inc, npeels+1))
  
  for (i in 1:length(last.yr)) ret[[i]]=2*ret[[i]]/length(grid[[3]])
  colfunc <- colorRampPalette(c("red2", "blue"))
  
   ret.peels <- ret[1:(1+npeels)]
  
  rhos <- numeric(length(agesQ3Q4))
  for (a in 1:length(agesQ3Q4)) {
    dta <- as.data.frame(matrix(nrow=npeels+1+n.non.peeled, ncol=npeels+1, dimnames=list(as.character(tail(years.inc, npeels+1+n.non.peeled)), c("base", -(1:npeels)) ))) 
    for (i in 1:(npeels+1)) 
      dta[1:(npeels-i+2+n.non.peeled), i] <- tail(ret.peels[[i]][, a], npeels-i+2+n.non.peeled)
    rhos[a] <- icesRound(icesAdvice::mohn(dta))
    # mohn(dta, plot=TRUE, details=TRUE)
  }
  
  png(filename=paste0(output.dir,"had.27.46a.20 - Survey indices - Q3Q4 Retros.png"), height=7, width=9, bg="white", pointsize=12, res=300, units="in")
  par(mfrow=c(3, 3), mar=c(3.1, 3.1, 1, 1), mgp=c(1.9, 0.8, 0))
  for (a in 1:length(agesQ3Q4)) {
    maxy <- NULL
    for (i in 1:length(ret.peels)) maxy <- c(maxy, ret.peels[[i]][, a])
    maxy <- max(maxy)
    plot(NA, NA, type="l", lwd=1, xlab="Index", ylab="Year", main=paste("Age group", colnames(dQ3Q4$Nage)[a]), xlim=range(years.inc), ylim=c(0, maxy))
    for (i in length(ret.peels):1)
      lines(as.numeric(rownames(ret.peels[[i]])), ret.peels[[i]][, a], lty=1, lwd=1, col=adjustcolor(colfunc(length(ret.peels)))[i])
    legend("topright", legend=paste0("rho=", rhos[a]), bty="n", inset=0.0)
  }
  dev.off()
  
}