## Extract results of interest, write TAF output tables

## Before:
## After:

graphics.off()

###---------------------------------------------------------------------###

# Forecast input and comparison plots ####
#assess_year <- ay # the intermediate year when assessment is being conducted
#advice_year <- ay+1 # the year for TAC advice
data_yrs <- 1972:(ay-1)

## Forecast parameters:
Ay <- (ay-3):(ay-1) # for biols
Sy <- (ay-3):(ay-1)  # for sel
Ry <- 2000:(ay-1) # for rec

tab.inputs <- data.frame(age=0:8,Mat=NA,NM=NA,Sel=NA,LF=NA)

# mean weights -----------------------------------------#
ca.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - catch-at-age.txt")
st.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - stock-at-age.txt")
lan.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - landings-at-age.txt")
dis.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - discards-at-age incl BMS and IBC.txt")

colnames(ca.frct.wt) <- colnames(st.frct.wt) <- colnames(lan.frct.wt) <- colnames(dis.frct.wt) <- colnames(fit$data$catchMeanWeight)

ac<-as.character

fit$data$catchMeanWeight[,,"Residual catch"] <- rbind(fit$data$catchMeanWeight[ac(data_yrs),,"Residual catch"],ca.frct.wt)
fit$data$stockMeanWeight <- rbind(fit$data$stockMeanWeight[ac(data_yrs),],st.frct.wt) 
fit$data$landMeanWeight[,,"Residual catch"] <- rbind(fit$data$landMeanWeight[ac(data_yrs),,"Residual catch"],lan.frct.wt)
fit$data$disMeanWeight[,,"Residual catch"] <- rbind(fit$data$disMeanWeight[ac(data_yrs),,"Residual catch"],dis.frct.wt) 

ca.frct.wt$Year <- st.frct.wt$Year <- lan.frct.wt$Year <- dis.frct.wt$Year <- row.names(ca.frct.wt)
ca.frct.wt$Cat <- "catch"
st.frct.wt$Cat <- "stock"
lan.frct.wt$Cat <- "landings"
dis.frct.wt$Cat <- "discards"

dat <- reshape2::melt(rbind(ca.frct.wt,st.frct.wt,lan.frct.wt,dis.frct.wt),id.vars=c("Year","Cat"))
names(dat) <- c("Year","Cat","age","wt")

# age 0 - no observations - set to NA
dat[dat$age==0 & dat$wt==0,"wt"]<-NA

png(paste0("output/Forecast/Forecast inputs - mean weights.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=dat, aes(x=age, y=wt,colour=Cat,group=Cat)) + 
  facet_wrap(~Year,ncol=2)+ geom_point()+
  geom_line() + theme_bw()+ labs(y="mean weight (kg)",colour="",x="") +
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) + scale_colour_manual(values=col.pal9)
print(p1)
dev.off()

png(paste0("output/Forecast/Forecast inputs - mean weights v2.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=dat, aes(x=age, y=wt,colour=Year,group=Year)) + 
  facet_wrap(~Cat)+ geom_point()+
  geom_line() + theme_bw()+ labs(y="mean weight (kg)",colour="",x="") +
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) + scale_colour_manual(values=col.pal9)
print(p1)
dev.off()

# plot maturity and natural mortality -----------------------------------------#


fit$data$propMat[ac(ay),] <- round(colMeans(fit$data$propMat[ac(Ay),]),3)
fit$data$natMor[ac(ay),] <- round(colMeans(fit$data$natMor[ac(Ay),]),3)

tab.inputs$Mat <- fit$data$propMat[ac(ay),]
tab.inputs$NM <- fit$data$natMor[ac(ay),]

dat <- fit$data$propMat[ac(1972:(ay-1)),] #data years only
dat.frcst <- round(colMeans(fit$data$propMat[ac(Ay),]),3)
dat.frcst <- rbind(dat.frcst,dat.frcst,dat.frcst)
rownames(dat.frcst) <- (ay-1):(ay+1)
# maturity
png(paste0("output/Forecast/Forecast inputs - maturity.png"),width = 11, height = 7, units = "in", res = 600)

par(mar=c(5, 4, 4, 10), xpd=TRUE)
for (a in 0:8){
  if(a==0){
    plot(1972:(ay-1),dat[,ac(a)],ylim=c(0,1),xlim=c(1972,ay+2),type="l",col=col.pal9[(a+1)],ylab="proportion mature",xlab="")
    points(ay:(ay+2),dat.frcst[,ac(a)],pch=16,col=col.pal9[(a+1)])
  }else{
    lines(1972:(ay-1),dat[,ac(a)],col=col.pal9[(a+1)])
    points(ay:(ay+2),dat.frcst[,ac(a)],pch=16,col=col.pal9[(a+1)])
  }
}
legend("topright", inset=c(-0.2, 0),legend=0:8,col=col.pal9,lty="solid",pch=16)
dev.off()

#natural mortality
dat <- fit$data$natMor[ac(1972:(ay-1)),] #data years only
dat.frcst <- round(colMeans(fit$data$natMor[ac(Ay),]),3)
dat.frcst <- rbind(dat.frcst,dat.frcst,dat.frcst)
rownames(dat.frcst) <- (ay-1):(ay+1)


png(paste0("output/Forecast/Forecast inputs - natural mortality.png"),width = 11, height = 7, units = "in", res = 600)

col.pal9 <- c(brewer.pal(n = 8, name = "Dark2"),brewer.pal(n=6,name="Set2")[4])
par(mar=c(5, 4, 4, 10), xpd=TRUE)
for (a in 0:8){
  if(a==0){
    plot(1972:(ay-1),dat[,ac(a)],ylim=c(0,1.8),xlim=c(1972,ay+2),type="l",col=col.pal9[(a+1)],ylab="natural mortality",xlab="")
    points(ay:(ay+2),dat.frcst[,ac(a)],pch=16,col=col.pal9[(a+1)])
  }else{
    lines(1972:(ay-1),dat[,ac(a)],col=col.pal9[(a+1)])
    points(ay:(ay+2),dat.frcst[,ac(a)],pch=16,col=col.pal9[(a+1)])
  }
}
legend("topright", inset=c(-0.2, 0),legend=0:8,col=col.pal9,lty="solid",pch=16)
dev.off()


# selectivity -----------------------------------------#
Fsel <- faytable(fit)/rowSums(faytable(fit))
Fsel.frcst <- colMeans(Fsel[ac(Sy),])

tab.inputs$Sel <- round(colMeans(Fsel[ac(Sy),]),3)

png(paste0("output/Forecast/Forecast inputs - selectivity.png"),width = 11, height = 7, units = "in", res = 600)

col.pal9 <- c(brewer.pal(n = 8, name = "Dark2"),brewer.pal(n=6,name="Set2")[4])
for (y in Sy){
  if(y==Sy[1]){
    plot(0:8,Fsel[ac(y),],ylim=c(0,max(Fsel)),type="l",col=col.pal9[(y-Sy[1])+1],ylab="Selectivity",xlab="",lwd=2)
  }else{
    lines(0:8,Fsel[ac(y),],col=col.pal9[(y-Sy[1])+1],lwd=2)
  }
}
points(0:8,Fsel.frcst,pch=16,col=col.pal9[4])
lines(0:8,Fsel.frcst,col=col.pal9[4],lwd=2,lty=3)
legend("bottomright", inset=0.02,legend=c(Sy,"Forecast"),col=col.pal9[1:4],lty=c(1,1,1,3),lwd=c(2,2,2,2),pch=c(NA,NA,NA,16))
dev.off()

# Catch proportions -----------------------------------------#
lf <- fit$data$landFrac[ac(1972:(ay-1)),,"Residual catch"] #data years only
lf.frcst <- round(colMeans(lf[ac(Sy),]),3)
df.frcst <- 1-lf.frcst
df <-(1-lf)

tab.inputs$LF <- lf.frcst
tab.inputs$DF <- df.frcst

png(paste0("output/Forecast/Forecast inputs - Catch proportions by age.png"),width = 11, height = 7, units = "in", res = 600)
par(mar=c(5, 4, 4, 10), xpd=TRUE)
for (y in Sy){
  if(y==Sy[1]){
    plot(0:8,lf[ac(y),],type="l",col=col.pal9[(y-Sy[1]+1)],ylim=c(0,1),ylab="Catch proportions",xlab="")
    lines(0:8,df[ac(y),],col=col.pal9[((y-Sy[1]+1))],lty=2)
  }else{
    lines(0:8,lf[ac(y),],col=col.pal9[((y-Sy[1]+1))])
    lines(0:8,df[ac(y),],col=col.pal9[((y-Sy[1]+1))],lty=2)
  }
}
points(0:8,lf.frcst,pch=16,col=col.pal9[4],type="o")
points(0:8,df.frcst,pch=16,col=col.pal9[4],type="o",lty=2)
legend("bottomright", inset=c(-0.2, 0),legend=c(Sy,"Forecast","Landings","Discards+BMS+IBC"),col=c(col.pal9[1:4],"black","black"),
       lty=c(rep("solid",4),"solid","dashed"),pch=c(NA,NA,NA,16,NA,NA),cex=0.75)
dev.off()


# plot Recruitment -----------------------------------------#
png(paste0("output/Forecast/Forecast inputs - recruitment.png"),width = 11, height = 7, units = "in", res = 600)

R <- rectable(fit)[,1]
R <- R[as.character(1972:(ay-1))]
R_datayr <- attr(FC[[1]], "tab")[as.character(advice_year-2), "rec:median"]
R_ay <- attr(FC[[1]], "tab")[as.character(advice_year-1), "rec:median"]
R_TACyr <- attr(FC[[1]], "tab")[as.character(advice_year), "rec:median"] 
R_geoMean <- exp(mean(log(R[ac(Ry)]))) # better summary stat for tables and plots when length(Ry) is even


plot(names(R),R,xlab="",ylab="Recruitment age 0 (thousands)",pch=16,type="o")
lines(Ry,rep(R_datayr,length(Ry)),lty=2,col="lightseagreen",lwd=2)
lines(Ry,rep(R_ay,length(Ry)),lty=3,col="violet",lwd=2)
lines(Ry,rep(R_TACyr,length(Ry)),lty=4,col="magenta",lwd=2)

#if((length(Ry) %% 2) == 0){
  # is even
  lines(Ry,rep(R_geoMean,length(Ry)),lty=1,col="gold",lwd=2)
  legend("topright", inset=0.02,legend=c(paste0("Rec ",ay-1),paste0("Rec ",ay),paste0("Rec ",ay+1),"geometric mean"),
         col=c("lightseagreen","violet","magenta","gold"),
         lty=c(2,3,4,1),lwd=2)
#}else{
#  legend("topright", inset=0.02,legend=c(paste0("Rec ",ay-1),paste0("Rec ",ay),paste0("Rec ",ay+1)),col=c("orange","lightseagreen","magenta"),
#         lty=c(2,2,3),lwd=2)
#}
dev.off()

#----------------------------------------------------------------------#
# Forecast results tables and plots ####

write.taf(tab.inputs,file="output/Forecast/Forecast inputs table.csv")

#plot FMSY option

frcst.Fmsy <- FC[["Fsq, then Fmsy"]] 
label <-attr(frcst.Fmsy,"label")
attr(frcst.Fmsy,"estimateLabel") <- "median"
png(paste0("output/Forecast/Forecast results - ",label,".png"),width = 11, height = 7, units = "in", res = 600)
plot(frcst.Fmsy,main=label,xlab="")
dev.off()

# intermediate year numbers
tab <- attr(frcst.Fmsy,"tab")
tab[ac(ay),c("fbar:median","rec:median","ssb:median","catch:median","Land:median","Discard:median")]
tab[ac(ay+1),c("rec:median","ssb:median")]


# make advice table -----------------------------------------#

# forecasts table 
# update scenarios that hit SSB refs
scen_num <- which(grepl(paste0("then SSB(", advice_year+1, ") = "),
                        names(FC), fixed = TRUE))
FC[scen_num] <- FC2


# prob of falling below Blim

tmp <- lapply(names(FC),function(x){
  pr.intyr <- sum(FC[[x]][[2]]$ssb < Blim)/length(FC[[x]][[2]]$ssb)
  pr.tacyr <- sum(FC[[x]][[3]]$ssb < Blim)/length(FC[[x]][[3]]$ssb)
  pr.ssbyr <- sum(FC[[x]][[4]]$ssb < Blim)/length(FC[[x]][[4]]$ssb)
  
  df<-data.frame(year=ay:(ay+2),prob=c(pr.intyr,pr.tacyr,pr.ssbyr))
})
names(tmp) <- names(FC)

ssb_blim_prob <- dplyr::bind_rows(tmp, .id = "column_label")

toplot <- ssb_blim_prob[-grep("FMSY = ",ssb_blim_prob$column_label),]
ggplot(toplot,aes(x=year,y=prob,colour=column_label))+geom_line()+theme_bw()+ylim(c(0,1))

ssb_blim_prob[ssb_blim_prob$prob != 0,]

writeLines("", con = "output/Forecast/tab_forecasts.txt", sep = "\t")
FC_df <- vector("list", length(FC))
for(i in seq(FC)){
  f <- FC[[i]]
  fc_tab <- attr(f, "tab")
  fc_lab <- attr(f, "label")
  tmp <- as.data.frame(fc_tab)
  tmp <- cbind(data.frame("scenario" = fc_lab), tmp)
  tmp <- xtab2taf(tmp)
  FC_df[[i]] <- tmp
  
  fc_tab <- xtab2taf(fc_tab)
  fc_lab <- gsub(pattern = '*', replacement = "star", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = ",", replacement = "", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = "+", replacement = "plus", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = "-", replacement = "minus", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = "=", replacement = "equals", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = "%", replacement = "perc", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = " ", replacement = "_", x = fc_lab, fixed = TRUE)
  fname <- paste0("tab_fc_", fc_lab, ".csv")
  
  write.taf(fc_tab, file.path("output/Forecast", fname))
  
  write.table(x = paste("\n", attr(f,"label")), file = "output/Forecast/tab_forecasts.txt", append = TRUE,
              row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  write.table(x = fc_tab, file="output/Forecast/tab_forecasts.txt", append = TRUE,
              row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
}
FC_df <- do.call("rbind", FC_df)
save(FC_df, file = "output/Forecast/FC_df.Rdata")

## Export forecast results for MAP with step 0.1:
mapIdx <- grepl(pattern = "^TACcont, then FMSY = [.[:digit:]]+$",
                names(FC))

res <- lapply(FC[mapIdx],
              function(x)
              {
                fc_tab <- attr(x, "tab")
                fc_lab <- attr(x, "label")
                tmp <- as.data.frame(fc_tab)
                tmp <- cbind(data.frame("scenario" = fc_lab),
                             year = row.names(tmp),
                             tmp)
                row.names(tmp) <- NULL
                return(tmp)
              })#, simplify = FALSE, USE.NAMES = FALSE)
resFmap <- do.call(rbind, res)

row.names(resFmap) <- NULL

write.csv(resFmap,
          file = file.path("output/Forecast", "Large_F_range_forecast_all_years.csv"))

write.csv(resFmap[resFmap$year %in% advice_year, ],
          file = file.path("output/Forecast", paste0("Large_F_range_forecast_", advice_year, ".csv")))


