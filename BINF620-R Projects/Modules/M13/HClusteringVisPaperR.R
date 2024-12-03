# Installing Packages 
install.packages("ClusterR") 
install.packages("cluster") 
install.packages("lme4")

# Loading package 
library(ClusterR) 
library(cluster) 
library(lme4)
library(lattice)
library(reshape2)

df<-matrix(c(1,2,4,3,2,1,7,9),nrow=4)
rownames(df)<-c("x1","x2","x3","x4")
colnames(df)<-c("a","b")
df
dist(df)

plot(hclust(dist(df)))

nvar=5
data<-data.frame(diag=factor(rep(c("Sepsis","AEC
OPD","Surgery","MODS","Poisoning"),50)))

for (i in 1:nvar) {
  data[[paste("x",i,sep="")]]<-rnorm(250)
}
attach(data)
data$y<-3*data$x1+2*data$x2-2*data$x3+data$x3^2-data$x4+data$x5^3-2*data$x5
detach()

library(lme4)
coeff<-lapply(data[,2:6],function(x) {
  coef(lmList(y~x|diag,data=data.frame(x=x,y=data$y,
                                       diag=data$diag)))[2]
})

coefficient<-t(as.data.frame(coeff))
varlist<-names(data[,2:6])
row.names(coefficient)<-varlist

library("gplots")
heatmap.2(coefficient,ColSideColors=rainbow(ncol
  (coefficient)),RowSideColors=rainbow(nrow(coefficient)),
         srtCol=45)

heatmap.2(coefficient,
          hclust(dist(coefficient,method="minkowski"),
                 method="mcquitty"),
          ColSideColors=rainbow(ncol(coefficient)),
          RowSideColors=rainbow(nrow(coefficient)),
          srtCol=45)

library(lattice)
library(reshape2)
m.data<-melt(data, id.vars=c("diag", "y"))
head(m.data)

dd.row <- as.dendrogram(hclust(dist(coefficient)))
row.ord <- order.dendrogram(dd.row)
dd.col <- as.dendrogram(hclust(dist(t(coefficient))))
col.ord <- order.dendrogram(dd.col)
par(mfrow=c(2,1))
plot(dd.row)
plot(dd.col)

coeff.order<-coefficient[row.ord,col.ord]
scale.coef<-as.vector(round((coeff.order
                               -min(coefficient))*10+1))

par(mfrow=c(3,2))
palette(rainbow(10))
barplot(rep(1,10), yaxt="n",main="rainbow", col=1:10)
palette(rainbow(10,start=0,end=0.7))
barplot(rep(1,10), yaxt="n",main="rainbow (0-0.7)",
          col=1:10)
palette(heat.colors(10))
barplot(rep(1,10), yaxt="n",main="heat colors",
          col=1:10)
palette(terrain.colors(10))
barplot(rep(1,10), yaxt="n",main="terrain colors",
          col=1:10)
palette(topo.colors(10))
barplot(rep(1,10), yaxt="n",main="topo colors",
          col=1:10)
palette(topo.colors(10,alpha=0.7))
barplot(rep(1,10), yaxt="n",main="topo colors (alpha=0.7)", col=1:10)
palette(rainbow(round
                ((max(coefficient)-min(coefficient))*10)+1,start=0,end=0.7))


library(latticeExtra)
plot<-xyplot(y~value|variable+diag,data=m.data,par.strip.text = list(cex = 0.6),
             key=list(space="left",
                      lines=list(col=seq(1,round((max(coefficient)-min(coefficient))*10)+1,4),lwd=4,size=1),
                      text=list(as.character(round((seq(1,round((
                        max(coefficient)-min(coefficient))*10)+1,4)-
                          1)/10+min(coefficient),1)))
             ),
             legend =
               list(right =
                      list(fun = dendrogramGrob,
                           args =
                             list(x = dd.col, ord = col.ord,
                                  side = "right",
                                  size = 10)),
                    top =
                      list(fun = dendrogramGrob,
                           args =
                             list(x = dd.row,
                                  side = "top",
                                  type = "triangle"))),
             mycolors =scale.coef,
             panel = function(x, y,col,mycolors) {
               panel.fill(col=mycolors[panel.number()])
               panel.xyplot(x, y,cex=0.2,col="black")
               panel.loess(x, y, col="black",lwd=2)
             },
             index.cond=list(row.ord,col.ord),
             xlab="x value"
)
useOuterStrips(plot) 
row.ord

data$y.bin = 1/(1+exp(-data$y)) > 0.5
coeff.bin<-lapply(data[,2:6],
                  function(x) {
                    coef(lmList(y~x|diag,
                                family=binomial(link="logit"),
                                data=data.frame(x=x,y=data$y.bin,diag=data$diag)))[2]
                                    })
                                
 coeff.bin<-as.matrix(as.data.frame(coeff.bin))
 colnames(coeff.bin)<-varlist
 heatmap.2(coeff.bin,
            ColSideColors=rainbow(ncol(coeff.bin)),
            RowSideColors=rainbow(nrow(coeff.bin)),
            srtRow=45)
 models<-lapply(data[,2:6],
                  function(x) {
                    lmList(y~x|diag,
                           family=binomial(link="logit"),
                           data=data.frame(x=x,y=data$y.bin,diag=data$diag))
                  })
 
 prob<-as.vector(NULL)
for (i in 1:5) {
   prob<-c(prob,predict(models[[i]]))
}
 
 data.bin<-melt(data[,-7], id.vars=c("diag", "y.bin"))
diaglist<-data[1:5,]$diag
data.sort<-data.bin[0,]
for (var in varlist) {
   for (dia in diaglist) {
     data.sort<-rbind(data.sort,
                      data.bin[data.bin$variable==var&data.bin$diag==dia,])
   }
 }

data.pred<-cbind(data.sort,prob)

dd.row.bin <- as.dendrogram(hclust(dist(coeff.bin)))
row.ord.bin <- order.dendrogram(dd.row.bin)
 dd.col.bin <- as.dendrogram(hclust(dist(t(coeff.bin))))
 col.ord.bin <- order.dendrogram(dd.col.bin)
 coeff.order.bin<-coeff.bin[row.ord.bin, col.ord.bin]
 scale.coef.bin<-as.vector(round((t(coeff.order.bin)-
                                     min(coeff.bin))*10+1))
 
 palette(rainbow(round((max(coeff.bin)-min(coeff.bin))*10)+1,start=0,end=0.7))
plot.bin<-xyplot(prob~value|variable+diag,data=data.pred,
                    par.strip.text = list(cex = 0.6),
                    key=list(space="left",
                             lines=list(col=seq(1,round((max(coeff.bin)-min(coeff.bin))*10)+1,2),lwd=4,size=1),
                             text=list(as.character(round((seq(1,round((max(coeff.bin)-min(coeff.bin))*10)+1,2)-1)/10
                                                          +min(coeff.bin),1)))
                    ),
                    legend =
                      list(right =
                             list(fun = dendrogramGrob,
                                  args =
                                    list(x = dd.col.bin, ord = col.ord.bin,
                                         side = "right",
                                         size = 10)),
                           top =
                             list(fun = dendrogramGrob,
                                  args =
                                    list(x = dd.row.bin,
                                         side = "top",
                                         type = "triangle"))),
                    colors.bin =scale.coef.bin,
                    panel = function(x, y,col,colors.bin) {
                      panel.fill(col=colors.bin[panel.number()])
                      panel.xyplot(x, y,cex=0.2,col="black")
                      panel.loess(x, y, col="black",lwd=1)
                    },
                    index.cond=list(col.ord.bin, row.ord.bin),
                    xlab="x value",
                    ylab="probability"
 )
useOuterStrips(plot.bin) 