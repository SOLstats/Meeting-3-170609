mydata=read.table("glht2.txt",header=T,sep="\t")

mydata$factor.cat=factor(mydata$factor.cat,levels=c("a","b"))
# contrasts(mydata$factor1)=c(-.5,.5)
mydata.lm=lm(outcome~factor.cat*factor.num,mydata)

summary(mydata.lm)
model.matrix(mydata.lm)

(0.10139234*c(mydata[1:10,"factor2"],rep(0,10))+
0.08266777*c(rep(0,10),mydata[11:20,"factor2"]))
-
predict(mydata.lm)
L=matrix(c(0,0,1,0,   
           0,0,1,1) # 
           ,byrow=T,ncol=4) # simple slopes
 
rownames(L)=c("slope 1","slope 2")

slopes.glht=glht(mydata.lm,linfct=L)
summary(slopes.glht)
