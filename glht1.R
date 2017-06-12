dfr=read.table("glht1.txt",header=T,sep="\t")
dfr[,c("A","B")]=lapply(dfr[,c("A","B")],function(x){factor(x)})
with(dfr,tapply(score,list(A,B),mean))

dfr.lm=lm(score~A*B,dfr)
summary(dfr.lm)

# Four coefficients: C1, C2, C3, C4, four means: M11, M12, M21, M22

# M11 = C1
# M12 = C1 + C2
# M21 = C1 + C3
# M22 = C1 + C2 + C3 + C4

# Four comparisons:

# M11 - M12 = C1 - (C1 + C2) = -C2
# M21 - M22 = (C1 + C3) - (C1 + C2 + C3 + C4) = -C2 - C4
# M11 - M21 = C1 - (C1 + C3) = -C3
# M12 - M22 = (C1 + C2) - (C1 + C2 + C3 + C4) = -C3 - C4
# Expanding the number of contrasts, simple main effect of A for B  = 2

K=matrix(c(0,-1,0,0,   
           0,-1,0,-1,
           0,0,-1,0,
           0,0,-1,-1) # 
           ,byrow=T,ncol=4) # simple main effect of A for B = 1

rownames(K)=c("M11-M12","M21-M22","M11-M21","M12-M22")

glht.glht=glht(glht.lm,linfct=K)
summary(glht.glht)
