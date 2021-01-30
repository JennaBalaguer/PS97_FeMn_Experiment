
### Jenna Balaguer  _ 031220
### PS97 _ POLASTERN 2016 _ STATIONS

# GF1/BIO1/GF2/GF3/GF4/GF5/GF6/GF7/BIO2/BIO3/GF8

dFe=c(2.88,0.05,NaN,0.50,4.51,2.93,1.53,0.89,2.99,0.20,3.21)
 dMn=c(0.36,0.15,1.52,0.39,6.92,5.46,1.53,1.27,1.21,0.08,0.31)
dR=dFe/dMn

FvFm=c(0.18,0.29,0.27,0.21,0.47,0.52,0.38,0.37,0.36,0.18,0.12)
 


par(mfrow=c(1,3))

plot(dFe,FvFm,ylab="",xlab="",cex.axis=1.5,xlim=c(0,7),ylim=c(0,0.55))
abline(lm(FvFm~dFe),col = "red")
cor1=cor.test(dFe, FvFm, method=c("pearson"))
l1=summary(lm(FvFm~dFe))


plot(dMn,FvFm,ylab="",xlab="",cex.axis=1.5,xlim=c(0,7),ylim=c(0,0.55))
abline(lm(FvFm~dMn),col = "red")
cor2=cor.test(dMn, FvFm, method=c("pearson"))
l2=summary(lm(FvFm~dMn))

plot(dR,FvFm,ylab="",xlab="",cex.axis=1.5,xlim=c(0,11),ylim=c(0,0.55))
abline(lm(FvFm~dR),col = "red")
cor3=cor.test(dR, FvFm, method=c("pearson"))
l3=summary(lm(FvFm~dR))