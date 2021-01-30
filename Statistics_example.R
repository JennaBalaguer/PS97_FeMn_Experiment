
### Jenna Balaguer  _ 01120
### PS97 _ POLASTERN 2016 _ STATION BIO1
install.packages('Hmisc')
install.packages('agricolae')
library('agricolae')
library('Hmisc')

#FC_GR_BIO1 : name of the file to upload

### Flow Cytometry - _ STATION BIO1
  ### GROWTH RATE ON P1

Cont1= as.numeric(FC_GR_BIO1[5,2:7])
Cont1_mean=mean(Cont1)
Cont1_sd = sd(Cont1)

Mn1=as.numeric(FC_GR_BIO1[8,2:7])
Mn1_mean=mean(Mn1)
Mn1_sd=sd(Mn1)

Fe1=as.numeric(FC_GR_BIO1[11,2:7])
Fe1_mean=mean(Fe1)
Fe1_sd=sd(Fe1)

FeMn1=as.numeric(FC_GR_BIO1[14,2:7])
FeMn1_mean=mean(FeMn1)
FeMn1_sd=sd(FeMn1)

mean1=c(Cont1_mean,Mn1_mean,Fe1_mean,FeMn1_mean)
sd1=c(Cont1_sd,Mn1_sd,Fe1_sd,FeMn1_sd)

### GROWTH RATE ON P2


Cont2= as.numeric(FC_GR_BIO1[20,2:7])
Cont2_mean=mean(Cont2)
Cont2_sd = sd(Cont2)

Mn2=as.numeric(FC_GR_BIO1[23,2:7])
Mn2_mean=mean(Mn2)
Mn2_sd=sd(Mn2)

Fe2=as.numeric(FC_GR_BIO1[26,2:7])
Fe2_mean=mean(Fe2)
Fe2_sd=sd(Fe2)

FeMn2=as.numeric(FC_GR_BIO1[29,2:7])
FeMn2_mean=mean(FeMn2)
FeMn2_sd=sd(FeMn2)

mean2=c(Cont2_mean,Mn2_mean,Fe2_mean,FeMn2_mean)
sd2=c(Cont2_sd,Mn2_sd,Fe2_sd,FeMn2_sd)

### GROWTH RATE ON P3

Cont3= as.numeric(FC_GR_BIO1[36,2:7])
Cont3_mean=mean(Cont3)
Cont3_sd = sd(Cont3)

Mn3=as.numeric(FC_GR_BIO1[39,2:7])
Mn3_mean=mean(Mn3)
Mn3_sd=sd(Mn3)

Fe3=as.numeric(FC_GR_BIO1[42,2:7])
Fe3_mean=mean(Fe3)
Fe3_sd=sd(Fe3)

FeMn3=as.numeric(FC_GR_BIO1[45,2:7])
FeMn3_mean=mean(FeMn3)
FeMn3_sd=sd(FeMn3)


mean3=c(Cont3_mean,Mn3_mean,Fe3_mean,FeMn3_mean)
sd3=c(Cont3_sd,Mn3_sd,Fe3_sd,FeMn3_sd)

########### Plots

######

#1=P1
#2=P2
#3=P3

### MEAN

m_control = c(Cont1_mean,Cont2_mean,Cont3_mean)
m_mn = c(Mn1_mean,Mn2_mean,Mn3_mean)
m_fe = c(Fe1_mean,Fe2_mean,Fe3_mean)
m_femn = c(FeMn1_mean,FeMn2_mean,FeMn3_mean)


### SD  
sd_control = c(Cont1_sd,Cont2_sd,Cont3_sd)
sd_mn = c(Mn1_sd,Mn2_sd,Mn3_sd)
sd_fe = c(Fe1_sd,Fe2_sd,Fe3_sd)
sd_femn = c(FeMn1_sd,FeMn2_sd,FeMn3_sd)

######

moy <-matrix(c(m_control,m_mn,m_fe,m_femn),nrow=3, ncol=4)

sd <--matrix(c(sd_control,sd_mn,sd_fe,sd_femn),nrow=3, ncol=4)



# Separation plots by size

bp=barplot(moy, beside=T, col=c("#CCCCCC","#666666","#000000"), ylim=c(-0.15,0.2))
arrows(bp,moy-sd,bp, moy+sd, lwd=1.5, angle=90,length=0.1,code=3)
legend("topleft", legend = c("", "",""), col = c("#CCCCCC","#666666","#000000"),pch = 15, bty = "n", pt.cex = 3, cex = 1.5)



########### Statistics

# Mean vector

y1= c(Cont1,Mn1, Fe1,FeMn1)
y2= c(Cont2,Mn2, Fe2,FeMn2)
y3= c(Cont3,Mn3, Fe3,FeMn3)

n=rep(6,4)

# Separation of the different treatments 

group=c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4) 

# Creating the table
data1 = data.frame(y1 = y1, group= factor(group))
data2 = data.frame(y2 = y2, group= factor(group))
data3 = data.frame(y3 = y3, group= factor(group))


fit1 = lm(y1 ~ group, data1)
fit2 = lm(y2 ~ group, data2)
fit3 = lm(y3 ~ group, data3)


res.aov1 <- aov(y1 ~ group, data = data1)
res.aov2 <- aov(y2 ~ group, data = data2)
res.aov3 <- aov(y3 ~ group, data = data3)


tukey.test21 <- HSD.test(res.aov1, trt = 'group')
tukey.test21
TukeyHSD(res.aov1)
pairwise.t.test(data1$y1, data1$group, p.adjust.method = "BH")
pairwise.t.test(data1$y1, data1$group, p.adjust.method = "bonferroni")



tukey.test22 <- HSD.test(res.aov2, trt = 'group')
tukey.test22
TukeyHSD(res.aov2)
pairwise.t.test(data2$y2, data2$group, p.adjust.method = "BH")
pairwise.t.test(data2$y2, data2$group, p.adjust.method = "bonferroni")



tukey.test23 <- HSD.test(res.aov3, trt = 'group')
tukey.test23
TukeyHSD(res.aov3)
pairwise.t.test(data3$y3, data3$group, p.adjust.method = "BH")
pairwise.t.test(data3$y3, data3$group, p.adjust.method = "bonferroni")


