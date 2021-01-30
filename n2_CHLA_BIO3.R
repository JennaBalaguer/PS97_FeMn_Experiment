### Jenna Balaguer  _ 120919
### PS97 _ POLASTERN 2016 _ STATION BIO3

install.packages('Hmisc')
library('Hmisc')
install.packages('agricolae')
library('agricolae')

###########################################################
###########################################################
###########################################################

# Legend
# 1 = <2um
# 2 = >2um

###########################################################
###########################################################
####################### Chla ##############################
###########################################################
###########################################################
standard_21 = mean(c(0.01))
standard_22 = mean(c(0.096,0.096))
standard_23 = mean(c(0.106,0.106))


Control_1=c((log(0.11)-log(standard_21))/15.6,(log(0.095)-log(standard_21))/15.6,(log(0.105)-log(standard_21))/15.6)
m_Control_1=mean(Control_1)
sd_Control_1=sd(Control_1)

Control_2=c((log(0.726)-log(standard_22))/15.6,(log(0.732)-log(standard_22))/15.6,(log(0.606)-log(standard_22))/15.6)
m_Control_2=mean(Control_2)
sd_Control_2=sd(Control_2)

Control_3=c((log(0.836)-log(standard_23))/15.6,(log(0.827)-log(standard_23))/15.6,(log(0.711)-log(standard_23))/15.6)
m_Control_3=mean(Control_3)
sd_Control_3=sd(Control_3)


Fe_1=c((log(0.378)-log(standard_21))/14.6,(log(0.156)-log(standard_21))/14.6,(log(0.228)-log(standard_21))/14.6)
m_Fe_1=mean(Fe_1)
sd_Fe_1=sd(Fe_1)

Fe_2=c((log(4.434)-log(standard_22))/14.6,(log(4.668)-log(standard_22))/14.6,(log(3.906)-log(standard_22))/14.6)
m_Fe_2=mean(Fe_2)
sd_Fe_2=sd(Fe_2)

Fe_3=c((log(4.812)-log(standard_23))/14.6,(log(4.824)-log(standard_23))/14.6,(log(4.134)-log(standard_23))/14.6)
m_Fe_3=mean(Fe_3)
sd_Fe_3=sd(Fe_3)


Mn_1=c((log(0.012)-log(standard_21))/15.6,(log(0.01)-log(standard_21))/15.6,(log(0.01)-log(standard_21))/15.6)
m_Mn_1=mean(Mn_1)
sd_Mn_1=sd(Mn_1)

Mn_2=c((log(0.402)-log(standard_22))/15.6,(log(0.666)-log(standard_22))/15.6,(log(0.432)-log(standard_22))/15.6)
m_Mn_2=mean(Mn_2)
sd_Mn_2=sd(Mn_2)

Mn_3=c((log(0.414)-log(standard_23))/15.6,(log(0.672)-log(standard_23))/15.6,(log(0.432)-log(standard_23))/15.6)
m_Mn_3=mean(Mn_3)
sd_Mn_3=sd(Mn_3)


FeMn_1=c((log(2.58)-log(standard_21))/14.6,(log(0.942)-log(standard_21))/14.6,(log(2.55)-log(standard_21))/14.6)
m_FeMn_1=mean(FeMn_1)
sd_FeMn_1=sd(FeMn_1)

FeMn_2=c((log(4.074)-log(standard_22))/14.6,(log(4.32)-log(standard_22))/14.6,(log(4.308)-log(standard_22))/14.6)
m_FeMn_2=mean(FeMn_2)
sd_FeMn_2=sd(FeMn_2)

FeMn_3=c((log(6.654)-log(standard_23))/14.6,(log(5.262)-log(standard_23))/14.6,(log(6.858)-log(standard_23))/14.6)
m_FeMn_3=mean(FeMn_3)
sd_FeMn_3=sd(FeMn_3)


######



### MEAN

m_control = c(m_Control_3,m_Control_1,m_Control_2)
m_fe = c(m_Fe_3,m_Fe_1,m_Fe_2)
m_mn = c(m_Mn_3,m_Mn_1,m_Mn_2)
m_femn = c(m_FeMn_3,m_FeMn_1,m_FeMn_2)


### SD  
sd_control = c(sd_Control_3,sd_Control_1,sd_Control_2)
sd_fe = c(sd_Fe_3,sd_Fe_1,sd_Fe_2)
sd_mn = c(sd_Mn_3,sd_Mn_1,sd_Mn_2)
sd_femn = c(sd_FeMn_3,sd_FeMn_1,sd_FeMn_2)


# modif 21/10/19
Mtot=c(m_Control_3,m_Mn_3,m_Fe_3,m_FeMn_3)
Msmall=c(m_Control_1,m_Mn_1,m_Fe_1,m_FeMn_1)   
Mbig=c(m_Control_2,m_Mn_2,m_Fe_2,m_FeMn_2) 

SDtot=c(sd_Control_3,sd_Mn_3,sd_Fe_3,sd_FeMn_3)
SDsmall=c(sd_Control_1,sd_Mn_1,sd_Fe_1,sd_FeMn_1)   
SDbig=c(sd_Control_2,sd_Mn_2,sd_Fe_2,sd_FeMn_2) 

### PLOTS 


moy <-matrix(c(m_control,m_mn,m_fe,m_femn),nrow=3, ncol=4)

sd <--matrix(c(sd_control,sd_mn,sd_fe,sd_femn),nrow=3, ncol=4)

#type = c("Control","+ Mn","+ Fe","+FeMn")
#colnames(moy) = type
moy


# Separation plots by size

bp=barplot(moy, beside=T, col=c("#000000","#CCCCCC","#666666"), ylim=c(0,0.4))
arrows(bp,moy-sd,bp, moy+sd, lwd=1.5, angle=90,length=0.1,code=3)
legend("topleft", legend = c("", "",""), col = c("#000000","#CCCCCC","#666666"),pch = 15, bty = "n", pt.cex = 3, cex = 1.5)

### ANOVA 


# Mean vector
y1= c(Control_1,Mn_1, Fe_1,FeMn_1)
y2= c(Control_2,Mn_2, Fe_2,FeMn_2)
y3= c(Control_3,Mn_3, Fe_1,FeMn_3)
n = rep(3, 4)

# Separation of the different treatments 
group=c(1,1,1,2,2,2,3,3,3,4,4,4) 

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

tukey.test22 <- HSD.test(res.aov2, trt = 'group')
tukey.test22
TukeyHSD(res.aov2)

tukey.test23 <- HSD.test(res.aov3, trt = 'group')
tukey.test23
TukeyHSD(res.aov3)
pairwise.t.test(data3$y3, data3$group, p.adjust.method = "BH")
pairwise.t.test(data3$y3, data3$group, p.adjust.method = "bonferroni")

