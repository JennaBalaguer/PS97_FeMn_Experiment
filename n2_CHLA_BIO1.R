### Jenna Balaguer  _ 120919
### PS97 _ POLASTERN 2016 _ STATION BIO1 

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
# 3 = total 

###########################################################
###########################################################
####################### Chla ##############################
###########################################################
###########################################################

initial_21=mean(0.01)
initial_22=mean(c(0.048,0.032))
initial_23=mean(c(0.058,0.042))



Control_1=c((log(0.162)-log(initial_21))/15,(log(0.144)-log(initial_21))/15,(log(0.176)-log(initial_21))/15)
m_Control_1=mean(Control_1)
sd_Control_1=sd(Control_1)


Control_2=c((log(0.224)-log(initial_22))/15,(log(0.208)-log(initial_22))/15,(log(0.256)-log(initial_22))/15)
m_Control_2=mean(Control_2)
sd_Control_2=sd(Control_2)


Control_3=c((log(0.386)-log(initial_23))/15,(log(0.352)-log(initial_23))/15,(log(0.432)-log(initial_23))/15)
m_Control_3=mean(Control_3)
sd_Control_3=sd(Control_3)

Fe_1=c((log(1.28)-log(initial_21))/14,(log(1.488)-log(initial_21))/14,(log(1.536)-log(initial_21))/14)
m_Fe_1=mean(Fe_1)
sd_Fe_1=sd(Fe_1)

Fe_2=c((log(2.048)-log(initial_22))/14,(log(2.028)-log(initial_22))/14,(log(2.304)-log(initial_22))/14)
m_Fe_2=mean(Fe_2)
sd_Fe_2=sd(Fe_2)

Fe_3=c((log(3.328)-log(initial_23))/14,(log(3.536)-log(initial_23))/14,(log(3.84)-log(initial_23))/14)
m_Fe_3=mean(Fe_3)
sd_Fe_3=sd(Fe_3)


Mn_1=c((log(0.048)-log(initial_21))/14.3,(log(0.016)-log(initial_21))/14.3,(log(0.016)-log(initial_21))/14.3)
m_Mn_1=mean(Mn_1)
sd_Mn_1=sd(Mn_1)

Mn_2=c((log(0.464)-log(initial_22))/14.3,(log(0.224)-log(initial_22))/14.3,(log(0.24)-log(initial_22))/14.3)
m_Mn_2=mean(Mn_2)
sd_Mn_2=sd(Mn_2)

Mn_3=c((log(0.512)-log(initial_23))/14.3,(log(0.24)-log(initial_23))/14.3,(log(0.256)-log(initial_23))/14.3)
m_Mn_3=mean(Mn_3)
sd_Mn_3=sd(Mn_3)


FeMn_1=c((log(0.752)-log(initial_21))/14,(log(0.096)-log(initial_21))/14,(log(0.128)-log(initial_21))/14)
m_FeMn_1=mean(FeMn_1)
sd_FeMn_1=sd(FeMn_1)

FeMn_2=c((log(1.712)-log(initial_22))/14,(log(1.968)-log(initial_22))/14,(log(2)-log(initial_22))/14)
m_FeMn_2=mean(FeMn_2)
sd_FeMn_2=sd(FeMn_2)

FeMn_3=c((log(2.464)-log(initial_23))/14,(log(2.064)-log(initial_23))/14,(log(2.128)-log(initial_23))/14)
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

###

  # modif 21/10/19
Mtot=c(m_Control_3,m_Mn_3,m_Fe_3,m_FeMn_3)
Msmall=c(m_Control_1,m_Mn_1,m_Fe_1,m_FeMn_1)   
Mbig=c(m_Control_2,m_Mn_2,m_Fe_2,m_FeMn_2) 

SDtot=c(sd_Control_3,sd_Mn_3,sd_Fe_3,sd_FeMn_3)
SDsmall=c(sd_Control_1,sd_Mn_1,sd_Fe_1,sd_FeMn_1)   
SDbig=c(sd_Control_2,sd_Mn_2,sd_Fe_2,sd_FeMn_2) 

###


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
#n = rep(3, 4)

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

# small
tukey.test21 <- HSD.test(res.aov1, trt = 'group')
tukey.test21
i=TukeyHSD(res.aov1)
i=matrix(i)
#big
tukey.test22 <- HSD.test(res.aov2, trt = 'group')
tukey.test22
TukeyHSD(res.aov2)
#total
tukey.test23 <- HSD.test(res.aov3, trt = 'group')
tukey.test23
TukeyHSD(res.aov3)
pairwise.t.test(data3$y3, data3$group, p.adjust.method = "BH")
pairwise.t.test(data3$y3, data3$group, p.adjust.method = "bonferroni")
