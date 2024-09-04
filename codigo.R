library(MASS)
library(tidyverse)
library(readxl)
library(DescTools)
#library(xlsx)
library(gridExtra)
#library(alr3)
#library(fpp2)
library(lubridate)
library(scales)
library(ggthemes)
library(reshape2)
#library(zoo)
library(matrixStats)
library(nortest)
#library(googlesheets)
library(prettyunits)
library(utils)
library(qcc)
library(disk.frame)
library(arrow)
library(dbscan)
library(forecast)


#Dados
x12 <- 0.5i
x13 <- 0.2i
x23 <- 0.25i

v1 <-1
t1<-0 #theta
v2<-1
p2_esp<- 60/100
p3_esp<- 80/100
q3_esp<-60/100

#flat start
t2 =0
v3= 0
t3= 0
q2=0 #barra PV

#reatancia para admitancia
yy12<- 1/x12
yy13<-1/x13
yy23 <- 1/x23

#matriz de admitancia

y11<- yy12 + yy13
y12 =y21 <- (-yy12)
y13 = y31<- (-yy13)
y22<- (yy12+yy23)
y23=y32 <- (-yy23)
y33<- yy13+yy23


#deltas

dp2 <- p2_esp - p2_calc
dp3 <- p3_esp - p3_calc
dq3 <- q3_esp - q3_calc

#p_calc <- somatorio(vk*vn)*(-ykn*sin(tn-tk))
p2_calc <- ((v2*v1)*(-y21*sin(t1-t2)))+((v2*v3)*(-y23*sin(t3-t2)))
p3_calc <- ((v3*v1)*(-y31*sin(t1-t3)))+((v3*v1)*(-y31*sin(t1-t3)))
q3_calc <- (-(v3)^2)*y33 - (((v3*v1)*(y31*sin(t1-t3)))+((v3*v1)*(y31*sin(t1-t3))))


#jacobianos

H22<- (-q2_calc-(v2^2)*y22)
H23<- (-(v2*v3)*(y23*cos(t3-t2)))
H33<- (-q3_calc-(v3^2)*y33)
H32=H23

N22<- p2_calc
N23 <- (v2*v3)*(-y23*sin(t3-t2))

M23 <-(v2*v3)*(y23*sin(t3-t2))
M22<- p2_calc

L22<- q2_calc-((v2^2)*y22)

jacobiano<-matrix(c(H22,H23,N22,H32,H33,N32,M22,M23,L22),byrow = T)