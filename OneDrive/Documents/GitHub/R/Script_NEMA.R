

nemaforense <- read.table("dados.csv", header=TRUE, sep=";", dec=".", strip.white=TRUE)
attach(nemaforense)

density <- read.table("density_R.csv", header= T, sep = ";", dec=".", strip.white = T)
attach(density)

library(lattice)
library(vegan)
library(car)
library(iNEXT)

nema=subset(nemaforense, select=c("Tratamento","Replica","Dia","Familia"))

#Vetores para Porcos e os dias
PD01 <- subset(nema, Tratamento=="Porco" & Dia==1)
PD02 <- subset(nema, Tratamento=="Porco" & Dia==2)
PD03 <- subset(nema, Tratamento=="Porco" & Dia==3)
PD04 <- subset(nema, Tratamento=="Porco" & Dia==4)
PD05 <- subset(nema, Tratamento=="Porco" & Dia==5)
PD06 <- subset(nema, Tratamento=="Porco" & Dia==6)
PD07 <- subset(nema, Tratamento=="Porco" & Dia==7)

#Vetores para Porco 1 e os dias
P01D01 <- subset(nema, Tratamento=="Porco" & Replica==1 & Dia==1)
P01D02 <- subset(nema, Tratamento=="Porco" & Replica==1 & Dia==2)
P01D03 <- subset(nema, Tratamento=="Porco" & Replica==1 & Dia==3)
P01D04 <- subset(nema, Tratamento=="Porco" & Replica==1 & Dia==4)
P01D05 <- subset(nema, Tratamento=="Porco" & Replica==1 & Dia==5)
P01D06 <- subset(nema, Tratamento=="Porco" & Replica==1 & Dia==6)
P01D07 <- subset(nema, Tratamento=="Porco" & Replica==1 & Dia==7)

#Vetores para Porco 2 e os dias
P02D01 <- subset(nema, Tratamento=="Porco" & Replica==2 & Dia==1)
P02D02 <- subset(nema, Tratamento=="Porco" & Replica==2 & Dia==2)
P02D03 <- subset(nema, Tratamento=="Porco" & Replica==2 & Dia==3)
P02D04 <- subset(nema, Tratamento=="Porco" & Replica==2 & Dia==4)
P02D05 <- subset(nema, Tratamento=="Porco" & Replica==2 & Dia==5)
P02D06 <- subset(nema, Tratamento=="Porco" & Replica==2 & Dia==6)
P02D07 <- subset(nema, Tratamento=="Porco" & Replica==2 & Dia==7)

#Vetores para Porco 3 e os dias
P03D01 <- subset(nema, Tratamento=="Porco" & Replica==4 & Dia==1)
P03D02 <- subset(nema, Tratamento=="Porco" & Replica==4 & Dia==2)
P03D03 <- subset(nema, Tratamento=="Porco" & Replica==4 & Dia==3)
P03D04 <- subset(nema, Tratamento=="Porco" & Replica==4 & Dia==4)
P03D05 <- subset(nema, Tratamento=="Porco" & Replica==4 & Dia==5)
P03D06 <- subset(nema, Tratamento=="Porco" & Replica==4 & Dia==6)
P03D07 <- subset(nema, Tratamento=="Porco" & Replica==4 & Dia==7)

#Vetores para Porco 4 e os dias
P04D01 <- subset(nema, Tratamento=="Porco" & Replica==4 & Dia==1)
P04D02 <- subset(nema, Tratamento=="Porco" & Replica==4 & Dia==2)
P04D03 <- subset(nema, Tratamento=="Porco" & Replica==4 & Dia==3)
P04D04 <- subset(nema, Tratamento=="Porco" & Replica==4 & Dia==4)
P04D05 <- subset(nema, Tratamento=="Porco" & Replica==4 & Dia==5)
P04D06 <- subset(nema, Tratamento=="Porco" & Replica==4 & Dia==6)
P04D07 <- subset(nema, Tratamento=="Porco" & Replica==4 & Dia==7)

#Vetores para Controles e os dias
CD01 <- subset(nema, Tratamento=="Controle" & Dia==1)
CD02 <- subset(nema, Tratamento=="Controle" & Dia==2)
CD03 <- subset(nema, Tratamento=="Controle" & Dia==3)
CD04 <- subset(nema, Tratamento=="Controle" & Dia==4)
CD05 <- subset(nema, Tratamento=="Controle" & Dia==5)
CD06 <- subset(nema, Tratamento=="Controle" & Dia==6)
CD07 <- subset(nema, Tratamento=="Controle" & Dia==7)

#Vetores para Controle 1 e os dias
C01D01 <- subset(nema, Tratamento=="Controle" & Replica==1 & Dia==1)
C01D02 <- subset(nema, Tratamento=="Controle" & Replica==1 & Dia==2)
C01D03 <- subset(nema, Tratamento=="Controle" & Replica==1 & Dia==3)
C01D04 <- subset(nema, Tratamento=="Controle" & Replica==1 & Dia==4)
C01D05 <- subset(nema, Tratamento=="Controle" & Replica==1 & Dia==5)
C01D06 <- subset(nema, Tratamento=="Controle" & Replica==1 & Dia==6)
C01D07 <- subset(nema, Tratamento=="Controle" & Replica==1 & Dia==7)

#Vetores para Controle 2 e os dias
C02D01 <- subset(nema, Tratamento=="Controle" & Replica==2 & Dia==1)
C02D02 <- subset(nema, Tratamento=="Controle" & Replica==2 & Dia==2)
C02D03 <- subset(nema, Tratamento=="Controle" & Replica==2 & Dia==3)
C02D04 <- subset(nema, Tratamento=="Controle" & Replica==2 & Dia==4)
C02D05 <- subset(nema, Tratamento=="Controle" & Replica==2 & Dia==5)
C02D06 <- subset(nema, Tratamento=="Controle" & Replica==2 & Dia==6)
C02D07 <- subset(nema, Tratamento=="Controle" & Replica==2 & Dia==7)

#Vetores para Controle 3 e os dias
C03D01 <- subset(nema, Tratamento=="Controle" & Replica==3 & Dia==1)
C03D02 <- subset(nema, Tratamento=="Controle" & Replica==3 & Dia==2)
C03D03 <- subset(nema, Tratamento=="Controle" & Replica==3 & Dia==3)
C03D04 <- subset(nema, Tratamento=="Controle" & Replica==3 & Dia==4)
C03D05 <- subset(nema, Tratamento=="Controle" & Replica==3 & Dia==5)
C03D06 <- subset(nema, Tratamento=="Controle" & Replica==3 & Dia==6)
C03D07 <- subset(nema, Tratamento=="Controle" & Replica==3 & Dia==7)

#Vetores para Controle 4 e os dias
C04D01 <- subset(nema, Tratamento=="Controle" & Replica==4 & Dia==1)
C04D02 <- subset(nema, Tratamento=="Controle" & Replica==4 & Dia==2)
C04D03 <- subset(nema, Tratamento=="Controle" & Replica==4 & Dia==3)
C04D04 <- subset(nema, Tratamento=="Controle" & Replica==4 & Dia==4)
C04D05 <- subset(nema, Tratamento=="Controle" & Replica==4 & Dia==5)
C04D06 <- subset(nema, Tratamento=="Controle" & Replica==4 & Dia==6)
C04D07 <- subset(nema, Tratamento=="Controle" & Replica==4 & Dia==7)

#Vetores para Fakes e os dias
FD01 <- subset(nema, Tratamento=="Fake" & Dia==1)
FD02 <- subset(nema, Tratamento=="Fake" & Dia==2)
FD03 <- subset(nema, Tratamento=="Fake" & Dia==3)
FD04 <- subset(nema, Tratamento=="Fake" & Dia==4)
FD05 <- subset(nema, Tratamento=="Fake" & Dia==5)
FD06 <- subset(nema, Tratamento=="Fake" & Dia==6)
FD07 <- subset(nema, Tratamento=="Fake" & Dia==7)

#Vetores para Fake 1 e os dias
F01D01 <- subset(nema, Tratamento=="Fake" & Replica==1 & Dia==1)
F01D02 <- subset(nema, Tratamento=="Fake" & Replica==1 & Dia==2)
F01D03 <- subset(nema, Tratamento=="Fake" & Replica==1 & Dia==3)
F01D04 <- subset(nema, Tratamento=="Fake" & Replica==1 & Dia==4)
F01D05 <- subset(nema, Tratamento=="Fake" & Replica==1 & Dia==5)
F01D06 <- subset(nema, Tratamento=="Fake" & Replica==1 & Dia==6)
F01D07 <- subset(nema, Tratamento=="Fake" & Replica==1 & Dia==7)

#Vetores para Fake 2 e os dias
F02D01 <- subset(nema, Tratamento=="Fake" & Replica==2 & Dia==1)
F02D02 <- subset(nema, Tratamento=="Fake" & Replica==2 & Dia==2)
F02D03 <- subset(nema, Tratamento=="Fake" & Replica==2 & Dia==3)
F02D04 <- subset(nema, Tratamento=="Fake" & Replica==2 & Dia==4)
F02D05 <- subset(nema, Tratamento=="Fake" & Replica==2 & Dia==5)
F02D06 <- subset(nema, Tratamento=="Fake" & Replica==2 & Dia==6)
F02D07 <- subset(nema, Tratamento=="Fake" & Replica==2 & Dia==7)

#Vetores para Fake 3 e os dias
F03D01 <- subset(nema, Tratamento=="Fake" & Replica==3 & Dia==1)
F03D02 <- subset(nema, Tratamento=="Fake" & Replica==3 & Dia==2)
F03D03 <- subset(nema, Tratamento=="Fake" & Replica==3 & Dia==3)
F03D04 <- subset(nema, Tratamento=="Fake" & Replica==3 & Dia==4)
F03D05 <- subset(nema, Tratamento=="Fake" & Replica==3 & Dia==5)
F03D06 <- subset(nema, Tratamento=="Fake" & Replica==3 & Dia==6)
F03D07 <- subset(nema, Tratamento=="Fake" & Replica==3 & Dia==7)

#Vetores para Fake 4 e os dias
F04D01 <- subset(nema, Tratamento=="Fake" & Replica==4 & Dia==1)
F04D02 <- subset(nema, Tratamento=="Fake" & Replica==4 & Dia==2)
F04D03 <- subset(nema, Tratamento=="Fake" & Replica==4 & Dia==3)
F04D04 <- subset(nema, Tratamento=="Fake" & Replica==4 & Dia==4)
F04D05 <- subset(nema, Tratamento=="Fake" & Replica==4 & Dia==5)
F04D06 <- subset(nema, Tratamento=="Fake" & Replica==4 & Dia==6)
F04D07 <- subset(nema, Tratamento=="Fake" & Replica==4 & Dia==7)

##Densidade Familias
Density.family <-summary(P01D01$Familia)
#Densidade das Familias dos Porcos 
D_PD01 <- data.frame(summary(PD01$Familia))
D_PD02 <- data.frame(summary(PD02$Familia))
D_PD03 <- data.frame(summary(PD03$Familia))
D_PD04 <- data.frame(summary(PD04$Familia))
D_PD05 <- data.frame(summary(PD05$Familia))
D_PD06 <- data.frame(summary(PD06$Familia))
D_PD07 <- data.frame(summary(PD07$Familia))

#Densidade das Familias no Porco 01
D_P01D01 <- data.frame(summary(P01D01$Familia))
D_P01D02 <- data.frame(summary(P01D02$Familia))
D_P01D03 <- data.frame(summary(P01D03$Familia))
D_P01D04 <- data.frame(summary(P01D04$Familia))
D_P01D05 <- data.frame(summary(P01D05$Familia))
D_P01D06 <- data.frame(summary(P01D06$Familia))
D_P01D07 <- data.frame(summary(P01D07$Familia))

#Densidade das Familias no Porco 02
D_P02D01 <- data.frame(summary(P01D01$Familia))
D_P02D02 <- data.frame(summary(P02D02$Familia))
D_P02D03 <- data.frame(summary(P02D03$Familia))
D_P02D04 <- data.frame(summary(P02D04$Familia))
D_P02D05 <- data.frame(summary(P02D05$Familia))
D_P02D06 <- data.frame(summary(P02D06$Familia))
D_P02D07 <- data.frame(summary(P02D07$Familia))

#Densidade das Familias no Porco 03
D_P03D01 <- data.frame(summary(P03D01$Familia))
D_P03D02 <- data.frame(summary(P03D02$Familia))
D_P03D03 <- data.frame(summary(P03D03$Familia))
D_P03D04 <- data.frame(summary(P03D04$Familia))
D_P03D05 <- data.frame(summary(P03D05$Familia))
D_P03D06 <- data.frame(summary(P03D06$Familia))
D_P03D07 <- data.frame(summary(P03D07$Familia))

#Densidade das Familias no Porco 04
D_P04D01 <- data.frame(summary(P04D01$Familia))
D_P04D02 <- data.frame(summary(P04D02$Familia))
D_P04D03 <- data.frame(summary(P04D03$Familia))
D_P04D04 <- data.frame(summary(P04D04$Familia))
D_P04D05 <- data.frame(summary(P04D05$Familia))
D_P04D06 <- data.frame(summary(P04D06$Familia))
D_P04D07 <- data.frame(summary(P04D07$Familia))

#Densidade das Familias nos Controles 
D_CD01 <- data.frame(summary(CD01$Familia))
D_CD02 <- data.frame(summary(CD02$Familia))
D_CD03 <- data.frame(summary(CD03$Familia))
D_CD04 <- data.frame(summary(CD04$Familia))
D_CD05 <- data.frame(summary(CD05$Familia))
D_CD06 <- data.frame(summary(CD06$Familia))
D_CD07 <- data.frame(summary(CD07$Familia))

#Densidade das familias no Controle 01
D_C01D01 <- data.frame(summary(C01D01$Familia))
D_C01D02 <- data.frame(summary(C01D02$Familia))
D_C01D03 <- data.frame(summary(C01D03$Familia))
D_C01D04 <- data.frame(summary(C01D04$Familia))
D_C01D05 <- data.frame(summary(C01D05$Familia))
D_C01D06 <- data.frame(summary(C01D06$Familia))
D_C01D07 <- data.frame(summary(C01D07$Familia))

##Densidade das familias no Controle 02
D_C02D01 <- data.frame(summary(C02D01$Familia))
D_C02D02 <- data.frame(summary(C02D02$Familia))
D_C02D03 <- data.frame(summary(C02D03$Familia))
D_C02D04 <- data.frame(summary(C02D04$Familia))
D_C02D05 <- data.frame(summary(C02D05$Familia))
D_C02D06 <- data.frame(summary(C02D06$Familia))
D_C02D07 <- data.frame(summary(C02D07$Familia))

#Densidade das familias no Controle 03
D_C03D01 <- data.frame(summary(C03D01$Familia))
D_C03D02 <- data.frame(summary(C03D02$Familia))
D_C03D03 <- data.frame(summary(C03D03$Familia))
D_C03D04 <- data.frame(summary(C03D04$Familia))
D_C03D05 <- data.frame(summary(C03D05$Familia))
D_C03D06 <- data.frame(summary(C03D06$Familia))
D_C03D07 <- data.frame(summary(C03D07$Familia))

#Densidade das familias no Controle 04
D_C04D01 <- data.frame(summary(C04D01$Familia))
D_C04D02 <- data.frame(summary(C04D02$Familia))
D_C04D03 <- data.frame(summary(C04D03$Familia))
D_C04D04 <- data.frame(summary(C04D04$Familia))
D_C04D05 <- data.frame(summary(C04D05$Familia))
D_C04D06 <- data.frame(summary(C04D06$Familia))
D_C04D07 <- data.frame(summary(C04D07$Familia))

#Densidade das familias nos Fakes
D_FD01 <- data.frame(summary(FD01$Familia))
D_FD02 <- data.frame(summary(FD02$Familia))
D_FD03 <- data.frame(summary(FD03$Familia))
D_FD04 <- data.frame(summary(FD04$Familia))
D_FD05 <- data.frame(summary(FD05$Familia))
D_FD06 <- data.frame(summary(FD06$Familia))
D_FD07 <- data.frame(summary(FD07$Familia))

#Densidade das familias no Fake 01
D_F01D01 <- data.frame(summary(F01D01$Familia))
D_F01D02 <- data.frame(summary(F01D02$Familia))
D_F01D03 <- data.frame(summary(F01D03$Familia))
D_F01D04 <- data.frame(summary(F01D04$Familia))
D_F01D05 <- data.frame(summary(F01D05$Familia))
D_F01D06 <- data.frame(summary(F01D06$Familia))
D_F01D07 <- data.frame(summary(F01D07$Familia))

#Densidade das familias no Fake 02
D_F02D01 <- data.frame(summary(F02D01$Familia))
D_F02D02 <- data.frame(summary(F02D02$Familia))
D_F02D03 <- data.frame(summary(F02D03$Familia))
D_F02D04 <- data.frame(summary(F02D04$Familia))
D_F02D05 <- data.frame(summary(F02D05$Familia))
D_F02D06 <- data.frame(summary(F02D06$Familia))
D_F02D07 <- data.frame(summary(F02D07$Familia))

#Densidade das familias no Fake 03
D_F03D01 <- data.frame(summary(F03D01$Familia))
D_F03D02 <- data.frame(summary(F03D02$Familia))
D_F03D03 <- data.frame(summary(F03D03$Familia))
D_F03D04 <- data.frame(summary(F03D04$Familia))
D_F03D05 <- data.frame(summary(F03D05$Familia))
D_F03D06 <- data.frame(summary(F03D06$Familia))
D_F03D07 <- data.frame(summary(F03D07$Familia))

#Densidade das familias no Fake 04
D_F04D01 <- data.frame(summary(F04D01$Familia))
D_F04D02 <- data.frame(summary(F04D02$Familia))
D_F04D03 <- data.frame(summary(F04D03$Familia))
D_F04D04 <- data.frame(summary(F04D04$Familia))
D_F04D05 <- data.frame(summary(F04D05$Familia))
D_F04D06 <- data.frame(summary(F04D06$Familia))
D_F04D07 <- data.frame(summary(F04D07$Familia))

##Frequencia

#Frequencia das Familias dos Porcos 
FF_PD01 <- data.frame(table(PD01$Familia))
FF_PD02 <- data.frame(table(PD02$Familia))
FF_PD03 <- data.frame(table(PD03$Familia))
FF_PD04 <- data.frame(table(PD04$Familia))
FF_PD05 <- data.frame(table(PD05$Familia))
FF_PD06 <- data.frame(table(PD06$Familia))
FF_PD07 <- data.frame(table(PD07$Familia))

#Frequencia das Familias no Porco 01
FF_P01D01 <- data.frame(table(P01D01$Familia))
FF_P01D02 <- data.frame(table(P01D02$Familia))
FF_P01D03 <- data.frame(table(P01D03$Familia))
FF_P01D04 <- data.frame(table(P01D04$Familia))
FF_P01D05 <- data.frame(table(P01D05$Familia))
FF_P01D06 <- data.frame(table(P01D06$Familia))
FF_P01D07 <- data.frame(table(P01D07$Familia))

#Frequencia das Familias no Porco 02
FF_P02D01 <- data.frame(table(P01D01$Familia))
FF_P02D02 <- data.frame(table(P02D02$Familia))
FF_P02D03 <- data.frame(table(P02D03$Familia))
FF_P02D04 <- data.frame(table(P02D04$Familia))
FF_P02D05 <- data.frame(table(P02D05$Familia))
FF_P02D06 <- data.frame(table(P02D06$Familia))
FF_P02D07 <- data.frame(table(P02D07$Familia))

#Frequencia das Familias no Porco 03
FF_P03D01 <- data.frame(table(P03D01$Familia))
FF_P03D02 <- data.frame(table(P03D02$Familia))
FF_P03D03 <- data.frame(table(P03D03$Familia))
FF_P03D04 <- data.frame(table(P03D04$Familia))
FF_P03D05 <- data.frame(table(P03D05$Familia))
FF_P03D06 <- data.frame(table(P03D06$Familia))
FF_P03D07 <- data.frame(table(P03D07$Familia))

#Frequencia das Familias no Porco 04
FF_P04D01 <- data.frame(table(P04D01$Familia))
FF_P04D02 <- data.frame(table(P04D02$Familia))
FF_P04D03 <- data.frame(table(P04D03$Familia))
FF_P04D04 <- data.frame(table(P04D04$Familia))
FF_P04D05 <- data.frame(table(P04D05$Familia))
FF_P04D06 <- data.frame(table(P04D06$Familia))
FF_P04D07 <- data.frame(table(P04D07$Familia))

#Frequencia das Familias nos Controles 
FF_CD01 <- data.frame(table(CD01$Familia))
FF_CD02 <- data.frame(table(CD02$Familia))
FF_CD03 <- data.frame(table(CD03$Familia))
FF_CD04 <- data.frame(table(CD04$Familia))
FF_CD05 <- data.frame(table(CD05$Familia))
FF_CD06 <- data.frame(table(CD06$Familia))
FF_CD07 <- data.frame(table(CD07$Familia))

#Frequencia das familias no Controle 01
FF_C01D01 <- data.frame(table(C01D01$Familia))
FF_C01D02 <- data.frame(table(C01D02$Familia))
FF_C01D03 <- data.frame(table(C01D03$Familia))
FF_C01D04 <- data.frame(table(C01D04$Familia))
FF_C01D05 <- data.frame(table(C01D05$Familia))
FF_C01D06 <- data.frame(table(C01D06$Familia))
FF_C01D07 <- data.frame(table(C01D07$Familia))

#Frequencia das familias no Controle 02
FF_C02D01 <- data.frame(table(C02D01$Familia))
FF_C02D02 <- data.frame(table(C02D02$Familia))
FF_C02D03 <- data.frame(table(C02D03$Familia))
FF_C02D04 <- data.frame(table(C02D04$Familia))
FF_C02D05 <- data.frame(table(C02D05$Familia))
FF_C02D06 <- data.frame(table(C02D06$Familia))
FF_C02D07 <- data.frame(table(C02D07$Familia))

#Frequencia das familias no Controle 03
FF_C03D01 <- data.frame(table(C03D01$Familia))
FF_C03D02 <- data.frame(table(C03D02$Familia))
FF_C03D03 <- data.frame(table(C03D03$Familia))
FF_C03D04 <- data.frame(table(C03D04$Familia))
FF_C03D05 <- data.frame(table(C03D05$Familia))
FF_C03D06 <- data.frame(table(C03D06$Familia))
FF_C03D07 <- data.frame(table(C03D07$Familia))

#Frequencia das familias no Controle 04
FF_C04D01 <- data.frame(table(C04D01$Familia))
FF_C04D02 <- data.frame(table(C04D02$Familia))
FF_C04D03 <- data.frame(table(C04D03$Familia))
FF_C04D04 <- data.frame(table(C04D04$Familia))
FF_C04D05 <- data.frame(table(C04D05$Familia))
FF_C04D06 <- data.frame(table(C04D06$Familia))
FF_C04D07 <- data.frame(table(C04D07$Familia))

#Frequencia das familias nos Fakes
FF_FD01 <- data.frame(table(FD01$Familia))
FF_FD02 <- data.frame(table(FD02$Familia))
FF_FD03 <- data.frame(table(FD03$Familia))
FF_FD04 <- data.frame(table(FD04$Familia))
FF_FD05 <- data.frame(table(FD05$Familia))
FF_FD06 <- data.frame(table(FD06$Familia))
FF_FD07 <- data.frame(table(FD07$Familia))

#Frequencia das familias no Fake 01
FF_F01D01 <- data.frame(table(F01D01$Familia))
FF_F01D02 <- data.frame(table(F01D02$Familia))
FF_F01D03 <- data.frame(table(F01D03$Familia))
FF_F01D04 <- data.frame(table(F01D04$Familia))
FF_F01D05 <- data.frame(table(F01D05$Familia))
FF_F01D06 <- data.frame(table(F01D06$Familia))
FF_F01D07 <- data.frame(table(F01D07$Familia))

#Frequencia das familias no Fake 02
FF_F02D01 <- data.frame(table(F02D01$Familia))
FF_F02D02 <- data.frame(table(F02D02$Familia))
FF_F02D03 <- data.frame(table(F02D03$Familia))
FF_F02D04 <- data.frame(table(F02D04$Familia))
FF_F02D05 <- data.frame(table(F02D05$Familia))
FF_F02D06 <- data.frame(table(F02D06$Familia))
FF_F02D07 <- data.frame(table(F02D07$Familia))

#Frequencia das familias no Fake 03
FF_F03D01 <- data.frame(table(F03D01$Familia))
FF_F03D02 <- data.frame(table(F03D02$Familia))
FF_F03D03 <- data.frame(table(F03D03$Familia))
FF_F03D04 <- data.frame(table(F03D04$Familia))
FF_F03D05 <- data.frame(table(F03D05$Familia))
FF_F03D06 <- data.frame(table(F03D06$Familia))
FF_F03D07 <- data.frame(table(F03D07$Familia))

#Frequencia das familias no Fake 04
FF_F04D01 <- data.frame(table(F04D01$Familia))
FF_F04D02 <- data.frame(table(F04D02$Familia))
FF_F04D03 <- data.frame(table(F04D03$Familia))
FF_F04D04 <- data.frame(table(F04D04$Familia))
FF_F04D05 <- data.frame(table(F04D05$Familia))
FF_F04D06 <- data.frame(table(F04D06$Familia))
FF_F04D07 <- data.frame(table(F04D07$Familia))


#gráficos primer
Porcos <- cbind(FF_P01D01$Freq,FF_P02D01$Freq,FF_P03D01$Freq,FF_P04D01$Freq,FF_P01D02$Freq,FF_P02D02$Freq,FF_P03D02$Freq,FF_P04D02$Freq,
                FF_P01D03$Freq,FF_P02D03$Freq,FF_P03D03$Freq,FF_P04D03$Freq,FF_P01D04$Freq,FF_P02D04$Freq,FF_P03D04$Freq,FF_P04D04$Freq,
                FF_P01D05$Freq,FF_P02D05$Freq,FF_P03D05$Freq,FF_P04D05$Freq,FF_P01D06$Freq,FF_P02D06$Freq,FF_P03D06$Freq,FF_P04D06$Freq,
                FF_P01D07$Freq,FF_P02D07$Freq,FF_P03D07$Freq,FF_P04D07$Freq)
Controles <- cbind(FF_C01D01$Freq,FF_C02D01$Freq,FF_C03D01$Freq,FF_C04D01$Freq,FF_C01D03$Freq,FF_C02D03$Freq,FF_C03D03$Freq,FF_C04D03$Freq,
                   FF_C01D05$Freq,FF_C02D05$Freq,FF_C03D05$Freq,FF_C04D05$Freq,FF_C01D07$Freq,FF_C02D07$Freq,FF_C03D07$Freq,FF_C04D07$Freq)
Fakes <- cbind(FF_F01D01$Freq,FF_F02D01$Freq,FF_F03D01$Freq,FF_F04D01$Freq,FF_F01D03$Freq,FF_F02D03$Freq,FF_F03D03$Freq,FF_F04D03$Freq,
               FF_F01D05$Freq,FF_F02D05$Freq,FF_F03D05$Freq,FF_F04D05$Freq,FF_F01D07$Freq,FF_F02D07$Freq,FF_F03D07$Freq,FF_F04D07$Freq)
samples <- cbind(Porcos, Controles, Fakes)
samples <- samples[2:24,]
fami <- FF_C01D01$Var1
fami <- fami[-1]
treatment <- cbind("treatment","Pig","Pig","Pig","Pig","Pig","Pig","Pig","Pig","Pig","Pig","Pig","Pig","Pig","Pig","Pig","Pig","Pig","Pig","Pig","Pig","Pig","Pig","Pig","Pig","Pig","Pig","Pig","Pig",
                 "Control","Control","Control","Control","Control","Control","Control","Control","Control","Control","Control","Control","Control","Control","Control","Control",
                  "Fake","Fake","Fake","Fake","Fake","Fake","Fake","Fake","Fake","Fake","Fake","Fake","Fake","Fake","Fake","Fake")
days <- cbind("days","0","0","0","0","2","2","2","2","4","4","4","4", "6","6","6","6","8","8","8","8","10","10","10","10","17","17","17","17",
              "0","0","0","0","4","4","4","4","8","8","8","8","17","17","17","17",
              "0","0","0","0","4","4","4","4","8","8","8","8","17","17","17","17")
temperature <- cbind("temperature","30","31","31","30","32","32","31","30","36","34","36","30","37","37","37","37","29","31","26","26","29","31","30","31","30","30","31","30",
                     "30","31","30","30","30","31","34","31","26","31","30","28","32","31","30","31",
                     "30","30","31","30","24","27","26","26","27","25","26","26","26","27","25","27")
tabela <- data.frame(Taxon = fami, samples)
linha <- data.frame(rbind(treatment, days,temperature))
rownames(tabela) <- tabela[,1]
table <- tabela[-1,]
table <- rbind(table, linha)
rownames(table) <- tabela[,1]
attach(table)

write.xlsx(table, "table_primer2.xlsx", row.names = FALSE)
#write.csv(linha, "amb_primer.csv", row.names = FALSE)
#write.csv(table, "table.csv", row.names = FALSE)

table2 <- read.table("table.xlsx", header=TRUE)


Ninja
t.table <- t(tabela)
treatment <- rbind("","PigD0","PigD0","PigD0","PigD0","PigD2","PigD2","PigD2","PigD2","PigD4","PigD4","PigD4","PigD4","PigD6","PigD6","PigD6","PigD6","PigD8","PigD8","PigD8","PigD8","PigD10","PigD10","PigD10","PigD10","PigD17","PigD17","PigD17","PigD17",
                   "ControlD0","ControlD0","ControlD0","ControlD0","ControlD4","ControlD4","ControlD4","ControlD4","ControlD8","ControlD8","ControlD8","ControlD8","ControlD17","ControlD17","ControlD17","ControlD17",
                   "FakeD0","FakeD0","FakeD0","FakeD0","FakeD4","FakeD4","FakeD4","FakeD4","FakeD8","FakeD8","FakeD8","FakeD8","FakeD17","FakeD17","FakeD17","FakeD17")
Names <- rbind("","P01D01","P02D01","P03D01","P04D01","P01D02","P02D02","P03D02","P04D02","P01D03","P02D03","P03D03","P04D03","P01D04","P02D04","P03D04","P04D04",
              "P01D05","P02D05","P03D05","P04D05","P01D06","P02D06","P03D06","P04D06","P01D07","P02D07","P03D07","P04D07",
              "C01D01","C02D01","C03D01","C04D01","C01D03","C02D03","C03D03","C04D03","C01D05","C02D05","C03D05","C04D05","C01D07","C02D07","C03D07","C04D07",
              "F01D01","F02D01","F03D01","F04D01","F01D03","F02D03","F03D03","F04D03","F01D05","F02D05","F03D05","F04D05","F01D07","F02D07","F03D07","F04D07")
t2.table <- cbind(Names, treatment, t.table)

#write.xlsx(t2.table, "forensic_ninja.xlsx")


#Density.family
D_Porcos <- cbind(D_P01D01$summary.P01D01.Familia.,D_P02D01$summary.P02D01.Familia.,D_P03D01$summary.P03D01.Familia.,D_P04D01$summary.P04D01.Familia.,D_P01D02$summary.P01D02.Familia.,D_P02D02$summary.P02D02.Familia.,D_P03D02$summary.P03D02.Familia.,D_P04D02$summary.P04D02.Familia.,
                                  D_P01D03$summary.P01D03.Familia.,D_P02D03$summary.P02D03.Familia.,D_P03D03$summary.P03D03.Familia.,D_P04D03$summary.P04D03.Familia.,D_P01D04$summary.P01D04.Familia.,D_P02D04$summary.P02D04.Familia.,D_P03D04$summary.P03D04.Familia.,D_P04D04$summary.P04D04.Familia.,
                                  D_P01D05$summary.P01D05.Familia.,D_P02D05$summary.P02D05.Familia.,D_P03D05$summary.P03D05.Familia.,D_P04D05$summary.P04D05.Familia.,D_P01D06$summary.P01D06.Familia.,D_P02D06$summary.P02D06.Familia.,D_P03D06$summary.P03D06.Familia.,D_P04D06$summary.P04D06.Familia.,
                                  D_P01D07$summary.P01D07.Familia.,D_P02D07$summary.P02D07.Familia.,D_P03D07$summary.P03D07.Familia.,D_P04D07$summary.P04D07.Familia.)
D_Controles <- cbind(D_C01D01$summary.C01D01.Familia.,D_C02D01$summary.C02D01.Familia.,D_C03D01$summary.C03D01.Familia.,D_C04D01$summary.C04D01.Familia.,D_C01D03$summary.C01D03.Familia.,D_C02D03$summary.C02D03.Familia.,D_C03D03$summary.C03D03.Familia.,D_C04D03$summary.C04D03.Familia.,
                   D_C01D05$summary.C01D05.Familia.,D_C02D05$summary.C02D05.Familia.,D_C03D05$summary.C03D05.Familia.,D_C04D05$summary.C04D05.Familia.,D_C01D07$summary.C01D07.Familia.,D_C02D07$summary.C02D07.Familia.,D_C03D07$summary.C03D07.Familia.,D_C04D07$summary.C04D07.Familia.)
D_Fakes <- cbind(D_F01D01$summary.F01D01.Familia.,D_F02D01$summary.F02D01.Familia.,D_F03D01$summary.F03D01.Familia.,D_F04D01$summary.F04D01.Familia.,D_F01D03$summary.F01D03.Familia.,D_F02D03$summary.F02D03.Familia.,D_F03D03$summary.F03D03.Familia.,D_F04D03$summary.F04D03.Familia.,
               D_F01D05$summary.F01D05.Familia.,D_F02D05$summary.F02D05.Familia.,D_F03D05$summary.F03D05.Familia.,D_F04D05$summary.F04D05.Familia.,D_F01D07$summary.F01D07.Familia.,D_F02D07$summary.F02D07.Familia.,D_F03D07$summary.F03D07.Familia.,D_F04D07$summary.F04D07.Familia.)
D_samples <- cbind(D_Porcos, D_Controles, D_Fakes)
D_tabela <- data.frame(Taxon = fami, D_samples)

#calculando densidade anova

log.density = log(Density)



P_graph <- density[1:28,]
CF_graph <- density[29:60,]
attach(d_mean)

b = bwplot(CF_graph$Dia~CF_graph$Density | CF_graph$Tratamento)
p = bwplot(P_graph$Dia~P_graph$Density | P_graph$Tratamento)


