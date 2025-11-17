library(pROC)
library(OptimalCutpoints)
library(readxl)
DB <- read_excel("Desktop/TESI/DB per calcoli.xlsx")
attach(DB)

DB$`outcome si/no` <- DB$`outcome si/no` == "si"
DB$LUS15  <- DB$T0_LUS_tot>15
DB$`necessità NIV` <- DB$`necessità NIV` == "si" 
DB$`Radiologia anormale` <- DB$`Radiologia anormale`== "1"
DB$T0_LUS_tot <- as.numeric(T0_LUS_tot)
DB$`T0_Bili dir` <- as.numeric(DB$T0_LUS_tot)
DB$`T0_p/f ratio` <- as.numeric(DB$`T0_p/f ratio`)
DB$T0_linfoc <- as.numeric(DB$T0_linfoc)
DB$T0_Consolidamenti <- as.numeric(DB$T0_Consolidamenti)
DB$T1_PEEP <- as.numeric(DB$T1_PEEP)
DB$T1_Consolidamenti <- as.numeric(DB$T1_Consolidamenti)
DB$`T1_p/f ratio` <- as.numeric(DB$`T1_p/f ratio`)
DB$Incoscienza <- as.logical(DB$Incoscienza)
DB$Dispnea <- as.logical(DB$Dispnea)
DB$Emottisi <- as.logical(DB$Emottisi)
DB$Tumore <- as.logical(DB$Tumore)
DB <- DB[-275,]
lus <- glm(DB$`outcome si/no`~ DB$T0_LUS_tot, family = "binomial")
GRAM <- glm(formula = DB$`outcome si/no`~DB$Incoscienza + DB$Età +DB$Dispnea 
            +  DB$Emottisi + DB$Tumore + DB$`Numero malattie` 
            + DB$`Radiologia anormale` + DB$T0_LDH + DB$`N/L ratio` + DB$`T0_Bili dir` , family = "binomial")
COWS <- glm(DB$`outcome si/no` ~ DB$`Numero malattie` + DB$`T0_p/f ratio`
            + DB$Giorni_sintomi_accesso + DB$LUS15 + DB$Dispnea, , family = "binomial")



##LUS TORNA
A <- roc(response= DB$`outcome si/no`, predictor=DB$T0_LUS_tot)
plot.roc(A)
smooth(A)
#otimal cutpoint
zA <- DB$`outcome si/no`
xA <- DB$T0_LUS_tot
Ao <- data.frame(zA, xA)
OCPA1 <- optimal.cutpoints(X = xA~zA, methods = "ROC01", data=Ao, tag.healthy = F, conf.level = 0.95 )
summary(OCPA1)
OCPA2 <- optimal.cutpoints(X = xA~zA, methods = "Youden", data=Ao, tag.healthy = F, conf.level = 0.95 )
summary(OCPA2)
OCPA3 <- optimal.cutpoints(X = xA~zA, methods = "CB", data=Ao, tag.healthy = F, conf.level = 0.95 )
summary(OCPA3)
OCPA4 <- optimal.cutpoints(X = xA~zA, methods = "Minimax", data=Ao, tag.healthy = F, conf.level = 0.95 )
summary(OCPA4)

#abbiamo quindi un optimal cutpoint di 15,5 


#GRAM torna
B <- roc(response=GRAM$y, predictor=GRAM$fitted.values)
plot.roc(B)
smooth(B)
#auc gram molto piu elevata di lus provare con test quindi è più attendibile
#otimal cutpoint
zB <- B$original.response
xB <- B$original.predictor
Bo <- data.frame(zB, xB)
OCPB1 <- optimal.cutpoints(X = xB~zB, methods = "ROC01", data=Bo, tag.healthy = F, conf.level = 0.95 )
summary(OCPB1)
OCPB2 <- optimal.cutpoints(X = xB~zB, methods = "Youden", data=Bo, tag.healthy = F, conf.level = 0.95 )
summary(OCPB2)
OCPB3 <- optimal.cutpoints(X = xB~zB, methods = "CB", data=Bo, tag.healthy = F, conf.level = 0.95 )
summary(OCPB3)
OCPB4 <- optimal.cutpoints(X = xB~zB, methods = "Minimax", data=Bo, tag.healthy = F, conf.level = 0.95 )
summary(OCPB4)

#COWS 
D <- roc(response=COWS$y, predictor=COWS$fitted.values)
plot.roc(D)
smooth(D)
#ha l'auc maggiore
#otimal cutpoint
zD <- D$original.response
xD <- D$original.predictor
Do <- data.frame(zD, xD)
OCPD1 <- optimal.cutpoints(X = xD~zD, methods = "ROC01", data=Do, tag.healthy = F, conf.level = 0.95 )
summary(OCPD1)
OCPD2 <- optimal.cutpoints(X = xD~zD, methods = "Youden", data=Do, tag.healthy = F, conf.level = 0.95 )
summary(OCPD2)
OCPD3 <- optimal.cutpoints(X = xD~zD, methods = "CB", data=Do, tag.healthy = F, conf.level = 0.95 )
summary(OCPD3)
OCPD4 <- optimal.cutpoints(X = xD~zD, methods = "Minimax", data=Do, tag.healthy = F, conf.level = 0.95 )
summary(OCPD4)

roc.test(B,D, methods = "Delong", alternative= "less")
roc.test(B,D, method = "bootstrap", alternative= "less")

#plot
plot.optimal.cutpoints(OCPA2)

plot.optimal.cutpoints(OCPB2)
plot.optimal.cutpoints(OCPD2)