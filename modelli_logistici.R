
library(ResourceSelection)
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

#LUS 
lus <- glm(DB$`outcome si/no`~ DB$T0_LUS_tot, family = "binomial")
summary(lus)
hoslem.test(lus$y,fitted(lus))

#GRAM
GRAM <- glm(formula = DB$`outcome si/no`~DB$Incoscienza + DB$Età +DB$Dispnea 
            +  DB$Emottisi + DB$Tumore + DB$`Numero malattie` 
            + DB$`Radiologia anormale` + DB$T0_LDH + DB$`N/L ratio` + DB$`T0_Bili dir` , family = "binomial")
summary(GRAM)
hoslem.test(GRAM$y,fitted(GRAM) )


#COWS
COWS <- glm(DB$`outcome si/no` ~ DB$`Numero malattie` + DB$`T0_p/f ratio`
            + DB$Giorni_sintomi_accesso + DB$LUS15 + DB$Dispnea, , family = "binomial")
summary(COWS)
hoslem.test(COWS$y,fitted(COWS) )



