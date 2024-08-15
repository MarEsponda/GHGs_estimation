##Maria de Mar Esponda
##Agosto 2023
## Sensitivity Analysis of Emissions

# Libraries
library(dplyr)
library(openxlsx)
library(readxl)
library(RColorBrewer)
library(ggplot2)
library(MASS)
library(triangle)
library(haven)

# Set working directory
setwd("your_file_location")
setwd("D:/OneDrive - CGIAR/2021 -- Climate change rice Colombia/2022 -- Towards site specific GHG measures rice/Analysis/Green/Github")
# Data loading-------------------------------------------------------------------------------------------------------------------------
EVA <- read.xlsx("EVA_Arroz_2020.xlsx")                                             # Agricultural evaluation base for rice in 2020
EVA <- mutate(EVA, sistema = ifelse(Desagregación.cultivo == "Arroz Riego", 1, 2))  # Create dummy variable for irrigation = 1, rainfed = 2
base <- read.xlsx("final_base.xlsx")                                                              # Our estimation (from line 1280 in Estimation_process.R)
time_lapse <- read_dta("time_lapse.dta")
base <- left_join(base, time_lapse, by = "id_productor")  # Add period column

# Filtering data based on production cycle periods:
# (1) 12 obs for the first semester (January-June) 2019
# (2) 42 obs for the second semester (June-December) 2019
# (3) 284 obs for the first semester (January-June) 2020
# (4) 145 obs for the second semester (June-December) 2020
# (77) 98 obs for other periods. Unknown which periods these refer to
base <- subset(base, d_0 == 3 | d_0 == 4)

# Base curves plot (presentation graph)
P0 <- density(base$CO2_eq_total_ha)
PR <- density(subset(base$CO2_eq_total_ha, base$f_3 == 1))
PS <- density(subset(base$CO2_eq_total_ha, base$f_3 == 2))

plot(PS, col = "#8B6508", lwd = 2.5)
lines(PR, col = "#00688B", lwd = 2.5)

plot(PS, col = "black")

# Variables of interest
base2 <- base[, c("id_productor", "a_1", "f_3", "cant_N_kg", "EF_dir", "EDirectas_co2eq",
                  "EVol", "EIndirectas_co2eq", "EFc", "SFw_2", "SFp", "duracion_ciclo_dias_mod", 
                  "f_19", "Met_co2_2", "Urea_kg", "Urea_ha", "EUrea", "Cal_kg", "ECal", "CO2_eq_total", "d_0", "f20_cosecha_ton")]

# Add additional variables and check final emission values
base2 <- mutate(base2, EF4_vol = EVol, Frac_Gasf = 0.15, EF5 = 0.011, Frac_Leach = 0.24, EFUrea = 0.2, EFCal = 0.13, SFw = SFw_2,
                Co2_eq = EDirectas_co2eq + EIndirectas_co2eq + Met_co2_2 + EUrea + ECal)
base2[is.na(base2)] <- 0  # Replace NA with zeros
base2$Urea_kg <- ifelse(base$Urea_ha <= 600, base$Urea_kg, 0)  # Limit urea emissions to applications of 600 kg per hectare or less

# Create 9 variables with min, max, and normal values of factors-------------------------------------------------------------
# The EF_dir variable includes both EF1 and EF1FR
base2$SFw <- round(base2$SFw, 2)  # Round these numbers for use in the if-else function later
base2 <- mutate(base2, EF1_min = ifelse(base2$EF_dir == 0.016, 0.013, 0), 
                FracG_min = 0.03, EF4_min = 0.011, FracL_min = 0.01, EF5_min = 0,
                EFc_min = 0.86, SFw_min = ifelse(base2$SFw == 0.16, 0.11, ifelse(base2$SFw == 1, 0.73, NA)), 
                SFp_min = 0.8, EFUrea_min = 0.1, EFCal_min = 0.065)

base2 <- mutate(base2, EF1_max = ifelse(base2$EF_dir == 0.016, 0.019, 0.01), 
                FracG_max = 0.43, EF4_max = 0.017, FracL_max = 0.073, EF5_max = 0.02,
                EFc_max = 1.88, SFw_max = ifelse(base2$SFw == 0.16, 0.24, ifelse(base2$SFw == 1, 1.27, NA)), 
                SFp_max = 0.99, EFUrea_max = 0.3, EFCal_max = 0.195)

# These variables are the same as the originals, but created for convention in the code
base2 <- mutate(base2, EF1_n = EF_dir, 
                FracG_n = 0.15, EF4_n = 0.014, FracL_n = 0.024, EF5_n = 0.011,
                EFc_n = 1.27, SFw_n = SFw, SFp_n = 0.89, EFUrea_n = 0.2, EFCal_n = 0.13)

# Emissions with triangular distribution, 100 values per factor (using IPCC values)-----------------------------------------------------------------------------------------------------------------------------------------------
# Empty lists
set.seed(1)
EF_dir <- list()
Frac_Gasf <- list()
EF4_vol <- list()
Frac_Leach <- list()
EF5 <- list()
EFc <- list()
SFw <- list()
SFp <- list()
EFUrea <- list()
EFCal <- list()
area <- list()
Emisiones <- list()
Emisionesp <- list()
production <- list()
Emisiones2_100 <- data.frame(columna = rep(NA, 100))
Emisiones2p_100 <- data.frame(columna = rep(NA, 100))

# Loop to generate 100 random values within a triangular distribution for each producer.
# Each producer has 100 distinct values of emissions per hectare (the sum of these values is not comparable)
for (i in 1:nrow(base2)) {
  EF_dir[[i]] <- round(rtriangle(100, a = base2$EF1_min[i], b = base2$EF1_max[i], c = base2$EF1_n[i]), 3)
  Frac_Gasf[[i]] <- round(rtriangle(100, a = base2$FracG_min[i], b = base2$FracG_max[i], c = base2$FracG_n[i]), 3)  
  EF4_vol[[i]] <- round(rtriangle(100, a = base2$EF4_min[i], b = base2$EF4_max[i], c = base2$EF4_n[i]), 3)  
  Frac_Leach[[i]] <- round(rtriangle(100, a = base2$FracL_min[i], b = base2$FracL_max[i], c = base2$FracL_n[i]), 3)  
  EF5[[i]] <- round(rtriangle(100, a = base2$EF5_min[i], b = base2$EF5_max[i], c = base2$EF5_n[i]), 3)  
  EFc[[i]] <- round(rtriangle(100, a = base2$EFc_min[i], b = base2$EFc_max[i], c = base2$EFc_n[i]), 3)  
  SFw[[i]] <- round(rtriangle(100, a = base2$SFw_min[i], b = base2$SFw_max[i], c = base2$SFw_n[i]), 3)  
  SFp[[i]] <- round(rtriangle(100, a = base2$SFp_min[i], b = base2$SFp_max[i], c = base2$SFp_n[i]), 3)  
  EFUrea[[i]] <- round(rtriangle(100, a = base2$EFUrea_min[i], b = base2$EFUrea_max[i], c = base2$EFUrea_n[i]), 3)  
  EFCal[[i]] <- round(rtriangle(100, a = base2$EFCal_min[i], b = base2$EFCal_max[i], c = base2$EFCal_n[i]), 3)
  area[[i]] <- base2$f_19[i]  # Area per producer
  production[[i]] <- base2$f20_cosecha_ton[i]  # Reported production in tons of paddy rice
  Emisiones_i <- vector("numeric", 100)
  Emisionesp_i <- vector("numeric", 100)
  
  for (j in 1:100) {
    Emisiones_i[j] <- ((base2$cant_N_kg[i] * (EF_dir[[i]][j] + Frac_Gasf[[i]][j] * EF4_vol[[i]][j] + Frac_Leach[[i]][j] * EF5[[i]][j]) * (44 / 28) * 265) +
                          (EFc[[i]][j] * SFw[[i]][j] * SFp[[i]][j] * base2$duracion_ciclo_dias_mod[i] * base2$f_19[i] * 28) +
                          ((base2$Urea_kg[i] * EFUrea[[i]][j] + base2$Cal_kg[i] * EFCal[[i]][j]) * (44 / 12))) / area[[i]]
                       Emisionesp_i[j] <- ((base2$cant_N_kg[i] * (EF_dir[[i]][j] + Frac_Gasf[[i]][j] * EF4_vol[[i]][j] + Frac_Leach[[i]][j] * EF5[[i]][j]) * (44 / 28) * 265) +
                                              (EFc[[i]][j] * SFw[[i]][j] * SFp[[i]][j] * base2$duracion_ciclo_dias_mod[i] * base2$f_19[i] * 28) +
                                              ((base2$Urea_kg[i] * EFUrea[[i]][j] + base2$Cal_kg[i] * EFCal[[i]][j]) * (44 / 12))) / production[[i]]
  }
  Emisiones[[i]] <- Emisiones_i  # Emissions per hectare
  Emisiones2 <- data.frame(Valor = unlist(Emisiones))  # Emissions per hectare
  Emisiones2_100[i, 1] <- data.frame(Valor = mean(Emisiones[[i]]))  # Mean emissions per hectare
  Emisionesp[[i]] <- Emisionesp_i  # Emissions per ton of paddy
  Emisiones2p <- data.frame(Valor = unlist(Emisionesp))  # Emissions per ton of paddy
  Emisiones2p_100[i, 1] <- data.frame(Valor = mean(Emisionesp[[i]]))  # Mean emissions per ton of paddy
}


# Calculating minimum, maximum, mean, median, and standard deviation of emissions
minimo <- list()
maximo <- list()
media <- list()
desv <- list()
mediana <- list()

for (i in 1:length(Emisiones)) {
  minimo[[i]] <- min(Emisiones[[i]])
  maximo[[i]] <- max(Emisiones[[i]])
  media[[i]] <- mean(Emisiones[[i]])
  desv[[i]] <- sd(Emisiones[[i]])
  mediana[[i]] <- median(Emisiones[[i]])
  Medidas <- data.frame(minimo = unlist(minimo),
                        maximo = unlist(maximo),
                        media = unlist(media),
                        mediana = unlist(mediana),
                        desvi = unlist(desv))
}
Medidas <- mutate(Medidas, id = base2$id_productor)
# write.xlsx(Medidas, file="Medidas_sensi_productor.xlsx")

# Distribution function subsets
# Logical vector discriminating whether the condition is met or not
{
  PeriodoA <- ifelse(base2$d_0 == 3, TRUE, FALSE)
  PeriodoB <- ifelse(base2$d_0 == 4, TRUE, FALSE)
  Riego <- ifelse(base2$f_3 == 1, TRUE, FALSE)
  Secano <- ifelse(base2$f_3 == 2, TRUE, FALSE)
  Cordoba <- ifelse(base2$a_1 == 1, TRUE, FALSE)
  Meta <- ifelse(base2$a_1 == 2, TRUE, FALSE)
  Sucre <- ifelse(base2$a_1 == 3, TRUE, FALSE)
  Tolima <- ifelse(base2$a_1 == 4, TRUE, FALSE)
  Casanare <- ifelse(base2$a_1 == 5, TRUE, FALSE)
  Valle <- ifelse(base2$a_1 == 8, TRUE, FALSE)
  RiegoA <- ifelse(base2$f_3 == 1 & base2$d_0 == 3, TRUE, FALSE)
  RiegoB <- ifelse(base2$f_3 == 1 & base2$d_0 == 4, TRUE, FALSE)
  SecanoA <- ifelse(base2$f_3 == 2 & base2$d_0 == 3, TRUE, FALSE)
  SecanoB <- ifelse(base2$f_3 == 2 & base2$d_0 == 4, TRUE, FALSE)
  
  # Subset of list (using logical vectors)
}

{
PA<-Emisiones[PeriodoA]
PB<-Emisiones[PeriodoB]
Ri<-Emisiones[Riego]
Se<-Emisiones[Secano]
Cor<-Emisiones[Cordoba]
Me<-Emisiones[Meta]
Su<-Emisiones[Sucre]
Tol<-Emisiones[Tolima]
Cas<-Emisiones[Casanare]
Vall<-Emisiones[Valle]
RA<-Emisiones[RiegoA]
RB<-Emisiones[RiegoB]
SA<-Emisiones[SecanoA]
SB<-Emisiones[SecanoB]

PA<-data.frame(Valor = unlist(PA))
PB<-data.frame(Valor = unlist(PB))
Ri<-data.frame(Valor = unlist(Ri))
Se<-data.frame(Valor = unlist(Se))
Cor<-data.frame(Valor = unlist(Cor))
Me<-data.frame(Valor = unlist(Me))
Su<-data.frame(Valor = unlist(Su))
Tol<-data.frame(Valor = unlist(Tol))
Cas<-data.frame(Valor = unlist(Cas))
Vall<-data.frame(Valor = unlist(Vall))
RA<-data.frame(Valor = unlist(RA))
RB<-data.frame(Valor = unlist(RB))
SA<-data.frame(Valor = unlist(SA))
SB<-data.frame(Valor = unlist(SB))

}                                                                           #area
# Mean (kg CO2 eq/ha)
{
  # Original values
  a1 <- mean(base$CO2_eq_total_ha)
  # Curves Extrapolation 1 Periods
  a2 <- mean(subset(base$CO2_eq_total_ha, base$d_0 == 3))
  a3 <- mean(subset(base$CO2_eq_total_ha, base$d_0 == 4))
  # Curves Extrapolation 2 Irrigation
  a4 <- mean(subset(base$CO2_eq_total_ha, base$f_3 == 1))
  a5 <- mean(subset(base$CO2_eq_total_ha, base$f_3 == 2))
  # Curves Extrapolation 3 Department
  a6 <- mean(subset(base$CO2_eq_total_ha, base$a_1 == 1))
  a7 <- mean(subset(base$CO2_eq_total_ha, base$a_1 == 2))
  a8 <- mean(subset(base$CO2_eq_total_ha, base$a_1 == 3))
  a9 <- mean(subset(base$CO2_eq_total_ha, base$a_1 == 4))
  a10 <- mean(subset(base$CO2_eq_total_ha, base$a_1 == 5))
  a11 <- mean(subset(base$CO2_eq_total_ha, base$a_1 == 8))
  # Curves Extrapolation 4 Irrigation and periods
  a12 <- mean(subset(base$CO2_eq_total_ha, base$d_0 == 3 & base$f_3 == 1))
  a13 <- mean(subset(base$CO2_eq_total_ha, base$d_0 == 4 & base$f_3 == 1))
  a14 <- mean(subset(base$CO2_eq_total_ha, base$d_0 == 3 & base$f_3 == 2))
  a15 <- mean(subset(base$CO2_eq_total_ha, base$d_0 == 4 & base$f_3 == 2))
  
  # Mean values
  a16 <- mean(Emisiones2$Valor)
  a17 <- mean(PA$Valor)
  a18 <- mean(PB$Valor)
  a19 <- mean(Ri$Valor)
  a20 <- mean(Se$Valor)
  a21 <- mean(Cor$Valor)
  a22 <- mean(Me$Valor)
  a23 <- mean(Su$Valor)
  a24 <- mean(Tol$Valor)
  a25 <- mean(Cas$Valor)
  a26 <- mean(Vall$Valor)
  a27 <- mean(RA$Valor)
  a28 <- mean(RB$Valor)
  a29 <- mean(SA$Valor)
  a30 <- mean(SB$Valor)
}
# Standard deviation (kg CO2 eq/ha)
{
  # Original values
  a1 <- sd(base$CO2_eq_total_ha)
  # Curves Extrapolation 1 Periods
  a2 <- sd(subset(base$CO2_eq_total_ha, base$d_0 == 3))
  a3 <- sd(subset(base$CO2_eq_total_ha, base$d_0 == 4))
  # Curves Extrapolation 2 Irrigation
  a4 <- sd(subset(base$CO2_eq_total_ha, base$f_3 == 1))
  a5 <- sd(subset(base$CO2_eq_total_ha, base$f_3 == 2))
  # Curves Extrapolation 3 Department
  a6 <- sd(subset(base$CO2_eq_total_ha, base$a_1 == 1))
  a7 <- sd(subset(base$CO2_eq_total_ha, base$a_1 == 2))
  a8 <- sd(subset(base$CO2_eq_total_ha, base$a_1 == 3))
  a9 <- sd(subset(base$CO2_eq_total_ha, base$a_1 == 4))
  a10 <- sd(subset(base$CO2_eq_total_ha, base$a_1 == 5))
  a11 <- sd(subset(base$CO2_eq_total_ha, base$a_1 == 8))
  # Curves Extrapolation 4 Irrigation and periods
  a12 <- sd(subset(base$CO2_eq_total_ha, base$d_0 == 3 & base$f_3 == 1))
  a13 <- sd(subset(base$CO2_eq_total_ha, base$d_0 == 3 & base$f_3 == 2))
  a14 <- sd(subset(base$CO2_eq_total_ha, base$d_0 == 4 & base$f_3 == 1))
  a15 <- sd(subset(base$CO2_eq_total_ha, base$d_0 == 4 & base$f_3 == 2))
  
  # Standard deviation
  a16 <- sd(Emisiones2$Valor)
  a17 <- sd(PA$Valor)
  a18 <- sd(PB$Valor)
  a19 <- sd(Ri$Valor)
  a20 <- sd(Se$Valor)
  a21 <- sd(Cor$Valor)
  a22 <- sd(Me$Valor)
  a23 <- sd(Su$Valor)
  a24 <- sd(Tol$Valor)
  a25 <- sd(Cas$Valor)
  a26 <- sd(Vall$Valor)
  a27 <- sd(RA$Valor)
  a28 <- sd(RB$Valor)
  a29 <- sd(SA$Valor)
  a30 <- sd(SB$Valor)
}
# t-Test
{
  a31 <- t.test(base$CO2_eq_total_ha, Emisiones2$Valor)
  a32 <- t.test(subset(base$CO2_eq_total_ha, base$d_0 == 3), PA$Valor)
  a33 <- t.test(subset(base$CO2_eq_total_ha, base$d_0 == 4), PB$Valor)
  a34 <- t.test(subset(base$CO2_eq_total_ha, base$f_3 == 1), Ri$Valor)
  a35 <- t.test(subset(base$CO2_eq_total_ha, base$f_3 == 2), Se$Valor)
  a36 <- t.test(subset(base$CO2_eq_total_ha, base$a_1 == 1), Cor$Valor)
  a37 <- t.test(subset(base$CO2_eq_total_ha, base$a_1 == 2), Me$Valor)
  a38 <- t.test(subset(base$CO2_eq_total_ha, base$a_1 == 3), Su$Valor)
  a39 <- t.test(subset(base$CO2_eq_total_ha, base$a_1 == 4), Tol$Valor)
  a40 <- t.test(subset(base$CO2_eq_total_ha, base$a_1 == 5), Cas$Valor)
  a41 <- t.test(subset(base$CO2_eq_total_ha, base$a_1 == 8), Vall$Valor)
  a42 <- t.test(subset(base$CO2_eq_total_ha, base$d_0 == 3 & base$f_3 == 1), RA$Valor)
  a43 <- t.test(subset(base$CO2_eq_total_ha, base$d_0 == 4 & base$f_3 == 1), RB$Valor)
  a44 <- t.test(subset(base$CO2_eq_total_ha, base$d_0 == 3 & base$f_3 == 2), SA$Valor)
  a45 <- t.test(subset(base$CO2_eq_total_ha, base$d_0 == 4 & base$f_3 == 2), SB$Valor)
  
  media <- data.frame(Original = c(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15),
                      Sensitivity = c(a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30),
                      t_Test = c(a31$statistic, a32$statistic, a33$statistic, a34$statistic, a35$statistic,
                                 a36$statistic, a37$statistic, a38$statistic, a39$statistic, a40$statistic,
                                 a41$statistic, a42$statistic, a43$statistic, a44$statistic, a45$statistic),
                      p_value = c(a31$p.value, a32$p.value, a33$p.value, a34$p.value, a35$p.value,
                                  a36$p.value, a37$p.value, a38$p.value, a39$p.value, a40$p.value,
                                  a41$p.value, a42$p.value, a43$p.value, a44$p.value, a45$p.value))
  
  attr(media$Original, "label") <- "CO2 eq per ha [kg CO2 eq /ha]"
  attr(media$Sensitivity, "label") <- "CO2 eq per ha [kg CO2 eq /ha]"
  attr(media$t_Test, "label") <- "Value of the statistic"
  attr(media$p_value, "label") <- "Null hypothesis p-value"
  # write.xlsx(media, file="ttest_ha.xlsx")             # Results of the t-test on the difference in distributions
}

# Logical vector to discriminate if the condition is met or not
{
  PeriodoA <- ifelse(base2$d_0 == 3, TRUE, FALSE)
  PeriodoB <- ifelse(base2$d_0 == 4, TRUE, FALSE)
  Irrigation <- ifelse(base2$f_3 == 1, TRUE, FALSE)
  Dryland <- ifelse(base2$f_3 == 2, TRUE, FALSE)
  Cordoba <- ifelse(base2$a_1 == 1, TRUE, FALSE)
  Meta <- ifelse(base2$a_1 == 2, TRUE, FALSE)
  Sucre <- ifelse(base2$a_1 == 3, TRUE, FALSE)
  Tolima <- ifelse(base2$a_1 == 4, TRUE, FALSE)
  Casanare <- ifelse(base2$a_1 == 5, TRUE, FALSE)
  Valle <- ifelse(base2$a_1 == 8, TRUE, FALSE)
  IrrigationA <- ifelse(base2$f_3 == 1 & base2$d_0 == 3, TRUE, FALSE)
  IrrigationB <- ifelse(base2$f_3 == 1 & base2$d_0 == 4, TRUE, FALSE)
  DrylandA <- ifelse(base2$f_3 == 2 & base2$d_0 == 3, TRUE, FALSE)
  DrylandB <- ifelse(base2$f_3 == 2 & base2$d_0 == 4, TRUE, FALSE)
  
  # Subset list (using logical vectors)
  PA <- Emisionesp[PeriodoA]
  PB <- Emisionesp[PeriodoB]
  Ri <- Emisionesp[Irrigation]
  Se <- Emisionesp[Dryland]
  Cor <- Emisionesp[Cordoba]
  Me <- Emisionesp[Meta]
  Su <- Emisionesp[Sucre]
  Tol <- Emisionesp[Tolima]
  Cas <- Emisionesp[Casanare]
  Vall <- Emisionesp[Valle]
  RA <- Emisionesp[IrrigationA]
  RB <- Emisionesp[IrrigationB]
  SA <- Emisionesp[DrylandA]
  SB <- Emisionesp[DrylandB]
  
  PA <- data.frame(Value = unlist(PA))
  PB <- data.frame(Value = unlist(PB))
  Ri <- data.frame(Value = unlist(Ri))
  Se <- data.frame(Value = unlist(Se))
  Cor <- data.frame(Value = unlist(Cor))
  Me <- data.frame(Value = unlist(Me))
  Su <- data.frame(Value = unlist(Su))
  Tol <- data.frame(Value = unlist(Tol))
  Cas <- data.frame(Value = unlist(Cas))
  Vall <- data.frame(Value = unlist(Vall))
  RA <- data.frame(Value = unlist(RA))
  RB <- data.frame(Value = unlist(RB))
  SA <- data.frame(Value = unlist(SA))
  SB <- data.frame(Value = unlist(SB))
}
# Mean (kg CO2 eq/t)

{
        a1<-mean(base$CO2_eq_total_ton)
        #Curvas Extrapolacion 1 Periodos
        a2<-mean(subset(base$CO2_eq_total_ton, base$d_0==3))
        a3<-mean(subset(base$CO2_eq_total_ton, base$d_0==4))
        #Curvas Extrapolacion 2 Riego
        a4<-mean(subset(base$CO2_eq_total_ton, base$f_3==1))
        a5<-mean(subset(base$CO2_eq_total_ton, base$f_3==2))
        #Curvas Extrapolacion 3 Departamento
        a6<-mean(subset(base$CO2_eq_total_ton, base$a_1==1))
        a7<-mean(subset(base$CO2_eq_total_ton, base$a_1==2))
        a8<-mean(subset(base$CO2_eq_total_ton, base$a_1==3))
        a9<-mean(subset(base$CO2_eq_total_ton, base$a_1==4))
        a10<-mean(subset(base$CO2_eq_total_ton, base$a_1==5))
        a11<-mean(subset(base$CO2_eq_total_ton, base$a_1==8))
        #Curvas Extrapolacion 4 Riego y periodos
        a12<-mean(subset(base$CO2_eq_total_ton, base$d_0==3 & base$f_3==1))
        a13<-mean(subset(base$CO2_eq_total_ton, base$d_0==4 & base$f_3==1))
        a14<-mean(subset(base$CO2_eq_total_ton, base$d_0==3 & base$f_3==2))
        a15<-mean(subset(base$CO2_eq_total_ton, base$d_0==4 & base$f_3==2))
        
        #Media valores 
        a16<-mean(Emisiones2p$Valor)
        a17<-mean(PA$Valor)
        a18<-mean(PB$Valor)
        a19<-mean(Ri$Valor)
        a20<-mean(Se$Valor)
        a21<-mean(Cor$Valor)
        a22<-mean(Me$Valor)
        a23<-mean(Su$Valor)
        a24<-mean(Tol$Valor)
        a25<-mean(Cas$Valor)
        a26<-mean(Vall$Valor)
        a27<-mean(RA$Valor)
        a28<-mean(RB$Valor)
        a29<-mean(SA$Valor)
        a30<-mean(SB$Valor)
}
# Standard deviation (kg CO2 eq/ton)
{
  # Original values
  a1 <- sd(base$CO2_eq_total_ton)
  # Curves Extrapolation 1 Periods
  a2 <- sd(subset(base$CO2_eq_total_ton, base$d_0 == 3))
  a3 <- sd(subset(base$CO2_eq_total_ton, base$d_0 == 4))
  # Curves Extrapolation 2 Irrigation
  a4 <- sd(subset(base$CO2_eq_total_ton, base$f_3 == 1))
  a5 <- sd(subset(base$CO2_eq_total_ton, base$f_3 == 2))
  # Curves Extrapolation 3 Department
  a6 <- sd(subset(base$CO2_eq_total_ton, base$a_1 == 1))
  a7 <- sd(subset(base$CO2_eq_total_ton, base$a_1 == 2))
  a8 <- sd(subset(base$CO2_eq_total_ton, base$a_1 == 3))
  a9 <- sd(subset(base$CO2_eq_total_ton, base$a_1 == 4))
  a10 <- sd(subset(base$CO2_eq_total_ton, base$a_1 == 5))
  a11 <- sd(subset(base$CO2_eq_total_ton, base$a_1 == 8))
  # Curves Extrapolation 4 Irrigation and periods
  a12 <- sd(subset(base$CO2_eq_total_ton, base$d_0 == 3 & base$f_3 == 1))
  a13 <- sd(subset(base$CO2_eq_total_ton, base$d_0 == 3 & base$f_3 == 2))
  a14 <- sd(subset(base$CO2_eq_total_ton, base$d_0 == 4 & base$f_3 == 1))
  a15 <- sd(subset(base$CO2_eq_total_ton, base$d_0 == 4 & base$f_3 == 2))
  
  # Standard deviation
  a16 <- sd(Emisiones2p$Valor)
  a17 <- sd(PA$Valor)
  a18 <- sd(PB$Valor)
  a19 <- sd(Ri$Valor)
  a20 <- sd(Se$Valor)
  a21 <- sd(Cor$Valor)
  a22 <- sd(Me$Valor)
  a23 <- sd(Su$Valor)
  a24 <- sd(Tol$Valor)
  a25 <- sd(Cas$Valor)
  a26 <- sd(Vall$Valor)
  a27 <- sd(RA$Valor)
  a28 <- sd(RB$Valor)
  a29 <- sd(SA$Valor)
  a30 <- sd(SB$Valor)
}

# t-Test
{
  a31 <- t.test(base$CO2_eq_total_ton, Emisiones2p$Valor)
  a32 <- t.test(subset(base$CO2_eq_total_ton, base$d_0 == 3), PA$Valor)
  a33 <- t.test(subset(base$CO2_eq_total_ton, base$d_0 == 4), PB$Valor)
  a34 <- t.test(subset(base$CO2_eq_total_ton, base$f_3 == 1), Ri$Valor)
  a35 <- t.test(subset(base$CO2_eq_total_ton, base$f_3 == 2), Se$Valor)
  a36 <- t.test(subset(base$CO2_eq_total_ton, base$a_1 == 1), Cor$Valor)
  a37 <- t.test(subset(base$CO2_eq_total_ton, base$a_1 == 2), Me$Valor)
  a38 <- t.test(subset(base$CO2_eq_total_ton, base$a_1 == 3), Su$Valor)
  a39 <- t.test(subset(base$CO2_eq_total_ton, base$a_1 == 4), Tol$Valor)
  a40 <- t.test(subset(base$CO2_eq_total_ton, base$a_1 == 5), Cas$Valor)
  a41 <- t.test(subset(base$CO2_eq_total_ton, base$a_1 == 8), Vall$Valor)
  a42 <- t.test(subset(base$CO2_eq_total_ton, base$d_0 == 3 & base$f_3 == 1), RA$Valor)
  a43 <- t.test(subset(base$CO2_eq_total_ton, base$d_0 == 4 & base$f_3 == 1), RB$Valor)
  a44 <- t.test(subset(base$CO2_eq_total_ton, base$d_0 == 3 & base$f_3 == 2), SA$Valor)
  a45 <- t.test(subset(base$CO2_eq_total_ton, base$d_0 == 4 & base$f_3 == 2), SB$Valor)
  
  mediap <- data.frame(Original = c(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15),
                       Sensitivity = c(a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30),
                       t_Test = c(a31$statistic, a32$statistic, a33$statistic, a34$statistic, a35$statistic,
                                  a36$statistic, a37$statistic, a38$statistic, a39$statistic, a40$statistic,
                                  a41$statistic, a42$statistic, a43$statistic, a44$statistic, a45$statistic),
                       p_value = c(a31$p.value, a32$p.value, a33$p.value, a34$p.value, a35$p.value,
                                   a36$p.value, a37$p.value, a38$p.value, a39$p.value, a40$p.value,
                                   a41$p.value, a42$p.value, a43$p.value, a44$p.value, a45$p.value))
  
  attr(mediap$Original, "label") <- "CO2 eq per ton [kg CO2 eq /ton]"
  attr(mediap$Sensitivity, "label") <- "CO2 eq per ton [kg CO2 eq /ton]"
  attr(mediap$t_Test, "label") <- "Value of the statistic"
  attr(mediap$p_value, "label") <- "Null hypothesis p-value"
  # write.xlsx(mediap, file="ttest_ton.xlsx")             # Results of the t-test on the difference in distributions
}

# Median
{
  # Original values
  a1 <- median(base$CO2_eq_total_ha)
  # Curves Extrapolation 1 Periods
  a2 <- median(subset(base$CO2_eq_total_ha, base$d_0 == 3))
  a3 <- median(subset(base$CO2_eq_total_ha, base$d_0 == 4))
  # Curves Extrapolation 2 Irrigation
  a4 <- median(subset(base$CO2_eq_total_ha, base$f_3 == 1))
  a5 <- median(subset(base$CO2_eq_total_ha, base$f_3 == 2))
  # Curves Extrapolation 3 Department
  a6 <- median(subset(base$CO2_eq_total_ha, base$a_1 == 1))
  a7 <- median(subset(base$CO2_eq_total_ha, base$a_1 == 2))
  a8 <- median(subset(base$CO2_eq_total_ha, base$a_1 == 3))
  a9 <- median(subset(base$CO2_eq_total_ha, base$a_1 == 4))
  a10 <- median(subset(base$CO2_eq_total_ha, base$a_1 == 5))
  a11 <- median(subset(base$CO2_eq_total_ha, base$a_1 == 8))
  # Curves Extrapolation 4 Irrigation and periods
  a12 <- median(subset(base$CO2_eq_total_ha, base$d_0 == 3 & base$f_3 == 1))
  a13 <- median(subset(base$CO2_eq_total_ha, base$d_0 == 3 & base$f_3 == 2))
  a14 <- median(subset(base$CO2_eq_total_ha, base$d_0 == 4 & base$f_3 == 1))
  a15 <- median(subset(base$CO2_eq_total_ha, base$d_0 == 4 & base$f_3 == 2))
  
  # Median values
  a16 <- median(Emisiones2$Valor)
  a17 <- median(PA$Valor)
  a18 <- median(PB$Valor)
  a19 <- median(Ri$Valor)
  a20 <- median(Se$Valor)
  a21 <- median(Cor$Valor)
  a22 <- median(Me$Valor)
  a23 <- median(Su$Valor)
  a24 <- median(Tol$Valor)
  a25 <- median(Cas$Valor)
  a26 <- median(Vall$Valor)
  a27 <- median(RA$Valor)
  a28 <- median(RB$Valor)
  a29 <- median(SA$Valor)
  a30 <- median(SB$Valor)
  
  Median <- data.frame(Original = c(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15),
                       Sensitivity = c(a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30))
  Median <- mutate(Median, variation_percentage = (Sensitivity - Original) / Original)
  attr(Median$Original, "label") <- "CO2 eq per ha [kg CO2 eq /ha]"
  attr(Median$Sensitivity, "label") <- "CO2 eq per ha [kg CO2 eq /ha]"
  write.xlsx(Median, file="Sensibilidad_sd.xlsx")             # Save file with table
}

# Generate density curves with the values from the data frames
{
  P0 <- density(Emisiones2$Valor)
  PA <- density(PA$Valor)
  PB <- density(PB$Valor)
  Ri <- density(Ri$Valor)
  Se <- density(Se$Valor)
  Pcordoba <- density(Cor$Valor)
  Pmeta <- density(Me$Valor)
  Psucre <- density(Su$Valor)
  Ptolima <- density(Tol$Valor)
  Pcasanare <- density(Cas$Valor)
  Pvalle <- density(Vall$Valor)
  PA_r <- density(RA$Valor)
  PA_s <- density(SA$Valor)
  PB_r <- density(RB$Valor)
  PB_s <- density(SB$Valor)
}

# Original curves (calculated results)

{
  P0_o <- density(base$CO2_eq_total_ha)
  # Extrapolation Curves 1 Periods
  PA_o <- density(subset(base$CO2_eq_total_ha, base$d_0 == 3))
  PB_o <- density(subset(base$CO2_eq_total_ha, base$d_0 == 4))
  # Extrapolation Curves 2 Irrigation
  Pr_o <- density(subset(base$CO2_eq_total_ha, base$f_3 == 1))
  Ps_o <- density(subset(base$CO2_eq_total_ha, base$f_3 == 2))
  # Extrapolation Curves 3 Department
  Pcordoba_o <- density(subset(base$CO2_eq_total_ha, base$a_1 == 1))
  Pmeta_o <- density(subset(base$CO2_eq_total_ha, base$a_1 == 2))
  Psucre_o <- density(subset(base$CO2_eq_total_ha, base$a_1 == 3))
  Ptolima_o <- density(subset(base$CO2_eq_total_ha, base$a_1 == 4))
  Pcasanare_o <- density(subset(base$CO2_eq_total_ha, base$a_1 == 5))
  Pvalle_o <- density(subset(base$CO2_eq_total_ha, base$a_1 == 8))
  # Extrapolation Curves 4 Irrigation and Periods
  PA_r_o <- density(subset(base$CO2_eq_total_ha, base$d_0 == 3 & base$f_3 == 1))
  PB_r_o <- density(subset(base$CO2_eq_total_ha, base$d_0 == 4 & base$f_3 == 1))
  PA_s_o <- density(subset(base$CO2_eq_total_ha, base$d_0 == 3 & base$f_3 == 2))
  PB_s_o <- density(subset(base$CO2_eq_total_ha, base$d_0 == 4 & base$f_3 == 2))
}
# Graphical Comparison
{
  # General
  options(scipen = 2)
  plot(P0_o, xlim = c(0, 9000), ylim = c(0, 0.00045), col = "black", main = "General density curves", xlab = "kg CO2 eq/cycle", ylab = "Density", lwd = 2)
  lines(P0, col = "#52b49b", lwd = 1.5)
  legend("topright", c("Normal", "Random 100 values per factor (IPCC)"),
         bty = "n", 
         cex = 0.6, xjust = 7, yjust = 1.0, fill = c("black", "#52b49b", "#FFD000", "purple", "#BE1704", "#0064a2", "orange"))
  
  # Period A
  options(scipen = 2)
  plot(PA_o, xlim = c(0, 9000), ylim = c(0, 0.0006), col = "black", main = "Density curves Period A", xlab = "kg CO2 eq/cycle", ylab = "Density", lwd = 2)
  lines(PA, col = "#52b49b", lwd = 1.5)
  # Period B
  options(scipen = 2)
  plot(PB_o, xlim = c(0, 9000), ylim = c(0, 0.0006), col = "black", main = "Density curves Period B", xlab = "kg CO2 eq/cycle", ylab = "Density", lwd = 2)
  lines(PB, col = "#52b49b", lwd = 1.5)
  # Irrigation
  options(scipen = 2)
  plot(Pr_o, xlim = c(0, 6000), ylim = c(0, 0.0008), col = "black", main = "Density curves Irrigation", xlab = "kg CO2 eq/cycle", ylab = "Density", lwd = 2)
  lines(Ri, col = "#52b49b", lwd = 1.5)
  # Rainfed
  options(scipen = 2)
  plot(Ps_o, xlim = c(0, 6000), ylim = c(0, 0.001), col = "black", main = "Density curves Rainfed", xlab = "kg CO2 eq/cycle", ylab = "Density", lwd = 2)
  lines(Se, col = "#52b49b", lwd = 1.5)
  # Cordoba
  options(scipen = 2)
  plot(Pcordoba_o, xlim = c(0, 6000), ylim = c(0, 0.001), col = "black", main = "Density curves Cordoba", xlab = "kg CO2 eq/cycle", ylab = "Density", lwd = 2)
  lines(Pcordoba, col = "#52b49b", lwd = 1.5)
  # Meta
  plot(Pmeta_o, xlim = c(0, 6000), ylim = c(0, 0.001), col = "black", main = "Density curves Meta", xlab = "kg CO2 eq/cycle", ylab = "Density", lwd = 2)
  lines(Pmeta, col = "#52b49b", lwd = 1.5)
  # Sucre
  plot(Psucre_o, xlim = c(0, 6000), ylim = c(0, 0.001), col = "black", main = "Density curves Sucre", xlab = "kg CO2 eq/cycle", ylab = "Density", lwd = 2)
  lines(Psucre, col = "#52b49b", lwd = 1.5)
  # Tolima
  plot(Ptolima_o, xlim = c(0, 6000), ylim = c(0, 0.001), col = "black", main = "Density curves Tolima", xlab = "kg CO2 eq/cycle", ylab = "Density", lwd = 2)
  lines(Ptolima, col = "#52b49b", lwd = 1.5)
  # Casanare
  plot(Pcasanare_o, xlim = c(0, 6000), ylim = c(0, 0.0013), col = "black", main = "Density curves Casanare", xlab = "kg CO2 eq/cycle", ylab = "Density", lwd = 2)
  lines(Pcasanare, col = "#52b49b", lwd = 1.5)
  # Valle
  plot(Pvalle_o, xlim = c(0, 6000), ylim = c(0, 0.0006), col = "black", main = "Density curves Valle", xlab = "kg CO2 eq/cycle", ylab = "Density", lwd = 2)
  lines(Pvalle, col = "#52b49b", lwd = 1.5)
  # Irrigation period A
  plot(PA_r_o, xlim = c(0, 6000), ylim = c(0, 0.0008), col = "black", main = "Density curves Irrigation period A", xlab = "kg CO2 eq/cycle", ylab = "Density", lwd = 2)
  lines(PA_r, col = "#52b49b", lwd = 1.5)
  # Irrigation period B
  plot(PB_r_o, xlim = c(0, 6000), ylim = c(0, 0.0009), col = "black", main = "Density curves Irrigation period B", xlab = "kg CO2 eq/cycle", ylab = "Density", lwd = 2)
  lines(PB_r, col = "#52b49b", lwd = 1.5)
  # Rainfed period A
  plot(PA_s_o, xlim = c(0, 6000), ylim = c(0, 0.0010), col = "black", main = "Density curves Rainfed period A", xlab = "kg CO2 eq/cycle", ylab = "Density", lwd = 2)
  lines(PA_s, col = "#52b49b", lwd = 1.5)
  # Rainfed period B
  plot(PB_s_o, xlim = c(0, 6000), ylim = c(0, 0.0009), col = "black", main = "Density curves Rainfed period B", xlab = "kg CO2 eq/cycle", ylab = "Density", lwd = 2)
  lines(PB_s, col = "#52b49b", lwd = 2)
}
# Extrapolations----------------------------------------------------------------
# Areas
{
  a0 <- sum(EVA$Área.cosechada)
  a1 <- aggregate(EVA$Área.cosechada, by = list(EVA$Periodo), FUN = sum, na.rm = TRUE)               # Sum of area by period
  a2 <- aggregate(EVA$Área.cosechada, by = list(EVA$sistema), FUN = sum, na.rm = TRUE)               # Sum of area by irrigation system
  a3 <- aggregate(EVA$Área.cosechada, by = list(EVA$Departamento), FUN = sum, na.rm = TRUE)          # Sum of area by department
  a4 <- aggregate(EVA$Área.cosechada, by = list(EVA$Periodo, EVA$sistema), FUN = sum, na.rm = TRUE)  # Sum of area by period and irrigation systems
}
# 0 General Extrapolation
{set.seed(0)
  vr0 <- list()
  for(i in 1:a0){
    vr0[i] <- pmax(sample(P0$x, size = 1, prob = P0$y, replace = FALSE), 0)
  }
  vr0df <- as.data.frame(unlist(vr0))
  vr0 <- sum(vr0df$`unlist(vr0df)`) / 1000
  extra0 <- vr0df
}
# 1 Extrapolate by periods
{set.seed(1)
  vr1A <- list()
  vr1B <- list()
  for(i in 1:a1[1, 2]){                                                                          # Area from 1 to the value of ha of period 1
    vr1A[i] <- pmax(sample(PA$x, size = 1, prob = PA$y, replace = FALSE), 0)
  }
  vr1Adf <- as.data.frame(unlist(vr1A))
  vr1A <- sum(vr1Adf$`unlist(vr1Adf)`) / 1000
  for(i in 1:a1[2, 2]){
    vr1B[i] <- pmax(sample(PB$x, size = 1, prob = PB$y, replace = FALSE), 0)
  }
  vr1Bdf <- as.data.frame(unlist(vr1B))
  vr1B <- sum(vr1Bdf$`unlist(vr1Bdf)`) / 1000
  extra1 <- vr1A + vr1B
}
# 2 Extrapolate by irrigation system
{set.seed(2)
  vr2r <- list()
  vr2s <- list()
  for(i in 1:a2[1, 2]){
    vr2r[i] <- pmax(sample(Ri$x, size = 1, prob = Ri$y, replace = FALSE), 0)
  }
  vr2rdf <- as.data.frame(unlist(vr2r))
  vr2r <- sum(vr2rdf$`unlist(vr2rdf)`) / 1000
  for(i in 1:a2[2, 2]){
    vr2s[i] <- pmax(sample(Se$x, size = 1, prob = Se$y, replace = FALSE), 0)
  }
  vr2sdf <- as.data.frame(unlist(vr2s))
  vr2s <- sum(vr2sdf$`unlist(vr2sdf)`) / 1000
  extra2 <- vr2r + vr2s
}
# 3 Extrapolate by departments

{
        vr3cordoba<-list()
        vr3meta<-list()
        vr3sucre<-list()
        vr3tolima<-list()
        vr3casanare<-list()
        vr3valle<-list()
        vr3otros<-list()
        set.seed(4)
        for(i in 1:a3[a3$Group.1== "Córdoba",2]){
                vr3cordoba[i]<-pmax(sample(Pcordoba$x, size = 1, prob = Pcordoba$y),0)
        }
        vr3cordobadf<-as.data.frame(unlist(vr3cordoba))
        vr3cordoba<- sum(vr3cordobadf$`unlist(vr3cordobadf)`)/1000
        
        for(i in 1:a3[a3$Group.1== "Meta",2]){
                vr3meta[i]<-pmax(sample(Pmeta$x, size = 1, prob = Pmeta$y),0)
        }
        vr3metadf<-as.data.frame(unlist(vr3meta))
        vr3meta <- sum(vr3metadf$`unlist(vr3metadf)`)/1000
        
        for(i in 1:a3[a3$Group.1== "Sucre",2]){
                vr3sucre[i]<-pmax(sample(Psucre$x, size = 1, prob = Psucre$y),0)
        }
        vr3sucredf<-as.data.frame(unlist(vr3sucre))
        vr3sucre <- sum(vr3sucredf$`unlist(vr3sucredf)`)/1000
        
        for(i in 1:a3[a3$Group.1== "Tolima",2]){
                vr3tolima[i]<-pmax(sample(Ptolima$x, size = 1, prob = Ptolima$y),0)
        }
        vr3tolimadf<-as.data.frame(unlist(vr3tolima))
        vr3tolima  <- sum(vr3tolimadf$`unlist(vr3tolimadf)`)/1000
        
        for(i in 1:a3[a3$Group.1== "Casanare",2]){
                vr3casanare[i]<-pmax(sample(Pcasanare$x, size = 1, prob = Pcasanare$y),0)
        }
        vr3casanaredf<-as.data.frame(unlist(vr3casanare))
        vr3casanare <- sum(vr3casanaredf$`unlist(vr3casanaredf)`)/1000
        
        for(i in 1:a3[a3$Group.1== "Valle del Cauca",2]){
                vr3valle[i]<-pmax(sample(Pvalle$x, size = 1, prob = Pvalle$y),0)
        }
        vr3valledf<-as.data.frame(unlist(vr3valle))
        vr3valle <- sum(vr3valledf$`unlist(vr3valledf)`)/1000
        
        a5<-sum(a3$x)-a3[a3$Group.1== "Córdoba",2]-a3[a3$Group.1== "Meta",2]-a3[a3$Group.1== "Sucre",2]-a3[a3$Group.1== "Tolima",2]-a3[a3$Group.1== "Casanare",2]-a3[a3$Group.1== "Valle del Cauca",2]
        for(i in 1:a5){
                vr3otros[i]<-pmax(sample(P0$x, size = 1, prob = P0$y),0)
        }
        vr3otrosdf <-as.data.frame(unlist(vr3otros))
        vr3otros <- sum(vr3otrosdf$`unlist(vr3otrosdf)`)/1000
        extra3   <-vr3cordoba+vr3meta+vr3sucre+vr3tolima+vr3casanare+vr3valle+vr3otros
}
#Extrapolate by irrigation periods and irrigation systems.
{
        vr4Ar<-list()
        vr4As<-list()
        vr4Br<-list()
        vr4Bs<-list()
        set.seed(5)
        for(i in 1:a4[1,3]){
                vr4Ar[i]<-pmax(sample(PA_r$x, size = 1, prob = PA_r$y,replace = FALSE),0)
        }
        vr4Ardf<-as.data.frame(unlist(vr4Ar))
        vr4Ar <- sum(vr4Ardf$`unlist(vr4Ardf)`)/1000
        
        for(i in 1:a4[3,3]){
                vr4As[i]<-pmax(sample(PA_s$x, size = 1, prob = PA_s$y,replace = FALSE),0)
        }
        vr4Asdf<-as.data.frame(unlist(vr4As))
        vr4As <- sum(vr4Asdf$`unlist(vr4Asdf)`)/1000
        
        for(i in 1:a4[2,3]){
                vr4Br[i]<-pmax(sample(PB_r$x, size = 1, prob = PB_r$y,replace = FALSE),0)
        }
        vr4Brdf<-as.data.frame(unlist(vr4Br))
        vr4Br <- sum(vr4Brdf$`unlist(vr4Brdf)`)/1000
        
        for(i in 1:a4[4,3]){
                vr4Bs[i]<-pmax(sample(PB_s$x, size = 1, prob = PB_s$y,replace = FALSE),0)
        }
        vr4Bsdf<-as.data.frame(unlist(vr4Bs))
        vr4Bs <- sum(vr4Bsdf$`unlist(vr4Bsdf)`)/1000
        extra4<-vr4Ar+vr4As+vr4Br+vr4Bs
}
Extrap_sensibi<-data.frame(Exp0=extra0,Exp1=extra1,Exp2=extra2,Exp3=extra3,Exp4=extra4)

#Department distribution matrix
#Curvas de densidad----------------------------------------------
#Logic vectors
RCordoba<-ifelse(base2$a_1== 1 & base2$f_3==1, TRUE, FALSE)
RMeta<-ifelse(base2$a_1== 2 & base2$f_3==1, TRUE, FALSE)
RSucre<-ifelse(base2$a_1== 3 & base2$f_3==1, TRUE, FALSE)
RTolima<-ifelse(base2$a_1== 4 & base2$f_3==1, TRUE, FALSE)
RCasanare<-ifelse(base2$a_1== 5 & base2$f_3==1, TRUE, FALSE)
RValle<-ifelse(base2$a_1== 8 & base2$f_3==1, TRUE, FALSE)

SCordoba<-ifelse(base2$a_1== 1 & base2$f_3==2, TRUE, FALSE)
SMeta<-ifelse(base2$a_1== 2 & base2$f_3==2, TRUE, FALSE)
SSucre<-ifelse(base2$a_1== 3 & base2$f_3==2, TRUE, FALSE)
STolima<-ifelse(base2$a_1== 4 & base2$f_3==2, TRUE, FALSE)
SCasanare<-ifelse(base2$a_1== 5 & base2$f_3==2, TRUE, FALSE)
SValle<-ifelse(base2$a_1== 8 & base2$f_3==2, TRUE, FALSE)

#Subset of list (using logical vectors)
RCordoba<-Emisiones[RCordoba]
RMeta<-Emisiones[RMeta]
RSucre<-Emisiones[RSucre]
RTolima<-Emisiones[RTolima]
RCasanare<-Emisiones[RCasanare]
RValle<-Emisiones[RValle]

SCordoba<-Emisiones[SCordoba]
SMeta<-Emisiones[SMeta]
SSucre<-Emisiones[SSucre]
STolima<-Emisiones[STolima]
SCasanare<-Emisiones[SCasanare]
SValle<-Emisiones[SValle]

#Convert list values to DF
RCordoba<-data.frame(Valor = unlist(RCordoba))
RMeta<-data.frame(Valor = unlist(RMeta))
RSucre<-data.frame(Valor = unlist(RSucre))
RTolima<-data.frame(Valor = unlist(RTolima))
RCasanare<-data.frame(Valor = unlist(RCasanare))
RValle<-data.frame(Valor = unlist(RValle))

SCordoba<-data.frame(Valor = unlist(SCordoba))
SMeta<-data.frame(Valor = unlist(SMeta))
SSucre<-data.frame(Valor = unlist(SSucre))
STolima<-data.frame(Valor = unlist(STolima))
SCasanare<-data.frame(Valor = unlist(SCasanare))
SValle<-data.frame(Valor = unlist(SValle))

#Original versions
RPcordoba_o<-density(subset(base$CO2_eq_total_ha, base$a_1==1 & base2$f_3==1))
RPtolima_o<-density(subset(base$CO2_eq_total_ha, base$a_1==4 & base2$f_3==1))
RPcasanare_o<-density(subset(base$CO2_eq_total_ha, base$a_1==5 & base2$f_3==1))
RPvalle_o<-density(subset(base$CO2_eq_total_ha, base$a_1==8 & base2$f_3==1))

SPcordoba_o<-density(subset(base$CO2_eq_total_ha, base$a_1==1 & base2$f_3==2))
SPmeta_o<-density(subset(base$CO2_eq_total_ha, base$a_1==2 & base2$f_3==2))
SPsucre_o<-density(subset(base$CO2_eq_total_ha, base$a_1==3 & base2$f_3==2))
SPcasanare_o<-density(subset(base$CO2_eq_total_ha, base$a_1==5 & base2$f_3==2))

#Plots--------------------
#Density curve Córdoba
options(scipen = 2)
plot(Pcordoba_o, xlim = c(-2000, 6000), ylim=c(0,0.0005),col = "black", main = "Curvas de densidad Córdoba", xlab = "kg CO2 eq/ciclo",ylab = "Density", lwd = 2)
lines(Pcordoba, col = "#52b49b",lwd = 1.5)

#Density curve Córdoba irrigation
plot(RPcordoba_o, xlim = c(-2000, 10000), ylim=c(0,0.001), col = "black", main = "Curvas de densidad Córdoba riego", xlab = "kg CO2 eq/ciclo",ylab = "Density", lwd = 2)
lines(density(RCordoba$Valor), col = "#52b49b", lwd = 1.5)

#Density curve Cordoba rainfed
plot(SPcordoba_o, xlim = c(-2000, 100000), ylim=c(0,0.000045), col = "black", main = "Curvas de densidad Córdoba secano", xlab = "kg CO2 eq/ciclo",ylab = "Density", lwd = 2)
lines(density(SCordoba$Valor), col = "#52b49b", lwd = 1.5)

#Density curve Meta
plot(Pmeta_o, xlim = c(-2000, 20000), ylim=c(0,0.001), col = "black", main = "Curvas de densidad Meta", xlab = "kg CO2 eq/ciclo",ylab = "Density", lwd = 2)
lines(Pmeta, col = "#52b49b", lwd = 1.5)

#Density curve Meta rainfed
plot(SPmeta_o, xlim = c(-2000, 100000), ylim=c(0,0.000035), col = "black", main = "Curvas de densidad Meta secano", xlab = "kg CO2 eq/ciclo",ylab = "Density", lwd = 2)
lines(density(SMeta$Valor), col = "#52b49b", lwd = 1.5)

#Density curve  Sucre
plot(Psucre_o, xlim = c(-2000, 40000), ylim=c(0,0.00015), col = "black", main = "Curvas de densidad Sucre", xlab = "kg CO2 eq/ciclo",ylab = "Density", lwd = 2)
lines(Psucre, col = "#52b49b", lwd = 1.5)

#Density curve  Sucre rainfed (same)
plot(SPsucre_o, xlim = c(-2000, 40000), ylim=c(0,0.00015), col = "black", main = "Curvas de densidad Sucre secano", xlab = "kg CO2 eq/ciclo",ylab = "Density", lwd = 2)
lines(density(SSucre$Valor), col = "#52b49b", lwd = 1.5)

#Density curve Tolima
plot(Ptolima_o, xlim = c(-2000, 100000), ylim=c(0,0.000035), col = "black", main = "Curvas de densidad Tolima", xlab = "kg CO2 eq/ciclo",ylab = "Density", lwd = 2)
lines(Ptolima, col = "#52b49b", lwd = 1.55)

#Density curve Tolima irrigation (same)
plot(RPtolima_o, xlim = c(-2000, 100000), ylim=c(0,0.000035), col = "black", main = "Curvas de densidad Tolima riego", xlab = "kg CO2 eq/ciclo",ylab = "Density", lwd = 2)
lines(density(RTolima$Valor), col = "#52b49b", lwd = 1.5)

# Casanare Density Curve
plot(Pcasanare_o, xlim = c(-2000, 300000), ylim = c(0, 0.00001), col = "black", main = "Casanare Density Curves", xlab = "kg CO2 eq/cycle", ylab = "Density", lwd = 2)
lines(Pcasanare, col = "#52b49b", lwd = 1.5)

# Casanare Irrigation Density Curve
plot(RPcasanare_o, xlim = c(-2000, 5000), ylim = c(0, 0.000665), col = "black", main = "Casanare Irrigation Density Curves", xlab = "kg CO2 eq/cycle", ylab = "Density", lwd = 2)
lines(density(RCasanare$Valor), col = "#52b49b", lwd = 1.5)

# Casanare Rainfed Density Curve
plot(SPcasanare_o, xlim = c(-2000, 4000), ylim = c(0, 0.000010), col = "black", main = "Casanare Rainfed Density Curves", xlab = "kg CO2 eq/cycle", ylab = "Density", lwd = 2)
lines(density(SCasanare$Valor), col = "#52b49b", lwd = 1.5)

# Valle del Cauca Density Curve
plot(Pvalle_o, xlim = c(-2000, 150000), ylim = c(0, 0.00002), col = "black", main = "Valle del Cauca Density Curves", xlab = "kg CO2 eq/cycle", ylab = "Density", lwd = 2)
lines(Pvalle, col = "#52b49b", lwd = 1.5)

# Valle del Cauca Irrigation Density Curve (same)
plot(RPvalle_o, xlim = c(-2000, 150000), ylim = c(0, 0.00002), col = "black", main = "Valle del Cauca Irrigation Density Curves", xlab = "kg CO2 eq/cycle", ylab = "Density", lwd = 2)
lines(density(RValle$Valor), col = "#52b49b", lwd = 1.5)

# Triangular Distribution Plots

plot(density(rtriangle(100, 0.1, 0.3, 0.2)), main = "Distribution of CO2 Emissions (urea)")
plot(density(rtriangle(100, 0.065, 0.195, 0.13)), main = "CO2 emissions (lime)")
plot(density(rtriangle(100, 0.8, 0.99, 0.89)), main = "SFp")
plot(density(rtriangle(100, 0.86, 1.88, 1.27)), main = "EFc")
plot(density(rtriangle(100, 0.73, 1.27, 1.0)), main = "SFw Continuous flooding")
plot(density(rtriangle(100, 0.11, 0.24, 0.16)), main = "SFw Drought-prone")
plot(density(rtriangle(100, 0.013, 0.019, 0.016)), main = "Direct N2O emissions")
plot(density(rtriangle(100, 0.0, 0.01, 0.003)), main = "Direct N2O emissions (irrigated)")
plot(density(rtriangle(100, 0.011, 0.017, 0.014)), main = "EF4 Indirect N2O emissions (volatilization)")
plot(density(rtriangle(100, 0.03, 0.43, 0.15)), main = "FracGASF Indirect N2O emissions (volatilization)")
plot(density(rtriangle(100, 0.0, 0.02, 0.011)), main = "EF5 Indirect N2O emissions (Leaching)")
plot(density(rtriangle(100, 0.01, 0.73, 0.24)), main = "FracLEACH Indirect N2O emissions (Leaching)")

