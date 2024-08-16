#Extrapolación de las emisiones a nivel nacional
#Maria del Mar Esponda
#2023
# Extrapolating based on emissions per hectare and emissions per production

# Libraries
library(openxlsx)
library(dplyr)
library(readxl)
library(haven)
library(ggplot2)
library(RColorBrewer)
library(ggplot2)

# Set directory
setwd("your_file_location")

# Load and modify datasets--------------------------------------------------------------------------------------
EVA <- read.xlsx("EVA_Arroz_2020.xlsx")                                                           # Agricultural Evaluation base for rice for the year 2020
EVA <- mutate(EVA, sistema = ifelse(Desagregación.cultivo == "Arroz Riego", 1, 2))                # Create dummy for irrigation = 1, rainfed = 2
base <- read.xlsx("final_base.xlsx")                                                              # Our estimation (from line 1295 in Estimation_process.R)
base$d_0 <- replace(base$d_0, is.na(base$d_0), 77)                                                # There is an observation with NA in the period, so we will classify it as 'other'                                
table(base$d_0)
a <- aggregate(base$f_19, by = list(base$d_0), FUN = sum, na.rm = TRUE)                           # Sum of sample area by production cycle
b <- aggregate(base$f20_cosecha_ton, by = list(base$d_0), FUN = sum, na.rm = TRUE)                # Sum of sample area by production cycle
# According to our sample, we have records for the production cycle periods (d_0): 
# (1) 12 observations for the first half (January-June) 2019, 
# (2) 42 observations for the second half (June-December) 2019, 
# (3) 284 observations for the first half (January-June) 2020
# (4) 145 observations for the second half (June-December) 2020
# (77) 98 observations in other periods. We do not know which ones

# Therefore, for our extrapolation, we only have 429 observations
base <- subset(base, d_0 == 3 | d_0 == 4)                                                          # Limit the base to the 2020 periods

# Extrapolation by rule of three at the departmental level with average irrigation and rainfed (Units in tons of CO2 eq)-------------------------------------------------------------------
dptos <- as.data.frame(tapply(EVA$Área.cosechada, EVA$Departamento, FUN = sum,                     # Total area by department [ha]
                              simplify = TRUE, na.rm = TRUE))
colnames(dptos)[1] <- "area_ha"
a <- mean(base$CO2_eq_total_ha)
dptos <- mutate(dptos, emisiones_tco2eq = round(area_ha * a, 1), dpto = rownames(dptos))           # Multiply by 2.810 to convert the results directly to tons and not kg (Note that this value corresponds to the average emission intensity for 2020)    

riego_ha = aggregate(subset(EVA$Área.cosechada, EVA$sistema == 1),                                 # Sum of ha by department for irrigation
                     by = list(subset(EVA$Departamento, EVA$sistema == 1)), FUN = sum, na.rm = TRUE)
secano_ha = aggregate(subset(EVA$Área.cosechada, EVA$sistema == 2),                                # Sum of ha by department for rainfed
                      by = list(subset(EVA$Departamento, EVA$sistema == 2)), FUN = sum, na.rm = TRUE)
colnames(riego_ha) <- c("dpto", "riego_ha")
colnames(secano_ha) <- c("dpto", "secano_ha")
dptos <- left_join(dptos, riego_ha, by = "dpto")
dptos <- left_join(dptos, secano_ha, by = "dpto")

a1 <- mean(subset(base$CO2_eq_total_ha, base$f_3 == 1))                                                   # Average emissions for irrigation (Only 2020 values)
a2 <- mean(subset(base$CO2_eq_total_ha, base$f_3 == 2))                                                   # Average emissions for rainfed (Only 2020 values)

dptos <- mutate(dptos, emir_tco2eq = round(riego_ha * a1, 1))
dptos <- mutate(dptos, emis_tco2eq = round(secano_ha * a2, 1))

# Previous method
# Extrapolation by rule of three at the departmental level with average irrigation and rainfed by department (Units in tons of CO2 eq)
emisi <- as.data.frame(tapply(base$CO2_eq_total_ha, base$a_1, FUN = mean,                        # Average emissions per hectare by department
                              simplify = TRUE, na.rm = TRUE))
riego_em = aggregate(subset(base$CO2_eq_total_ha, base$f_3 == 1),                                # Average emissions by department for irrigation
                     by = list(subset(base$a_1, base$f_3 == 1)), FUN = mean, na.rm = TRUE)
secano_em = aggregate(subset(base$CO2_eq_total_ha, base$f_3 == 2),                               # Average emissions by department for rainfed
                      by = list(subset(base$a_1, base$f_3 == 2)), FUN = mean, na.rm = TRUE)

# Calculate emission values for departments with data
{dptos <- mutate(dptos, emisiones_tco2eq = ifelse(dpto == "Córdoba", round(area_ha * 1.794339, 1), emisiones_tco2eq))
  dptos <- mutate(dptos, emisiones_tco2eq = ifelse(dpto == "Meta", round(area_ha * 1.542148, 1), emisiones_tco2eq))
  dptos <- mutate(dptos, emisiones_tco2eq = ifelse(dpto == "Sucre", round(area_ha * 1.3602, 1), emisiones_tco2eq))
  dptos <- mutate(dptos, emisiones_tco2eq = ifelse(dpto == "Tolima", round(area_ha * 4.300440, 1), emisiones_tco2eq))
  dptos <- mutate(dptos, emisiones_tco2eq = ifelse(dpto == "Casanare", round(area_ha * 2.044402, 1), emisiones_tco2eq))
  dptos <- mutate(dptos, emisiones_tco2eq = ifelse(dpto == "Valle del Cauca", round(area_ha * 4.324493, 1), emisiones_tco2eq))
  
  dptos <- mutate(dptos, emir_tco2eq = ifelse(dpto == "Córdoba", round(area_ha * 4.012202, 1), emir_tco2eq))
  dptos <- mutate(dptos, emir_tco2eq = ifelse(dpto == "Tolima", round(area_ha * 4.300440, 1), emir_tco2eq))
  dptos <- mutate(dptos, emir_tco2eq = ifelse(dpto == "Casanare", round(area_ha * 4.121557, 1), emir_tco2eq))
  dptos <- mutate(dptos, emir_tco2eq = ifelse(dpto == "Valle del Cauca", round(area_ha * 4.324493, 1), emir_tco2eq))
  
  dptos <- mutate(dptos, emis_tco2eq = ifelse(dpto == "Córdoba", round(area_ha * 1.290279, 1), emis_tco2eq))
  dptos <- mutate(dptos, emis_tco2eq = ifelse(dpto == "Meta", round(area_ha * 1.542148, 1), emis_tco2eq))
  dptos <- mutate(dptos, emis_tco2eq = ifelse(dpto == "Sucre", round(area_ha * 1.3602, 1), emis_tco2eq))
  dptos <- mutate(dptos, emis_tco2eq = ifelse(dpto == "Casanare", round(area_ha * 1.547691, 1), emis_tco2eq))
}

# Basic method----------------------------
# Areas________________________________________
{
  a0 <- sum(EVA$Área.cosechada)
  a1 <- aggregate(EVA$Área.cosechada, by = list(EVA$Periodo), FUN = sum, na.rm = TRUE)               # Sum of area by period
  a2 <- aggregate(EVA$Área.cosechada, by = list(EVA$sistema), FUN = sum, na.rm = TRUE)               # Sum of area by irrigation system
  a3 <- aggregate(EVA$Área.cosechada, by = list(EVA$Departamento), FUN = sum, na.rm = TRUE)          # Sum of area by department
  a4 <- aggregate(EVA$Área.cosechada, by = list(EVA$Periodo, EVA$sistema), FUN = sum, na.rm = TRUE)  # Sum of area by period and irrigation systems
}
# Density curves for each disaggregation AREA
{
  # Curve 0
  P0 <- density(base$CO2_eq_total_ha)
  # Extrapolation Curves 1 Periods
  PA <- density(subset(base$CO2_eq_total_ha, base$d_0 == 3))
  PB <- density(subset(base$CO2_eq_total_ha, base$d_0 == 4))
  # Extrapolation Curves 2 Irrigation
  Pr <- density(subset(base$CO2_eq_total_ha, base$f_3 == 1))
  Ps <- density(subset(base$CO2_eq_total_ha, base$f_3 == 2))
  # Extrapolation Curves 3 Department
  Pcordoba <- density(subset(base$CO2_eq_total_ha, base$a_1 == 1))
  Pmeta <- density(subset(base$CO2_eq_total_ha, base$a_1 == 2))
  Psucre <- density(subset(base$CO2_eq_total_ha, base$a_1 == 3))
  Ptolima <- density(subset(base$CO2_eq_total_ha, base$a_1 == 4))
  Pcasanare <- density(subset(base$CO2_eq_total_ha, base$a_1 == 5))
  Pvalle <- density(subset(base$CO2_eq_total_ha, base$a_1 == 8))
  # Extrapolation Curves 4 Irrigation and periods
  PA_r <- density(subset(base$CO2_eq_total_ha, base$d_0 == 3 & base$f_3 == 1))
  PA_s <- density(subset(base$CO2_eq_total_ha, base$d_0 == 3 & base$f_3 == 2))
  PB_r <- density(subset(base$CO2_eq_total_ha, base$d_0 == 4 & base$f_3 == 1))
  PB_s <- density(subset(base$CO2_eq_total_ha, base$d_0 == 4 & base$f_3 == 2))
}

{
  P0$n
  # Extrapolation Curves 1 Periods
  PA$n
  PB$n
  # Extrapolation Curves 2 Irrigation
  Pr$n
  Ps$n
  # Extrapolation Curves 3 Department
  Pcordoba$n
  Pmeta$n
  Psucre$n
  Ptolima$n
  Pcasanare$n
  Pvalle$n
  # Extrapolation Curves 4 Irrigation and periods
  PA_r$n
  PA_s$n
  PB_r$n
  PB_s$n
} # Bandwidth

# Distribution curves plots
{plot(P0)
  
  plot(PA)
  plot(PB)
  
  plot(Pr)
  plot(Ps)
  
  plot(Pcordoba)
  plot(Pmeta)
  plot(Psucre)
  plot(Ptolima)
  plot(Pcasanare)
  plot(Pvalle)
  
  plot(PA_r)
  plot(PA_s)
  plot(PB_r)
  plot(PB_s)
}

# 0 General Extrapolation
{set.seed(0)
  vr0 <- list()
  for(i in 1:a0){
    vr0[i] <- pmax(sample(P0$x, size = 1, prob = P0$y, replace = FALSE), 0)
  }
  vr0df <- as.data.frame(unlist(vr0))
  vr0 <- sum(vr0df$`unlist(vr0df)`)
  extra0 <- vr0
}

# 1 Extrapolate by periods
{set.seed(1)
  vr1A <- list()
  vr1B <- list()
  for(i in 1:a1[1, 2]){
    vr1A[i] <- pmax(sample(PA$x, size = 1, prob = PA$y, replace = FALSE), 0)
  }
  vr1Adf <- as.data.frame(unlist(vr1A))
  vr1A <- sum(vr1Adf$`unlist(vr1Adf)`)
  for(i in 1:a1[2, 2]){
    vr1B[i] <- pmax(sample(PB$x, size = 1, prob = PB$y, replace = FALSE), 0)
  }
  vr1Bdf <- as.data.frame(unlist(vr1B))
  vr1B <- sum(vr1Bdf$`unlist(vr1Bdf)`)
  extra1 <- vr1A + vr1B
}

# 2 Extrapolate by irrigation system
{set.seed(2)
  vr2r <- list()
  vr2s <- list()
  for(i in 1:a2[1, 2]){
    vr2r[i] <- pmax(sample(Pr$x, size = 1, prob = Pr$y, replace = FALSE), 0)
  }
  vr2rdf <- as.data.frame(unlist(vr2r))
  vr2r <- sum(vr2rdf$`unlist(vr2rdf)`)
  for(i in 1:a2[2, 2]){
    vr2s[i] <- pmax(sample(Ps$x, size = 1, prob = Ps$y, replace = FALSE), 0)
  }
  vr2sdf <- as.data.frame(unlist(vr2s))
  vr2s <- sum(vr2sdf$`unlist(vr2sdf)`)
  extra2 <- vr2r + vr2s
}

# 3 Extrapolate by departments
{set.seed(9)
  wr3cordoba <- list()
  wr3meta <- list()
  wr3sucre <- list()
  wr3tolima <- list()
  wr3casanare <- list()
  wr3valle <- list()
  wr3others <- list()
  
  for(i in 1:a3[a3$Group.1 == "Córdoba", 2]){
    wr3cordoba[i] <- pmax(sample(Pcordoba$x, size = 1, prob = Pcordoba$y), 0)
  }
  wr3cordoba <- as.data.frame(unlist(wr3cordoba))
  wr3cordoba <- sum(wr3cordoba$`unlist(wr3cordoba)`)
  
  for(i in 1:a3[a3$Group.1 == "Meta", 2]){
    wr3meta[i] <- pmax(sample(Pmeta$x, size = 1, prob = Pmeta$y), 0)
  }
  wr3meta <- as.data.frame(unlist(wr3meta))
  wr3meta <- sum(wr3meta$`unlist(wr3meta)`)
  
  for(i in 1:a3[a3$Group.1 == "Sucre", 2]){
    wr3sucre[i] <- pmax(sample(Psucre$x, size = 1, prob = Psucre$y), 0)
  }
  wr3sucre <- as.data.frame(unlist(wr3sucre))
  wr3sucre <- sum(wr3sucre$`unlist(wr3sucre)`)
  
  for(i in 1:a3[a3$Group.1 == "Tolima", 2]){
    wr3tolima[i] <- pmax(sample(Ptolima$x, size = 1, prob = Ptolima$y), 0)
  }
  wr3tolima <- as.data.frame(unlist(wr3tolima))
  wr3tolima <- sum(wr3tolima$`unlist(wr3tolima)`)
  
  for(i in 1:a3[a3$Group.1 == "Casanare", 2]){
    wr3casanare[i] <- pmax(sample(Pcasanare$x, size = 1, prob = Pcasanare$y), 0)
  }
  wr3casanare <- as.data.frame(unlist(wr3casanare))
  wr3casanare <- sum(wr3casanare$`unlist(wr3casanare)`)
  
  for(i in 1:a3[a3$Group.1 == "Valle del Cauca", 2]){
    wr3valle[i] <- pmax(sample(Pvalle$x, size = 1, prob = Pvalle$y), 0)
  }
  wr3valle <- as.data.frame(unlist(wr3valle))
  wr3valle <- sum(wr3valle$`unlist(wr3valle)`)
  
  a5 <- sum(a3$x) - a3[a3$Group.1 == "Córdoba", 2] - a3[a3$Group.1 == "Meta", 2] - a3[a3$Group.1 == "Sucre", 2] - a3[a3$Group.1 == "Tolima", 2] - a3[a3$Group.1 == "Casanare", 2] - a3[a3$Group.1 == "Valle del Cauca", 2]
  for(i in 1:a5){
    wr3others[i] <- pmax(sample(P0$x, size = 1, prob = P0$y), 0)
  }
  wr3others <- as.data.frame(unlist(wr3others))
  wr3others <- sum(wr3others$`unlist(wr3others)`)
  extra3 <- wr3cordoba + wr3meta + wr3sucre + wr3tolima + wr3casanare + wr3valle + wr3others
}

# 4 Extrapolate by periods and irrigation systems
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
vr4Ar <- sum(vr4Ardf$`unlist(vr4Ardf)`)

for(i in 1:a4[3,3]){
        vr4As[i]<-pmax(sample(PA_s$x, size = 1, prob = PA_s$y,replace = FALSE),0)
}
vr4Asdf<-as.data.frame(unlist(vr4As))
vr4As <- sum(vr4Asdf$`unlist(vr4Asdf)`)

for(i in 1:a4[2,3]){
        vr4Br[i]<-pmax(sample(PB_r$x, size = 1, prob = PB_r$y,replace = FALSE),0)
}
vr4Brdf<-as.data.frame(unlist(vr4Br))
vr4Br <- sum(vr4Brdf$`unlist(vr4Brdf)`)

for(i in 1:a4[4,3]){
        vr4Bs[i]<-pmax(sample(PB_s$x, size = 1, prob = PB_s$y,replace = FALSE),0)
}
vr4Bsdf<-as.data.frame(unlist(vr4Bs))
vr4Bs <- sum(vr4Bsdf$`unlist(vr4Bsdf)`)
extra4<-vr4Ar+vr4As+vr4Br+vr4Bs
}

# Bind analysis ----------------
# At this point, we rerun the Extrapolacion.R script to compare the results with the original extrapolations
uno_0 <- vr0df$`unlist(vr0)`
dos_0 <- c(vr1Adf$`unlist(vr1A)`, vr1Bdf$`unlist(vr1B)`)
tres_0 <- c(vr2rdf$`unlist(vr2r)`, vr2sdf$`unlist(vr2s)`)
cuatro_0 <- c(vr4Ardf$`unlist(vr4Ar)`, vr4Asdf$`unlist(vr4As)`,
              vr4Brdf$`unlist(vr4Br)`, vr4Bsdf$`unlist(vr4Bs)`)

# Fill the shorter vectors with NA if necessary
dos_0 <- c(dos_0, rep(NA, 693844 - length(dos_0)))
tres_0 <- c(tres_0, rep(NA, 693844 - length(tres_0)))
cuatro_0 <- c(cuatro_0, rep(NA, 693844 - length(cuatro_0)))

Resultados2 <- as.data.frame(cbind(uno_0, dos_0, tres_0, cuatro_0))
# write.xlsx(Resultados2, file = "Extrapolacion_resultados.xlsx")  # Save file with table

Extrapolaciones1 <- data.frame(Exp0 = extra0, Exp1 = extra1, Exp2 = extra2, Exp3 = extra3, Exp4 = extra4)

# Production ________________________________________
{
  p0 <- sum(EVA$Producción)
  p1 <- aggregate(EVA$Producción, by = list(EVA$Periodo), FUN = sum, na.rm = TRUE)  # Sum of production by period
  p2 <- aggregate(EVA$Producción, by = list(EVA$sistema), FUN = sum, na.rm = TRUE)  # Sum of production by irrigation system
  p3 <- aggregate(EVA$Producción, by = list(EVA$Departamento), FUN = sum, na.rm = TRUE)  # Sum of production by department
  p4 <- aggregate(EVA$Producción, by = list(EVA$Periodo, EVA$sistema), FUN = sum, na.rm = TRUE)  # Sum of production by period and irrigation system
}        

# Density curves for each production disaggregation

{
  # Curve 0
  P0 <- density(base$CO2_eq_total_ton)
  
  # Curves Extrapolation 1 Periods
  PA <- density(subset(base$CO2_eq_total_ton, base$d_0 == 3))
  PB <- density(subset(base$CO2_eq_total_ton, base$d_0 == 4))
  
  # Extrapolation curves 2 Irrigation
  Pr <- density(subset(base$CO2_eq_total_ton, base$f_3 == 1))
  Ps <- density(subset(base$CO2_eq_total_ton, base$f_3 == 2))
  
  # Curves Extrapolation 3 Department
  Pcordoba <- density(subset(base$CO2_eq_total_ton, base$a_1 == 1))
  Pmeta <- density(subset(base$CO2_eq_total_ton, base$a_1 == 2))
  Psucre <- density(subset(base$CO2_eq_total_ton, base$a_1 == 3))
  Ptolima <- density(subset(base$CO2_eq_total_ton, base$a_1 == 4))
  Pcasanare <- density(subset(base$CO2_eq_total_ton, base$a_1 == 5))
  Pvalle <- density(subset(base$CO2_eq_total_ton, base$a_1 == 8))
  
  # Curves Extrapolation4 Watering and periods
  PA_r <- density(subset(base$CO2_eq_total_ton, base$d_0 == 3 & base$f_3 == 1))
  PA_s <- density(subset(base$CO2_eq_total_ton, base$d_0 == 3 & base$f_3 == 2))
  PB_r <- density(subset(base$CO2_eq_total_ton, base$d_0 == 4 & base$f_3 == 1))
  PB_s <- density(subset(base$CO2_eq_total_ton, base$d_0 == 4 & base$f_3 == 2))
}

# 0 General extrapolation
{
  set.seed(6)
  wr0 <- list()
  for (i in 1:p0) {
    wr0[i] <- pmax(sample(P0$x, size = 1, prob = P0$y, replace = FALSE), 0)
  }
  wr0 <- as.data.frame(unlist(wr0))
  wr0 <- sum(wr0$`unlist(wr0)`)
  extra0 <- wr0
}

# 1 Extrapolate by periods
{
  set.seed(7)
  wr1A <- list()
  wr1B <- list()
  for (i in 1:p1[1, 2]) {
    wr1A[i] <- pmax(sample(PA$x, size = 1, prob = PA$y, replace = FALSE), 0)
  }
  wr1A <- as.data.frame(unlist(wr1A))
  wr1A <- sum(wr1A$`unlist(wr1A)`)
  for (i in 1:p1[2, 2]) {
    wr1B[i] <- pmax(sample(PB$x, size = 1, prob = PB$y, replace = FALSE), 0)
  }
  wr1B <- as.data.frame(unlist(wr1B))
  wr1B <- sum(wr1B$`unlist(wr1B)`)
  extra1 <- wr1A + wr1B
}

# 2 Extrapolate by irrigation system
{
  set.seed(8)
  wr2r <- list()
  wr2s <- list()
  for (i in 1:p2[1, 2]) {
    wr2r[i] <- pmax(sample(Pr$x, size = 1, prob = Pr$y, replace = FALSE), 0)
  }
  wr2r <- as.data.frame(unlist(wr2r))
  wr2r <- sum(wr2r$`unlist(wr2r)`)
  for (i in 1:p2[2, 2]) {
    wr2s[i] <- pmax(sample(Ps$x, size = 1, prob = Ps$y, replace = FALSE), 0)
  }
  wr2s <- as.data.frame(unlist(wr2s))
  wr2s <- sum(wr2s$`unlist(wr2s)`)
  extra2 <- wr2r + wr2s
}

# 3 Extrapolate by department
{
  set.seed(9)
  wr3cordoba <- list()
  wr3meta <- list()
  wr3sucre <- list()
  wr3tolima <- list()
  wr3casanare <- list()
  wr3valle <- list()
  wr3otros <- list()
  
  for (i in 1:p3[p3$Group.1 == "Córdoba", 2]) {
    wr3cordoba[i] <- pmax(sample(Pcordoba$x, size = 1, prob = Pcordoba$y), 0)
  }
  wr3cordoba <- as.data.frame(unlist(wr3cordoba))
  wr3cordoba <- sum(wr3cordoba$`unlist(wr3cordoba)`)
  
  for (i in 1:p3[p3$Group.1 == "Meta", 2]) {
    wr3meta[i] <- pmax(sample(Pmeta$x, size = 1, prob = Pmeta$y), 0)
  }
  wr3meta <- as.data.frame(unlist(wr3meta))
  wr3meta <- sum(wr3meta$`unlist(wr3meta)`)
  
  for (i in 1:p3[p3$Group.1 == "Sucre", 2]) {
    wr3sucre[i] <- pmax(sample(Psucre$x, size = 1, prob = Psucre$y), 0)
  }
  wr3sucre <- as.data.frame(unlist(wr3sucre))
  wr3sucre <- sum(wr3sucre$`unlist(wr3sucre)`)
  
  for (i in 1:p3[p3$Group.1 == "Tolima", 2]) {
    wr3tolima[i] <- pmax(sample(Ptolima$x, size = 1, prob = Ptolima$y), 0)
  }
  wr3tolima <- as.data.frame(unlist(wr3tolima))
  wr3tolima <- sum(wr3tolima$`unlist(wr3tolima)`)
  
  for (i in 1:p3[p3$Group.1 == "Casanare", 2]) {
    wr3casanare[i] <- pmax(sample(Pcasanare$x, size = 1, prob = Pcasanare$y), 0)
  }
  wr3casanare <- as.data.frame(unlist(wr3casanare))
  wr3casanare <- sum(wr3casanare$`unlist(wr3casanare)`)
  
  for (i in 1:p3[p3$Group.1 == "Valle del Cauca", 2]) {
    wr3valle[i] <- pmax(sample(Pvalle$x, size = 1, prob = Pvalle$y), 0)
  }
  wr3valle <- as.data.frame(unlist(wr3valle))
  wr3valle <- sum(wr3valle$`unlist(wr3valle)`)
  
  p5 <- sum(p3$x) - p3[p3$Group.1 == "Córdoba", 2] - p3[p3$Group.1 == "Meta", 2] - p3[p3$Group.1 == "Sucre", 2] - p3[p3$Group.1 == "Tolima", 2] - p3[p3$Group.1 == "Casanare", 2] - p3[p3$Group.1 == "Valle del Cauca", 2]
  for (i in 1:p5) {
    wr3otros[i] <- pmax(sample(P0$x, size = 1, prob = P0$y), 0)
  }
  wr3otros <- as.data.frame(unlist(wr3otros))
  wr3otros <- sum(wr3otros$`unlist(wr3otros)`)
  extra3 <- wr3cordoba + wr3meta + wr3sucre + wr3tolima + wr3casanare + wr3valle + wr3otros
}

#4 Extrapolate by irrigation periods and irrigation systems
{set.seed(10)
        wr4Ar<-list()
        wr4As<-list()
        wr4Br<-list()
        wr4Bs<-list()
        
        for(i in 1:p4[1,3]){
                wr4Ar[i]<-pmax(sample(PA_r$x, size = 1, prob = PA_r$y,replace = FALSE),0)
        }
        wr4Ar<-as.data.frame(unlist(wr4Ar))
        wr4Ar <- sum(wr4Ar$`unlist(wr4Ar)`)
        
        for(i in 1:p4[3,3]){
                wr4As[i]<-pmax(sample(PA_s$x, size = 1, prob = PA_s$y,replace = FALSE),0)
        }
        wr4As<-as.data.frame(unlist(wr4As))
        wr4As <- sum(wr4As$`unlist(wr4As)`)
        
        for(i in 1:p4[2,3]){
                wr4Br[i]<-pmax(sample(PB_r$x, size = 1, prob = PB_r$y,replace = FALSE),0)
        }
        wr4Br<-as.data.frame(unlist(wr4Br))
        wr4Br <- sum(wr4Br$`unlist(wr4Br)`)
        
        for(i in 1:p4[4,3]){
                wr4Bs[i]<-pmax(sample(PB_s$x, size = 1, prob = PB_s$y,replace = FALSE),0)
        }
        wr4Bs<-as.data.frame(unlist(wr4Bs))
        wr4Bs <- sum(wr4Bs$`unlist(wr4Bs)`)
        extra4<-wr4Ar+wr4As+wr4Br+wr4Bs
}

Extrapolaciones2<-data.frame(Exp0=extra0,Exp1=extra1,Exp2=extra2,Exp3=extra3,Exp4=extra4)

#Data Frame with all extrapolation values
# Create a list of data frames
lista_df2 <- list(wr0,wr1A,wr1B, wr2r,wr2s,wr3cordoba,wr3meta,wr3sucre,wr3tolima,wr3casanare,wr3valle,wr3otros,
                 wr4Ar,wr4As,wr4Br,wr4Bs)

# Change the column name in each data frame in the list to do the rbind

# Export tables-----------------------------------------
#Create a new Excel file
wb <- createWorkbook()
nombres<-c("E0_All_A",
           "E1_First_semester_2020_P",
           "E1_Second_semester_2020_P",
           "E2_Irrigation_P",
           "E2_Rainfed_P",
           "E3_Cordoba_P",
           "E3_Meta_P",
           "E3_Sucre_P",
           "E3_Tolima_P",
           "E3_Casanare_P",
           "E3_Valle_P",
           "E3_Others_departments_P",
           "E4_First_2020_irrigation_P",
           "E4_Second_2020_irrigation_P",
           "E4_First_2020_rainfed_P",
           "E4_Second_2020_rainfed_P")
# Add each table to a separate sheet in the Excel file
for (i in 1:length(lista_df2)) {
        addWorksheet(wb, sheetName = nombres[i])
        writeData(wb, sheet = i, lista_df2[[i]])
}

saveWorkbook(wb, "tab2.xlsx")

# Create a directory for CSV files if it does not exist
if (!dir.exists("CSV_Files")) {
        dir.create("CSV_Files")
}

# Export each table as a CSV file without cuts
for (i in 1:length(lista_df2)) {
        csv_file <- paste("CSV_Files/", nombres[i], ".csv", sep = "")
        write.csv(lista_df2[[i]], csv_file, row.names = FALSE, fileEncoding = "UTF-8")
}

cat("Tablas exportadas como archivos CSV en la carpeta 'CSV_Files'.\n")
# write.csv(wr0, file = "my_data.csv")

# Join tables----------------------------
A.general = vr0
A.periodos = rbind(vr1A,vr1B)
A.riego = rbind(vr2r,vr2s)
A.periodos_riego = rbind(vr4Ar,vr4As,vr4Br,vr4Bs)
P.general = wr0
P.periodos = rbind(wr1A,wr1B)
P.riego = rbind(wr2r,wr2s)
P.periodos_riego = rbind(wr4Ar,wr4As,wr4Br,wr4Bs)

Total<-data.frame(A.general,A.periodos,A.riego,A.periodos_riego)

# mean(base$CO2_eq_total_ton)*sum(EVA$Producción)
# mean(subset(base$CO2_eq_total_ton, base$d_0==3))*sum(subset(EVA$Producción, EVA$Periodo== "2020A"))+mean(subset(base$CO2_eq_total_ton, base$d_0==4))*sum(subset(EVA$Producción, EVA$Periodo== "2020B"))
# mean(subset(base$CO2_eq_total_ton, base$f_3==1))*sum(subset(EVA$Producción, EVA$sistema == 1))+mean(subset(base$CO2_eq_total_ton, base$f_3==2))*sum(subset(EVA$Producción, EVA$sistema == 2))
# 
# Pcordoba<-density(subset(base$CO2_eq_total_ton, base$a_1==1))
# Pmeta<-density(subset(base$CO2_eq_total_ton, base$a_1==2))
# Psucre<-density(subset(base$CO2_eq_total_ton, base$a_1==3))
# Ptolima<-density(subset(base$CO2_eq_total_ton, base$a_1==4))
# Pcasanare<-density(subset(base$CO2_eq_total_ton, base$a_1==5))
# Pvalle<-density(subset(base$CO2_eq_total_ton, base$a_1==8))
# #Curvas Extrapolacion 4 Riego y periodos
# PA_r<-density(subset(base$CO2_eq_total_ton, base$d_0==3 & base$f_3==1))
# PA_s<-density(subset(base$CO2_eq_total_ton, base$d_0==3 & base$f_3==2))
# PB_r<-density(subset(base$CO2_eq_total_ton, base$d_0==4 & base$f_3==1))
# PB_s<-density(subset(base$CO2_eq_total_ton, base$d_0==4 & base$f_3==2))
