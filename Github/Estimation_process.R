#Analysis of Rice Crop Emissions prior. LECRA Survey 2020
#Maria del Mar Esponda #2023

#Libraries
library(haven)
library(dplyr)
library(ggplot2)
library(lessR)
library(openxlsx)
library(GGally)
library(readxl)
library(RColorBrewer)

#Set directory
setwd("your_file_location")
setwd("D:/OneDrive - CGIAR/2021 -- Climate change rice Colombia/2022 -- Towards site specific GHG measures rice/Analysis/Green/Github")

#Load and modify datasets----------------------------------------------------------
prior <- read_dta("prior_base.dta")                                                          #Load dataset (previous results, the values will be replaced by other values)
prior <- prior[,c(1,2,3,4,6,7,10,24,25,27)]                                                  #Select columns of interest from survey dataset (LECRA)
ingr <- read_dta("ingresousd.dta")                                                           #Attach Income from survey dataset (LECRA)
prior <- left_join(prior, ingr, by = "id_productor")
time_lapse <- read_dta("time_lapse.dta")                                                     #Time lapse of cultivation from survey dataset (LECRA)
prior <- left_join(prior, time_lapse, by = "id_productor")
attr(prior$ingreso_usd2020, "label") <- "total income in dollars"

#Nitrogen content in kg
Fertiliz <- read_dta("fertilizantes.dta")                                                    #Fertilizer dataset
Fertiliz <- mutate(Fertiliz, cantidad_N_ap = (g_18_a_ * grado_N) / 100)                      #Create variable cantidad_N_ap
attr(Fertiliz$cantidad_N_ap, "label") <- "[kg]"
Nitro <- as.data.frame(tapply(Fertiliz$cantidad_N_ap,                                        #Sum of kg N per producer
                              Fertiliz$id_productor, FUN = sum,
                              simplify = TRUE, na.rm = TRUE))
colnames(Nitro) <- "cant_N_kg"                                                                            #Rename the variable
Nitro <- mutate(Nitro, id_productor = rownames(Nitro))
Nitro$id_productor <- as.numeric(Nitro$id_productor)                                                      #Change variable class to numeric (for joining)
prior <- left_join(prior, Nitro, by = "id_productor")                                                     #Add kg N per producer column to the prior (Lina)
attr(prior$cant_N_kg, "label") <- "Taken from fertilizer amount * % nitrogen of that fertilizer"          #Create variable description kg N

#Urea applied per producer
Fertiliz$Urea <- ifelse(Fertiliz$g_15_ == "Urea", Fertiliz$g_18_a_, NA)                                   #Separate Urea contents in kg
attr(Fertiliz$Urea, "label") <- "Urea application [kg]"
urea_s <- aggregate(Fertiliz$Urea, by = list(Fertiliz$id_productor), FUN = sum, na.rm = TRUE)             #Sum of urea per producer
colnames(urea_s) <- c("id_productor", "Urea_kg")
prior <- left_join(prior, urea_s, by = "id_productor")
attr(prior$Urea_kg, "label") <- "Sum of Urea in kg"

#Lime applied per producer
Fertiliz$Cal <- ifelse(Fertiliz$g_15_ == "Cal Dolomita" | Fertiliz$g_15_ == "Cal", Fertiliz$g_18_a_, NA)  #Separate Lime contents in kg
attr(Fertiliz$Cal, "label") <- "Dolomite Lime application [kg]"
cal_s <- aggregate(Fertiliz$Cal, by = list(Fertiliz$id_productor), FUN = sum, na.rm = TRUE)               #Sum of lime per producer
colnames(cal_s) <- c("id_productor", "Cal_kg")
attr(cal_s$Cal_kg, "label") <- "Sum of Lime in kg"
prior <- left_join(prior, cal_s, by = "id_productor")

#Municipality prior with name codes
Municipios <- read.xlsx("Munici_name.xlsx")                                                               #Load municipality names corresponding to each municipality code

#Virtual prior for CO2 analysis: Exclude 2 yield outliers and 10 empty harvest ton values
codigos_lina <- c("170001", "190022",                                                                     #These codes are considered complete outliers by Lina
                  "130004", "130011",
                  "130015", "130016",
                  "130017", "130018",
                  "130023", "180005",
                  "220012", "270001")
#These codes are considered complete outliers due to yield
#codes to be removed
codigos_a_eliminar <- c("210052", "130024", #These codes are considered complete outliers due to yield
                        "220028", "220026",
                        "190045", "190034",
                        "230031", "230017",
                        "260006", "260004",
                        "280001", "140004",
                        "140043", "110009",
                        "230049", "230016",
                        "140006", "140046",
                        "290001", "210034",
                        "160021", "130020",
                        "180023")

# Remove the rows corresponding to the codes
prior <- prior[!(prior$id_productor %in% codigos_lina), ]
prior <- prior[!(prior$id_productor %in% codigos_a_eliminar), ]

#Direct nitrous oxide emissions----------------------------------------------------------
prior$EF_dir <- ifelse(prior$f_3 == 1, 0.003,                                                                #We will consider EF1FR which is for irrigated rice cultivation (0.003). In this case, we take 0.016 for rainfed.
                      ifelse(prior$f_3 == 2, 0.016, NA))
attr(prior$EF_dir, "label") <- "Direct EF correction"
prior <- mutate(prior, EDirectas_co2eq = cant_N_kg * EF_dir * (44/28) * 265)
attr(prior$EDirectas_co2eq, "label") <- "Direct emissions  [kg CO2 eq]"

#Indirect nitrous oxide emissions----------------------------------------------------------
#Volatilization
prior <- mutate(prior, EVol = round((cant_N_kg * 0.15 * 0.014) * (44/28) * 265, 2))
attr(prior$EVol, "label") <- "Volatilization emissions  [kg CO2 eq]"

#Leaching
prior <- mutate(prior, ELix = round((cant_N_kg * 0.24 * 0.011) * (44/28) * 265, 2))
attr(prior$ELix, "label") <- "Leaching emissions  [kg CO2 eq]"

prior <- mutate(prior, EIndirectas_co2eq = round((EVol + ELix), 2))
attr(prior$EIndirectas_co2eq, "label") <- "Indirect emissions  [kg CO2 eq]"

prior <- mutate(prior, Oxido_N_co2eq = round((EDirectas_co2eq + EIndirectas_co2eq), 2))
attr(prior$Oxido_N_co2eq, "label") <- "Total nitrous oxide emissions [kg CO2 eq]"

#Methane emissions----------------------------------------------------------
prior$SFw_2 <- ifelse(prior$f_3 == 1, 1, ifelse(prior$f_3 == 2, 0.16, NA))                                      #New factors, irrigation-> 1.0, rainfed-> 0.16
attr(prior$SFw_2, "label") <- "Modified factor associated with the crop's water regime"
prior <- mutate(prior, Met_co2_2 = round((EFc * SFw_2 * SFp * duracion_ciclo_dias_mod * f_19 * 28), 2))
attr(prior$Met_co2_2, "label") <- "Methane emissions [kg CO2 eq]"

#Urea emissions------------------------------------------------------------------------------------------
prior <- mutate(prior, EUrea = round((Urea_kg * 0.2 * (44/12)), 2))
prior <- mutate(prior, Urea_ha = round((Urea_kg / f_19), 2))
attr(prior$Urea_ha, "label") <- "Urea in kg per hectare"
prior$EUrea <- ifelse(prior$Urea_ha <= 600, prior$EUrea, 0)                            #Limit urea emissions to only applications less than 600 kg per hectare
attr(prior$EUrea, "label") <- "Urea emissions  [kg CO2 eq]"

#Lime emissions------------------------------------------------------------------------------------------
prior <- mutate(prior, ECal = round((Cal_kg * 0.13 * (44/12)), 2))
prior <- mutate(prior, Cal_ha = round(Cal_kg / f_19, 2))
attr(prior$ECal, "label") <- "Recalculated lime emissions"

#Total emissions-----------------------------------------------------------------------------------------------------
##Create emissions per ha-1 columns
prior <- mutate(prior, 
               eindirecta = EIndirectas_co2eq / f_19,
               edirecta = EDirectas_co2eq / f_19,
               emetano = Met_co2_2 / f_19,
               eurea = EUrea / f_19,
               ecal = ECal / f_19)

#Recalculate the total CO2 eq column considering methane emissions even in cases without fertilization
prior <- mutate(prior, fert_CO2_eq_total = Oxido_N_co2eq + EUrea + ECal)
attr(prior$fert_CO2_eq_total, "label") <- "Emissions from fertilizers"

prior <- mutate(prior, fert_CO2_eq_total_ha = fert_CO2_eq_total / f_19)
attr(prior$fert_CO2_eq_total_ha, "label") <- "Emissions from fertilizers per hectare"

prior <- mutate(prior, CO2_eq_total = Met_co2_2 + fert_CO2_eq_total)                                   
attr(prior$CO2_eq_total, "label") <- "Total CO2 eq emissions [kg CO2 eq]"

prior <- mutate(prior, CO2_eq_total_ha = CO2_eq_total / f_19)
attr(prior$CO2_eq_total_ha, "label") <- "Total CO2 eq emissions per hectare [kg CO2 eq ha-1]"

prior <- mutate(prior, CO2_eq_total_ton = CO2_eq_total / f20_cosecha_ton)
attr(prior$CO2_eq_total_ton, "label") <- "Total CO2 eq emissions per ton of paddy"

prior <- mutate(prior, CO2_ingr = CO2_eq_total / ingreso_usd2020)                       #calculate emissions/revenues 
attr(prior$CO2_ingr, "label") <- "CO2/revenue [kg CO2 eq/dollar]"

#Remove infinite or NaN values due to division
prior <- do.call(data.frame, lapply(prior, function(x) replace(x, is.infinite(x), NA)))
prior <- do.call(data.frame, lapply(prior, function(x) replace(x, is.nan(x), NA)))

#Identify the dataset-------------------------------------------------------------------------------------------------
#Data description
#Dictionary
nom <- as.data.frame(colnames(prior))
atri <- sapply(prior, attr, "label")
atri <- as.data.frame(unlist(atri))
atri <- mutate(atri, variable = rownames(atri))
colnames(nom)[1] <- "variable"
dicc <- left_join(nom, atri, by = "variable")

#General table
count(prior[1])
summary(prior$CO2_eq_total)
summary(prior$CO2_eq_total_ha)
summary(prior$CO2_eq_total_ton)
sd(prior$CO2_eq_total, na.rm = TRUE)
sd(prior$CO2_eq_total_ha, na.rm = TRUE)
sd(prior$CO2_eq_total_ton, na.rm = TRUE)
sum(prior$CO2_eq_total, na.rm = TRUE)
sum(prior$CO2_eq_total_ha, na.rm = TRUE)
sum(prior$CO2_eq_total_ton, na.rm = TRUE)

summary(subset(prior$CO2_eq_total_ha, prior$f_3 == 1))  #Irrigation
summary(subset(prior$CO2_eq_total_ha, prior$f_3 == 2))  #Dryland

sd(subset(prior$CO2_eq_total_ha, prior$f_3 == 1))  #Irrigation
sd(subset(prior$CO2_eq_total_ha, prior$f_3 == 2))  #Dryland

sum(subset(prior$CO2_eq_total, prior$f_3 == 1))  #Irrigation
sum(subset(prior$CO2_eq_total, prior$f_3 == 2))  #Dryland

sum(subset(prior$f_19, prior$f_3 == 1))  #Irrigation
sum(subset(prior$f_19, prior$f_3 == 2))  #Dryland
sum(prior$f_19) 

summary(prior$rendimiento_ton_ha)
summary(subset(prior$rendimiento_ton_ha, prior$f_3 == 1))  #Irrigation
summary(subset(prior$rendimiento_ton_ha, prior$f_3 == 2))  #Dryland

#Corrected averages-----------------------------------------------------------

#Average emissions
{
  # Unweighted (by producer)
  # > mean(prior$CO2_eq_total_ton)
  # [1] 527.1753
  # > mean(prior$CO2_eq_total_ha)
  # [1] 2629.155
  # Weighted (general)
  # > sum(prior$CO2_eq_total) / sum(prior$f20_cosecha_ton)
  # [1] 385.0629
  # > sum(prior$CO2_eq_total) / sum(prior$f_19)
  # [1] 1970.618
  
  sum(prior$CO2_eq_total, na.rm = TRUE) / sum(prior$f_19)                                  # Average emissions per hectare
  sum(subset(prior$CO2_eq_total, prior$d_0 == 3)) / sum(subset(prior$f_19, prior$d_0 == 3))      # Average emissions per hectare 2020A
  sum(subset(prior$CO2_eq_total, prior$d_0 == 4)) / sum(subset(prior$f_19, prior$d_0 == 4))      # Average emissions per hectare 2020B
  sum(subset(prior$CO2_eq_total, prior$f_3 == 1)) / sum(subset(prior$f_19, prior$f_3 == 1))      # Average emissions per hectare irrigation
  sum(subset(prior$CO2_eq_total, prior$f_3 == 2)) / sum(subset(prior$f_19, prior$f_3 == 2))      # Average emissions per hectare dryland
  
  sum(subset(prior$CO2_eq_total, prior$a_1 == 1)) / sum(subset(prior$f_19, prior$a_1 == 1))      # Average emissions per hectare Córdoba
  sum(subset(prior$CO2_eq_total, prior$a_1 == 2)) / sum(subset(prior$f_19, prior$a_1 == 2))      # Average emissions per hectare Meta
  sum(subset(prior$CO2_eq_total, prior$a_1 == 3)) / sum(subset(prior$f_19, prior$a_1 == 3))      # Average emissions per hectare Sucre
  sum(subset(prior$CO2_eq_total, prior$a_1 == 4)) / sum(subset(prior$f_19, prior$a_1 == 4))      # Average emissions per hectare Tolima
  sum(subset(prior$CO2_eq_total, prior$a_1 == 5)) / sum(subset(prior$f_19, prior$a_1 == 5))      # Average emissions per hectare Casanare
  sum(subset(prior$CO2_eq_total, prior$a_1 == 8)) / sum(subset(prior$f_19, prior$a_1 == 8))      # Average emissions per hectare Valle del Cauca
  
  sum(subset(prior$CO2_eq_total, prior$a_1 == 1 & prior$f_3 == 1)) / sum(subset(prior$f_19, prior$a_1 == 1 & prior$f_3 == 1))      # Average emissions per hectare Córdoba irrigation
  sum(subset(prior$CO2_eq_total, prior$a_1 == 2 & prior$f_3 == 1)) / sum(subset(prior$f_19, prior$a_1 == 2 & prior$f_3 == 1))      # Average emissions per hectare Meta irrigation
  sum(subset(prior$CO2_eq_total, prior$a_1 == 5 & prior$f_3 == 1)) / sum(subset(prior$f_19, prior$a_1 == 5 & prior$f_3 == 1))      # Average emissions per hectare Casanare irrigation
  sum(subset(prior$CO2_eq_total, prior$a_1 == 1 & prior$f_3 == 2)) / sum(subset(prior$f_19, prior$a_1 == 1 & prior$f_3 == 2))      # Average emissions per hectare Córdoba dryland
  sum(subset(prior$CO2_eq_total, prior$a_1 == 2 & prior$f_3 == 2)) / sum(subset(prior$f_19, prior$a_1 == 2 & prior$f_3 == 2))      # Average emissions per hectare Meta dryland
  sum(subset(prior$CO2_eq_total, prior$a_1 == 5 & prior$f_3 == 2)) / sum(subset(prior$f_19, prior$a_1 == 5 & prior$f_3 == 2))      # Average emissions per hectare Casanare dryland
}
# Yield
{
  sum(prior$f20_cosecha_ton, na.rm = TRUE) / sum(prior$f_19)                                      # Average yield per hectare
  sum(subset(prior$f20_cosecha_ton, prior$f_3 == 1)) / sum(subset(prior$f_19, prior$f_3 == 1))      # Average yield per hectare irrigation
  sum(subset(prior$f20_cosecha_ton, prior$f_3 == 2)) / sum(subset(prior$f_19, prior$f_3 == 2))      # Average yield per hectare dryland
  
  sum(subset(prior$f20_cosecha_ton, prior$a_1 == 1)) / sum(subset(prior$f_19, prior$a_1 == 1))      # Average yield per hectare Córdoba
  sum(subset(prior$f20_cosecha_ton, prior$a_1 == 2)) / sum(subset(prior$f_19, prior$a_1 == 2))      # Average yield per hectare Meta
  sum(subset(prior$f20_cosecha_ton, prior$a_1 == 3)) / sum(subset(prior$f_19, prior$a_1 == 3))      # Average yield per hectare Sucre
  sum(subset(prior$f20_cosecha_ton, prior$a_1 == 4)) / sum(subset(prior$f_19, prior$a_1 == 4))      # Average yield per hectare Tolima
  sum(subset(prior$f20_cosecha_ton, prior$a_1 == 5)) / sum(subset(prior$f_19, prior$a_1 == 5))      # Average yield per hectare Casanare
  sum(subset(prior$f20_cosecha_ton, prior$a_1 == 8)) / sum(subset(prior$f_19, prior$a_1 == 8))      # Average yield per hectare Valle del Cauca
  
  sum(subset(prior$f20_cosecha_ton, prior$a_1 == 1 & prior$f_3 == 1)) / sum(subset(prior$f_19, prior$a_1 == 1 & prior$f_3 == 1))      # Average yield per hectare Córdoba irrigation
  sum(subset(prior$f20_cosecha_ton, prior$a_1 == 2 & prior$f_3 == 1)) / sum(subset(prior$f_19, prior$a_1 == 2 & prior$f_3 == 1))      # Average yield per hectare Meta irrigation
  sum(subset(prior$f20_cosecha_ton, prior$a_1 == 5 & prior$f_3 == 1)) / sum(subset(prior$f_19, prior$a_1 == 5 & prior$f_3 == 1))      # Average yield per hectare Casanare irrigation
  sum(subset(prior$f20_cosecha_ton, prior$a_1 == 1 & prior$f_3 == 2)) / sum(subset(prior$f_19, prior$a_1 == 1 & prior$f_3 == 2))      # Average yield per hectare Córdoba dryland
  sum(subset(prior$f20_cosecha_ton, prior$a_1 == 2 & prior$f_3 == 2)) / sum(subset(prior$f_19, prior$a_1 == 2 & prior$f_3 == 2))      # Average yield per hectare Meta dryland
  sum(subset(prior$f20_cosecha_ton, prior$a_1 == 5 & prior$f_3 == 2)) / sum(subset(prior$f_19, prior$a_1 == 5 & prior$f_3 == 2))      # Average yield per hectare Casanare dryland
}
# Emissions per ton of production
{
  # prior <- mutate(prior, CO2_eq_total_ton = CO2_eq_total / f20_cosecha_ton)
  sum(prior$CO2_eq_total, na.rm = TRUE) / sum(prior$f20_cosecha_ton)                                 # Average emissions per ton of paddy rice
  sum(subset(prior$CO2_eq_total, prior$f_3 == 1)) / sum(subset(prior$f20_cosecha_ton, prior$f_3 == 1))     # Average emissions per ton irrigation
  sum(subset(prior$CO2_eq_total, prior$f_3 == 2)) / sum(subset(prior$f20_cosecha_ton, prior$f_3 == 2))     # Average emissions per ton dryland
  
  mean(prior$CO2_eq_total_ton)
  mean(subset(prior$CO2_eq_total_ton, prior$a_1 == 2))
  sum(subset(prior$CO2_eq_total, prior$a_1 == 1)) / sum(subset(prior$f20_cosecha_ton, prior$a_1 == 1))      # Average emissions per ton Córdoba
  sum(subset(prior$CO2_eq_total, prior$a_1 == 2)) / sum(subset(prior$f20_cosecha_ton, prior$a_1 == 2))      # Average emissions per ton Meta
  sum(subset(prior$CO2_eq_total, prior$a_1 == 3)) / sum(subset(prior$f20_cosecha_ton, prior$a_1 == 3))      # Average emissions per ton Sucre
  sum(subset(prior$CO2_eq_total, prior$a_1 == 4)) / sum(subset(prior$f20_cosecha_ton, prior$a_1 == 4))      # Average emissions per ton Tolima
  sum(subset(prior$CO2_eq_total, prior$a_1 == 5)) / sum(subset(prior$f20_cosecha_ton, prior$a_1 == 5))      # Average emissions per ton Casanare
  sum(subset(prior$CO2_eq_total, prior$a_1 == 8)) / sum(subset(prior$f20_cosecha_ton, prior$a_1 == 8))      # Average emissions per ton Valle del Cauca
  
  sum(subset(prior$CO2_eq_total, prior$a_1 == 1 & prior$f_3 == 1)) / sum(subset(prior$f20_cosecha_ton, prior$a_1 == 1 & prior$f_3 == 1))      # Average emissions per ton Córdoba irrigation
  sum(subset(prior$CO2_eq_total, prior$a_1 == 2 & prior$f_3 == 1)) / sum(subset(prior$f20_cosecha_ton, prior$a_1 == 2 & prior$f_3 == 1))      # Average emissions per ton Meta irrigation
  sum(subset(prior$CO2_eq_total, prior$a_1 == 5 & prior$f_3 == 1)) / sum(subset(prior$f20_cosecha_ton, prior$a_1 == 5 & prior$f_3 == 1))      # Average emissions per ton Casanare irrigation
  sum(subset(prior$CO2_eq_total, prior$a_1 == 1 & prior$f_3 == 2)) / sum(subset(prior$f20_cosecha_ton, prior$a_1 == 1 & prior$f_3 == 2))      # Average emissions per ton Córdoba dryland
  sum(subset(prior$CO2_eq_total, prior$a_1 == 2 & prior$f_3 == 2)) / sum(subset(prior$f20_cosecha_ton, prior$a_1 == 2 & prior$f_3 == 2))      # Average emissions per ton Meta dryland
  sum(subset(prior$CO2_eq_total, prior$a_1 == 5 & prior$f_3 == 2)) / sum(subset(prior$f20_cosecha_ton, prior$a_1 == 5 & prior$f_3 == 2))      # Average emissions per ton Casanare dryland
}

sum(prior$Urea_kg) / sum(prior$f_19)                                                             # Average application of kg of urea per hectare
sum(subset(prior$Cal_kg, prior$Cal_kg > 0)) / sum(subset(prior$f_19, prior$Cal_kg > 0))            # Average application of kg of lime per hectare 
mean(subset(prior$Cal_ha, prior$Cal_ha > 0))                                                     # Average application of kg of lime per hectare (previous)
sum(prior$ingreso_usd2020) / sum(prior$f20_cosecha_ton)                                          # Average earnings per ton of paddy
prior <- mutate(prior, N_ha = round(cant_N_kg / f_19, 2))                                        # Create variable for N per hectare
mean(subset(prior$N_ha, prior$N_ha > 0))                                                         # Average application of kg of Nitrogen per hectare (previous, good version)
mean(prior$N_ha)                                                                                # Average application of kg of Nitrogen per hectare (previous, bad version)
sum(subset(prior$cant_N_kg, prior$cant_N_kg > 0)) / sum(subset(prior$f_19, prior$cant_N_kg > 0))   # Average application of kg of Nitrogen per hectare (new)

resultado_t <- t.test(prior$CO2_eq_total_ha, conf.level = 0.90)
intervalo_confianza_t <- resultado_t$conf.int
print(intervalo_confianza_t)

resultado_t <- t.test(subset(prior$CO2_eq_total_ha, prior$f_3 == 1), conf.level = 0.90)
intervalo_confianza_t <- resultado_t$conf.int
print(intervalo_confianza_t)

resultado_t <- t.test(subset(prior$CO2_eq_total_ha, prior$f_3 == 2), conf.level = 0.90)
intervalo_confianza_t <- resultado_t$conf.int
print(intervalo_confianza_t)


# Summary statistics of various variables
summary(prior$f_3)                         # Summary of the variable f_3
summary(prior$f_19)                        # Summary of the variable f_19
summary(prior$f20_cosecha_ton)             # Summary of the variable f20_cosecha_ton
summary(prior$rendimiento_ton_ha)          # Summary of the variable rendimiento_ton_ha
summary(prior$duracion_ciclo_dias_mod)     # Summary of the variable duracion_ciclo_dias_mod

# Create a new data frame with selected variables
prueba <- prior[, c(1, 5, 13, 14, 15)]             # Select columns from the prior data frame
prueba <- mutate(prueba, c_ha = Cal_kg / f_19)     # Calculate lime application per hectare
prueba <- mutate(prueba, u_ha = Urea_kg / f_19)    # Calculate urea application per hectare
prueba <- mutate(prueba, n_ha = cant_N_kg / f_19)  # Calculate nitrogen application per hectare

# Summary statistics for new variables
summary(prueba$n_ha)                      # Summary of nitrogen application per hectare
summary(prior$ingreso_usd2020)            # Summary of income in USD 2020

# Sum of variables
sum(prior$f_3)                            # Total of f_3
sum(prior$f_19)                           # Total of f_19
sum(prior$f20_cosecha_ton)                # Total of f20_cosecha_ton
sum(prior$rendimiento_ton_ha)             # Total of rendimiento_ton_ha
sum(prior$duracion_ciclo_dias_mod)        # Total of duracion_ciclo_dias_mod
sum(prueba$n_ha)                          # Total of nitrogen application per hectare
sum(prior$ingreso_usd2020)                # Total of income in USD 2020

# Standard deviation of variables
sd(prior$f_3)                             # Standard deviation of f_3
sd(prior$f_19)                            # Standard deviation of f_19
sd(prior$f20_cosecha_ton)                 # Standard deviation of f20_cosecha_ton
sd(prior$rendimiento_ton_ha)              # Standard deviation of rendimiento_ton_ha
sd(prior$duracion_ciclo_dias_mod)         # Standard deviation of duracion_ciclo_dias_mod
sd(prueba$n_ha)                           # Standard deviation of nitrogen application per hectare
sd(prior$ingreso_usd2020)                 # Standard deviation of income in USD 2020

#Data frame for departments----------------------------------------------------------------------------------------------------------
departamento <- prior[, c(2, 4, 36)]      # Select columns for department analysis

# Create columns for CO2 emissions by department and irrigation/dryland
departamento <- mutate(departamento, Cordoba = ifelse(departamento$a_1 == 1, departamento$CO2_eq_total_ha, NA))
departamento <- mutate(departamento, Cordobar = ifelse(departamento$a_1 == 1 & departamento$f_3 == 1, departamento$CO2_eq_total_ha, NA))
departamento <- mutate(departamento, Cordobas = ifelse(departamento$a_1 == 1 & departamento$f_3 == 2, departamento$CO2_eq_total_ha, NA))

departamento <- mutate(departamento, Meta = ifelse(departamento$a_1 == 2, departamento$CO2_eq_total_ha, NA))
departamento <- mutate(departamento, Metar = ifelse(departamento$a_1 == 2 & departamento$f_3 == 1, departamento$CO2_eq_total_ha, NA))
departamento <- mutate(departamento, Metas = ifelse(departamento$a_1 == 2 & departamento$f_3 == 2, departamento$CO2_eq_total_ha, NA))

departamento <- mutate(departamento, Sucre = ifelse(departamento$a_1 == 3, departamento$CO2_eq_total_ha, NA))
departamento <- mutate(departamento, Sucres = ifelse(departamento$a_1 == 3 & departamento$f_3 == 2, departamento$CO2_eq_total_ha, NA))

departamento <- mutate(departamento, Tolima = ifelse(departamento$a_1 == 4, departamento$CO2_eq_total_ha, NA))
departamento <- mutate(departamento, Tolimar = ifelse(departamento$a_1 == 4 & departamento$f_3 == 1, departamento$CO2_eq_total_ha, NA))

departamento <- mutate(departamento, Casanare = ifelse(departamento$a_1 == 5, departamento$CO2_eq_total_ha, NA))
departamento <- mutate(departamento, Casanarer = ifelse(departamento$a_1 == 5 & departamento$f_3 == 1, departamento$CO2_eq_total_ha, NA))
departamento <- mutate(departamento, Casanares = ifelse(departamento$a_1 == 5 & departamento$f_3 == 2, departamento$CO2_eq_total_ha, NA))

departamento <- mutate(departamento, Valle = ifelse(departamento$a_1 == 8, departamento$CO2_eq_total_ha, NA))
departamento <- mutate(departamento, Valler = ifelse(departamento$a_1 == 8 & departamento$f_3 == 1, departamento$CO2_eq_total_ha, NA))  # Emissions Valle

#Histograms for CO2 emissions by department-------------------------------------------------------------------------------------------------
colors <- brewer.pal(6, "Set3")
par(mar = c(4, 4, 4, 2) + 0.1)               # Adjust margins
options(scipen = 2)                         # Adjust scientific notation options
hist(departamento$Cordoba, breaks = 20, xlim = c(0, 6000), ylim = c(0, 30),   # Histogram for Córdoba emissions
     col = colors[1], alpha = 0.1, main = "Histogram of Emissions", xlab = "kg CO2 eq ha-1", ylab = "Frequency")
hist(departamento$Meta, breaks = 20, col = colors[2], alpha = 0.1, add = TRUE)   # Histogram for Meta emissions
hist(departamento$Sucre, breaks = 20, col = colors[3], alpha = 0.1, add = TRUE)  # Histogram for Sucre emissions
hist(departamento$Tolima, breaks = 20, col = colors[4], alpha = 0.1, add = TRUE) # Histogram for Tolima emissions
hist(departamento$Casanare, breaks = 20, col = colors[5], alpha = 0.1, add = TRUE) # Histogram for Casanare emissions
hist(departamento$Valle, breaks = 20, col = colors[6], alpha = 0.1, add = TRUE)  # Histogram for Valle emissions

# Density plots for CO2 emissions by department
set.seed(1)
colors <- brewer.pal(6, "Set3")
labels <- c("Meta", "Valle", "Casanare", "Sucre", "Tolima", "Córdoba")

# Create density plot
ggplot(data = departamento) +
  geom_density(aes(x = Meta, color = colors[1]), fill = alpha(colors[1], 0.3), alpha = 0.3) +
  geom_density(aes(x = Valle, color = colors[2]), fill = alpha(colors[2], 0.3), alpha = 0.3) +
  geom_density(aes(x = Casanare, color = colors[3]), fill = alpha(colors[3], 0.3), alpha = 0.3) +
  geom_density(aes(x = Sucre, color = colors[4]), fill = alpha(colors[4], 0.3), alpha = 0.3) +
  geom_density(aes(x = Tolima, color = colors[5]), fill = alpha(colors[5], 0.3), alpha = 0.3) +
  geom_density(aes(x = Cordoba, color = colors[6]), fill = alpha(colors[6], 0.3), alpha = 0.3) +
  labs(x = "kg CO2 eq ha-1", y = "Density") +
  ggtitle("Density Plot of Emissions by Department") +
  scale_color_manual(values = colors, labels = labels) +
  theme_bw()

# Boxplot of CO2 emissions by department
boxplot(departamento[, 4:18],
        col = c("aquamarine3", "cyan3", "darkgoldenrod2",
                "aquamarine3", "cyan3", "darkgoldenrod2",
                "aquamarine3", "darkgoldenrod2",
                "aquamarine3", "cyan3",
                "aquamarine3", "cyan3", "darkgoldenrod2",
                "aquamarine3", "cyan3"),
        xaxt = "n",  # Remove x-axis labels
        xlab = "Departments",
        ylab = "CO2 eq ha-1")

# Names of regions for x-axis labels
nombres <- c("Córdoba", "Irrigation", "Rainfed", "Meta", "Irrigation", "Rainfed",
             "Sucre", "Rainfed", "Tolima", "Irrigation", "Casanare",
             "Irrigation", "Rainfed","Valle del Cauca", "Irrigation")

# Vertical orientation of labels
par(las = 2)  
# Reduce margin values
par(mar = c(8.5, 5, 1, 3) + 0.1)  

# Add vertical lines after specific boxes
axis(side = 1, at = 1:15, labels = nombres, tick = FALSE)
lin_1 <- 3
lin_2 <- 6
lin_3 <- 8
lin_4 <- 10
lin_5 <- 13

abline(v = lin_1 + 0.5, col = "burlywood", lty = 2)
abline(v = lin_2 + 0.5, col = "burlywood", lty = 2)
abline(v = lin_3 + 0.5, col = "burlywood", lty = 2)
abline(v = lin_4 + 0.5, col = "burlywood", lty = 2)
abline(v = lin_5 + 0.5, col = "burlywood", lty = 2)

# Color selection
group <- c("cyan3","darkgoldenrod2")

# Convert numeric variable to factor (necessary for plotting)
prior$f_3 <- factor(prior$f_3) 

# Create scatter plot divided by groups with custom colors
ggplot(prior, aes(x = CO2_eq_total_ha, y = rendimiento_ton_ha, color = factor(f_3))) +
  geom_point() +
  labs(x = "Emission [kg CO2 eq ha-1]", y = "Yield [t ha-1]") +
  scale_color_manual(values = c("1" = "cyan3", "2" = "darkgoldenrod2")) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 10000, 1000), labels = c("0", "1000", "2000","3000" ,"4000","5000" ,"6000","7000","8000", "9000","10000")) +
  scale_y_continuous(breaks = seq(0, 14, 2), labels = c("0","2","4","6","8","10","12","14"))

#Emissions by emission source, all observations (including zeros)-------------------------------------------------------------
g0<-c("Indirect_emissions", "Direct_emissions", "Methane", "Urea_Fertilization", "Liming")
g1<-c(sum(prior$EIndirectas_co2eq,na.rm=TRUE),
      sum(prior$EDirectas_co2eq, na.rm=TRUE),
      sum(prior$Met_co2_2, na.rm=TRUE),
      sum(prior$EUrea, na.rm=TRUE),
      sum(prior$ECal,  na.rm=TRUE))
##Emissions by emission source irrigation
g2<-c(sum(subset(prior$EIndirectas_co2eq, prior$f_3 == 1), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$f_3 == 1),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$f_3 == 1),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$f_3 == 1),na.rm=TRUE),
      sum(subset(prior$ECal, prior$f_3 == 1),na.rm=TRUE))
##Emissions by emission source rainfed
g3<-c(sum(subset(prior$EIndirectas_co2eq, prior$f_3 == 2), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$f_3 == 2),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$f_3 == 2),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$f_3 == 2),na.rm=TRUE),
      sum(subset(prior$ECal, prior$f_3 == 2),na.rm=TRUE))
#Create dataframe and round
fuente <- data.frame(Emission = g0, Total = g1, Irrigated =g2, Rainfed=g3)
fuente[,2:4]<-round(fuente[,2:4], digits=1)

#Total = Irrigation + Rainfed
##
b0<-c("Indirect emissions", "Direct emissions", "Methane", "Urea Fertilization", "Liming")
b0_1<-c(sum(prior$EIndirectas_co2eq,na.rm=TRUE),
        sum(prior$EDirectas_co2eq, na.rm=TRUE),
        sum(prior$Met_co2_2, na.rm=TRUE),
        sum(prior$EUrea, na.rm=TRUE),
        sum(prior$ECal,  na.rm=TRUE))

##Emissions by emission source Córdoba
b1<-c(sum(subset(prior$EIndirectas_co2eq, prior$a_1 == 1), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$a_1 == 1),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$a_1 == 1),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$a_1 == 1),na.rm=TRUE),
      sum(subset(prior$ECal, prior$a_1 == 1),na.rm=TRUE))

##Emissions by emission source Meta
b2<-c(sum(subset(prior$EIndirectas_co2eq, prior$a_1 == 2), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$a_1 == 2),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$a_1 == 2),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$a_1 == 2),na.rm=TRUE),
      sum(subset(prior$ECal, prior$a_1 == 2),na.rm=TRUE))

##Emissions by emission source Sucre
b3<-c(sum(subset(prior$EIndirectas_co2eq, prior$a_1 == 3), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$a_1 == 3),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$a_1 == 3),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$a_1 == 3),na.rm=TRUE),
      sum(subset(prior$ECal, prior$a_1 == 3),na.rm=TRUE))

##Emissions by emission source Tolima
b4<-c(sum(subset(prior$EIndirectas_co2eq, prior$a_1 == 4), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$a_1 == 4),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$a_1 == 4),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$a_1 == 4),na.rm=TRUE),
      sum(subset(prior$ECal, prior$a_1 == 4),na.rm=TRUE))

##Emissions by emission source Casanare
b5<-c(sum(subset(prior$EIndirectas_co2eq, prior$a_1 == 5), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$a_1 == 5),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$a_1 == 5),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$a_1 == 5),na.rm=TRUE),
      sum(subset(prior$ECal, prior$a_1 == 5),na.rm=TRUE))

##Emissions by emission source Valle
b6<-c(sum(subset(prior$EIndirectas_co2eq, prior$a_1 == 8), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$a_1 == 8),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$a_1 == 8),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$a_1 == 8),na.rm=TRUE),
      sum(subset(prior$ECal, prior$a_1 == 8),na.rm=TRUE))

#Create dataframe
departamentos <- data.frame(Emission = g0, Total = g1, 
                            Córdoba = b1, 
                            Meta =b2, 
                            Sucre=b3, 
                            Tolima=b4, 
                            Casanare=b5, 
                            Valle=b6)
#Round
departamentos[,2:8]<-round(departamentos[,2:8], digits=1)

#Emissions in Irrigation by Department-------------------------------------------------

c0<-c("Indirect emissions", "Direct emissions", "Methane", "Urea Fertilization", "Liming")
c0_1<-c(sum(subset(prior$EIndirectas_co2eq, prior$f_3 == 1), na.rm=TRUE),
        sum(subset(prior$EDirectas_co2eq, prior$f_3 == 1),na.rm=TRUE),
        sum(subset(prior$Met_co2_2, prior$f_3 == 1),na.rm=TRUE),
        sum(subset(prior$EUrea, prior$f_3 == 1),na.rm=TRUE),
        sum(subset(prior$ECal, prior$f_3 == 1),na.rm=TRUE))

##Emissions in irrigation Córdoba
c1<-c(sum(subset(prior$EIndirectas_co2eq, prior$f_3 == 1 & prior$a_1 == 1), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$f_3 == 1 & prior$a_1 == 1),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$f_3 == 1 & prior$a_1 == 1),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$f_3 == 1 & prior$a_1 == 1),na.rm=TRUE),
      sum(subset(prior$ECal, prior$f_3 == 1 & prior$a_1 == 1),na.rm=TRUE))

##Emissions in irrigation Meta
c2<-c(sum(subset(prior$EIndirectas_co2eq, prior$f_3 == 1 & prior$a_1 == 2), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$f_3 == 1 & prior$a_1 == 2),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$f_3 == 1 & prior$a_1 == 2),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$f_3 == 1 & prior$a_1 == 2),na.rm=TRUE),
      sum(subset(prior$ECal, prior$f_3 == 1 & prior$a_1 == 2),na.rm=TRUE))

##Emissions in irrigation Sucre
c3<-c(sum(subset(prior$EIndirectas_co2eq, prior$f_3 == 1 & prior$a_1 == 3), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$f_3 == 1 & prior$a_1 == 3),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$f_3 == 1 & prior$a_1 == 3),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$f_3 == 1 & prior$a_1 == 3),na.rm=TRUE),
      sum(subset(prior$ECal, prior$f_3 == 1 & prior$a_1 == 3),na.rm=TRUE))

##Emissions in irrigation Tolima
c4<-c(sum(subset(prior$EIndirectas_co2eq, prior$f_3 == 1 & prior$a_1 == 4), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$f_3 == 1 & prior$a_1 == 4),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$f_3 == 1 & prior$a_1 == 4),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$f_3 == 1 & prior$a_1 == 4),na.rm=TRUE),
      sum(subset(prior$ECal, prior$f_3 == 1 & prior$a_1 == 4),na.rm=TRUE))

##Emissions in irrigation Casanare
c5<-c(sum(subset(prior$EIndirectas_co2eq, prior$f_3 == 1 & prior$a_1 == 5), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$f_3 == 1 & prior$a_1 == 5),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$f_3 == 1 & prior$a_1 == 5),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$f_3 == 1 & prior$a_1 == 5),na.rm=TRUE),
      sum(subset(prior$ECal, prior$f_3 == 1 & prior$a_1 == 5),na.rm=TRUE))

##Emissions in irrigation Valle
c6<-c(sum(subset(prior$EIndirectas_co2eq, prior$f_3 == 1 & prior$a_1 == 8), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$f_3 == 1 & prior$a_1 == 8),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$f_3 == 1 & prior$a_1 == 8),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$f_3 == 1 & prior$a_1 == 8),na.rm=TRUE),
      sum(subset(prior$ECal, prior$f_3 == 1 & prior$a_1 == 8),na.rm=TRUE))

#Create dataframe and round
irrigation_departments <- data.frame(Emission = c0, Total = c0_1, 
                                     Córdoba = c1, 
                                     Meta = c2, 
                                     Sucre = c3, 
                                     Tolima = c4, 
                                     Casanare = c5, 
                                     Valle = c6)
irrigation_departments[,2:8]<-round(irrigation_departments[,2:8], digits=1)

#Emissions in Rainfed by Department-------------------------------------------------
d0<-c("Indirect emissions", "Direct emissions", "Methane", "Urea Fertilization", "Liming")
d0_1<-c(sum(subset(prior$EIndirectas_co2eq, prior$f_3 == 2), na.rm=TRUE),
        sum(subset(prior$EDirectas_co2eq, prior$f_3 == 2),na.rm=TRUE),
        sum(subset(prior$Met_co2_2, prior$f_3 == 2),na.rm=TRUE),
        sum(subset(prior$EUrea, prior$f_3 == 2),na.rm=TRUE),
        sum(subset(prior$ECal, prior$f_3 == 2),na.rm=TRUE))

##Emissions in rainfed Córdoba
d1<-c(sum(subset(prior$EIndirectas_co2eq, prior$f_3 == 2 & prior$a_1 == 1), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$f_3 == 2 & prior$a_1 == 1),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$f_3 == 2 & prior$a_1 == 1),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$f_3 == 2 & prior$a_1 == 1),na.rm=TRUE),
      sum(subset(prior$ECal, prior$f_3 == 2 & prior$a_1 == 1),na.rm=TRUE))

##Emissions in rainfed Meta
d2<-c(sum(subset(prior$EIndirectas_co2eq, prior$f_3 == 2 & prior$a_1 == 2), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$f_3 == 2 & prior$a_1 == 2),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$f_3 == 2 & prior$a_1 == 2),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$f_3 == 2 & prior$a_1 == 2),na.rm=TRUE),
      sum(subset(prior$ECal, prior$f_3 == 2 & prior$a_1 == 2),na.rm=TRUE))

##Emissions in rainfed Sucre
d3<-c(sum(subset(prior$EIndirectas_co2eq, prior$f_3 == 2 & prior$a_1 == 3), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$f_3 == 2 & prior$a_1 == 3),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$f_3 == 2 & prior$a_1 == 3),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$f_3 == 2 & prior$a_1 == 3),na.rm=TRUE),
      sum(subset(prior$ECal, prior$f_3 == 2 & prior$a_1 == 3),na.rm=TRUE))

##Emissions in rainfed Tolima
d4<-c(sum(subset(prior$EIndirectas_co2eq, prior$f_3 == 2 & prior$a_1 == 4), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$f_3 == 2 & prior$a_1 == 4),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$f_3 == 2 & prior$a_1 == 4),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$f_3 == 2 & prior$a_1 == 4),na.rm=TRUE),
      sum(subset(prior$ECal, prior$f_3 == 2 & prior$a_1 == 4),na.rm=TRUE))

##Emissions in rainfed Casanare
d5<-c(sum(subset(prior$EIndirectas_co2eq, prior$f_3 == 2 & prior$a_1 == 5), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$f_3 == 2 & prior$a_1 == 5),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$f_3 == 2 & prior$a_1 == 5),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$f_3 == 2 & prior$a_1 == 5),na.rm=TRUE),
      sum(subset(prior$ECal, prior$f_3 == 2 & prior$a_1 == 5),na.rm=TRUE))

##Emissions in rainfed Valle
d6<-c(sum(subset(prior$EIndirectas_co2eq, prior$f_3 == 2 & prior$a_1 == 8), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$f_3 == 2 & prior$a_1 == 8),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$f_3 == 2 & prior$a_1 == 8),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$f_3 == 2 & prior$a_1 == 8),na.rm=TRUE),
      sum(subset(prior$ECal, prior$f_3 == 2 & prior$a_1 == 8),na.rm=TRUE))

#Create dataframe and round
rainfed_departments <- data.frame(Emission = d0, Total = d0_1, 
                                  Córdoba = d1, 
                                  Meta = d2, 
                                  Sucre = d3, 
                                  Tolima = d4, 
                                  Casanare = d5, 
                                  Valle = d6)
rainfed_departments[,2:8]<-round(rainfed_departments[,2:8], digits=1)

#Add labels
labels <- c("Indirect emissions", "Direct emissions", "Methane", "Urea Fertilization", "Liming")

d3<-c(sum(subset(prior$EIndirectas_co2eq, prior$a_1 == 3 & prior$f_3 == 2), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$a_1 == 3 & prior$f_3 == 2),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$a_1 == 3 & prior$f_3 == 2),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$a_1 == 3 & prior$f_3 == 2),na.rm=TRUE),
      sum(subset(prior$ECal, prior$a_1 == 3 & prior$f_3 == 2),na.rm=TRUE))
## Emissions by source in Tolima
d4<-c(sum(subset(prior$EIndirectas_co2eq, prior$a_1 == 4 & prior$f_3 == 2), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$a_1 == 4 & prior$f_3 == 2),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$a_1 == 4 & prior$f_3 == 2),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$a_1 == 4 & prior$f_3 == 2),na.rm=TRUE),
      sum(subset(prior$ECal, prior$a_1 == 4 & prior$f_3 == 2),na.rm=TRUE))
## Emissions by source in Casanare
d5<-c(sum(subset(prior$EIndirectas_co2eq, prior$a_1 == 5 & prior$f_3 == 2), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$a_1 == 5 & prior$f_3 == 2),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$a_1 == 5 & prior$f_3 == 2),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$a_1 == 5 & prior$f_3 == 2),na.rm=TRUE),
      sum(subset(prior$ECal, prior$a_1 == 5 & prior$f_3 == 2),na.rm=TRUE))
## Emissions by source in Valle
d6<-c(sum(subset(prior$EIndirectas_co2eq, prior$a_1 == 8  & prior$f_3 == 2), na.rm=TRUE),
      sum(subset(prior$EDirectas_co2eq, prior$a_1 == 8 & prior$f_3 == 2),na.rm=TRUE),
      sum(subset(prior$Met_co2_2, prior$a_1 == 8 & prior$f_3 == 2),na.rm=TRUE),
      sum(subset(prior$EUrea, prior$a_1 == 8 & prior$f_3 == 2),na.rm=TRUE),
      sum(subset(prior$ECal, prior$a_1 == 8 & prior$f_3 == 2),na.rm=TRUE))
#Create dataframe
departamentosS <- data.frame(Emission = d0, Total= d0_1, 
                             Córdoba = d1, 
                             Meta =d2, 
                             Sucre=d3, 
                             Tolima=d4, 
                             Casanare=d5, 
                             Valle=d6)
#Round values
departamentosS[,2:8]<-round(departamentosS[,2:8], digits=1)

#Average emissions by source for all observations-----------------------
g0<-c("Indirect_emissions", "Direct_emissions", "Methane", "Urea_Fertilization", "Liming")
g1<-c(mean(prior$eindirecta, na.rm=TRUE),
      mean(prior$edirecta, na.rm=TRUE),
      mean(prior$emetano, na.rm=TRUE),
      mean(prior$eurea, na.rm=TRUE),
      mean(prior$ecal, na.rm=TRUE))

## Emissions by source in irrigated areas
g2<-c(mean(subset(prior$eindirecta, prior$f_3 == 1), na.rm=TRUE),
      mean(subset(prior$edirecta, prior$f_3 == 1),na.rm=TRUE),
      mean(subset(prior$emetano, prior$f_3 == 1),na.rm=TRUE),
      mean(subset(prior$eurea, prior$f_3 == 1),na.rm=TRUE),
      mean(subset(prior$ecal, prior$f_3 == 1),na.rm=TRUE))

## Emissions by source in rainfed areas

g3<-c(mean(subset(prior$eindirecta, prior$f_3 == 2), na.rm=TRUE),
      mean(subset(prior$edirecta, prior$f_3 == 2),na.rm=TRUE),
      mean(subset(prior$emetano, prior$f_3 == 2),na.rm=TRUE),
      mean(subset(prior$eurea, prior$f_3 == 2),na.rm=TRUE),
      mean(subset(prior$ecal, prior$f_3 == 2),na.rm=TRUE))

#Create dataframe and round values
fuente_m <- data.frame(Emission = g0, Total = g1, Irrigated = g2, Rainfed = g3)
fuente_m[,2:4] <- round(fuente_m[,2:4], digits=1)

#Municipalities--------------------------------------------------------------------
#ALL
m_0 <- table(prior$a_2)                                                                    #Observations by municipality
m_1 <- aggregate(prior$CO2_eq_total, by = list(prior$a_2), FUN = sum, na.rm=TRUE)           #Sum of emissions by municipality
m_2 <- aggregate(prior$CO2_eq_total_ha, by = list(prior$a_2), FUN = mean, na.rm=TRUE)       #Average emissions ha-1 by municipality
m_3 <- aggregate(prior$rendimiento_ton_ha, by = list(prior$a_2), FUN = mean, na.rm=TRUE)    #Average yield by municipality
m_4 <- aggregate(prior$CO2_eq_total_ton, by = list(prior$a_2), FUN = mean, na.rm=TRUE)      #Average emissions efficiency by municipality 
m_5 <- aggregate(prior$ingreso_usd2020, by = list(prior$a_2), FUN = sum, na.rm=TRUE)        #Sum of dollars produced
m_6 <- table(subset(prior$a_2, prior$f_3 == 1))                                             #Observations by municipality for Irrigation
m_7 <- aggregate(subset(prior$CO2_eq_total, prior$f_3 == 1),                                #Sum of emissions by municipality for Irrigation
                 by = list(subset(prior$a_2, prior$f_3 == 1)), FUN = sum, na.rm=TRUE)      
m_8 <- aggregate(subset(prior$CO2_eq_total_ha, prior$f_3 == 1),                             #Average emissions ha-1 by municipality for Irrigation
                 by = list(subset(prior$a_2, prior$f_3 == 1)), FUN = mean, na.rm=TRUE)
m_9 <- aggregate(subset(prior$rendimiento_ton_ha, prior$f_3 == 1), 
                 by = list(subset(prior$a_2, prior$f_3 == 1)), FUN = mean, na.rm=TRUE)      #Average yield by department for Irrigation
m_10 <- aggregate(subset(prior$CO2_eq_total_ton, prior$f_3 == 1), 
                  by = list(subset(prior$a_2, prior$f_3 == 1)), FUN = mean, na.rm=TRUE)     #Average emissions efficiency by municipality for Irrigation
m_11 <- aggregate(subset(prior$ingreso_usd2020, prior$f_3 == 1), 
                  by = list(subset(prior$a_2, prior$f_3 == 1)), FUN = sum, na.rm=TRUE)      #Sum of emissions per dollar produced for Irrigation
m_12 <- table(subset(prior$a_2, prior$f_3 == 2))                                            #Observations by municipality for Rainfed
m_13 <- aggregate(subset(prior$CO2_eq_total, prior$f_3 == 2),                               #Sum of emissions by municipality for Rainfed
                  by = list(subset(prior$a_2, prior$f_3 == 2)), FUN = sum, na.rm=TRUE)  
m_14 <- aggregate(subset(prior$CO2_eq_total_ha, prior$f_3 == 2),                            #Average emissions ha-1 by municipality for Rainfed
                  by = list(subset(prior$a_2, prior$f_3 == 2)), FUN = mean, na.rm=TRUE)
m_15 <- aggregate(subset(prior$rendimiento_ton_ha, prior$f_3 == 2), 
                  by = list(subset(prior$a_2, prior$f_3 == 2)), FUN = mean, na.rm=TRUE)     #Average yield by department for Rainfed

m_16 <- aggregate(subset(prior$CO2_eq_total_ton, prior$f_3 == 2), 
                  by = list(subset(prior$a_2, prior$f_3 == 2)), FUN = mean, na.rm=TRUE)     #Average emissions efficiency by municipality for Rainfed
m_17 <- aggregate(subset(prior$ingreso_usd2020, prior$f_3 == 2), 
                  by = list(subset(prior$a_2, prior$f_3 == 2)), FUN = sum, na.rm=TRUE)      #Sum of emissions per dollar produced for Rainfed

#Create dataframe
municipios <- data.frame(observaciones = m_0, 
                         sum_CO2_eq = m_1,
                         prom_CO2_eq_ha = m_2, 
                         Yield=m_3,
                         Ef_emi=m_4,
                         CO2_ingr=m_5)

mun_Riego <- data.frame(observaciones = m_6,
                        sum_CO2_eq = m_7,
                        prom_CO2_eq_ha = m_8, 
                        Yield=m_9,
                        Ef_emi=m_10,
                        CO2_ingr=m_11)

mun_Secan <- data.frame(observaciones = m_12,
                        sum_CO2_eq = m_13,
                        prom_CO2_eq_ha = m_14, 
                        Yield=m_15,
                        Ef_emi=m_16,
                        CO2_ingr=m_17)
#Output tables---------------------------------------------------------------------------------------------------------------------------------------------
municipios[,c(3,5,7,9,11)] <- NULL                                                           #Remove repeated columns
municipios[,c(3,4,6,7)] <- round(municipios[,c(3,4,6,7)], digits = 1)                       #Round to one decimal place
municipios[,c(5)] <- round(municipios[,c(5)], digits = 2)                                   #Round to two decimal places
names(municipios)[1] <- "num"                                                                #Change column name for merging
municipios <- merge.data.frame(municipios, Municipios, by = "num")                           #Merge to get the municipality name
municipios <- municipios[,c(1,8,9,2,3,4,5,6,7)]                                              #Reorder columns
colnames(municipios) <- c("Codigo_municipio", "Municipio", "Municipio2", "Observaciones",   
                          "suma_CO2_eq", "Promedio_de_CO2_eq_ha-1", "Rendimiento_prom",      
                          "CO2/rendimiento", "Suma_dolar")                                  #Rename columns
municipios <- mutate(municipios, CO2_Dollar = suma_CO2_eq / Suma_dolar)
municipios[,9] <- round(municipios[,9], digits = 2)                                          #Round to two decimal places

mun_Riego[,c(3,5,7,9,11)] <- NULL                                                            #Remove repeated columns
mun_Riego[,c(3,4,6,7)] <- round(mun_Riego[,c(3,4,6,7)], digits = 1)                          #Round to one decimal place
mun_Riego[,c(5)] <- round(mun_Riego[,c(5)], digits = 2)                                      #Round to two decimal places
names(mun_Riego)[1] <- "num"                                                                 #Change column name for merging
mun_Riego <- merge.data.frame(mun_Riego, Municipios, by = "num")                             #Merge to get the municipality name
mun_Riego <- mun_Riego[,c(1,8,9,2,3,4,5,6,7)]                                                #Reorder columns
colnames(mun_Riego) <- c("Codigo_municipio", "Municipio", "Municipio2", "Observaciones",  
                         "suma_CO2_eq", "Promedio_de_CO2_eq_ha-1", "Rendimiento_prom",      
                         "CO2/rendimiento", "Suma_dolar")                                    #Rename columns
mun_Riego <- mutate(mun_Riego, CO2_Dollar = round(suma_CO2_eq / Suma_dolar, 2))
mun_Riego[,9] <- round(mun_Riego[,9], digits = 2)                                            #Round to two decimal places

mun_Secan[,c(3,5,7,9,11)] <- NULL                                                            #Remove repeated columns
mun_Secan[,c(3,4,6,7)] <- round(mun_Secan[,c(3,4,6,7)], digits = 1)                          #Round to one decimal place
mun_Secan[,c(5)] <- round(mun_Secan[,c(5)], digits = 2)                                      #Round to two decimal places
names(mun_Secan)[1] <- "num"                                                                 #Change column name for merging
mun_Secan <- merge.data.frame(mun_Secan, Municipios, by = "num")                             #Merge to get the municipality name
mun_Secan <- mun_Secan[,c(1,8,9,2,3,4,5,6,7)]                                                #Reorder columns
colnames(mun_Secan) <- c("Codigo_municipio", "Municipio", "Municipio2", "Observaciones",   
                         "suma_CO2_eq", "Promedio_de_CO2_eq_ha-1", "Rendimiento_prom",     
                         "CO2/rendimiento", "Suma_dolar")                                    #Rename columns
mun_Secan <- mutate(mun_Secan, CO2_Dollar = suma_CO2_eq / Suma_dolar)
mun_Secan[,9] <- round(mun_Secan[,9], digits = 2)                                            #Round to two decimal places

cor(municipios$Rendimiento_prom, municipios$`Promedio_de_CO2_eq_ha-1`)                       #Correlation between yield and emissions

#Emission sources by department----------------------------------------------
b0 <- c("Indirect emissions", "Direct emissions", "Methane", "Urea Fertilization", "Liming")
b0_1 <- c(mean(prior$eindirecta, na.rm=TRUE),
          mean(prior$edirecta, na.rm=TRUE),
          mean(prior$emetano, na.rm=TRUE),
          mean(prior$eurea, na.rm=TRUE),
          mean(prior$ecal, na.rm=TRUE))

##Emissions by source for Córdoba
b1 <- c(mean(subset(prior$eindirecta, prior$a_1 == 1), na.rm=TRUE),
        mean(subset(prior$edirecta, prior$a_1 == 1), na.rm=TRUE),
        mean(subset(prior$emetano, prior$a_1 == 1), na.rm=TRUE),
        mean(subset(prior$eurea, prior$a_1 == 1), na.rm=TRUE),
        mean(subset(prior$ecal, prior$a_1 == 1), na.rm=TRUE))

##Emissions by source for Meta
b2 <- c(mean(subset(prior$eindirecta, prior$a_1 == 2), na.rm=TRUE),
        mean(subset(prior$edirecta, prior$a_1 == 2), na.rm=TRUE),
        mean(subset(prior$emetano, prior$a_1 == 2), na.rm=TRUE),
        mean(subset(prior$eurea, prior$a_1 == 2), na.rm=TRUE),
        mean(subset(prior$ecal, prior$a_1 == 2), na.rm=TRUE))

##Emissions by source for Sucre
b3 <- c(mean(subset(prior$eindirecta, prior$a_1 == 3), na.rm=TRUE),
        mean(subset(prior$edirecta, prior$a_1 == 3), na.rm=TRUE),
        mean(subset(prior$emetano, prior$a_1 == 3), na.rm=TRUE),
        mean(subset(prior$eurea, prior$a_1 == 3), na.rm=TRUE),
        mean(subset(prior$ecal, prior$a_1 == 3), na.rm=TRUE))

##Emissions by source for Tolima
b4 <- c(mean(subset(prior$eindirecta, prior$a_1 == 4), na.rm=TRUE),
        mean(subset(prior$edirecta, prior$a_1 == 4), na.rm=TRUE),
        mean(subset(prior$emetano, prior$a_1 == 4), na.rm=TRUE),
        mean(subset(prior$eurea, prior$a_1 == 4), na.rm=TRUE),
        mean(subset(prior$ecal, prior$a_1 == 4), na.rm=TRUE))

##Emissions by source for Casanare
b5 <- c(mean(subset(prior$eindirecta, prior$a_1 == 5), na.rm=TRUE),
        mean(subset(prior$edirecta, prior$a_1 == 5), na.rm=TRUE),
        mean(subset(prior$emetano, prior$a_1 == 5), na.rm=TRUE),
        mean(subset(prior$eurea, prior$a_1 == 5), na.rm=TRUE),
        mean(subset(prior$ecal, prior$a_1 == 5), na.rm=TRUE))

##Emissions by source for Valle
b6 <- c(mean(subset(prior$eindirecta, prior$a_1 == 8), na.rm=TRUE),
        mean(subset(prior$edirecta, prior$a_1 == 8), na.rm=TRUE),
        mean(subset(prior$emetano, prior$a_1 == 8), na.rm=TRUE),
        mean(subset(prior$eurea, prior$a_1 == 8), na.rm=TRUE),
        mean(subset(prior$ecal, prior$a_1 == 8), na.rm=TRUE))

#Create dataframe
departamentos_m <- data.frame(Emission = b0, Total = b0_1, 
                              Córdoba = b1, 
                              Meta = b2, 
                              Sucre = b3, 
                              Tolima = b4, 
                              Casanare = b5, 
                              Valle = b6)

#Round values
departamentos_m[,2:8] <- round(departamentos_m[,2:8], digits = 1)

#Irrigation

c0 <- c("Indirect emissions", "Direct emissions", "Methane", "Urea Fertilization", "Liming")
c0_1 <- c(mean(subset(prior$eindirecta, prior$f_3 == 1), na.rm=TRUE),
          mean(subset(prior$edirecta, prior$f_3 == 1), na.rm=TRUE),
          mean(subset(prior$emetano, prior$f_3 == 1), na.rm=TRUE),
          mean(subset(prior$eurea, prior$f_3 == 1), na.rm=TRUE),
          mean(subset(prior$ecal, prior$f_3 == 1), na.rm=TRUE))
##Emissions by source in Cordoba        
c1 <- c(mean(subset(prior$eindirecta, prior$a_1 == 1 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$edirecta, prior$a_1 == 1 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$emetano, prior$a_1 == 1 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$eurea, prior$a_1 == 1 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$ecal, prior$a_1 == 1 & prior$f_3 == 1), na.rm=TRUE))

##Emissions by source in Meta
c2 <- c(mean(subset(prior$eindirecta, prior$a_1 == 2 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$edirecta, prior$a_1 == 2 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$emetano, prior$a_1 == 2 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$eurea, prior$a_1 == 2 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$ecal, prior$a_1 == 2 & prior$f_3 == 1), na.rm=TRUE))
##Emissions by source in Sucre
c3 <- c(mean(subset(prior$eindirecta, prior$a_1 == 3 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$edirecta, prior$a_1 == 3 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$emetano, prior$a_1 == 3 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$eurea, prior$a_1 == 3 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$ecal, prior$a_1 == 3 & prior$f_3 == 1), na.rm=TRUE))
##Emissions by source in Tolima
c4 <- c(mean(subset(prior$eindirecta, prior$a_1 == 4 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$edirecta, prior$a_1 == 4 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$emetano, prior$a_1 == 4 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$eurea, prior$a_1 == 4 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$ecal, prior$a_1 == 4 & prior$f_3 == 1), na.rm=TRUE))
##Emissions by source in Casanare
c5 <- c(mean(subset(prior$eindirecta, prior$a_1 == 5 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$edirecta, prior$a_1 == 5 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$emetano, prior$a_1 == 5 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$eurea, prior$a_1 == 5 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$ecal, prior$a_1 == 5 & prior$f_3 == 1), na.rm=TRUE))
##Emissions by source in Valle
c6 <- c(mean(subset(prior$eindirecta, prior$a_1 == 8 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$edirecta, prior$a_1 == 8 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$emetano, prior$a_1 == 8 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$eurea, prior$a_1 == 8 & prior$f_3 == 1), na.rm=TRUE),
        mean(subset(prior$ecal, prior$a_1 == 8 & prior$f_3 == 1), na.rm=TRUE))
#Create dataframe
departamentosR_m <- data.frame(Emission = c0, Total= c0_1, 
                               Córdoba = c1, 
                               Meta =c2, 
                               Sucre=c3, 
                               Tolima=c4, 
                               Casanare=c5, 
                               Valle=c6)
#Round
departamentosR_m[,2:8] <- round(departamentosR_m[,2:8], digits=1)
#
##Rainfed
d0 <- c("Indirect emissions", "Direct emissions", "Methane", "Urea Fertilization", "Liming")
d0_1 <- c(mean(subset(prior$eindirecta, prior$f_3 == 2), na.rm=TRUE),
          mean(subset(prior$edirecta, prior$f_3 == 2), na.rm=TRUE),
          mean(subset(prior$emetano, prior$f_3 == 2), na.rm=TRUE),
          mean(subset(prior$eurea, prior$f_3 == 2), na.rm=TRUE),
          mean(subset(prior$ecal, prior$f_3 == 2), na.rm=TRUE))
##Emissions by source in Cordoba        
d1 <- c(mean(subset(prior$eindirecta, prior$a_1 == 1 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$edirecta, prior$a_1 == 1 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$emetano, prior$a_1 == 1 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$eurea, prior$a_1 == 1 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$ecal, prior$a_1 == 1 & prior$f_3 == 2), na.rm=TRUE))

##Emissions by source in Meta
d2 <- c(mean(subset(prior$eindirecta, prior$a_1 == 2 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$edirecta, prior$a_1 == 2 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$emetano, prior$a_1 == 2 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$eurea, prior$a_1 == 2 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$ecal, prior$a_1 == 2 & prior$f_3 == 2), na.rm=TRUE))
##Emissions by source in Sucre
d3 <- c(mean(subset(prior$eindirecta, prior$a_1 == 3 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$edirecta, prior$a_1 == 3 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$emetano, prior$a_1 == 3 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$eurea, prior$a_1 == 3 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$ecal, prior$a_1 == 3 & prior$f_3 == 2), na.rm=TRUE))
##Emissions by source in Tolima
d4 <- c(mean(subset(prior$eindirecta, prior$a_1 == 4 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$edirecta, prior$a_1 == 4 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$emetano, prior$a_1 == 4 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$eurea, prior$a_1 == 4 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$ecal, prior$a_1 == 4 & prior$f_3 == 2), na.rm=TRUE))
##Emissions by source in Casanare
d5 <- c(mean(subset(prior$eindirecta, prior$a_1 == 5 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$edirecta, prior$a_1 == 5 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$emetano, prior$a_1 == 5 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$eurea, prior$a_1 == 5 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$ecal, prior$a_1 == 5 & prior$f_3 == 2), na.rm=TRUE))
##Emissions by source in Valle
d6 <- c(mean(subset(prior$eindirecta, prior$a_1 == 8 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$edirecta, prior$a_1 == 8 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$emetano, prior$a_1 == 8 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$eurea, prior$a_1 == 8 & prior$f_3 == 2), na.rm=TRUE),
        mean(subset(prior$ecal, prior$a_1 == 8 & prior$f_3 == 2), na.rm=TRUE))
#Create dataframe
departamentosT_m <- data.frame(Emission = d0, Total= d0_1, 
                               Córdoba = d1, 
                               Meta = d2, 
                               Sucre=d3, 
                               Tolima=d4, 
                               Casanare=d5, 
                               Valle=d6)
#Round
departamentosT_m[,2:8] <- round(departamentosT_m[,2:8], digits=1)

#Emissions by emission source (excluding zeros)-------------------------------------------------------------------------------------
##Emissions by emission source all observations
g0<-c("Indirect_emissions", "Direct_emissions", "Methane", "Urea_Fertilization", "Liming")
###Emissions by source of total emissions
g1<-c(mean(subset(prior$eindirecta, prior$Urea_ha>0), na.rm=TRUE),
      mean(subset(prior$edirecta, prior$Urea_ha>0), na.rm=TRUE),
      mean(prior$emetano,na.rm=TRUE),
      mean(subset(prior$eurea, prior$Urea_ha>0), na.rm=TRUE),
      mean(subset(prior$ecal, prior$ecal>0), na.rm=TRUE))

##Emissions by emission source irrigation
g2<-c(mean(subset(prior$eindirecta, prior$Urea_ha>0 & prior$f_3 == 1), na.rm=TRUE),
      mean(subset(prior$edirecta, prior$Urea_ha>0 & prior$f_3 == 1),na.rm=TRUE),
      mean(subset(prior$emetano, prior$f_3 == 1),na.rm=TRUE),
      mean(subset(prior$eurea, prior$Urea_ha>0 & prior$f_3 == 1),na.rm=TRUE),
      mean(subset(prior$ecal, prior$ecal>0 & prior$f_3 == 1),na.rm=TRUE))

##Emissions by dryland emission source
g3<-c(mean(subset(prior$eindirecta, prior$Urea_ha>0 & prior$f_3 == 2), na.rm=TRUE),
      mean(subset(prior$edirecta, prior$Urea_ha>0 & prior$f_3 == 2),na.rm=TRUE),
      mean(subset(prior$emetano, prior$f_3 == 2),na.rm=TRUE),
      mean(subset(prior$eurea, prior$Urea_ha>0 & prior$f_3 == 2),na.rm=TRUE),
      mean(subset(prior$ecal, prior$ecal>0 & prior$f_3 == 2),na.rm=TRUE))
#Create dataframe and round off
fuente_m_0n <- data.frame(Emission = g0, Total = g1, Irrigated =g2, Rainfed=g3)
fuente_m_0n[,2:4]<-round(fuente_m_0n[,2:4], digits=1)

#Emission source by Department (excluding zero values)----------------------------------------------
b0 <- c("Indirect emissions", "Direct emissions", "Methane", "Urea Fertilization", "Liming")
b0_1 <- c(mean(subset(prior$eindirecta, prior$Urea_ha > 0), na.rm = TRUE),
          mean(subset(prior$edirecta, prior$Urea_ha > 0), na.rm = TRUE),
          mean(prior$emetano, na.rm = TRUE),
          mean(subset(prior$eurea, prior$Urea_ha > 0), na.rm = TRUE),
          mean(subset(prior$ecal, prior$ecal > 0), na.rm = TRUE))

## Emissions by source - Córdoba
b1 <- c(mean(subset(prior$eindirecta, prior$Urea_ha > 0 & prior$a_1 == 1), na.rm = TRUE),
        mean(subset(prior$edirecta, prior$Urea_ha > 0 & prior$a_1 == 1), na.rm = TRUE),
        mean(subset(prior$emetano, prior$a_1 == 1), na.rm = TRUE),
        mean(subset(prior$eurea, prior$Urea_ha > 0 & prior$a_1 == 1), na.rm = TRUE),
        mean(subset(prior$ecal, prior$ecal > 0 & prior$a_1 == 1), na.rm = TRUE))

## Emissions by source - Meta
b2 <- c(mean(subset(prior$eindirecta, prior$Urea_ha > 0 & prior$a_1 == 2), na.rm = TRUE),
        mean(subset(prior$edirecta, prior$Urea_ha > 0 & prior$a_1 == 2), na.rm = TRUE),
        mean(subset(prior$emetano, prior$a_1 == 2), na.rm = TRUE),
        mean(subset(prior$eurea, prior$Urea_ha > 0 & prior$a_1 == 2), na.rm = TRUE),
        mean(subset(prior$ecal, prior$ecal > 0 & prior$a_1 == 2), na.rm = TRUE))

## Emissions by source - Sucre
b3 <- c(mean(subset(prior$eindirecta, prior$Urea_ha > 0 & prior$a_1 == 3), na.rm = TRUE),
        mean(subset(prior$edirecta, prior$Urea_ha > 0 & prior$a_1 == 3), na.rm = TRUE),
        mean(subset(prior$emetano, prior$a_1 == 3), na.rm = TRUE),
        mean(subset(prior$eurea, prior$Urea_ha > 0 & prior$a_1 == 3), na.rm = TRUE),
        mean(subset(prior$ecal, prior$ecal > 0 & prior$a_1 == 3), na.rm = TRUE))

## Emissions by source - Tolima
b4 <- c(mean(subset(prior$eindirecta, prior$Urea_ha > 0 & prior$a_1 == 4), na.rm = TRUE),
        mean(subset(prior$edirecta, prior$Urea_ha > 0 & prior$a_1 == 4), na.rm = TRUE),
        mean(subset(prior$emetano, prior$a_1 == 4), na.rm = TRUE),
        mean(subset(prior$eurea, prior$Urea_ha > 0 & prior$a_1 == 4), na.rm = TRUE),
        mean(subset(prior$ecal, prior$ecal > 0 & prior$a_1 == 4), na.rm = TRUE))

## Emissions by source - Casanare
b5 <- c(mean(subset(prior$eindirecta, prior$Urea_ha > 0 & prior$a_1 == 5), na.rm = TRUE),
        mean(subset(prior$edirecta, prior$Urea_ha > 0 & prior$a_1 == 5), na.rm = TRUE),
        mean(subset(prior$emetano, prior$a_1 == 5), na.rm = TRUE),
        mean(subset(prior$eurea, prior$Urea_ha > 0 & prior$a_1 == 5), na.rm = TRUE),
        mean(subset(prior$ecal, prior$ecal > 0 & prior$a_1 == 5), na.rm = TRUE))

## Emissions by source - Valle
b6 <- c(mean(subset(prior$eindirecta, prior$Urea_ha > 0 & prior$a_1 == 6), na.rm = TRUE),
        mean(subset(prior$edirecta, prior$Urea_ha > 0 & prior$a_1 == 6), na.rm = TRUE),
        mean(subset(prior$emetano, prior$a_1 == 6), na.rm = TRUE),
        mean(subset(prior$eurea, prior$Urea_ha > 0 & prior$a_1 == 6), na.rm = TRUE),
        mean(subset(prior$ecal, prior$ecal > 0 & prior$a_1 == 6), na.rm = TRUE))

# Create dataframe
departamentos_m_n0 <- data.frame(Emission = b0, Total = b0_1, 
                                 Córdoba = b1, 
                                 Meta = b2, 
                                 Sucre = b3, 
                                 Tolima = b4, 
                                 Casanare = b5, 
                                 Valle = b6)
# Round
departamentos_m_n0[, 2:8] <- round(departamentos_m_n0[, 2:8], digits = 1)

#Emissions in Irrigation by Department -------------------------------------------------
c0 <- c("Indirect emissions", "Direct emissions", "Methane", "Urea Fertilization", "Liming")
c0_1 <- c(mean(subset(prior$eindirecta, prior$Urea_ha > 0 & prior$f_3 == 1), na.rm = TRUE),
          mean(subset(prior$edirecta, prior$Urea_ha > 0 & prior$f_3 == 1), na.rm = TRUE),
          mean(subset(prior$emetano, prior$f_3 == 1), na.rm = TRUE),
          mean(subset(prior$eurea, prior$Urea_ha > 0 & prior$f_3 == 1), na.rm = TRUE),
          mean(subset(prior$ecal, prior$ecal > 0 & prior$f_3 == 1), na.rm = TRUE))

## Emissions by source - Córdoba        
c1 <- c(mean(subset(prior$eindirecta, prior$Urea_ha > 0 & prior$a_1 == 1 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$edirecta, prior$Urea_ha > 0 & prior$a_1 == 1 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$emetano, prior$a_1 == 1 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$eurea, prior$Urea_ha > 0 & prior$a_1 == 1 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$ecal, prior$ecal > 0 & prior$a_1 == 1 & prior$f_3 == 1), na.rm = TRUE))

## Emissions by source - Meta
c2 <- c(mean(subset(prior$eindirecta, prior$Urea_ha > 0 & prior$a_1 == 2 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$edirecta, prior$Urea_ha > 0 & prior$a_1 == 2 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$emetano, prior$a_1 == 2 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$eurea, prior$Urea_ha > 0 & prior$a_1 == 2 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$ecal, prior$ecal > 0 & prior$a_1 == 2 & prior$f_3 == 1), na.rm = TRUE))

## Emissions by source - Sucre
c3 <- c(mean(subset(prior$eindirecta, prior$Urea_ha > 0 & prior$a_1 == 3 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$edirecta, prior$Urea_ha > 0 & prior$a_1 == 3 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$emetano, prior$a_1 == 3 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$eurea, prior$Urea_ha > 0 & prior$a_1 == 3 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$ecal, prior$ecal > 0 & prior$a_1 == 3 & prior$f_3 == 1), na.rm = TRUE))

## Emissions by source - Tolima
c4 <- c(mean(subset(prior$eindirecta, prior$Urea_ha > 0 & prior$a_1 == 4 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$edirecta, prior$Urea_ha > 0 & prior$a_1 == 4 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$emetano, prior$a_1 == 4 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$eurea, prior$Urea_ha > 0 & prior$a_1 == 4 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$ecal, prior$ecal > 0 & prior$a_1 == 4 & prior$f_3 == 1), na.rm = TRUE))

## Emissions by source - Casanare
c5 <- c(mean(subset(prior$eindirecta, prior$Urea_ha > 0 & prior$a_1 == 5 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$edirecta, prior$Urea_ha > 0 & prior$a_1 == 5 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$emetano, prior$a_1 == 5 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$eurea, prior$Urea_ha > 0 & prior$a_1 == 5 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$ecal, prior$ecal > 0 & prior$a_1 == 5 & prior$f_3 == 1), na.rm = TRUE))

## Emissions by source - Valle
c6 <- c(mean(subset(prior$eindirecta, prior$Urea_ha > 0 & prior$a_1 == 6 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$edirecta, prior$Urea_ha > 0 & prior$a_1 == 6 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$emetano, prior$a_1 == 6 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$eurea, prior$Urea_ha > 0 & prior$a_1 == 6 & prior$f_3 == 1), na.rm = TRUE),
        mean(subset(prior$ecal, prior$ecal > 0 & prior$a_1 == 6 & prior$f_3 == 1), na.rm = TRUE))

# Create dataframe
departamentosR_m_n0 <- data.frame(Emission = c0, Total= c0_1, 
                                  Córdoba = c1, 
                                  Meta =c2, 
                                  Sucre=c3, 
                                  Tolima=c4, 
                                  Casanare=c5, 
                                  Valle=c6)
# Round
departamentosR_m_n0[,2:8]<-round(departamentosR_m_n0[,2:8], digits=1)
#Emissions in Rainfed by Department -------------------------------------------------
d0 <- c("Indirect emissions", "Direct emissions", "Methane", "Urea Fertilization", "Liming")
d0_1 <- c(mean(subset(prior$eindirecta, prior$Urea_ha > 0 & prior$f_3 == 2), na.rm = TRUE),
          mean(subset(prior$edirecta, prior$Urea_ha > 0 & prior$f_3 == 2), na.rm = TRUE),
          mean(subset(prior$emetano, prior$f_3 == 2), na.rm = TRUE),
          mean(subset(prior$eurea, prior$Urea_ha > 0 & prior$f_3 == 2), na.rm = TRUE),
          mean(subset(prior$ecal, prior$ecal > 0 & prior$f_3 == 2), na.rm = TRUE))

## Emissions by source - Córdoba
d1 <- c(mean(subset(prior$eindirecta, prior$Urea_ha > 0 & prior$a_1 == 1 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$edirecta, prior$Urea_ha > 0 & prior$a_1 == 1 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$emetano, prior$a_1 == 1 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$eurea, prior$Urea_ha > 0 & prior$a_1 == 1 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$ecal, prior$ecal > 0 & prior$a_1 == 1 & prior$f_3 == 2), na.rm = TRUE))

## Emissions by source - Meta
d2 <- c(mean(subset(prior$eindirecta, prior$Urea_ha > 0 & prior$a_1 == 2 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$edirecta, prior$Urea_ha > 0 & prior$a_1 == 2 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$emetano, prior$a_1 == 2 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$eurea, prior$Urea_ha > 0 & prior$a_1 == 2 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$ecal, prior$ecal > 0 & prior$a_1 == 2 & prior$f_3 == 2), na.rm = TRUE))

## Emissions by source - Sucre
d3 <- c(mean(subset(prior$eindirecta, prior$Urea_ha > 0 & prior$a_1 == 3 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$edirecta, prior$Urea_ha > 0 & prior$a_1 == 3 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$emetano, prior$a_1 == 3 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$eurea, prior$Urea_ha > 0 & prior$a_1 == 3 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$ecal, prior$ecal > 0 & prior$a_1 == 3 & prior$f_3 == 2), na.rm = TRUE))

## Emissions by source - Tolima
d4 <- c(mean(subset(prior$eindirecta, prior$Urea_ha > 0 & prior$a_1 == 4 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$edirecta, prior$Urea_ha > 0 & prior$a_1 == 4 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$emetano, prior$a_1 == 4 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$eurea, prior$Urea_ha > 0 & prior$a_1 == 4 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$ecal, prior$ecal > 0 & prior$a_1 == 4 & prior$f_3 == 2), na.rm = TRUE))

## Emissions by source - Casanare
d5 <- c(mean(subset(prior$eindirecta, prior$Urea_ha > 0 & prior$a_1 == 5 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$edirecta, prior$Urea_ha > 0 & prior$a_1 == 5 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$emetano, prior$a_1 == 5 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$eurea, prior$Urea_ha > 0 & prior$a_1 == 5 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$ecal, prior$ecal > 0 & prior$a_1 == 5 & prior$f_3 == 2), na.rm = TRUE))

## Emissions by source - Valle
d6 <- c(mean(subset(prior$eindirecta, prior$Urea_ha > 0 & prior$a_1 == 6 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$edirecta, prior$Urea_ha > 0 & prior$a_1 == 6 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$emetano, prior$a_1 == 6 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$eurea, prior$Urea_ha > 0 & prior$a_1 == 6 & prior$f_3 == 2), na.rm = TRUE),
        mean(subset(prior$ecal, prior$ecal > 0 & prior$a_1 == 6 & prior$f_3 == 2), na.rm = TRUE))

# Create dataframe
departamentosS_m_n0 <- data.frame(Emission = d0, Total = d0_1, 
                                  Córdoba = d1, 
                                  Meta = d2, 
                                  Sucre = d3, 
                                  Tolima = d4, 
                                  Casanare = d5, 
                                  Valle = d6)

# Round
departamentosS_m_n0[, 2:8] <- round(departamentosS_m_n0[, 2:8], digits = 1)

#Yield by Department -------------------------------------------------------------------------
r_0 <- aggregate(prior$rendimiento_ton_ha, by = list(prior$a_1), FUN = mean, na.rm = TRUE)
r_1 <- aggregate(subset(prior$rendimiento_ton_ha, prior$f_3 == 1), 
                 by = list(subset(prior$a_1, prior$f_3 == 1)), FUN = mean, na.rm = TRUE)
r_2 <- aggregate(subset(prior$rendimiento_ton_ha, prior$f_3 == 2), 
                 by = list(subset(prior$a_1, prior$f_3 == 2)), FUN = mean, na.rm = TRUE)

mean(prior$rendimiento_ton_ha, na.rm = TRUE)
mean(subset(prior$rendimiento_ton_ha, prior$f_3 == 1), na.rm = TRUE)
mean(subset(prior$rendimiento_ton_ha, prior$f_3 == 2), na.rm = TRUE)

#CO2 Efficiency / Yield -------------------------------------------------------------------------
e_0 <- aggregate(prior$CO2_eq_total_ton, by = list(prior$a_1), FUN = mean, na.rm = TRUE)
e_1 <- aggregate(subset(prior$CO2_eq_total_ton, prior$f_3 == 1), 
                 by = list(subset(prior$a_1, prior$f_3 == 1)), FUN = mean, na.rm = TRUE)
e_2 <- aggregate(subset(prior$CO2_eq_total_ton, prior$f_3 == 2), 
                 by = list(subset(prior$a_1, prior$f_3 == 2)), FUN = mean, na.rm = TRUE)

# Mean
m_0 <- aggregate(prior$CO2_eq_total_ha, by = list(prior$a_1), FUN = mean, na.rm = TRUE)
m_1 <- aggregate(subset(prior$CO2_eq_total_ha, prior$f_3 == 1), 
                 by = list(subset(prior$a_1, prior$f_3 == 1)), FUN = mean, na.rm = TRUE)
m_2 <- aggregate(subset(prior$CO2_eq_total_ha, prior$f_3 == 2), 
                 by = list(subset(prior$a_1, prior$f_3 == 2)), FUN = mean, na.rm = TRUE)

#Searching for High Emission Sources in Tolima (SFw)----------------------------------------------

## Extra high-value sources
nomb <- c("Córdoba", "Meta", "Sucre", "Tolima", "Casanare", "Valle del Cauca")
FEc <- aggregate(prior$EFc, by = list(prior$a_1), FUN = mean, na.rm = TRUE)
SFw <- aggregate(prior$SFw, by = list(prior$a_1), FUN = mean, na.rm = TRUE)
SFp <- aggregate(prior$SFp, by = list(prior$a_1), FUN = mean, na.rm = TRUE)
t <- aggregate(prior$duracion_ciclo_dias_mod, by = list(prior$a_1), FUN = mean, na.rm = TRUE)
eM <- aggregate(prior$emetano, by = list(prior$a_1), FUN = mean, na.rm = TRUE)
eD <- aggregate(prior$edirecta, by = list(prior$a_1), FUN = mean, na.rm = TRUE)
eI <- aggregate(prior$eindirecta, by = list(prior$a_1), FUN = mean, na.rm = TRUE)
eC <- aggregate(prior$ecal, by = list(prior$a_1), FUN = mean, na.rm = TRUE)
eU <- aggregate(prior$eurea, by = list(prior$a_1), FUN = mean, na.rm = TRUE)
f <- aggregate(prior$fert_CO2_eq_total_ha, by = list(prior$a_1), FUN = mean, na.rm = TRUE)

f_Dep <- data.frame(departments = nomb, Region = FEc, Regimen = SFw, Regimen2 = SFp, cycle = t, 
                    Direct = eD, Indirect = eI, Cal = eC, Urea = eU, Fertilization = f, Methane = eM)
f_Dep[, c(4, 6, 8, 10, 12, 14, 16, 18, 20)] <- NULL

# Plot mean emission vs Yield
ggpairs(municipios, columns = 5:6, aes(color = "coral3", alpha = 0.5))

#Emissions (general) by department ---------------------------------------------
h_1 <- as.data.frame(table(prior$a_1))                                                         # Observations by department
h_2 <- as.data.frame(table(subset(prior$a_1, prior$f_3 == 1)))                                  # Number of observations for irrigation
h_3 <- as.data.frame(table(subset(prior$a_1, prior$f_3 == 2)))                                  # Number of observations for rainfed

h_4 <- aggregate(prior$CO2_eq_total, by = list(prior$a_1), FUN = sum, na.rm = TRUE)               # Total emissions by department
h_5 <- aggregate(subset(prior$CO2_eq_total, prior$f_3 == 1),                                    # Total emissions by department for irrigation
                 by = list(subset(prior$a_1, prior$f_3 == 1)), FUN = sum, na.rm = TRUE)
h_6 <- aggregate(subset(prior$CO2_eq_total, prior$f_3 == 2),                                    # Total emissions by department for rainfed
                 by = list(subset(prior$a_1, prior$f_3 == 2)), FUN = sum, na.rm = TRUE)  

h_7 <- aggregate(prior$CO2_eq_total_ha, by = list(prior$a_1), FUN = mean, na.rm = TRUE)           # Average emissions per ha by department
h_8 <- aggregate(subset(prior$CO2_eq_total_ha, prior$f_3 == 1),                                 # Average emissions by department for irrigation
                 by = list(subset(prior$a_1, prior$f_3 == 1)), FUN = mean, na.rm = TRUE)
h_9 <- aggregate(subset(prior$CO2_eq_total_ha, prior$f_3 == 2),                                 # Average emissions by department for rainfed
                 by = list(subset(prior$a_1, prior$f_3 == 2)), FUN = mean, na.rm = TRUE)  

h_10 <- aggregate(prior$rendimiento_ton_ha, by = list(prior$a_1), FUN = mean, na.rm = TRUE)       # Average yield by department
h_11 <- aggregate(subset(prior$rendimiento_ton_ha, prior$f_3 == 1),                             # Average yield by department for irrigation
                  by = list(subset(prior$a_1, prior$f_3 == 1)), FUN = mean, na.rm = TRUE)
h_12 <- aggregate(subset(prior$rendimiento_ton_ha, prior$f_3 == 2),                             # Average yield by department for rainfed
                  by = list(subset(prior$a_1, prior$f_3 == 2)), FUN = mean, na.rm = TRUE)

h_13 <- aggregate(prior$CO2_eq_total_ton, by = list(prior$a_1), FUN = mean, na.rm = TRUE)         # Average emissions per ton paddy by department
h_14 <- aggregate(subset(prior$CO2_eq_total_ton, prior$f_3 == 1),                               # Average emissions per ton paddy by department for irrigation
                  by = list(subset(prior$a_1, prior$f_3 == 1)), FUN = mean, na.rm = TRUE)
h_15 <- aggregate(subset(prior$CO2_eq_total_ton, prior$f_3 == 2),                               # Average emissions per ton paddy by department for rainfed
                  by = list(subset(prior$a_1, prior$f_3 == 2)), FUN = mean, na.rm = TRUE)

h_16 <- aggregate(prior$CO2_ingr, by = list(prior$a_1), FUN = mean, na.rm = TRUE)                 # Average emissions per dollar by department
h_17 <- aggregate(subset(prior$CO2_ingr, prior$f_3 == 1),                                       # Average emissions per dollar by department for irrigation
                  by = list(subset(prior$a_1, prior$f_3 == 1)), FUN = mean, na.rm = TRUE)
h_18 <- aggregate(subset(prior$CO2_ingr, prior$f_3 == 2),                                       # Average emissions per dollar by department for rainfed
                  by = list(subset(prior$a_1, prior$f_3 == 2)), FUN = mean, na.rm = TRUE)

colnames(h_1)[1] <- "Group.1"
colnames(h_2)[1] <- "Group.1"
colnames(h_3)[1] <- "Group.1"

dep_gener <- merge(h_1, h_2, by = "Group.1", all = TRUE)
dep_gener <- merge(dep_gener, h_3, by = "Group.1", all = TRUE)
dep_gener <- merge(dep_gener, h_4, by = "Group.1", all = TRUE)
dep_gener <- merge(dep_gener, h_5, by = "Group.1", all = TRUE)
dep_gener <- merge(dep_gener, h_6, by = "Group.1", all = TRUE)
dep_gener <- merge(dep_gener, h_7, by = "Group.1", all = TRUE)
dep_gener <- merge(dep_gener, h_8, by = "Group.1", all = TRUE)
dep_gener <- merge(dep_gener, h_9, by = "Group.1", all = TRUE)
dep_gener <- merge(dep_gener, h_10, by = "Group.1", all = TRUE)
dep_gener <- merge(dep_gener, h_11, by = "Group.1", all = TRUE)
dep_gener <- merge(dep_gener, h_12, by = "Group.1", all = TRUE)
dep_gener <- merge(dep_gener, h_13, by = "Group.1", all = TRUE)
dep_gener <- merge(dep_gener, h_14, by = "Group.1", all = TRUE)
dep_gener <- merge(dep_gener, h_15, by = "Group.1", all = TRUE)
dep_gener <- merge(dep_gener, h_16, by = "Group.1", all = TRUE)
dep_gener <- merge(dep_gener, h_17, by = "Group.1", all = TRUE)
dep_gener <- merge(dep_gener, h_18, by = "Group.1", all = TRUE)

colnames(dep_gener) <- c("ID", "Observations", "Irrigation_Obs", "Rainfed_Obs",
                         "sum_CO2_eq", "sum_CO2_eq_Irr", "sum_CO2_eq_Rainfed",
                         "mean_CO2_ha", "mean_CO2_ha_Irr", "mean_CO2_ha_Rainfed",
                         "mean_yield", "mean_yield_Irr", "mean_yield_Rainfed",
                         "mean_CO2_T", "mean_CO2_T_Irr", "mean_CO2_T_Rainfed",
                         "CO2/dollar", "CO2/dollar_Irr", "CO2/dollar_Rainfed")

#Exporting -------------------------
write.xlsx(prior, file="final_base.xlsx")                                     #prior modified (final base)
{md <- data.frame(strings = c("Metadata/Description of each table:",
                              "source_emission -> Sum of emissions disaggregated by source emission and irrigation system",
                              "source_mean -> Average rice farm emissions disaggregated by source emission and irrigation system",
                              "source_m_0n -> Average rice farm emissions disaggregated by source emission and irrigation system (excluding the 0 values)",
                              "municipalities -> Data by municipality: Observation number, yield, CO2/yield, income (rice) and CO2/income",
                              "municipal_Irr -> Data by municipality: Observation number, yield, CO2/yield, income (rice) and CO2/income (for irrigation system)",
                              "municipal_Rainfed -> Data by municipality: Observation number, yield, CO2/yield, income (rice) and CO2/income (for rainfed system)",
                              "departments -> Sum of emissions by department disaggregated by source emission",
                              "departments_m -> Average rice farm emission by department disaggregated by source emission and irrigation system",
                              "departments_m_n0 -> Average rice farm emission by department disaggregated by source emission and irrigation system (excluding the 0 values)",
                              "departments_Irr -> Sum of emissions by department disaggregated by source emission (for irrigation system)",
                              "departments_Rainfed -> Sum of emissions by department disaggregated by source emission (for rainfed system)",
                              "departments_Irr_m -> Mean of emissions by department disaggregated by source emission (for irrigation system)",
                              "departments_Rainfed_m -> Mean of emissions by department disaggregated by source emission (for rainfed system)",
                              "departments_Irr_m_n0 -> Mean of emissions by department disaggregated by source emission (for irrigation system, excluding the 0 values)",
                              "departments_Rainfed_m_n0 -> Mean of emissions by department disaggregated by source emission (for rainfed system, excluding the 0 values; dep_general -> General data associated with Departments)"))
}  # Metadata

{wb <- createWorkbook()
  addWorksheet(wb, "source_emission")
  addWorksheet(wb, "source_mean")
  addWorksheet(wb, "source_m_0n")
  addWorksheet(wb, "municipalities")
  addWorksheet(wb, "municipal_Irr")
  addWorksheet(wb, "municipal_Rainfed")
  addWorksheet(wb, "departments")
  addWorksheet(wb, "departments_m")
  addWorksheet(wb, "departments_m_n0")
  addWorksheet(wb, "departments_Irr")
  addWorksheet(wb, "departments_Rainfed")
  addWorksheet(wb, "departments_Irr_m")
  addWorksheet(wb, "departments_Rainfed_m")
  addWorksheet(wb, "departments_Irr_m_n0")
  addWorksheet(wb, "departments_Rainfed_m_n0")
  addWorksheet(wb, "dep_general")
  addWorksheet(wb, "metadata")
  
  writeData(wb, "source_emission", fuente)                                      # Sum of CO2eq ha-1 by source of emission
  writeData(wb, "source_mean", fuente_m)                                        # Average CO2eq ha-1 by source of emission
  writeData(wb, "source_m_0n", fuente_m_0n)                                     # Average CO2eq ha-1 by source of emission excluding zero values
  writeData(wb, "municipalities", municipios)                                   # Municipality data 
  writeData(wb, "municipal_Irr", mun_Riego)                                     # Municipality data for irrigation
  writeData(wb, "municipal_Rainfed", mun_Secan)                                 # Municipality data for rainfed
  writeData(wb, "departments", departamentos)                                   # Sum of CO2eq by emission source by department
  writeData(wb, "departments_m", departamentos_m)                               # Average CO2eq ha-1 by emission source by department
  writeData(wb, "departments_m_n0", departamentos_m_n0)                         # Average CO2eq ha-1 by emission source by department excluding zero values
  writeData(wb, "departments_Irr", departamentosR)                              # Sum of CO2eq by emission source by department for irrigation
  writeData(wb, "departments_Rainfed", departamentosS)                          # Sum of CO2eq by emission source by department for rainfed
  writeData(wb, "departments_Irr_m", departamentosR_m)                          # Average CO2eq ha-1 by emission source by department for irrigation
  writeData(wb, "departments_Rainfed_m", departamentosS_m)                      # Average CO2eq ha-1 by emission source by department for rainfed
  writeData(wb, "departments_Irr_m_n0", departamentosR_m_n0)                    # Average CO2eq ha-1 by emission source by department for irrigation excluding zero values
  writeData(wb, "departments_Rainfed_m_n0", departamentosS_m_n0)                # Average CO2eq ha-1 by emission source by department for rainfed excluding zero values
  writeData(wb, "dep_general", dep_gener)                                       # General data at the departmental level
  writeData(wb, "metadata", md)  
}  # Workbook

# saveWorkbook(wb, "Resultados28_n4.xlsx", overwrite = TRUE)                    #Workbook of results                  


