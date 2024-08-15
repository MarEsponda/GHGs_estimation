# Greenhouse Gas Estimation for Colombia in 2020
In this project, we calculate greenhouse gas emissions for rice cultivation in Colombia for the year 2020. We apply the IPCC Tier 1 methodology to estimate these emissions, based on the survey conducted by CIAT-MADR in Colombia in November 2020. The entire process was carried out in the RStudio environment, using R version 4.3.1, and leveraging common R libraries.
# Steps to Run the Code
1.	Estimation Process:
o	The first script to run is Estimation_process.R. The input data required for this script includes the following datasets: time_lapse.dta, prior_base.dta, ingresousd.dta, fertilizantes.dta, and Munici_name.xlsx.
o	This script generates an Excel file called final_base.xlsx, which contains the results of emissions per hectare and emissions per ton of rice produced.
2.	Extrapolation:
o	After running the estimation script, you can execute Extrapolation.R to calculate various emission estimates at the national level. This script requires the following input files: time_lapse.dta, final_base.xlsx, and EVA_Arroz_2020.xlsx.
3.	Sensitivity Analysis:
o	Finally, run Sensibility.R to perform a sensitivity analysis. This script varies the IPCC factors within a triangular probability distribution and evaluates the variations in the emission estimates.
# Purpose
This code is designed to replicate the emission estimation calculations using a site-specific methodology based on actual activity data reported by the producers themselves.

Contact
Maria del Mar Esponda
m.esponda@cgiar.org
