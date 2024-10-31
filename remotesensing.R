#Digital tool - Remote sensing

lightspeed_m_s <- 300e+06
wavelengths_m <- c(450e-09,
                   550e-09,
                   700e-09,
                   1e-10,
                   10)
Frequencies_Hz <- lightspeed_m_s/wavelengths_m
names(wavelengths_m) <-c ("blue", "green", "red", "x-rays", "FM-radio") 

#hands on 
plankConstant_Js <- 6.626e-34
Quantum_energy_J <- plankConstant_Js*Frequencies_Hz
Quantum_energy_J

  
  
  
  
  
  
  #hands on: looking at drone-data
  
  library(terra)
install.packages(terra)

