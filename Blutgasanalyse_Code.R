############################
############################
## Bloog-Gas-Analysis in R #
############################
############################

### Current variant used with a shiny front-endsolution by Rico SChmitt / ChatGPT.
### Updates with the full version in the near future....


BGA = function(pH, Bikarb, Stand_Bikarb, Base_Ex, pC02){
  
  # Set input values as vector:
  input = c(pH, Bikarb, Stand_Bikarb, Base_Ex, pC02)
  
  # Table with reference values (doc-check as source):
  # Order follows the sequence: 
  # c(pH, Bikarb, Stand_Bikarb, Base_Ex, pC02)
  low   = c(7.36, 21, 23,-2, 35)
  up    = c(7.44, 26, 27, 3, 46)
  
  # Possible diagnoses (source doc-check again):
  # 0 == low, 1 == high, 2 == norm:
  # Order again follows the sequence: 
  # c(pH, Bikarb, Stand_Bikarb, Base_Ex, pC02)
  
  # Respiratory acidosis:
  resp_az = c(0,1,2,2,1)
  # Metabolic acidosis:
  met_az = c(0,0,0,0,2)
  # Metabolic acidosis, partially compensated:
  met_az_komp = c(0,0,0,0,1)
  # Respiratory alkalosis:
  resp_alk = c(1,0,2,2,0)
  # Metabolic alkalosis:
  met_alk = c(1,1,1,1,2)
  # Metabolic alkalosis, partially compensated:
  met_alk_komp = c(1,1,1,1,1)
  
  BGA_diag = rbind(resp_az, met_az, met_az_komp, resp_alk, met_alk,met_alk_komp)
  BGA_diag_text = c("Respiratory acidosis", "Metabolic acidosis", "Metabolic acidosis, partially compensated", "Respiratory alkalosis", "Metabolic alkalosis", "Metabolic alkalosis, partially compensated")
  
  # Initialize empty vector:
  input_ref_logic = c()
  
  # Loop that turns measurements into logicals, based on reference table:
  # ==> 0 == low, 1 == high, 2 == norm.
  for (i in 1:length(BGA_diag[1,])){
    if(input[i] < low[i]){
      input_ref_logic[i] = 0
    }
    else if(input[i] > up[i]){
      input_ref_logic[i] = 1
    }
    else if (input[i] > low[i] & input[i] < up[i]){
      input_ref_logic[i] = 2
    }
    else if (input[i] == low[i] | input[i] == up[i]){
      input_ref_logic[i] = 2
    }
  } # End for i 
  
  # Initialize diagnosis variable for the case of no match with above schemes:
  diagnosis = "No exact diagnosis available." 
  
  # Check if measurement fits one of the above schemes:
  for(i in 1:length(BGA_diag[,1])){
    if(all.equal(input_ref_logic,BGA_diag[i,])==TRUE){
      diagnosis = BGA_diag_text[i]
    }
  } # End for i
  return(diagnosis)
} # End of function BGA


# Test function:
# Example Input:
BGA(pH = 7.48, Bikarb = 27, Stand_Bikarb = 28, Base_Ex = 3.1, pC02 = 50)
# [1] "Metabolic alkalosis, partially compensated"


#### Alternative Version that calculates BE and HCO3 via formulas:

# Background on BE:
# "Der Base Excess des Blutes (BE, mmol/l) gibt nach Siggaard-Andersen 
# diejenige Menge an H+ oder OH- an, die notwendig ist, den vom Normalwert
# abweichenden pH-Wert des Blutes mit HCl oder NaOH bis 7,400 zu titrieren,
# wobei eine Temperatur von 37°C und ein CO2-Partial- druck (pCO2) von 
# 40 mmHg einzuhalten sind." 
# (R. Zander, Die korrekte Bestimmung des Base Excess (BE, mmol/l) im Blut)

# Problem with this Version: the reference values from doc-check don't seem to fit anymore. 
# Unclear how BE is meassured / calculated properly. 

BGA = function(pH,pC02){ # Start of function BGA.
  
  # Calculate Bikarbonat:
  HCO3 = 0.0304 * pCO2 * 10 * (pH - 6.1)
  
  # Calculate Base Excess according to Siggaard-Anderson:
  BE = 0.9287 * HCO3 + 13.77 * pH - 124.58 
  
  # Set input values as vector:
  input = c(pH, HCO3, BE, pCO2)
  
  # Table with reference values (doc-check as source):
  # Order follows the sequence: 
  # c(pH, HCO3, BE, pC02)
  low   = c(7.36, 21, -2, 35)
  up    = c(7.44, 26,  3, 46)
  
  # Loop that turns measurements (input) into logicals, 
  # based on the reference table from doc-check:
  # ==> 0 == low, 1 == high, 2 == norm.
  # Initialize empty vector:
  input_ref_logic = c()
  
  for (i in 1:length(input)){ 
    if(input[i] < low[i]){
      input_ref_logic[i] = 0
    }
    else if(input[i] > up[i]){
      input_ref_logic[i] = 1
    }
    else if (input[i] > low[i] & input[i] < up[i]){
      input_ref_logic[i] = 2
    }
    else if (input[i] == low[i] | input[i] == up[i]){
      input_ref_logic[i] = 2
    }
  } # End for i 
  
  # Check for Possible Diagnoses (source doc-check again):
  # 0 == low, 1 == high, 2 == norm:
  # Order again follows the sequence: 
  # c(pH, HCO3, BE, pC02)
  
  # Respiratory acidosis:
  resp_az = c(0,1,2,1)
  # Metabolic acidosis:
  met_az = c(0,0,0,2)
  # Metabolic acidosis, partially compensated:
  met_az_komp = c(0,0,0,1)
  # Respiratory alkalosis:
  resp_alk = c(1,0,2,0)
  # Metabolic alkalosis:
  met_alk = c(1,1,1,2)
  # Metabolic alkalosis, partially compensated:
  met_alk_komp = c(1,1,1,1)
  
  BGA_diag = rbind(resp_az, met_az, met_az_komp, resp_alk, met_alk,met_alk_komp)
  BGA_diag_text = c("Respiratory acidosis", 
                    "Metabolic acidosis", 
                    "Metabolic acidosis, 
                    partially compensated", 
                    "Respiratory alkalosis", 
                    "Metabolic alkalosis", 
                    "Metabolic alkalosis, partially compensated")
  
  # Initialize diagnosis variable for the case of no match with above schemes:
  diagnosis = "No exact diagnosis available." 
  
  # Check if measurement fits one of the above schemes:
  for(i in 1:length(BGA_diag[,1])){
    if(all.equal(input_ref_logic,BGA_diag[i,])==TRUE){
      diagnosis = BGA_diag_text[i]
    }
  } # End for i
  return(diagnosis)
} # End of function BGA


# Test function:
pCO2 = 40
pH = 7.48

BGA(pH,pCO2)


