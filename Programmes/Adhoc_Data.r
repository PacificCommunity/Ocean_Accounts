##
##    Programme:  Adhoc_Data.r
##
##    Objective:  What is this programme designed to do?
##
##    Author:     <PROGRAMMER>, <TEAM>, <DATE STARTED>
##    Peer Review:<PROGRAMMER>, <TEAM>, <DATE STARTED>
##
##
   ##
   ##    Clear the memory
   ##
      rm(list=ls(all=TRUE))

      ##
      ##    Derived from Table 1.1, "Vulnerability of Tropical Pacific Fisheries and Aquaculture to Climate Change"
      ##
      Land_and_EEZ <- data.frame(PICT      = c("Fiji","New Caledonia","PNG","Solomon Islands","Vanuatu","FSM","Guam","Kiribati","Marshall Islands","Nauru","CNMI","Palau","American Samoa","Cook Islands","FrenchPolynesia","Niue","Pitcairn Islands","Samoa","Tokelau","Tonga","Tuvalu","Wallis and Futuna"),
                                 Land_Area = c(18272,19100,462243,27556,11880,700,541,810,112,21,478,494,197,240,3521,259,5,2935,10,699,26,255),
                                 Area_of_EEZ = c(1229728,1111900,2446757,1553444,668220,2939300,214059,3550000,2004888,293079,752922,605506,434503,1947760,4200000,296941,800000,110365,318990,676401,719174,242445),
                                 Ratio       = c(1.464,1.689,15.89,1.743,1.747,0.024,0.252,0.023,0.006,0.007,0.063,0.082,0.045,0.012,0.084,0.087,0.001,2.59,0.003,0.103,0.004,0.105))
      Land_and_EEZ <- Land_and_EEZ[order(1-Land_and_EEZ$Ratio),]
      Land_and_EEZ$Land_Area   <- format(round(Land_and_EEZ$Land_Area),big.mark   = ",")
      Land_and_EEZ$Area_of_EEZ <- format(round(Land_and_EEZ$Area_of_EEZ),big.mark   = ",")
      Land_and_EEZ$Ratio       <- paste0(str_trim(Land_and_EEZ$Ratio),"%")

      names(Land_and_EEZ) <- str_replace_all(names(Land_and_EEZ), "_", " ")

   ##
   ## Save files our produce some final output of something
   ##
      save(Land_and_EEZ, file = 'Data_Output/Land_and_EEZ.rda')
##
##    And we're done
##
