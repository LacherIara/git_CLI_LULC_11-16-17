############################ 
#PURPOSE: Outline the steps with reference to documents, data, and Dinamica models
#DEVELOPED: 1-27-17
#CONTACT: LacherI@si.edu
#NOTES:

#IMPORTANT: 
##### NEXT STEPS #####

############################

#INPUTS:
	# Tables:  
	# Rasters: 
#OUTPUTS:
	# Tables:  
	# Rasters:
	
################################################
######## BUILD RECENT TRENDS LULC MODEL ########
################################################

# ----------------------------------------------
# ----------------------------------------------
# STEP 1. SPEARMAN RANK CORRELATIONS - VARIABLE SELECTION
# ----------------------------------------------
# ----------------------------------------------


# ----------------------------------------------
# R SCRIPT 
# SpearmanRank.r

#PURPOSE: Conduct Spearman- Rank Correlation on Spatial Rasters representing each potential driver variable.
#INPUTS:
	# Tables:  
		# Random cell values

#OUTPUTS:
	# Tables: 
		# result of the Spearman- Rank Correlation	




# ----------------------------------------------
# ----------------------------------------------
# STEP 2. 
# ----------------------------------------------
# ----------------------------------------------



# ----------------------------------------------
# QUANTIFY CHANGE IN LANDUSE BETWEEN REFERENCE YEARS 
# ----------------------------------------------

# ----------------------------------------------
# R SCRIPT 
# CombinedandZonalHist_nlRCc_0111.r


#PURPOSE:  Do Combine and Zonal Hist for NLCD Land Cover years 2001 & 2011. - Avoid using ArcMap and doing all the steps by hand.
#INPUTS: 
	# Tables: 
		# county table with "Din_cty" and "GEOID"
	# Rasters: 
		# One raster of all regions (counties and buffer)
		# Intial landscape layer
		# Final landscape layer
#OUTPUTS:
	# Tables:  
		# Zonal histogram of transitions between initial and final landscape
		# zonal historgram of final landscape
	# Rasters: 
		# Raster of land use change
		

# ----------------------------------------------
# ----------------------------------------------
# STEP 3. 
# ----------------------------------------------
# ----------------------------------------------

# ----------------------------------------------
#  TRANSITION RATES
# ----------------------------------------------

# ----------------------------------------------
# R SCRIPT 
# Transition_Rates.r

#PURPOSE: Calculate transition rates for focal land use transitions. Writes them to one .csv file
#INPUTS:
	# Tables:  
		# Zonal histogram of transitions between initial and final landscape
		# zonal historgram of final landscape
	# Rasters: 
		# One raster of all regions (counties and buffer)
	
#OUTPUTS:
	# Tables:  
		# Zonal histogram of transitions between initial and final landscape
		# zonal historgram of final landscape
	# Rasters: 
		# raster of change in landscape.


# ----------------------------------------------
#  PATCH STATISTICS
# ----------------------------------------------

# ----------------------------------------------
# R SCRIPT 
# PatchStats.r

#PURPOSE:  A) Extract patch stats for both Expansion Patches and New Patches B) To extract 3 .csv files for Expansion Patch Parameters, New Patch Parameters, and % Expansion from the New/Expanded Patch Stats.
#INPUTS: 
	# Tables: 
		# county table with "Din_cty" and "GEOID"
	# Rasters: 
		# Individual rasters for each region (counties and buffer)
		# Intial landscape layer
		# Final landscape layer
		# Raster of land use change
#OUTPUTS:
	# Tables:  
		# New patch statistics
		# Expansion patch statistics
		# Percent expansion statistics

	
# ----------------------------------------------
# BUILD VARIABLE CUBE IN DINAMICA
# ----------------------------------------------

# ----------------------------------------------
# DINAMICA SCRIPT
# 

# ----------------------------------------------
# DINAMICA SCRIPT
# TEST_Variable_cube_final.egoml
# TEST_Variable_cube_init.egoml

#PURPOSE:  Create variable cubes for input into Dinamica Models
#INPUTS: 
	# Rasters: 
		# Each input variable, including land cover at 2001 or 2011 (init or final model)

#OUTPUTS:
	# Rasters: 
		# Variable Cube for 2001 (init) 
		# Variable Cube for 2011 (final)


# ----------------------------------------------
# ----------------------------------------------
# STEP 4. WEIGHTS OF EVIDENCE
# ----------------------------------------------
# ----------------------------------------------


# ----------------------------------------------
# CREATE DRAFT WEIGHTS OF EVIDENCE TABLE
# ----------------------------------------------

# ----------------------------------------------
# DINAMICA SCRIPT
# TEST_Calc_WOE.egoml

#PURPOSE:  Calculate Weights of Evidence values and ranges for each focal land use transition based on included variables and distance measures
#INPUTS: 
	# Rasters: 
		# Variable cube for final landscape
		# Final land cover 
		# Initial land cover
		# One raster of all regions (counties and buffer)

#OUTPUTS:
	# Tables: 
		# Weights of Evidence by region and land use transition




# ----------------------------------------------
# ADJUST WOE TABLE BASED ON SIGNIFICANCE
# ----------------------------------------------

# ----------------------------------------------
# R SCRIPT 
# Edit_WoE_AnyTrans.r

#PURPOSE:  Changing range classes on Dynamic Weights of evidence tables so that they only include significant range classes.
#INPUTS: 
	# Tables: 
		# Dinamica weights of evidence tables

#OUTPUTS:
	# Tables: 
		# Edited weights of evidence tables




# ----------------------------------------------
# RECALCULATE WEIGHTS OF EVIDENCE 
# ----------------------------------------------


# ----------------------------------------------
# DINAMICA SCRIPT
# TEST_ReCalc_WOE.egoml

#PURPOSE:  Calculate *NEW* Weights of Evidence values and ranges for each focal land use transition based on included variables and distance measures
#INPUTS: 
	# Tables:
		# Edited weights of evidence tables (from R script)
	# Rasters: 
		# Variable cube for final landscape
		# Final land cover 
		# Initial land cover
		# One raster of all regions (counties and buffer)

#OUTPUTS:
	# Tables: 
		# Weights of Evidence by region and land use transition


# ----------------------------------------------
# EDIT SPECIAL WOE VALUES (LIKE PROTECTED AREAS)
# ----------------------------------------------

# ----------------------------------------------
# R SCRIPT 
# WOE_ProtectedAreas.r

#PURPOSE: Assign value of "-100" to protected areas variables. See if this removes development or lc pressure from them.
#INPUTS: 
	# Tables: 
		# Recalculated Dinamica weights of evidence tables

#OUTPUTS:
	# Tables: 
		# Edited weights of evidence tables

# ----------------------------------------------
# CREATE BOX PLOTS TO EXAMINE WOE TRENDS
# ----------------------------------------------

# ----------------------------------------------
# R SCRIPT 
# VariableWOE.r

#PURPOSE: Create dataframe and scatter plots for WOE at each: transition, county, variable, bin
#INPUTS: 
	# Tables: 
		# county table with "Din_cty" and "GEOID" for the focal region only
		# Recalculated Dinamica weights of evidence tables

#OUTPUTS:
	# Tables: 
		# Binned WOE for each transition and variable
	# Figures:
		# Pdfs of box plots depicting binned WOE for each transition and variable
		
# ----------------------------------------------
# ----------------------------------------------
# STEP 5. RUN FULL LULC MODEL
# ----------------------------------------------
# ----------------------------------------------

# ----------------------------------------------
# DINAMICA SCRIPT
# TEST_LULC

#PURPOSE:  Full LULC model run in Dinamica, projecting land use change into future
#INPUTS: 
	# Tables:
		# Transition Rates 
		# Patch Stats: New patches 
		# Patch Stats: Expanded patches
		# Patch Stats: Percent Expansion
		# Edited weights of evidence tables (from most recent Dinamica WOE run)
	# Rasters: 
		# Variable cube for final landscape
		# Final land cover (labeled as "Initial Landscape" in Dinamica Model because it is the new initial)
		# One raster of all regions (counties and buffer)

#OUTPUTS:
	# Rasters: 
		# One raster per transition year. 

