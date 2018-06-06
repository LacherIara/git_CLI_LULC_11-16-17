Purpose: Generate zonal histograms for each scenario (RT, Q1, Q2, Q3, Q4) based on land cover type (3=Development, 5= Forest, 6= Grass, 7= Crop).  A zonal histogram table is generated for each time step (1-5 or 2021-2061).  The code has the capability of running an individual scenario at a time or all scenarios at once. This is why you will see the code repeat. Additionally, the code is written so it can easily be updated based on the version and scenario desired. However, it is important to ensure these are updated for the correct result. 
Inputs: 
- Ctny_StudyArea.tif: raster that designates the counties within the study area to create the zones for the zonal histogram 
- Sa_ctyGEOID: adds the geological ID to the county raster 
- Future Landscapes (ex. v2015_Q1_Landscape01): Future landscape from dinamic model. There is a total of 20 future landscapes 
- NLCD 2001: Land cover 2001 
- NLCD 2011: Land cover 2011
Outputs: 
8_ZonalHist_MultScens_50years
- Future Landscapes histogram (ex. V2015_Q1_Landscape01_hist) with value representing area (units are in pixels that are later converted to km)
- NLCD histogram (ex. Nlcd01_anC_hist) 

8_ZonalHist_MultScens_50years_MergeTables 
- County (Melt) : In order to use ggplot the histograms needed to be rearranged. Data kept on a county level. 
- Region (Melt): From the resulting melted data summed up to region level. 
- Sum (Melt): From the resulting melted county data summed up to the study area or the study area + buffer. 

8_ZonalHist_MultScens_50years_Graphs
Graphs: 
- Contains ggplot code to make graphs that display final histograms. 
o Graphs are generated to either compare scenarios over time for a specific land cover either for a specific county or the entire study area. 
o Graphs are generated to compare land cover types for a specific scenario 
* NOTE: any of the graphs can be slightly modified to give desired outcome 
