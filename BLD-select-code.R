##code to select BLD data from snapshot and subset for a given project

##data gaps
##Washington data lacks animals; there are some local copies of data from the state
##Mass, AZ, etc- don't have their spatial data

##BLD metadata
##https://natureserve01.sharepoint.com/:w:/g/teamsites/mjd/EXFrFZ3iAhxDgGqXdlkWJDwBK9OoaDEM9cxtLgw1_OD0QQ?CID=f107b6da-39da-7146-7ed6-d23c1ccc8359

##LOAD PACKAGES

##start by installing the package
#install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")
library(arcgisbinding)
## Check ArcGIS Pro product is installed locally
arc.check_product()

##install reticulate
##need to first install devtools packages and rtools
#install.packages("devtools")
#library(devtools)
#install_version("reticulate", version = "1.22", repos = "http://cran.us.r-project.org") ##older version required until package is updated to handle space in filepath name for python
library(reticulate)
reticulate::use_condaenv(condaenv='C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3', required = T)

#library(sf)
library(tidyverse)
#library(readxl)

arcpy<-import("arcpy")

##CREATE OUTPUT FOLDER
if (!file.exists(str_c("Output-", Sys.Date()))) {
  dir.create(str_c("Output-", Sys.Date()))
}

##LOAD SPATIAL LAYERS
##layer of spatial extent for selecting species
boundary.path<-"C:/Users/max_tarjan/OneDrive - NatureServe/Documents/GIS_data/DRECP Boundary/data/v101/0000DRECP Boundary.lyr"
#boundary.path<-"Data/BLM_National_Surface_Management_Agency/BLM_Lands_NAD83.shp"

##biotics snapshot
bld.path<-"S:/Data/NatureServe/BLD_Occurrences/NS_BLD_GeoDB/Snapshots/Monthly-2022-03/bld-2022-03.gdb/BLD_EO_SPECIES"

##CLIP BLD EOS BY A BOUNDARY LAYER
##result is written out as a shapefile
start.time<-Sys.time()
arcpy$Clip_analysis(in_features = bld.path, clip_features = boundary.path, out_feature_class = str_c("Output-", Sys.Date(), "/bld_desert"))

##open clipped bld using arcbridge for wrangling
bld.desert.path<-str_c("Output-", Sys.Date(), "/bld_desert.shp")
bld.desert<-arc.open(bld.desert.path)

bld.desert.df<-arc.select(bld.desert)
bld.desert.df<-arc.select(bld.desert, fields = c('EGT_ID', 'SNAME', 'SCOMNAME', 'G_RANK', 'MAJ_GRP1'), where_clause = "MAJ_GRP1 = 'Vascular Plants - Conifers and relatives'")

##visualizing data subset
bld.desert.sf<-arc.data2sf(bld.desert.df)
plot(st_zm(bld.desert.sf["EGT_ID"]))

library(sp)
bld.desert.sp<-arc.data2sp(bld.desert.df)
plot(bld.desert.sp)
