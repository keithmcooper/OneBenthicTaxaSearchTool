#### BRING IN DATASET ####

## Load packages
library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library (RPostgres)
library(DBI)
library(plyr)
library(dplyr)
library(geojsonio)
library(sp)
library(ggplot2)
library(config)
library(pool)
library(leaflet.esri)
#__________________________________________________________________________________________
#### CODE TO SOLVE ERROR: Missing dbQuoteLiteral methods for pool' ####

##https://github.com/rstudio/pool/issues/96
#' @importMethodsFrom DBI dbQuoteLiteral
#' @importFrom pool poolCheckout poolReturn
#' @export
setMethod("dbQuoteLiteral", c("Pool", "ANY"),
          function(conn, x, ...) {
            # As of 2020-05-07, this is necessary due to an incompatiblity
            # between packages `pool` (v 0.1.4.3) and `glue` (v >= 1.3.2).
            # In v1.3.2, `glue` started using `dbQuoteLiteral`, which is
            # currently not offered by `pool`, so creating it here.
            connection <- pool::poolCheckout(conn)
            on.exit(pool::poolReturn(connection))
            DBI::dbQuoteLiteral(connection, x, ...)
          }
)

#__________________________________________________________________________________________
#### CREATE A CONNECTION TO OneBenthic LIVE ####
Sys.setenv(R_CONFIG_ACTIVE = "one_benthic")

dw <- config::get()

pool <- dbPool(drv = dbDriver(dw$driver),
               dbname = dw$database,
               host = dw$server,
               port =  dw$port,
               user = dw$uid,
               password = dw$pwd)

#__________________________________________________________________________________________
#### GET POINT SAMPLE DATA (META & FAUNA) ####

## Get data
data = dbGetQuery(pool,
"SELECT su.surveyname,
s.samplecode,
s.samplelat,
s.samplelong,
g.gearname,
s.date,
s.macrosieve,
ts.worrmstaxa_taxonname,
ts.taxaqual_qualifier,
w.validname,
w.rank,
tq.qualifiername,
w.validaphiaid,
ts.abund,
su.datapubliclyavailable,
s.samplecode2,
o.ownername,
s.waterdepth,
s.grabsamplesize,
w.phylum,
w.class,
w.order,
w.family,
w.genus
                  
FROM 
associations.survey as su
INNER JOIN associations.surveysample as ss ON ss.survey_surveyname = su.surveyname 
INNER JOIN samples.sample as s ON ss.sample_samplecode = s.samplecode
INNER JOIN gear.gear as g ON s.gear_gearcode = g.gearcode
INNER JOIN faunal_data.taxasample as ts ON s.samplecode= ts.sample_samplecode 
LEFT JOIN faunal_data.taxaqual as tq ON ts.taxaqual_qualifier = tq.qualifier 
INNER JOIN faunal_data.worrms as w ON w.aphiaid = ts.worrms_aphiaid
INNER JOIN associations.sampleowner as so ON so.sample_samplecode = s.samplecode
INNER JOIN associations.owner as o ON so.owner_ownername = o.ownername

ORDER by su.surveyname;;
")
#WHERE datapubliclyavailable = TRUE
## Check names
names(data)

## Change column names
colnames(data)[1] <- "SurveyName"
colnames(data)[2] <- "SampleCode"
colnames(data)[3] <- "Latitude"
colnames(data)[4] <- "Longitude"
colnames(data)[5] <- "GearName"
colnames(data)[6] <- "Date"
colnames(data)[7] <- "MacroSieveSize_mm"
#colnames(data)[10] <- "validname(WoRMS)"
colnames(data)[11] <- "Rank"
colnames(data)[12] <- "TaxonQualifier"
colnames(data)[13] <- "AphiaID"
colnames(data)[14] <- "Abund"
colnames(data)[16] <- "SampleCode2"
colnames(data)[17] <- "DataOwner"
colnames(data)[18] <- "WaterDepth"
colnames(data)[19] <- "SampleSize"

## Check name changes
names(data)
#[1] "SurveyName"            
#2"SampleCode"            
#3"Latitude"              
#4"Longitude"             
#5"GearName"              
#6"Date"                  
#7"MacroSieveSize_mm"    
#8"worrmstaxa_taxonname"  
#9"taxaqual_qualifier"    
#10"validname"        
#11"Rank"                  
#12"TaxonQualifier"        
#13"AphiaID"               
#14"Abund"                
#15"datapubliclyavailable" "SampleCode2"          

## Create an object 'points' for sample positions
points <- unique(data[,c(4:3,1,2,10)])
#View(points)
head(points)
dim(points)

## Unique positions
points2 <- unique(data[,c(4:3,1,2)])

#__________________________________________________________________________________________
#### GET POINT SAMPLE DATA (META & PSA) ####

## Get data
psadata = dbGetQuery(pool,
                     "SELECT
su.surveyname,
s.samplecode,
s.samplelat,
s.samplelong,
g.gearname,
s.date,
svs.sedvar_sievesize,
svs.percentage,
su.datapubliclyavailable,
s.samplecode2,
o.ownername,
s.waterdepth,
s.grabsamplesize

FROM 
associations.survey as su
INNER JOIN associations.surveysample as ss ON ss.survey_surveyname = su.surveyname 
INNER JOIN samples.sample as s ON ss.sample_samplecode = s.samplecode
INNER JOIN gear.gear as g ON s.gear_gearcode = g.gearcode
INNER JOIN sediment_data.sedvarsample as svs ON s.samplecode= svs.sample_samplecode 
INNER JOIN associations.sampleowner as so ON so.sample_samplecode = s.samplecode
INNER JOIN associations.owner as o ON so.owner_ownername = o.ownername
ORDER by su.surveyname;")
#WHERE datapubliclyavailable = TRUE
## Check results of query 
names(psadata)

## Change names of certain columns
colnames(psadata)[1] <- "SurveyName"
colnames(psadata)[2] <- "SampleCode"
colnames(psadata)[3] <- "Latitude"
colnames(psadata)[4] <- "Longitude"
colnames(psadata)[5] <- "GearName"
colnames(psadata)[6] <- "Date"
colnames(psadata)[7] <- "SieveSize_mm"
colnames(psadata)[8] <- "Percentage"
colnames(psadata)[10] <- "SampleCode2"
colnames(psadata)[11] <- "DataOwner"
colnames(psadata)[12] <- "WaterDepth"
colnames(psadata)[13] <- "SampleSize"

## Check name changes
names(psadata)
#1 "SurveyName"            
#2 "SampleCode"            
#3 "Latitude"              
#4 "Longitude"             
#5 "GearName"              
#6 "Date"                  
#7 "SieveSize_mm"         
#8  "Percentage"            
#9 "datapubliclyavailable" 
#10 "SampleCode2"           
#11 "DataOwner"
#__________________________________________________________________________________________
#### GET SPATIAL LAYERS FROM OneBenthic - WHERE NO API AVAILABLE (MPAS, O&G, DISP) ####

mcz <-  st_read(pool, query = "SELECT * FROM spatial.c20190905_offshorempas_wgs84 WHERE site_statu = 'MCZ - Secretary of State';")
sac <-  st_read(pool, query = "SELECT * FROM spatial.c20190905_offshorempas_wgs84 WHERE site_statu = 'SAC'or site_statu = 'cSAC';")
ncmpa <-  st_read(pool, query = "SELECT * FROM spatial.c20190905_offshorempas_wgs84 WHERE site_statu = 'NCMPA';")
oga <- st_read(pool, query = "SELECT * FROM spatial.oga_licences_wgs84;")
disp  <-  st_read(pool, query = "SELECT * FROM spatial.disposalSiteJan2020;")
agg <- st_read(pool, query = "SELECT * FROM ap_marine_aggregate.extraction_areas;")
owf <- st_read(pool, query = "SELECT * FROM spatial.offshore_wind_site_agreements_england_wales__ni__the_crown_esta;")
owf_cab <- st_read(pool, query = "SELECT * FROM spatial.offshore_wind_cable_agreements_england_wales__ni_the_crown_esta;")
wave <- st_read(pool, query = "SELECT * FROM spatial.offshore_wave_site_agreements_england_wales__ni_the_crown_estat;")
wave_cab <- st_read(pool, query = "SELECT * FROM spatial.offshore_wave_cable_agreements_england_wales__ni_the_crown_esta;")
tidal <- st_read(pool, query = "SELECT * FROM spatial.offshore_tidal_stream_site_agreements_england_wales__ni_the_cro;")
tidal_cab <- st_read(pool, query = "SELECT * FROM spatial.offshore_tidal_stream_cable_agreements_england_wales__ni_the_cr;")
R4_chara <- st_read(pool, query = "SELECT * FROM spatial.offshore_wind_leasing_round_4_characterisation_areas_england_wa;")
R4_bid <- st_read(pool, query = "SELECT * FROM spatial.offshore_wind_leasing_round_4_bidding_areas_england_wales_and_n;")
phyclust <- st_read(pool, query = "SELECT * FROM spatial.physicalcluster;")

## Check class of objects
class(mcz)#[1] "sf"         "data.frame"
class(sac)#[1] "sf"         "data.frame"
class(ncmpa)#[1] "sf"         "data.frame"
class(oga)#[1] "sf"         "data.frame"
class(disp)#[1] "sf"         "data.frame"
class(agg) #[1] "sf"         "data.frame"
class(owf)#[1] "sf"         "data.frame"
class(owf_cab)#[1] "sf"         "data.frame"
class(wave)#[1] "sf"         "data.frame"
class(wave_cab)#[1] "sf"         "data.frame"
class(tidal)#[1] "sf"         "data.frame"
class(tidal_cab)#[1] "sf"         "data.frame"
class(R4_chara)#[1] "sf"         "data.frame"
class(R4_bid)#[1] "sf"         "data.frame"
class(phyclust)#[1] "sf"         "data.frame"

## Check CRS
st_crs(mcz)#Coordinate Reference System: NA
st_crs(sac)#Coordinate Reference System: NA
st_crs(ncmpa)#Coordinate Reference System: NA
st_crs(oga)#Coordinate Reference System: NA
st_crs(disp)#Coordinate Reference System: NA
st_crs(agg) # 4326
st_crs(owf)#Coordinate Reference System: NA
st_crs(owf_cab)#Coordinate Reference System: NA
st_crs(wave)#Coordinate Reference System: NA
st_crs(wave_cab)#Coordinate Reference System: NA
st_crs(tidal)#Coordinate Reference System: NA
st_crs(tidal_cab)#Coordinate Reference System: NA
st_crs(R4_chara)#Coordinate Reference System: NA
st_crs(R4_bid)#Coordinate Reference System: NA
st_crs(phyclust)#Coordinate Reference System: NA

## Set CRS where necessary
st_crs(mcz) <- 4326
st_crs(sac) <- 4326
st_crs(ncmpa) <- 4326
st_crs(oga) <- 4326
st_crs(disp) <- 4326
st_crs(owf) <- 4326
st_crs(owf_cab) <- 4326
st_crs(wave) <- 4326
st_crs(wave_cab) <- 4326
st_crs(tidal) <- 4326
st_crs(tidal_cab) <- 4326
st_crs(R4_chara) <- 4326
st_crs(R4_bid) <- 4326
st_crs(phyclust) <- 4326

## Assign colours to phyclust strata
phyclust$dn <- as.factor(phyclust$dn)
#levels(phyclust$dn)

factpal <- colorFactor(c("#F8766D","#DE8C00","#B79F00","#7CAE00","#00BA38","#00C08B","#00BFC4","#00B4F0","#619CFF","#C77CFF","#F564E3","#FF64B0"), phyclust$dn)

#leaflet(phyclust) %>%
#  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
 #             color = ~factpal(dn))
#__________________________________________________________________________________________
#### GET SPATIAL LAYERS VIA API (AGG, OWF, WAVE, TIDE) ####


## Bring in data via API
#aggdata <- geojson_read("https://opendata.arcgis.com/datasets/ced5788f014546b0b571e8d29b021166_0.geojson",  what = "sp")
#owfdata <- geojson_read("https://opendata.arcgis.com/datasets/091b2244bf534d398140452cc98de09b_0.geojson",  what = "sp")
#owf_cabdata <- geojson_read("https://opendata.arcgis.com/datasets/dabc9654ba92480caa914629004a1eba_0.geojson",  what = "sp")
#wavedata <- geojson_read("https://opendata.arcgis.com/datasets/89f05cce72db4bbe9688a98c5d4d8659_0.geojson",  what = "sp")
#wave_cabdata <- geojson_read("https://opendata.arcgis.com/datasets/9f694a7c71bc4bdd9435408f3cd0c3c1_0.geojson",  what = "sp")
#tidaldata <- geojson_read("https://opendata.arcgis.com/datasets/a722b677a6754187bd018ca1292af568_0.geojson",  what = "sp")
#tidal_cabdata <- geojson_read("https://opendata.arcgis.com/datasets/277644bfed424c9196db80911537c5c0_0.geojson",  what = "sp")
#R4_charadata <- geojson_read("https://opendata.arcgis.com/datasets/c0e61d8972e4438ab1c39304b7f28608_0.geojson",  what = "sp")
#R4_biddata <- geojson_read("https://opendata.arcgis.com/datasets/54dce8a263324a85b36523e31fff20cc_0.geojson",  what = "sp")

## Project data to long lat using sp
#agg <- spTransform(aggdata, CRS("+init=epsg:4326"))#Project your data to longlat using spTransform from either rgdal or sp.
#owf <- spTransform(owfdata, CRS("+init=epsg:4326"))
#owf_cab <- spTransform(owf_cabdata, CRS("+init=epsg:4326"))
#wave <- spTransform(wavedata, CRS("+init=epsg:4326"))
#wave_cab <- spTransform(wave_cabdata, CRS("+init=epsg:4326"))
#tidal <- spTransform(tidaldata, CRS("+init=epsg:4326"))
#tidal_cab <- spTransform(tidal_cabdata, CRS("+init=epsg:4326"))
#R4_chara <- spTransform(R4_charadata, CRS("+init=epsg:4326"))
#R4_bid <- spTransform(R4_biddata, CRS("+init=epsg:4326"))

## Convert api data from sp to sf
#agg <- st_as_sf(agg)
#owf <- st_as_sf(owf)
#owf_cab <-st_as_sf(owf_cab)
#wave <- st_as_sf(wave)
#wave_cab <-st_as_sf(wave_cab)
#tidal <- st_as_sf(tidal)
#tidal_cab <- st_as_sf(tidal_cab)
#R4_chara <- st_as_sf(R4_chara)
#R4_bid <- st_as_sf(R4_bid)

#__________________________________________________________________________________________
####

## Load libraries
#library(ggplot2)
library(rgdal)
#library(maptools)
library(plyr)
#library(dismo)
library(raster)



## Set working directory
#setwd('C:/Users/kmc00/OneDrive - CEFAS/R_PROJECTS/OneBenthicTaxaSearchTool/R')
###############################################

## Bring in raster data for PhyCluster group
  str_name<-'DATA/PhysicalCluster.tif' 
  phyclustif <- raster(str_name)

  phyclustif
  #plot(phyclustif)
  
  ## Create a df for predictor variables
  sdata <- extract(phyclustif, data[,4:3])
  
  #View(sdata)
  class(sdata)
  
  ##Change from matrix to df
  sdata=as.data.frame(sdata)
  ## Bind togeter data and extracted variables (sdata)
  #View(sdata)
  sdata2 <- cbind(data,sdata)
  #View(sdata2)
  str(sdata2)
  names(sdata2)
  colnames(sdata2)[25] <-"phyclus"
  #sdata2$phyclus <- as.factor(sdata2$phyclus)#new
  
  ## Count the number of samples by phyclus group
  
  sdata3 <-subset(sdata2, !duplicated(SampleCode))
  head(sdata3)
  sdata4 <- sdata3%>%group_by(phyclus)%>%count(phyclus)
  sdata4 <- as.data.frame(sdata4)
  head(sdata4)
  
#sdata4$phyclus <- as.factor(sdata4$phyclus)#new
  #levels(phyclust$dn)
sdata4$hex <- c("#F8766D","#DE8C00","#B79F00","#7CAE00","#00BA38","#00C08B","#00BFC4","#00B4F0","#619CFF","#C77CFF","#F564E3","#FF64B0",NA)#,"grey"
sdata4$hex <- as.factor(sdata4$hex)
str(sdata4)
#View(sdata4)
#addNA(sdata4$phyclus)
#__________________________________________________________________________________________
#### MAP LAYERS SOURCE INFO ####
layer <- data.frame("Layer"=c("owf","owf_cab","R4_chara","R4_bid","agg","disp","wave","wave_cab","tidal","tidal_cab","oga","mcz","sac","ncmpa"),
                    "Detail"=c("Offshore Wind Site Agreements (England, Wales & NI) - The Crown Estate",
                               "Offshore Wind Cable Agreements (England, Wales & NI), The Crown Estate",
                               "Offshore Wind Leasing Round 4 Characterisation Areas (England, Wales and NI) - The Crown Estate",
                               "Offshore Wind Leasing Round 4 Bidding Areas (England, Wales and NI) - The Crown Estate",
                               "Offshore Minerals Aggregates Site Agreements (England, Wales & NI), The Crown Estate",
                               "UK Disposal Site Layer, Cefas",
                               "Offshore Wave Site Agreements (England, Wales & NI), The Crown Estate",
                               "Offshore Wind Cable Agreements (England, Wales & NI), The Crown Estate",
                               "Offshore Tidal Stream Site Agreements (England, Wales & NI), The Crown Estate",
                               "Offshore Tidal Stream Cable Agreements (England, Wales & NI), The Crown Estate",
                               "OGA Licences WGS84, Oil and Gas Authority","Marine Conservation Zones (MCZ)","Special Area of Conservation","Nature Conservation Marine Protected Areas (Scotland)"),
                    "Link"=c("https://opendata.arcgis.com/datasets/091b2244bf534d398140452cc98de09b_0.geojson",
                             "https://opendata.arcgis.com/datasets/dabc9654ba92480caa914629004a1eba_0.geojson",
                             "https://opendata.arcgis.com/datasets/c0e61d8972e4438ab1c39304b7f28608_0.geojson",
                             "https://opendata.arcgis.com/datasets/54dce8a263324a85b36523e31fff20cc_0.geojson",
                             "https://opendata.arcgis.com/datasets/ced5788f014546b0b571e8d29b021166_0.geojson",
                             "http://data.cefas.co.uk/#/View/407",
                             "https://opendata.arcgis.com/datasets/89f05cce72db4bbe9688a98c5d4d8659_0.geojson",
                             "https://opendata.arcgis.com/datasets/dabc9654ba92480caa914629004a1eba_0.geojson",
                             "https://opendata.arcgis.com/datasets/a722b677a6754187bd018ca1292af568_0.geojson",
                             "https://opendata.arcgis.com/datasets/277644bfed424c9196db80911537c5c0_0.geojson",
                             "https://opendata.arcgis.com/datasets/3c950a2c8186438899f99ced733dd947_0.geojson",
                             "https://hub.jncc.gov.uk/assets/ade43f34-54d6-4084-b66a-64f0b4a5ef27/c20190905_OffshoreMPAs_WGS84.shp",
                             "https://hub.jncc.gov.uk/assets/ade43f34-54d6-4084-b66a-64f0b4a5ef27/c20190905_OffshoreMPAs_WGS84.shp",
                             "https://hub.jncc.gov.uk/assets/ade43f34-54d6-4084-b66a-64f0b4a5ef27/c20190905_OffshoreMPAs_WGS84.shp"))

layer$Link<- paste0("<a href='",layer$Link,"'>",layer$Link,"</a>")

#__________________________________________________________________________________________
#__________________________________________________________________________________________
#### USER INTERFACE ####

ui <- fluidPage(
 titlePanel(title=div(img(src="onebenthic.gif",tags$b(" OneBenthic"),"Taxa Search Tool",height = 65, width = 170),#HEIGHT65
                       style='background-color:#B4C7E7;
                    padding-right: 50px')),

  fluidRow(
    
#__________________________________________________________________________________________
#### SELECT SURVEY(S) ####    
    column(2,selectizeInput(inputId="taxonInput", multiple = F,h4("Select taxa",style="color:#808080"),choices =levels(as.factor(data$validname)),selected=NULL,options=list(maxOptions=4000)),h4("Taxon Rank:",style="color:#808080"),textOutput("selected_rank"),br(),h4("AphiaID:",style="color:#808080"),textOutput("selected_aphiaid"),br(),h4("Phylum:",style="color:#808080"),textOutput("selected_phylum"),h4("Class:",style="color:#808080"),textOutput("selected_class"),h4("Order:",style="color:#808080"),textOutput("selected_order"),h4("Family:",style="color:#808080"),textOutput("selected_family"),h4("Genus:",style="color:#808080"),textOutput("selected_genus")),
    
#__________________________________________________________________________________________
#### MAP ####    
    column(5,leafletOutput("map",width = "100%", height=850),style='border-left: 1px solid grey'),
    column(5,style='border-left: 1px solid grey',
           
#__________________________________________________________________________________________
#### TAB: DATA BY SURVEY ####
          tabsetPanel(             
            tabPanel("Data",div(DT::dataTableOutput("coordinates"),style = 'font-size:85%'),br(),
                    # tabPanel("Data (by Survey)",div(DT::dataTableOutput("coordinates"),style = 'font-size:85%')),
             downloadButton("downloadData", "Download faunal data"),
            downloadButton("downloadpsaData", "Download sediment data")),
#__________________________________________________________________________________________
#### TAB: DATA BY SAMPLE ####
 #            tabPanel("Data (by Sample)",div(DT::dataTableOutput("bbdata"),style = 'font-size:85%'),br(),
 #            downloadButton("downloadData2", "Download faunal data"),
 #            downloadButton("downloadpsaData2", "Download sediment data")),
#__________________________________________________________________________________________
#### TAB: HABITAT ####
tabPanel(
  "Habitat",
  br(),
  "Proportion of samples by physical habitat class (Phy Cluster) where the selected taxon is present. Habitat classes can be displayed in the map by checking the 'habitat' box. Numbers above each bar are the total number of samples associated with each habitat class."," See ",tags$a(href="https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2664.13381", "(see Cooper et al., 2019),")," for a full description of how habitats (Phy Cluster) were derived.",
  br(),
  #div(DT::dataTableOutput("perc"),style = 'font-size:80%'),
  plotOutput("percplot",height = 300, width = 700),
  br(),
  br(),
  "Range of environmental conditions associated with the different physical habitats",
  br(),
  (img(src="Figure_S1.png",height = 375, width = 550))
  ,style = 'font-size:90%'),
#__________________________________________________________________________________________
#### TAB: ABOUT  ####
             tabPanel("About",h4("App purpose"),"
Retrieval individual taxon distribution and associated sediment particle size data from the ",tags$b("OneBenthic")," database.
                      
                      Select a ",tags$b("taxon")," from the drop-down list. Samples containing the selected taxon are shown in the map, with details presented in a table (see 'Data' tab). Taxon records and associated sediment particle size data can be download as .csv files. Only publicly available data are shown in this tool. Under the 'Habitats' tab is a bar chart showing the proportion of samples within different physical habitat classes where the selected taxon is present. Environmental conditions associated with each  habitat class (Phy Cluster), and, by implication, the select taxon, are displayed in a series of box plots. Details of the activity layers shown in the map can be found in the 'Map Layers' tab.",br(),br(),
                      
                      h4("What is OneBenthic?"),

                      "Large quantities of benthic data (macrofauna and sediment particle size) are now in the public domain, but are distriburted across multiple repositories. ", "The purpose of ",tags$b("OneBenthic,"),"a PostgreSQL database, is to bring these disperate data together in one place, thereby aiding research and facilitating reuse of data.Taxon names are standardized according to the", tags$a(href="http://www.marinespecies.org/", "World Register of Marine Species.")," Sediment data are percentage weight by sieve size (mm).",
                      
                      "Many of the current key marine environmental issues (e.g. conservation of biodiversity, habitat mapping, marine spatial planning, effects of climate change, cumulative effects) require access to large datasets, and the database meets this need.",
                      
                       tags$b("OneBenthic")," links directly to a range of other apps",tags$a(href="https://openscience.cefas.co.uk/", "CefasOpenScience,"), "providing useable information back to data providers and the wider public",br(),br(),

                      "These tools are actively being used by offshore marine industries and government for purposes of project planning, licence compliance monitoring and research.",br(),br(),
h4("Origin of data"),"Data originate from multiple sources and providers, and were brought together in a study",tags$a(href="http://rdcu.be/wi6C", "(Cooper and Barry, 2017)")," jointly funded by the Department for Food and Rural Affairs, the Marine Management Organisation, the British Marine Aggregate Producers Association, the Crown Estate and the Welsh Government. Data from this study were",tags$a(href="https://doi.org/10.14466/CefasDataHub.34", "published"), "(in csv format) with the kind permission of data providers. The current app provides access to these data is a more user friendly way. Subject to funding, new publicly available data will continue to be added to",tags$b("OneBenthic")," so that it continues to provide an up-to-date and 'complete' source of UK benthic data.",br(),br(),

h4("Data access and use"),tags$b("OneBenthic"),"contains data from a range of data providers in industry and government. With the permission of these providers, data are made available under Crown Copyright and", tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", "Open Government Licence."),"
Please cite the database as follows: ",br(),tags$b("OneBenthic")," database (2023). Available from https://openscience.cefas.co.uk/obtst/. Accessed: DD/MM/YYYY.",br(),br(),
h4("Contact"),"For technical and data issues email keith.cooper@cefas.co.uk",
style = 'font-size:90%'),

#__________________________________________________________________________________________
#### TAB: MAP LAYERS ####
tabPanel("Map Layers",br(),DT::dataTableOutput("activitytable"),style = 'font-size:85%'),
#__________________________________________________________________________________________
#### TAB: DATA PROVIDERS ####
tabPanel("Data Providers",br(),"We gratefully acknowledge all the individual data providers:",br(),
         h4("Government"),  tags$i("Department for Environment, Food and Rural Affairs (DEFRA); Centre for Environment, Fisheries and Aquaculture Science (CEFAS); The Crown Estate; Natural Resources Wales (NRW); Department of the Environment Northern Ireland (DOENI); Agri-Food and Biosciences Institute; Natural England (NE) / Natural Resources Wales (NRW); Environment Agency (EA); Fisheries Research Services (FRS); Joint Nature Conservation Committee (JNCC); Natural Resources Wales (NRW) / Environment Agency (EA); Natural Resources Wales (NRW) / School of Ocean Sciences (SOS); Scottish Natural Heritage (SNH); Marine Scotland (MS); Scottish Environmental Protection Agency (SEPA)"),
         h4("Marine Aggregates"),tags$i("British Marine Aggregate Producers Association (BMAPA); Cemex UK Marine Limited;  Dredging International; Lafarge Tarmac Marine Limited; Volker Dredging Limited; Hanson Aggregates Marine Limited; Natural England (NE) / The Crown Estate (TCE) / Resource Managament Association (RMA); Resource Management Association (RMA); Sea Aggregates & Slag Limited; Britannia Aggregates Limited; Westminster Gravels Limited; DBM Building Materials; Humber Aggregate Dredging Association (HADA); Anglian Offshore Dredging Association (AODA); Thames Estuary Dredging Association (TEDA); East Channel Association (ECA); South Coast Dredging Association; Tarmac Marine Dredging Limited / Kendall Bros (Portsmouth) Limited; DEME Building Materials; United Marine Dredging Limited; Natural England (NE); United Marine Dredging Limited / Kendal Bros Portsmouth Limited; South Coast Shipping Co Limited; Tarmac Marine Dredging Limited / Norwest Sand & Ballast Company Limited; Hanson Aggregates Marine Limited / United Marine Dredging Limited; Llanelli Sand Dredging Limited"),
         h4("Offshore Wind"),tags$i("Centrica Energy; Dudgeon Offshore Wind Limited; Gwynt y Mor Offshore Wind Farm Limited; Humber Wind Limited; Triton Knoll Offshore Wind Farm Limited; East Anglia Offshore Wind Limited; E.ON Climate & Renewables UK Limited; Dong Wind (UK) Limited; Greater Gabbard Offshore Winds Limited; Gunfleet Sands II Limited (Dong Wind (UK) Limited; Galloper Wind Farm Limited; Lincs Wind Farm Limited (Centrica (DSW) Limited); Centrica (DSW) Limited; RWE Innogy; Westermost Rough Limited; Vattenfall; Dong Energy (UK) Limited; Dong Energy Walney Extension (UK) Limited; Ormonde Energy Limited; Innogy SE; Gunfleet Sands Limited; Scira Offshore Energy Limited; E.ON Climate & Renewables UK Rampion Offshore Wind Limited (E.ON Climate & Renewables GmbH); EDF Energy (Northern Offshore Wind) Limited (EDF Energy Renewables); Forewind; Walney (UK) Offshore Windfarms Limited (Dong Wind (UK) Limited); E.ON Climate & Renewables UK Robin Rigg West Limited (E.ON Climate & Renewables UK Limited); West of Duddon Sands; London Array Limited; Thanet Offshore Wind Limited (Vattenfall Vindkraft AB); Kentish Flats Limited (Vattenfall Wind Power Limited); Centrica Renewable Energy Limited (Centrica PLC); Teeside Windfarm Limited (EDF Energy Renewables); Neart Na Gaoithe Offshore Wind Limited (Mainstream Renewable Power); SSE Renewables (Scottish and Southern Energy (SSE)); Celtic Array Limited; DONG Energy AS"),
         h4("Oil & Gas"), tags$i("UKOOA; BP; Phillips; Britoil; Shell; Wintershall; Total; GDF Suez; Chevron; Gaz de France; BHP; Mobil; GDF Britain Limited"),
         h4("Nuclear"), tags$i("EDF Energy"),
         h4("Ports & Harbours"), tags$i("Dover Harbour Board (DHB)"),
style = 'font-size:90%'),
#__________________________________________________________________________________________
#### TAB: FUNDERS ####

tabPanel("Funders",(img(src="logos.png",height = 400, width = 800)))
                    
#__________________________________________________________________________________________

    )
  )
)
)

#__________________________________________________________________________________________
#### SERVER FUNCTION ####
server <- function(input, output) {
#__________________________________________________________________________________________
  #### TABLE FOR ACTIVITY LAYERS ####
  
  output$activitytable <- DT::renderDataTable(
    
    DT::datatable(layer, options = list(pageLength = 14),escape=FALSE)
  )
  #__________________________________________________________________________________________  
 #### TAXONOMIC INFO ####
   output$selected_rank <- renderText({ 
    rankinf=data[which(data$validname == input$taxonInput),]
    paste(unique(rankinf$Rank))
  })
  
  output$selected_aphiaid <- renderText({ 
    aphiaid=data[which(data$validname == input$taxonInput),]
    paste(unique(aphiaid$AphiaID))
  })
  output$selected_phylum <- renderText({ 
    phylum=data[which(data$validname == input$taxonInput),]
    paste(unique(phylum$phylum))
  })
  output$selected_class <- renderText({ 
    class=data[which(data$validname == input$taxonInput),]
    paste(unique(class$class))
  })
  output$selected_order <- renderText({ 
    order=data[which(data$validname == input$taxonInput),]
    paste(unique(order$order))
  })
  output$selected_family <- renderText({ 
    family=data[which(data$validname == input$taxonInput),]
    paste(unique(family$family))
  })
  output$selected_genus <- renderText({ 
    genus=data[which(data$validname == input$taxonInput),]
    paste(unique(genus$genus))
  })
#### INTERACTIVE MAP ####
  output$map <- renderLeaflet({
    
    ## Basic map
      leaflet() %>%
      addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels = F)%>%
      #addProviderTiles(providers$Esri.OceanBasemap,options = providerTileOptions(noWrap = TRUE))%>%
      addPolygons(data=owf,color = "#444444", weight = 1, smoothFactor = 0.5,group = "owf",popup = paste0("<b>Name: </b>", owf$Name_Prop, "<br>","<b>Status: </b>", owf$Inf_Status))%>%
      addPolygons(data=owf_cab,color = "#444444", weight = 1, smoothFactor = 0.5,group = "owf_cab",popup = paste0("<b>Name: </b>", owf_cab$Name_Prop, "<br>","<b>Status: </b>", owf_cab$Infra_Stat))%>%
      addPolygons(data=R4_chara,color = "#444444", weight = 1, smoothFactor = 0.5,group = "R4_chara",popup = paste0("<b>Name: </b>", R4_chara$Name))%>%
      addPolygons(data=R4_bid,color = "#444444", weight = 1, smoothFactor = 0.5,group = "R4_bid",popup = paste0("<b>Name: </b>", R4_bid$Name, "<br>","<b>Status: </b>", R4_bid$Bidding_Ar))%>%
      addPolygons(data=agg,color = "#444444", weight = 1, smoothFactor = 0.5,group = "agg",popup = paste0("<b>Name: </b>", agg$Area_name, "<br>","<b>Number: </b>", agg$Area_number))%>%
      addPolygons(data=disp,color = "#444444", weight = 1, smoothFactor = 0.5,group = "disp",popup = paste0("<b>Name: </b>", disp$name_, "<br>","<b>Number: </b>", disp$site_))%>%
      addPolygons(data=wave,color = "#444444", weight = 1, smoothFactor = 0.5,group = "wave",popup = paste0("<b>Name: </b>", wave$Name_Prop, "<br>","<b>Status: </b>", wave$Inf_Status))%>%
      addPolygons(data=wave_cab,color = "#444444", weight = 1, smoothFactor = 0.5,group = "wave_cab",popup = paste0("<b>Name: </b>", wave_cab$Name_Prop, "<br>","<b>Status: </b>", wave_cab$Infra_Stat))%>%
      addPolygons(data=tidal,color = "#444444", weight = 1, smoothFactor = 0.5,group = "tidal",popup = paste0("<b>Name: </b>", tidal$Name_Prop, "<br>","<b>Status: </b>", tidal$Inf_StatUS))%>%
      addPolygons(data=tidal_cab,color = "#444444", weight = 1, smoothFactor = 0.5,group = "tidal_cab",popup = paste0("<b>Name: </b>", tidal_cab$Name_Prop, "<br>","<b>Status: </b>", tidal_cab$Infra_Stat))%>%
      addPolygons(data=mcz,color = "#ff8b3d", weight = 1, smoothFactor = 0.5,group = "mcz",popup = paste0("<b>Name: </b>", mcz$site_name))%>%
     addPolygons(data=sac,color = "#ff8b3d", weight = 1, smoothFactor = 0.5,group = "sac",popup = paste0("<b>Name: </b>", sac$site_name))%>%
      addPolygons(data=ncmpa,color = "#ff8b3d", weight = 1, smoothFactor = 0.5,group = "ncmpa",popup = paste0("<b>Name: </b>", ncmpa$site_name))%>%
      addPolygons(data=oga,color = "#444444", weight = 1, smoothFactor = 0.5,group = "oga",popup = paste0("<b>Number: </b>", oga$licref, "<br>","<b>Organisation: </b>", oga$licorggrp))%>%
      addPolygons(data=phyclust,color = ~factpal(dn), weight = 1, smoothFactor = 0.5, fillOpacity = 0.7,group = "habitat",popup = paste0("<b>Phy Cluster: </b>", phyclust$dn))%>%
      addLayersControl(overlayGroups = c("owf","owf_cab","R4_chara","R4_bid","agg","disp","wave","wave_cab","tidal","tidal_cab","oga","mcz","sac","ncmpa","habitat"),options = layersControlOptions(collapsed = FALSE))%>%hideGroup(c("owf","owf_cab","R4_chara","R4_bid","agg","disp","wave","wave_cab","tidal","tidal_cab","oga","mcz","sac","ncmpa","habitat"))%>%
      addCircleMarkers(data=points2,~Longitude,~Latitude,radius = 2,stroke = FALSE,fillOpacity = 0.2,popup = paste0("<b>SurveyName: </b>",points2$SurveyName,"<br>","<b>SampleCode: </b>",points2$SampleCode))%>%
      addCircleMarkers(data=points2,~Longitude,~Latitude,radius = 2,stroke = FALSE,fillOpacity = 0.2,popup = ~as.character(SurveyName),group = "myMarkers")%>%
      #addDrawToolbar(polylineOptions = F, circleOptions = F, markerOptions = F,circleMarkerOptions = F, polygonOptions = F, singleFeature=TRUE)%>%

      
      setView(-3,54.6,zoom=5.5)
  })
  
#__________________________________________________________________________________________  
#### UPDATE MAP WITH SELECTED SURVEYS ####
#https://stackoverflow.com/questions/46979328/how-to-make-shiny-leaflet-map-reac-to-change-in-input-value-r
  
# Watch for selection of new survey(s) 
  observeEvent(input$taxonInput, { 

# Modify existing map
      leafletProxy("map") %>%

# Remove any previous selections 
      clearGroup("myMarkers") %>%

# Highlight new selected surveys
      addMarkers(data = points[points$validname == input$taxonInput, ],
                 ~Longitude,
                 ~Latitude,
                 group = "myMarkers")
  })
  
#__________________________________________________________________________________________ 
#### REACTIVE OBJECT: SAMPLE METADATA FOR SELECTED SURVEY(S) ####  
# Reactive object for sample metadata (selected survey(s))
    coord <- reactive({
    coord2 <- subset(data, validname == input$taxonInput)
        coord3 <- unique(coord2[,c(1:6,12)]) #SurveyName, SampleCode, Latitude, Longitude, GearName  
    return(coord3)
  })

#__________________________________________________________________________________________ 
#### OUTPUT  METADATA TABLE FOR SELECTED SURVEY(S) ####  
 
  output$coordinates <- DT::renderDataTable({
    coord()[,1:6] # Reactive metadata object
  })
  
#__________________________________________________________________________________________ 
  #### REACTIVE OBJECT: SAMPLE METADATA AND FAUNAL ABUNDANCES FOR SELECTED SURVEY(S) ####
  
  data2 <- reactive({
    data2 <- subset(data, validname == input$taxonInput)
  
    # names(data)
   # 1:"SurveyName"
   # 2:"SampleCode"
   # 3:"Latitude"
   # 4:"Longitude"
   # 5:"GearName"
   # 6:"Date"
   # 7:"MacroSieveSize_mm"
   # 8:"worrmstaxa_taxonname"
   # 9:"taxaqual_qualifier"
   # 10:"validname"
   # 11: "Rank"
   # 12:"TaxonQualifier"
   # 13:"AphiaID"
   # 14:"Abund"
   # 15:"datapubliclyavailable"
   # 16:"SampleCode2"
    #17:"DataOwner"
    #18:"WaterDepth"
    #19:"SampleSize" 

    
    data3 <- data2[,c(1,2,16,3,4,18,6,5,19,7,10,13,11,12,14,15,17)]
    
    return(data3)
    
  })
  
  #__________________________________________________________________________________________ 
  #### REACTIVE OBJECT FOR PUBLICLY AVAILABLE FAUNAL DATA ####
  data4t <- reactive({
    
    data4 <- data2() %>% filter(datapubliclyavailable == TRUE)
    #colnames data2:
    #1: SurveyName
    #2:SampleCode
    #3:SampleCode2
    #4:Latitude
    #5:Longitude
    #6:WaterDepth
    #7:Date
    #8:GearName
    #9:SampleSize
    #10:MacroSieveSize_mm
    #11:validname(WoRMS)
    #12:AphiaID
    #13:Rank
    #14:TaxonQualifier
    #15:Abund
    #16:datapubliclyavailable
    #17:DataOwner
    data5 <- data4[,c(1:15,17)] # drop datapubliclyavailable
    print(data5)
    #browser()
  })
  
  #__________________________________________________________________________________________ 
  #### REACTIVE OBJECT FOR NON PUBLICLY AVAILABLE FAUNAL DATA ####
  data4f <- reactive({
    
    data6 <- data2() %>% filter(datapubliclyavailable == FALSE)
    #names(data2)
    #1: SurveyName
    #2:SampleCode
    #3:SampleCode2
    #4:Latitude
    #5:Longitude
    #6:WaterDepth
    #7:Date
    #8:GearName
    #9:SampleSize
    #10:MacroSieveSize_mm
    #11:validname(WoRMS)
    #12:AphiaID
    #13:Rank
    #14:TaxonQualifier
    #15:Abund
    #16:datapubliclyavailable
    #17:DataOwner
    data7 <- unique(data6[,c(1:10,17)]) #i.e. drop faunal data and include only sample metadata
    print(data7)
    
  })
  
  #__________________________________________________________________________________________
  #### DOWNLOAD FAUNAL DATA FOR SELECTED SURVEY(S) ####  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("download",".csv",sep="")#data2-",Sys.Date(),
    },
    content = function(file) {
      
      # Join 2 reactive objects together. Using bind_rows which assigns "NA" to missing columns
      faunadownload <- dplyr::bind_rows(data4t(), data4f())
      
      # Replace NAs with 'data witheld'
      faunadownload[,11:15][is.na(faunadownload[,11:15])] <- "data witheld"     
      
      write.csv(faunadownload,file,row.names = F)
    })
  
  #__________________________________________________________________________________________  
###################
  ####################
  ####################
  #__________________________________________________________________________________________  
  #### REACTIVE OBJECT: SAMPLE METADATA AND PSA PERCENTAGES FOR SELECTED SURVEY(S) ####
  
  psadata2 <- reactive({
    #psadata2 <- subset(psadata, SampleCode == Data2()$SampleCode)
    psadata2 <-psadata%>%filter(SampleCode%in%data2()$SampleCode)
    #names(psadata2)
    #1:"SurveyName"
    #2:"SampleCode"
    #3:"Latitude"
    #4:"Longitude"
    #5:"GearName"
    #6:"Date"
    #7:"SieveSize_mm"
    #8:"Percentage"
    #9:"datapubliclyavailable"
    #10:"SampleCode2"
    #11:"DataOwner"                 
    #12:WaterDepth
    #13:SampleSize
    
    psadata3 <- psadata2[,c(1,2,10,3,4,12,6,5,13,7,8,9,11)]
    return(psadata3)
    
  })
  
  #__________________________________________________________________________________________ 
  #### REACTIVE OBJECT FOR PUBLICLY AVAILABLE PSA DATA ####
  psadata4t <- reactive({
    
    psadata4 <- psadata2() %>% filter(datapubliclyavailable == TRUE)
    #names(psadata2())
    #1:"SurveyName"
    #2:"SampleCode"
    #3:"SampleCode2"
    #4:"Latitude"
    #5:"Longitude"
    #6:WaterDepth
    #7:"Date"
    #8:"GearName"
    #9:SampleSize
    #10:"SieveSize_mm"
    #11:"Percentage"
    #12:"datapubliclyavailable"
    #13:"DataOwner"                 
    
    psadata5 <- psadata4[,c(1:11,13)] # drop datapubliclyavailable
    print(psadata5)
    #browser()
  })
  
  #__________________________________________________________________________________________ 
  #### REACTIVE OBJECT FOR NON PUBLICLY AVAILABLE PSA DATA ####
  psadata4f <- reactive({
    
    psadata6 <- psadata2() %>% filter(datapubliclyavailable == FALSE)
    
    #names(psadata2())
    #1:"SurveyName"
    #2:"SampleCode"
    #3:"SampleCode2"
    #4:"Latitude"
    #5:"Longitude"
    #6:WaterDepth
    #7:"Date"
    #8:"GearName"
    #9:SampleSize
    #10:"SieveSize_mm"
    #11:"Percentage"
    #12:"datapubliclyavailable"
    #13:"DataOwner"
    psadata7 <- unique(psadata6[,c(1:9,13)]) #i.e. drop psa data and include only sample metadata
    print(psadata7)
    
  })
  
  #__________________________________________________________________________________________
  #### DOWNLOAD PSA DATA FOR SELECTED SURVEY(S) ####  
  output$downloadpsaData <- downloadHandler(
    filename = function() {
      paste("download",".csv",sep="")#data2-",Sys.Date(),
    },
    content = function(file) {
      
      # Join 2 reactive objects together. Using bind_rows which assigns "NA" to missing columns
      psadownload <- dplyr::bind_rows(psadata4t(), psadata4f())
      
      # Replace NAs with 'data witheld'
      psadownload[,10:11][is.na(psadownload[,10:11])] <- "data witheld"     
      
      write.csv(psadownload,file,row.names = F)
    })
  
  #__________________________________________________________________________________________
  
  #######################
  #####################
  #######################
  #__________________________________________________________________________________________ 
  #### REACTIVE OBJECT FOR phy variables ####
  vardata <- reactive({
    
    
      vardata1 <- subset(sdata2, validname == input$taxonInput)
      vardata2 <- vardata1[,c(25,10)]
   
      vardata3 <- data.frame(vardata2%>%group_by(phyclus)%>%count(phyclus))
      vardata4 <- full_join(sdata4,vardata3,by = "phyclus", copy = FALSE, suffix = c(".all", ".taxa"))
      vardata4[, 4][is.na(vardata4[, 4])] <- 0
      colnames(vardata4) <- c("phyclus","ntotal","hex","n")
      vardata4$perc <- (vardata4$n/vardata4$ntotal)*100
      vardata4$phyclus <- as.factor(vardata4$phyclus)
      vardata4$hex <- as.factor(vardata4$hex)
      return(vardata4)
   
  })
 
  ###########################################################
  ## Call library 'reshape2' - required for melting data 
  
  
  ## Melt data into suitable form for facetting 

  ## Plotting

  
  ####################################
  

    
    output$perc <- DT::renderDataTable({
      vardata() # Reactive metadata object
 
  })
  
  
  output$percplot <- renderPlot({

    
    #vardata4$hex <- as.factor(vardata4$hex)
    
    #write.table(vardata4, file = "C:/Users/kmc00/OneDrive - CEFAS/R_PROJECTS/OneBenthicTaxaSearchTool/vardata4v2.csv")
    #browser() 
    #ggplot(vardata(),aes(x=phyclus,y=perc, col=hex))+#fill=phyclus
      ggplot(vardata(),aes(x=phyclus,y=perc, fill=hex))+#fill=phyclus
      geom_bar(stat="identity")+ 
      #scale_color_manual(values=c("#F8766D","#DE8C00","#B79F00","#7CAE00","#00BA38","#00C08B","#00BFC4","#00B4F0","#619CFF","#C77CFF","#F564E3","#FF64B0"))+
      scale_fill_manual(values = levels(vardata()$hex),na.value = "black")+
      geom_text(aes(label=ntotal), vjust=-0.25)+
      #scale_fill_identity() 
    #scale_fill_discrete(drop=TRUE,limits = levels(vardata()$hex))+
      guides(fill=FALSE)+
      labs(x="Habitat (Phy Cluster)",y="%")+
      theme_bw(base_size=12)
      #ggtitle("Percentage of samples where selected taxon is present")
    
  })
  #__________________________________________________________________________________________
}
shinyApp(ui = ui, server = server)