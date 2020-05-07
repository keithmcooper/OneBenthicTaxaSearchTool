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

#__________________________________________________________________________________________
#### GET POINT SAMPLE DATA (META & FAUNA) ####

## create a connection to OneBenthic Live. Save the password
pw <- {
  "0neBenth!c5374"
}
logged= FALSE;

## Load PostgreSQL driver
drv <- dbDriver("Postgres")

## Create connection to the Postgres database. Note that "con" will be used later in each connection to the database
con =  dbConnect(drv, dbname = "one_benthic",
                 host = "azsclnxgis-ext01.postgres.database.azure.com",
                 port = 5432,
                 user = "editors_one_benthic@azsclnxgis-ext01 ",
                 password = pw)

rm(pw) # remove the password

## Get data
data = dbGetQuery(con,
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
a.worrms_validaphiaid,
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
INNER JOIN faunal_data.worrmstaxa as wt ON wt.taxonname = ts.worrmstaxa_taxonname 
INNER JOIN faunal_data.aphia as a ON wt.aphia_aphiaid = a.aphiaid 
INNER JOIN faunal_data.worrms as w ON w.validaphiaid = a.worrms_validaphiaid
INNER JOIN associations.sampleowner as so ON so.sample_samplecode = s.samplecode
INNER JOIN associations.owner as o ON so.owner_ownername = o.ownername
ORDER by su.surveyname;
")
#WHERE w.validname = 'Crepidula fornicata'
## Check results of query 


names(data)
dim(data)
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
#[1] "SurveyName"            "SampleCode"            "Latitude"              "Longitude"             "GearName"              "Date"                  "MacroSieveSize_mm"    
#[8] "worrmstaxa_taxonname"  "taxaqual_qualifier"    "validname"        "Rank"                  "TaxonQualifier"        "AphiaID"               "Abund"                
#[15] "datapubliclyavailable" "SampleCode2"          

## Create an object 'points' for sample positions
points <- unique(data[,c(4:3,1,2,10)])
#View(points)
head(points)
dim(points)
#plot(points$Longitude,points$Latitude)

points2 <- unique(data[,c(4:3,1,2)])
#__________________________________________________________________________________________
###############################
###############################
##############################
#__________________________________________________________________________________________
#### GET POINT SAMPLE DATA (META & PSA) ####

## Get data
psadata = dbGetQuery(con,
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
# [1] "SurveyName"            "SampleCode"            "Latitude"              "Longitude"             "GearName"              "Date"                  "SieveSize_mm"         
# [8] "Percentage"            "datapubliclyavailable" "SampleCode2"           "DataOwner"
#__________________________________________________________________________________________


#################################
##################################
##############################
#__________________________________________________________________________________________
#### GET SPATIAL LAYERS FROM OneBenthic - WHERE NO API AVAILABLE (MPAS, O&G, DISP) ####

mcz <-  st_read(con, query = "SELECT * FROM spatial.c20190905_offshorempas_wgs84 WHERE site_statu = 'MCZ - Secretary of State';")
sac <-  st_read(con, query = "SELECT * FROM spatial.c20190905_offshorempas_wgs84 WHERE site_statu = 'SAC'or site_statu = 'cSAC';")
ncmpa <-  st_read(con, query = "SELECT * FROM spatial.c20190905_offshorempas_wgs84 WHERE site_statu = 'NCMPA';")
oga <- st_read(con, query = "SELECT * FROM spatial.oga_licences_wgs84;")
disp  <-  st_read(con, query = "SELECT * FROM spatial.disposalSiteJan2020;")
agg <- st_read(con, query = "SELECT * FROM ap_marine_aggregate.extraction_areas;")
owf <- st_read(con, query = "SELECT * FROM spatial.offshore_wind_site_agreements_england_wales__ni__the_crown_esta;")
owf_cab <- st_read(con, query = "SELECT * FROM spatial.offshore_wind_cable_agreements_england_wales__ni_the_crown_esta;")
wave <- st_read(con, query = "SELECT * FROM spatial.offshore_wave_site_agreements_england_wales__ni_the_crown_estat;")
wave_cab <- st_read(con, query = "SELECT * FROM spatial.offshore_wave_cable_agreements_england_wales__ni_the_crown_esta;")
tidal <- st_read(con, query = "SELECT * FROM spatial.offshore_tidal_stream_site_agreements_england_wales__ni_the_cro;")
tidal_cab <- st_read(con, query = "SELECT * FROM spatial.offshore_tidal_stream_cable_agreements_england_wales__ni_the_cr;")
R4_chara <- st_read(con, query = "SELECT * FROM spatial.offshore_wind_leasing_round_4_characterisation_areas_england_wa;")
R4_bid <- st_read(con, query = "SELECT * FROM spatial.offshore_wind_leasing_round_4_bidding_areas_england_wales_and_n;")

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

#__________________________________________________________________________________________

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
#################################################
###############################################
## Load libraries
#library(ggplot2)
library(rgdal)
#library(maptools)
library(plyr)
#library(dismo)
library(raster)



## Set working directory
setwd('C:/Users/kmc00/OneDrive - CEFAS/R_PROJECTS/OneBenthicTaxaSearchTool/R')

## Identify raster files
list <- list.files(path='DATA/FINAL_RASTERS',pattern=".tif", full.names=TRUE)
list

## Create raster stack
predictors <- stack(list)
predictors

## Simple names for predictor variables
names(predictors)=c("AvCur","Chla","Depth","Gravel","Mud","Sal","Sand","SPM","Stress","Temp","WOV")#"Sand",
names(predictors)

## Plot raster stack
#plot(predictors)

#### 20. RANDOM FOREST (BIO): EXTRACT PREDICTOR VARIABLES FROM RASTER STACK ####

#head(FaunalCluster)

## Create a df for predictor variables
sdata <- extract(predictors, data[,4:3])

#View(sdata)
class(sdata)

##Change from matrix to df
sdata=as.data.frame(sdata)
## Bind togeter data and extracted variables (sdata)
#View(sdata)
sdata2 <- cbind(data,sdata)
View(sdata2)
str(sdata2)
names(sdata2)

#################################################
## Create a df 'bp2' with only the phy variables and taxon 
sdata3=sdata2[,c(25:35,10)]


## Call library 'reshape2' - required for melting data 
library(reshape2) 

## Melt data into suitable form for facetting 
sdata4 <- melt(sdata3,"validname") 
# Produces a df of 3 cols (PhyCluster, variable, value)
sdata5 <- sdata4[which(sdata4$validname =="Sabellaria spinulosa"),]
sdata6 <- sdata5[,2:3]
View(sdata6)
## Plotting
ggplot(sdata6, aes(x=value)) + 
  geom_histogram(binwidth=0.5)+
  facet_wrap(~variable,scales="free")
###############################################
#__________________________________________________________________________________________
#### USER INTERFACE ####

ui <- fluidPage(
 titlePanel(title=div(img(src="onebenthic.GIF",tags$b(" OneBenthic"),"Taxa Search Tool",height = 65, width = 170),#HEIGHT65
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
#### TAB: ABOUT  ####
             tabPanel("About",h4("App purpose"),"
To allow users to explore and download macrofaunal and sediment particle size data (grabs/cores) from the",tags$b("OneBenthic")," database.
                      
                      Select by",tags$b("survey")," (drop-down list), or",tags$b("sample"),"(map drawing tool). Selected samples are shown in a table (see appropriate tab), with data available for csv download. Where data providers withold permission, only sample metadata will be output. Activity layers for aggregates, owf, tidal and wave come from the Crown Estate's",tags$a(href="https://opendata-thecrownestate.opendata.arcgis.com/", "Open Data Portal."), "Activity layers for oil and gas come from the Oil and Gas Authority's (OGA) ",tags$a(href="https://data-ogauthority.opendata.arcgis.com/", "Open Data Portal."), "Disposal sites (disp) come from the  ",tags$a(href="http://mdrviewer/#/View/407", "Cefas Data Hub.")," Marine Protected Area layers (mcz, sac and ncmpa) come from the JNCC's Offshore MPAs Shapefile avaialble from their",tags$a(href="http://archive.jncc.gov.uk/page-4661", "website."),br(),br(),
                      
                      h4("What is OneBenthic?"),

                      "Large quantities of benthic data (macrofauna and sediment particle size) are now in the public domain, but are distriburted across multiple repositories. ", "The purpose of ",tags$b("OneBenthic,"),"a PostgreSQL database, is to bring these disperate data together in one place, thereby aiding research and facilitating reuse of data.Taxon names are standardized according to the", tags$a(href="http://www.marinespecies.org/", "World Register of Marine Species.")," Sediment data are percentage weight by sieve size (mm).",
                      
                      "Many of the current key marine environmental issues (e.g. conservation of biodiversity, habitat mapping, marine spatial planning, effects of climate change, cumulative effects) require access to large datasets, and the database meets this need.",
                      
                       tags$b("OneBenthic")," links directly to a range of other apps providing useable information back to data providers (and the wider public). These apps, available from",tags$a(href="https://openscience.cefas.co.uk/", "CefasOpenScience,")," include:",br(),br(),
                      tags$a(href="https://openscience.cefas.co.uk/ma_tool/", "1. Baseline Tool"),br(),
                      tags$a(href="https://openscience.cefas.co.uk/ma_tool/", "2. M-Test Tool"),br(),
                      tags$a(href="https://openscience.cefas.co.uk/invasive_species/", "3. Non-native Species Tool"),br(),
                      tags$a(href="https://openscience.cefas.co.uk/faunal_cluster_id/", "4. Faunal Cluster ID Tool"),br(),br(),

                      "These tools are actively being used by offshore marine industries and government for purposes of project planning, licence compliance monitoring and research.",br(),br(),
h4("Origin of data"),"Data originate from multiple sources and providers, and were brought together in a study",tags$a(href="http://rdcu.be/wi6C", "(Cooper and Barry, 2017)")," jointly funded by the Department for Food and Rural Affairs, the Marine Management Organisation, the British Marine Aggregate Producers Association, the Crown Estate and the Welsh Government. Data from this study were",tags$a(href="https://doi.org/10.14466/CefasDataHub.34", "published"), "(in csv format) with the kind permission of data providers. The current app provides access to these data is a more user friendly way. Subject to funding, new publicly available data will continue to be added to",tags$b("OneBenthic")," so that it continues to provide an up-to-date and 'complete' source of UK benthic data.",br(),br(),

h4("Data access and use"),tags$b("OneBenthic"),"contains data from a range of data providers in industry and government. With the permission of these providers, data are made available under Crown Copyright and", tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", "Open Government Licence."),"
Please cite the database as follows: ",br(),tags$b("OneBenthic")," database (2020). Available from https://openscience.cefas.co.uk/OneBenthicExtraction/. Accessed: DD/MM/YYYY.",br(),br(),
h4("Contact"),"For technical and data issues email keith.cooper@cefas.co.uk",
style = 'font-size:90%'),

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
tabPanel("Funders",(img(src="logos.png",height = 400, width = 800))),
                    tabPanel(
                      "variables",fluidRow(
                                           column(12,plotOutput(outputId="depthPlot", height="800px"))#,
                                          # column(3,plotOutput(outputId="tempPlot")),
                                          # column(3,plotOutput(outputId="gravelPlot")),
                                          # column(3,plotOutput(outputId="SandPlot")),
                                          # column(3,plotOutput(outputId="mudPlot")),
                                          # column(3,plotOutput(outputId="SPMPlot")),
                                          # column(3,plotOutput(outputId="salPlot")),
                                          # column(3,plotOutput(outputId="stressPlot")),
                                          # column(3,plotOutput(outputId="WOVPlot")),
                                          # column(3,plotOutput(outputId="ChlaPlot"))
                         #column(3,div(style="height=100px"),plotOutput(outputId="depthPlot"),plotOutput(outputId="tempPlot"),plotOutput(outputId="gravelPlot")),
            
                        
                         #column(3,div(style="height=100px"),plotOutput(outputId="SandPlot"),plotOutput(outputId="mudPlot"),plotOutput(outputId="SPMPlot")),

                         #column(3,div(style="height=100px"),plotOutput(outputId="salPlot"),plotOutput(outputId="stressPlot"),plotOutput(outputId="WOVPlot")),
                        # column(3,plotOutput(outputId="salPlot"),plotOutput(outputId="stressPlot"),plotOutput(outputId="WOVPlot")),
                         #column(3,div(style="height=100px"),plotOutput(outputId="ChlaPlot"))
    )
  )
)
)))

#__________________________________________________________________________________________
#### SERVER FUNCTION ####
server <- function(input, output) {
#__________________________________________________________________________________________
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
      addProviderTiles(providers$Esri.OceanBasemap,options = providerTileOptions(noWrap = TRUE))%>%
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
      addLayersControl(overlayGroups = c("owf","owf_cab","R4_chara","R4_bid","agg","disp","wave","wave_cab","tidal","tidal_cab","oga","mcz","sac","ncmpa"),options = layersControlOptions(collapsed = FALSE))%>%hideGroup(c("owf","owf_cab","R4_chara","R4_bid","agg","disp","wave","wave_cab","tidal","tidal_cab","oga","mcz","sac","ncmpa"))%>%
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
      vardata2=vardata1[,c(25:35,10)]
      library(reshape2) 
      vardata3 <- melt(vardata2,"validname")
      vardata4 <- vardata3[complete.cases(vardata3),]
      return(vardata4)
   
  })

  ###########################################################
  ## Call library 'reshape2' - required for melting data 
  
  
  ## Melt data into suitable form for facetting 

  ## Plotting

  
  ####################################
  output$depthPlot <- renderPlot({
    ggplot(vardata(), aes(x=value)) + 
    geom_histogram(binwidth=0.5)+
    geom_density(alpha=0.2,fill="#FF6666")+
      geom_vline(aes(xintercept=mean(value)),
                 color="blue", linetype="dashed", size=1)+  
    facet_wrap(~variable,scales="free")+
      
      theme_classic(base_size = 12)#x <- vardata()$Depth

  })
  
  #__________________________________________________________________________________________
}
shinyApp(ui = ui, server = server)