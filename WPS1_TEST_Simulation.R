#library(devtools)
#install_github("GeoinformationSystems/xtruso_R")



#install.packages("RCurl")
library(RCurl)

#library(XML)
library(swmmr)


url <- "http://colabis.dev.52north.org/wps/WebProcessingService"

# REQUEST MIT DEFAULT-WERTEN AUS WEB-CLIENT
# http://geoprocessing.demo.52north.org:8080/data/sample-points-wgs84.zip

# REQUEST MIT ESCHDORF-SHAPEFILE (https://colabis.de/data/COLABIS_eschdorf_4326.zip)
xml_request <- '<?xml version="1.0" encoding="UTF-8"?>
<wps:Execute service="WPS" version="1.0.0" mode="sync"
xmlns:wps="http://www.opengis.net/wps/1.0.0" 
xmlns:ows="http://www.opengis.net/ows/1.1" 
xmlns:xlink="http://www.w3.org/1999/xlink" 
xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
xsi:schemaLocation="http://www.opengis.net/wps/1.0.0 
http://schemas.opengis.net/wps/1.0.0/wpsExecute_request.xsd">
<ows:Identifier>org.n52.wps.server.r.colabis.dwd.radar.data.process</ows:Identifier>

<wps:DataInputs>
<wps:Input>
<ows:Identifier>features</ows:Identifier>
<wps:Reference xlink:href="https://colabis.de/data/COLABIS_eschdorf_4326.zip" mimeType="application/x-zipped-shp">
</wps:Reference>
</wps:Input>

<wps:Input>
<ows:Identifier>product</ows:Identifier>
<wps:Data>
<wps:LiteralData>RX</wps:LiteralData>
</wps:Data>
</wps:Input>

<wps:Input>
<ows:Identifier>maxNumberOfDatasets</ows:Identifier>
<wps:Data>
<wps:LiteralData>12</wps:LiteralData>
</wps:Data>
</wps:Input>

</wps:DataInputs>

<wps:ResponseForm>
		<wps:RawDataOutput>
<ows:Identifier>result</ows:Identifier>
<ows:Identifier>sessionInfo</ows:Identifier>
<ows:Identifier>warnings</ows:Identifier>
</wps:RawDataOutput>
</wps:ResponseForm>

</wps:Execute>'


myheader <- c(Connection="close",
              'Content-Type' = "application/xml",
              'Content-length' = nchar(xml_request))

data <- getURL(url = url,
               postfields=xml_request,
               httpheader=myheader,
               verbose=TRUE)

t <- gsub('\"','',data)
t2 <- unlist(strsplit(t,'\n'))

# FORMAT ANPASSEN
# Format RawDataOutput: ",x,y,value,timeStamp"
# Format timeStamp: YYMMDDhhmm
# Format Regendatei: N_ED	2005 02 02 10 15	0.1
# -> SWMM-Manual: each line of the file contains the station ID, year, month,  day,  hour,  minute,  and  non-zero  precipitation  reading,  all  separated  by  one  or more spaces
rain_data <- matrix(nrow=(length(t2)-1), ncol=7)
rain_data[,1] <- "N_ED   "
for(i in 2:length(t2)){
  # Datum/Zeit in GMT (+2h = CEST) -> anpassen ???
  rain_data[(i-1),2] <- as.character(as.numeric(substr(unlist(strsplit(t2[i],','))[5],1,2))+2000) # year
  rain_data[(i-1),3] <- substr(unlist(strsplit(t2[i],','))[5],3,4) # month
  rain_data[(i-1),4] <- substr(unlist(strsplit(t2[i],','))[5],5,6) # day
  rain_data[(i-1),5] <- substr(unlist(strsplit(t2[i],','))[5],7,8) # hour
  rain_data[(i-1),6] <- paste0(substr(unlist(strsplit(t2[i],','))[5],9,10),"   ") # minute
  rain_data[(i-1),7] <- unlist(strsplit(t2[i],','))[4] # rain value
}


# RADOLAN: Ausgabe (Datum/Zeit) in GMT -> umrechnen in CEST (+2h)
ISOdatetime(year=as.numeric(rain_data[1,2]),
            month=as.numeric(rain_data[1,3]), day=as.numeric(rain_data[1,4]),
            hour=as.numeric(rain_data[1,5]), min=as.numeric(rain_data[1,6]), sec=0) + 2*60*60


# Regenwerte = 0 ??? -> nicht erlaubt in SWMM-rainfile
#rain_data[,7] <- 0.1
rain_data <- rain_data[-which(rain_data[,7]==0),]
rain_file <- paste0("rain_", paste(rain_data[1,2:6], collapse="-"), "_",
                    paste(rain_data[nrow(rain_data),2:6], collapse="-"), ".dat")
rain_file <- "test_rain.dat"
write.table(rain_data, rain_file,
            row.names=F, col.names=F, quote=F)


# bei mehreren Stationen muss Stationsname (erste Spalte) angepasst werden!


# Regendatei im .inp-File aendern (entsprechend Dateiname oben)
raw_inp <- readLines("eschdorf_v6_20141208.inp")
p1 <- "FILE       \".*\"       N_ED"
r1 <- paste0("FILE       \"", rain_file, "\"       N_ED")
#r1 <- paste0("FILE       \"", "ED_N_test.dat", "\"       N_ED")
n1 <- gsub(pattern = p1, replace = r1, x = raw_inp)


# Start-/Endzeit fuer Simulation im .inp-File aendern
start_date_sim <- paste(rain_data[1,4:2], collapse="/")
p2 <- "START_DATE           .*"
r2 <- paste0("START_DATE           ", start_date_sim)
n2 <- gsub(pattern = p2, replace = r2, x = n1)

start_time_sim <- trimws(paste(rain_data[1,5:6], collapse=":"), "right")
p3 <- "START_TIME           .*"
r3 <- paste0("START_TIME           ", start_time_sim)
n3 <- gsub(pattern = p3, replace = r3, x = n2)

end_date_sim <- paste(rain_data[nrow(rain_data),4:2], collapse="/")
p4 <- "END_DATE             .*"
r4 <- paste0("END_DATE             ", end_date_sim)
n4 <- gsub(pattern = p4, replace = r4, x = n3)

end_time_sim <- trimws(paste(rain_data[nrow(rain_data),5:6], collapse=":"), "right")
p5 <- "END_TIME             .*"
r5 <- paste0("END_TIME             ", end_time_sim)
n5 <- gsub(pattern = p5, replace = r5, x = n4)


# neue .inp-Datei speichern
writeLines(n5, "SWMM_project_new.inp")



# SWMM aufrufen, .out-Datei erstellen
setwd(getwd())
binary <- run_swmm(inp = "SWMM_project_new.inp")
# exec = ... -> default: "C:/Program Files (x86)/EPA SWMM 5.1/swmm5.exe"








# INPUT ALS LOKALE REFERENZ ???
# file://D:/BueRO/SHK_TUD/COLABIS/WPS-R_XML/sample-points-wgs84.zip im Web-Client
# oder file://localhost/... bzw. file://localhost:8080/...
# -> org.n52.wps.server.ExceptionReport: Error occured while receiving the complexReferenceURL: inputID: features | dataURL: file://D:/BueRO/SHK_TUD/COLABIS/WPS-R_XML/sample-points-wgs84.zip
# http://localhost:8080/wps/R/resources/SWMM/sample-points-wgs84.zip
# -> java.lang.IndexOutOfBoundsException: Index: 0, Size: 0
# Zugriff auf lokale Dateien mit javascript nicht möglich (?)


# INPUT ALS LITERAL ???
# -> xml/gml erstellen...


# ANDERE STATION (Mittelpunkt der Bounding-Box [MAP] aus .inp-Datei)
# TEST: Koordinaten der Eschdorf-Station
# R: 5425083.085, H: 5657409.207
# in Dezimalgrad (WGS84): 51.046984, 13.929728











# TEST - STATUS REQUEST ----------------------------------------------------
# status <- "http://colabis.dev.52north.org:80/wps/RetrieveResultServlet?id=b9e1b0a5-1c88-4fad-ad00-c2254369a3a3"
# 
# status_req <- '<wps:GetStatus service=“WPS” version=“2.0.0”
# xmlns:wps=“http://www.opengis.net/wps/2.0”
# xmlns:xsi=“http://www.w3.org/2001/XMLSchema-instance”
# xsi:schemaLocation="http://www.opengis.net/wps/1.0.0 
# http://schemas.opengis.net/wps/1.0.0/wpsExecute_request.xsd">
#   <wps:JobID>b9e1b0a5-1c88-4fad-ad00-c2254369a3a3</wps:JobID>
#   </wps:GetStatus>'
# 
# myheader2 <- c(Connection="close",
#                'Content-Type' = "application/xml",
#                'Content-length' = nchar(status_req))
# data2 <- getURL(url = status,
#                 postfields = status_req,
#                 httpheader = myheader2,
#                 verbose = TRUE)
# (xmltext <- xmlTreeParse(data2, asText = TRUE, useInternalNodes=TRUE))
# 
# 
# 
# # TEST - RCURL ----------------------------------------------------
# h <- curl::new_handle()
# curl::handle_setopt(h, copypostfields = "xml=xml_request")
# curl::handle_setopt(h, copypostfields = "xml=status_req")
# curl::handle_setheaders(h,
#                         'Content-Type' = "text/xml")
# req <- curl::curl_fetch_memory(url = url, handle = h)
# req <- curl::curl_fetch_memory(url = status, handle = h)
# cat(rawToChar(req$content))