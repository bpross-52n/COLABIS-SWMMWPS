#library(devtools)
#install_github("GeoinformationSystems/xtruso_R")



#install.packages("RCurl")
library(RCurl)

library(XML)


url <- "http://colabis.dev.52north.org/wps/WebProcessingService"

# REQUEST MIT DEFAULT-WERTEN AUS WEB-CLIENT
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
<wps:Reference xlink:href="http://geoprocessing.demo.52north.org:8080/data/sample-points-wgs84.zip" mimeType="application/x-zipped-shp">
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
<wps:LiteralData>16</wps:LiteralData>
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
# Regenwerte = 0 ???



# INPUT ALS LOKALE REFERENZ ???
# file://D:/BueRO/SHK_TUD/COLABIS/WPS-R_XML/sample-points-wgs84.zip im Web-Client
# -> org.n52.wps.server.ExceptionReport: Error occured while receiving the complexReferenceURL: inputID: features | dataURL: file://D:/BueRO/SHK_TUD/COLABIS/WPS-R_XML/sample-points-wgs84.zip

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