#library(devtools)
#install_github("GeoinformationSystems/xtruso_R")

#install.packages("RCurl")
library(RCurl)

library(XML)


url <- "http://colabis.dev.52north.org/wps/WebProcessingService"

xml_request <- '<?xml version="1.0" encoding="UTF-8"?>
<wps:Execute service="WPS" version="1.0.0" 
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
<wps:Data>
<wps:Reference xlink:href="http://geoprocessing.demo.52north.org:8080/data/sample-points-wgs84.zip" mimeType="application/x-zipped-shp">
</wps:Reference>
</wps:Data>
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
<wps:LiteralData>20</wps:LiteralData>
</wps:Data>
</wps:Input>

</wps:DataInputs>

<wps:ResponseForm>
<wps:ResponseDocument storeExecuteResponse="true">
<wps:Output asReference="false">
<ows:Identifier>result</ows:Identifier>
</wps:Output>
<wps:Output asReference="false">
<ows:Identifier>sessionInfo</ows:Identifier>
</wps:Output>
<wps:Output asReference="false">
<ows:Identifier>warnings</ows:Identifier>
</wps:Output>
</wps:ResponseDocument>
</wps:ResponseForm>

</wps:Execute>'


myheader <- c(Connection="close",
              'Content-Type' = "application/xml",
              'Content-length' = nchar(xml_request))

data <- getURL(url = url,
               postfields=xml_request,
               httpheader=myheader,
               verbose=TRUE)
xmltext <- xmlTreeParse(data, asText = TRUE, useInternalNodes=TRUE)



h <- curl::new_handle()
curl::handle_setopt(h, copypostfields = "xml=xml_request")
curl::handle_setheaders(h,
                        'Content-Type' = "text/xml")
req <- curl::curl_fetch_memory(url = url, handle = h)
cat(rawToChar(req$content))




