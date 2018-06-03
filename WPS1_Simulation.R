# wps.des: id = COLABIS_WPS1_Simulation, title = SWMM-Simulation mit aktuellen Regendaten,
# abstract = Regendaten der vergangenen Stunde/n anfordern von DWD-Radar-Process-Service und SWMM-Simulation mit angepasstem inp-File ausführen;


# wps.res: SWMM/SWMM_project_original.inp;


# wps.in: id = inp, type = string,
# title = Name des inp-File ohne Dateiendung,
# abstract = Ausgangsprojekt fuer SWMM-Simulation mit/ohne Strassenreinigung;

# wps.in: id = time, type = integer,
# title = Simulationszeitraum in (ganzen) Stunden,
# abstract = Simulationsende ist Aufrufzeitpunkt des WPS / Simulationsstart ist Aufrufzeitpunkt minus angegebene Stundenzahl / default-Wert ist 5h / Zeitzone ist GMT, value = 5;


# Variablen fuer Testlauf in RStudio
# wps.off;
  inp <- "eschdorf_v6_20141208"
  time <- 5
# wps.on;



# Anzupassende Variablen (siehe Doku) ---------------------------------------------
# SWMM-Installationspfad
swmm_path <- "C:/Program Files (x86)/EPA SWMM 5.1/swmm5.exe"
# Pfad fuer Output-File (.out) -> wird von WPS2 angesteuert zum Auslesen
out_path <- "D:/BueRO/SHK_TUD/COLABIS/Outputs_Test"
# Shapefile mit Position der Regen-Station (rain gauge) im SWMM-Projekt
station <- "https://colabis.de/data/COLABIS_eschdorf_4326.zip"
# Inhalt der Fehler-Datei, wenn keine Simulation durchgefuehrt wurde
message <- "No simulation possible. No rain values for required time period available or required period too long (time-out error)."
# ---------------------------------------------------------------------------------



# Bibliotheken
library(RCurl)
library(swmmr)


# Regendaten abrufen
datasets <- time * (60/5) # Anzahl der abgefragten Regenwerte (5-Minuten-Intervall)
url <- "http://colabis.dev.52north.org/wps/WebProcessingService"
xml_request <- paste0('<?xml version="1.0" encoding="UTF-8"?>
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
<wps:Reference xlink:href="',station,'" mimeType="application/x-zipped-shp">
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
<wps:LiteralData>',datasets,'</wps:LiteralData>
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
</wps:Execute>')
header <- c(Connection="close",
            'Content-Type'="application/xml",
            'Content-length'=nchar(xml_request))
response <- getURL(url=url, postfields=xml_request, httpheader=header,
                   verbose=TRUE)
data <- unlist(strsplit(gsub('\"','',response),'\n'))

# Stationsname aus Input-File auslesen
raw_inp <- readLines(paste0(inp,".inp"))
inp_name <- raw_inp[grep("[RAINGAGES]", raw_inp, fixed=T)
                    :grep("[SUBCATCHMENTS]", raw_inp, fixed=T)]
station_name <- unlist(strsplit(inp_name[4],split=" +"))[7]

# Format anpassen fuer SWMM
rain_data <- matrix(nrow=(length(data)-1), ncol=7)
rain_data[,1] <- station_name # Stationsname
for(i in 2:length(data)){
  # Datum/Zeit in GMT (+2h = CEST)
  rain_data[(i-1),2] <- as.character(as.numeric(substr(unlist(strsplit(data[i],','))[5],1,2))+2000) # Jahr
  rain_data[(i-1),3] <- substr(unlist(strsplit(data[i],','))[5],3,4) # Monat
  rain_data[(i-1),4] <- substr(unlist(strsplit(data[i],','))[5],5,6) # Tag
  rain_data[(i-1),5] <- substr(unlist(strsplit(data[i],','))[5],7,8) # Stunden
  rain_data[(i-1),6] <- substr(unlist(strsplit(data[i],','))[5],9,10) # Minuten
  rain_data[(i-1),7] <- ifelse(unlist(strsplit(data[i],','))[4] == "NA",
                               0, round(as.numeric(unlist(strsplit(data[i],','))[4]),2)) # Regenwert (NA-Werte werden auf 0 gesetzt)
}

# keine 0-Werte
rain_data <- rain_data[-which(rain_data[,7]==0),]

# Simulation wird nur gestartet, wenn es im angegebenen Zeitraum geregnet hat
if(nrow(rain_data)>0){
  
  # Name der Regendatei mit Zeitstempel
  rain_file <- paste0("rain_", trimws(paste(rain_data[1,2:6], collapse="-")), "_",
                      trimws(paste(rain_data[nrow(rain_data),2:6], collapse="-")), ".dat")
  # Regendatei speichern
  write.table(rain_data, rain_file,
              row.names=F, col.names=F, quote=F)
  
  # Regendatei im .inp-File aendern (entsprechend Dateiname oben)
  p1 <- paste0("FILE       \".*\"       ", station_name)
  r1 <- paste0("FILE       \"", rain_file, "\"       ", station_name)
  n1 <- gsub(pattern = p1, replace = r1, x = raw_inp)
  
  
  # Start-/Endzeit fuer Simulation im .inp-File aendern
  start_date_sim <- paste(rain_data[1,c(3,4,2)], collapse="/")
  p2 <- "START_DATE           .*"
  r2 <- paste0("START_DATE           ", start_date_sim)
  n2 <- gsub(pattern = p2, replace = r2, x = n1)
  
  start_time_sim <- trimws(paste(rain_data[1,5:6], collapse=":"), "right")
  p3 <- "START_TIME           .*"
  r3 <- paste0("START_TIME           ", start_time_sim)
  n3 <- gsub(pattern = p3, replace = r3, x = n2)
  
  end_date_sim <- paste(rain_data[nrow(rain_data),c(3,4,2)], collapse="/")
  p4 <- "END_DATE             .*"
  r4 <- paste0("END_DATE             ", end_date_sim)
  n4 <- gsub(pattern = p4, replace = r4, x = n3)
  
  end_time_sim <- trimws(paste(rain_data[nrow(rain_data),5:6], collapse=":"), "right")
  p5 <- "END_TIME             .*"
  r5 <- paste0("END_TIME             ", end_time_sim)
  n5 <- gsub(pattern = p5, replace = r5, x = n4)
  
  
  # neue .inp-Datei speichern
  writeLines(n5, paste0(inp, "_new.inp"))
  
  
  # SWMM aufrufen, .out-Datei erstellen
  setwd(getwd())
  run_swmm(inp = paste0(inp, "_new.inp"), exec=swmm_path)
  out <- paste0(inp, "_new.out")
  
  # .out-Datei in angegebenen Ordner kopieren
  file.copy(paste0(inp, "_new.out"), out_path, overwrite=T)
  
  
} else {
  
  # Datei mit Fehlermeldung schreiben, wenn keine Simulation durchgefuehrt wurde
  write.table(message, "Error.txt", col.names=F, row.names=F, quote=F)
  out <- "Error.txt"
  
}




# wps.out: id = out, type = text,
# title = Simulationsergebnis,
# abstract = Ausgabe der SWMM-Simulation im Binaerformat zur Weiterverarbeitung mit WPS2_Auslesen;