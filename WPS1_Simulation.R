# wps.des: id = COLABIS_WPS1_Simulation, title = SWMM-Simulation mit aktuellen Regendaten,
# abstract = Regendaten der vergangenen Tage anfordern von DWD-Radar-Process-Service und SWMM-Simulation mit angepasstem inp-File ausfuehren / sind im angeforderten Zeitraum keine Regendaten vorhanden oder wirft die Anfrage einen Fehler werden Demodaten verwendet;


# wps.res: SWMM/2daysInt.inp, SWMM/9-15aug.dat;


# wps.in: id = days, type = integer,
# title = Simulationszeitraum in (ganzen) Tagen,
# abstract = Simulationsende ist Aufrufzeitpunkt des WPS / Simulationsstart ist Aufrufzeitpunkt minus angegebener Zeitraum / default-Wert ist 1d / Zeitzone ist GMT, value = 1;

# wps.in: id = sweep, type = integer,
# title = Strassenreinigungs-Intervall (in Tagen),
# abstract = erste Strassenreinigung findet am ersten Simulationstag statt / default-Wert ist 0 (keine Strassenreinigung), value = 0;


# Variablen fuer Testlauf in RStudio
# wps.off;
  days <- 1
  sweep <- 5
# wps.on;



# Anzupassende Variablen (siehe Doku) ---------------------------------------------
# SWMM-Installationspfad
swmm_path <- "C:/Program Files (x86)/EPA SWMM 5.1/swmm5.exe"
# Name des Input-File (.inp) ohne Dateiendung -> im Resources-Ordner
inp <- "2daysInt"
# Pfad fuer Output-File (.out) -> wird von WPS2 angesteuert zum Auslesen
out_path <- "D:/BueRO/SHK_TUD/COLABIS/Outputs_Test"
# Shapefile mit Position der Regen-Station (rain gauge) im SWMM-Projekt
station <- "http://colabis.de/data/rg-wgs84.zip"
# Name der Datei mit Demo-Regen-Werten -> im Resources-Ordner
demo_rain_file <- "9-15aug.dat"
# ---------------------------------------------------------------------------------



# Bibliotheken
library(RCurl)
library(swmmr)


# Anzahl der abgefragten Regenwerte (5-Minuten-Intervall)
datasets <- days * 24 * (60/5)

# Stationsname aus Input-File auslesen
raw_inp <- readLines(paste0(inp,".inp"))
inp_name <- raw_inp[grep("[RAINGAGES]", raw_inp, fixed=T)
                    :grep("[SUBCATCHMENTS]", raw_inp, fixed=T)]
station_name <- unlist(strsplit(inp_name[4],split=" +"))[7]


# Regendaten abrufen (RADOLAN-WPS)
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
response <- try(getURL(url = url,
                       postfields = xml_request,
                       httpheader = header,
                       verbose = TRUE))



# Request erfolgreich + Regen in abgefragtem Zeitraum
# -> Simulation mit aktuellen Regendaten

if(exists("response")){
  
  # Format anpassen fuer SWMM
  data <- unlist(strsplit(gsub('\"','',response),'\n'))
  rain_data <- matrix(nrow=(length(data)-1), ncol=7)
  rain_data[,1] <- station_name # Stationsname
  for(i in 2:length(data)){
    # Datum/Zeit in GMT (+2h = CEST)
    rain_data[(i-1),2] <- as.character(as.numeric(substr(unlist(strsplit(data[i],','))[5],1,2))+2000) # Jahr
    rain_data[(i-1),3] <- substr(unlist(strsplit(data[i],','))[5],3,4) # Monat
    rain_data[(i-1),4] <- substr(unlist(strsplit(data[i],','))[5],5,6) # Tag
    rain_data[(i-1),5] <- substr(unlist(strsplit(data[i],','))[5],7,8) # Stunden
    rain_data[(i-1),6] <- substr(unlist(strsplit(data[i],','))[5],9,10) # Minuten
    rain_data[(i-1),7] <- ifelse(unlist(strsplit(data[i],','))[4] == "NA", 0, round(as.numeric(unlist(strsplit(data[i],','))[4]),2)) # Regenwert (NA-Werte werden auf 0 gesetzt)
  }
  
  # keine 0-Werte
  rain_data <- rain_data[-which(rain_data[,7]==0),]
  
  if(nrow(rain_data)>0){
    
    # Text fuer Demo-Log
    demo_message <- "NODEMO\nRaindata request successfull"
    
    # Zeitstempel Start-/ Endwert
    start <- trimws(paste(rain_data[1,2:6], collapse="-"))
    end <- trimws(paste(rain_data[nrow(rain_data),2:6], collapse="-"))
    
    # Name der Regendatei mit Zeitstempel
    rain_file <- paste0("rain_", start, "_", end, ".dat")
    
    # Regendatei speichern
    write.table(rain_data, rain_file,
                row.names=F, col.names=F, quote=F)
    
  }

}



# Request nicht erfolgreich oder kein Regen in abgefragtem Zeitraum
# -> Simualtion mit demo-Regen

if(!exists("response") | nrow(rain_data)==0) {
  
  # Text fuer Demo-Log
  if(!exists("response")){
    demo_message <- "DEMO\nRaindata request failed"
  }
  if(nrow(rain_data)==0){
    demo_message <- "DEMO\nNo rain values for requested period"
  }
  
  # Demo-Regen einlesen
  rain_data <- read.table(demo_rain_file)
  rain_data <- rain_data[-which(rain_data[,7]==0),]
  
  # Zeitstempel in Demo-Daten anpassen
  end_timestamp <- Sys.time() - 2*60*60 # GMT
  end_date <- unlist(strsplit(as.character(end_timestamp), split=" "))[1]
  end_time <- unlist(strsplit(as.character(end_timestamp), split=" "))[2]

  rain_end <- paste(as.character(rain_data[nrow(rain_data),2:4]),
                      collapse="-")
  rain_end <- paste(rain_end,
                      paste(as.character(rain_data[nrow(rain_data),5:6]),
                            collapse=":"))
  rain_end_timestamp <- as.POSIXct(rain_end)
  
  time_diff <- end_timestamp - rain_end_timestamp
  
  for(r in 1:nrow(rain_data)){
    date <- paste(as.character(rain_data[r,2:4]), collapse="-")
    datetime <- paste(date,
                      paste(as.character(rain_data[r,5:6]), collapse=":"))
    timestamp <- as.POSIXct(datetime) + time_diff
    rain_data[r,2:6] <- unlist(strsplit(as.character(format(timestamp, "%Y %m %d %H %M")), split=" "))
  }
  
  # Name der Regendatei mit Zeitstempel
  start <- paste(rain_data[1,2:6], collapse="-")
  end <- paste(rain_data[nrow(rain_data),2:6], collapse="-")
  rain_file <- paste0("rain_", start, "_", end, ".dat")
  
  # Regendatei speichern
  write.table(rain_data, rain_file,
              row.names=F, col.names=F, quote=F)

}



# Regendatei im .inp-File aendern (entsprechend Dateiname oben)
p1 <- paste0("FILE       \".*\"    ", station_name)
r1 <- paste0("FILE       \"", rain_file, "\"    ", station_name)
n1 <- gsub(pattern = p1, replace = r1, x = raw_inp)


# Start-/Endzeit fuer Simulation im .inp-File aendern

start_date_sim <- paste(rain_data[1,c(3,4,2)], collapse="/")
p2s <- "START_DATE           .*"
r2s <- paste0("START_DATE           ", start_date_sim)
n2s <- gsub(pattern = p2s, replace = r2s, x = n1)
p2r <- "REPORT_START_DATE    .*"
r2r <- paste0("REPORT_START_DATE    ", start_date_sim)
n2 <- gsub(pattern = p2r, replace = r2r, x = n2s)

start_time_sim <- trimws(paste(rain_data[1,5:6], collapse=":"), "right")
p3s <- "START_TIME           .*"
r3s <- paste0("START_TIME           ", start_time_sim)
n3s <- gsub(pattern = p3s, replace = r3s, x = n2)
p3r <- "REPORT_START_TIME    .*"
r3r <- paste0("REPORT_START_TIME    ", start_time_sim)
n3 <- gsub(pattern = p3r, replace = r3r, x = n3s)

end_date_sim <- paste(rain_data[nrow(rain_data),c(3,4,2)], collapse="/")
p4 <- "END_DATE             .*"
r4 <- paste0("END_DATE             ", end_date_sim)
n4 <- gsub(pattern = p4, replace = r4, x = n3)

end_time_sim <- trimws(paste(rain_data[nrow(rain_data),5:6], collapse=":"), "right")
p5 <- "END_TIME             .*"
r5 <- paste0("END_TIME             ", end_time_sim)
n5 <- gsub(pattern = p5, replace = r5, x = n4)

start_date_sweep <- paste(rain_data[1,c(3,4)], collapse="/")
p6 <- "SWEEP_START          .*"
r6 <- paste0("SWEEP_START          ", start_date_sweep)
n6 <- gsub(pattern = p6, replace = r6, x = n5)

end_date_sweep <- paste(rain_data[nrow(rain_data),c(3,4)], collapse="/")
p7 <- "SWEEP_END            .*"
r7 <- paste0("SWEEP_END            ", end_date_sweep)
n7 <- gsub(pattern = p7, replace = r7, x = n6)


# Sweep-Intervall im .inp-File aendern
p8 <- "1                .          1          15"
r8 <- paste0("1                ",sweep,"          1          15")
n8 <- gsub(pattern = p8, replace = r8, x = n7)


# neue .inp-Datei + Demo-Log speichern
writeLines(n8, paste0(inp, "_new.inp"))
writeLines(demo_message, "demo_log.txt")


# SWMM aufrufen, .out-Datei erstellen
setwd(getwd())
run_swmm(inp = paste0(inp, "_new.inp"), exec=swmm_path)
out <- paste0(inp, "_new.out")

# .out-Datei + Demo-Log in angegebenen Ordner kopieren
file.copy(paste0(inp, "_new.out"), out_path, overwrite=T)
file.copy("demo_log.txt", out_path, overwrite=T)



# wps.out: id = out, type = text,
# title = Simulationsergebnis und Log-File,
# abstract = Ausgabe der SWMM-Simulation im Binaerformat zur Weiterverarbeitung mit WPS2 und txt-Datei mit Info zu verwendeten Regendaten;