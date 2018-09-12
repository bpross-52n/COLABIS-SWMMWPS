# wps.des: id = COLABIS_WPS2_Auslesen, title = SWMM-Binaerdatei auslesen,
# abstract = liest von COLABIS_WPS1_Simulation erstellte out-Datei aus;


# wps.res: SWMM/2daysInt.inp;


# wps.in: id = method, type = string,
# title = Ausgabemodus (SingleNode / MinMax / AllNodes),
# abstract = Schadstoffkonzentrationen fuer bestimmten Knoten / minimale/maximale Schadstoffkonzentrationen fuer alle Knoten / Schadstoffkonzentrationen fuer alle Knoten;

# wps.in: id = name, type = string,
# title = Name des Knoten,
# abstract = Auswahl eines Knotens im SingleNode-Modus;

# wps.in: id = pollutant, type = string,
# title = Name des Schadstoffes,
# abstract = Auswahl des Schadstoffes (pb / zn / cd / cu / PAH);


# Variablen fuer Testlauf in RStudio
# wps.off;
  method <- "SingleNode"
  name <- "36Q173"
  pollutant <- "pb"
# wps.on;



# Anzupassende Variablen (siehe Doku) ---------------------------------------------
# Pfad zur .out-Datei (= out_path in WPS1)
inp_path <- "C:/WPS-support-files/swmm-output"
# Name der .out-Datei (von WPS1 erstellt)
binary <- "2daysInt_new.out"
# ---------------------------------------------------------------------------------



# Bibliotheken
library(swmmr)
library(zoo)
library(sf)
library(jsonlite)


# Demo-Status aus Demo-Log abfragen
raw_log <- readLines(paste0(inp_path,"/demo_log.txt"))
demo <- ifelse(raw_log[1]=="DEMO", TRUE, FALSE)


#Dateiendung entfernen, damit die vom Simulations-Prozess erzeugte _new-Datei gefunden wird
binary_filename <- strsplit(binary, "[.]")[[1]][1]
# vollstaendiger Pfad zu .out-File
binary <- paste0(inp_path,"/",binary)

# Codierung zur Abfrage von Schadstoffen
raw_inp <- readLines(paste0(unlist(strsplit(binary_filename,
                                            "_new"))[1], ".inp"))
pollutants_inp <- raw_inp[grep("[POLLUTANTS]",raw_inp, fixed=T)
                          :grep("[LANDUSES]",raw_inp, fixed=T)]
pollutants <- rep(NA, length(pollutants_inp)-5)
for(p in 4:length(pollutants_inp)-2){
  pollutants[p-3] <- unlist(strsplit(pollutants_inp[p]," +"))[1]
}
var <- which(pollutants==pollutant)+5


# Namen + Koordinaten der Knoten aus inp-File extrahieren
nodes_inp <- raw_inp[grep("[COORDINATES]",raw_inp, fixed=T)
                     :grep("[VERTICES]",raw_inp, fixed=T)]
nodes_info <- data.frame(name=rep(NA, length(nodes_inp)-5),
                         title=rep(NA, length(nodes_inp)-5),
                         x=rep(NA, length(nodes_inp)-5),
                         y=rep(NA, length(nodes_inp)-5))
for(n in 4:(length(nodes_inp)-2)){
  nodes_info$name[n-3] <- unlist(strsplit(nodes_inp[n]," +"))[1]
  nodes_info$title[n-3] <- paste0("node",nodes_info$name[n-3])
  nodes_info$x[n-3] <- unlist(strsplit(nodes_inp[n]," +"))[2]
  nodes_info$y[n-3] <- unlist(strsplit(nodes_inp[n]," +"))[3]
}
nodes_info$x <- as.numeric(nodes_info$x)
nodes_info$y <- as.numeric(nodes_info$y)

# Koordinaten umrechnen: UTM Zone 33N (EPSG 32633) -> WGS84 (EPSG 4326)
nodes_sp <- st_as_sf(nodes_info, coords=c("x","y"), crs=32633)
nodes_wgs <- st_transform(nodes_sp, crs=4326)
for(n in 1:nrow(nodes_info)){
  nodes_info$x[n] <- nodes_wgs$geometry[[n]][1] # longitude
  nodes_info$y[n] <- nodes_wgs$geometry[[n]][2] # latitude
}


if(method == "SingleNode"){
  
  # .out-Datei auslesen fuer 1 Knoten (name)
  # und ausgewaehlten Schadstoff (pollutant bzw. var)
  list <- read_out(file=binary, iType=1, vIndex=var, object_name=name)
  xts <- list[[1]][[1]] # xts-Objekt mit Schadstoffwerten
  data_obj <- toJSON(data.frame(time=time(xts),
                                value=coredata(xts)))
  json_str <- sprintf(
'{
  "name" : "%s",
  "x" : "%s",
  "y" : "%s",
  "phenomenon" : "%s",
  "demo_data" : %s,
  "data" : %s
}',
    nodes_info$title[nodes_info$name==name],
    nodes_info$x[nodes_info$name==name],
    nodes_info$y[nodes_info$name==name],
    pollutant,
    tolower(demo),
    data_obj)
  
  # Ausgabe: json-Datei erstellen
  table <- paste0(nodes_info$title[nodes_info$name==name], "_",
                  pollutant, ".json") # Name des Output-File
  writeLines(json_str, table)

}


if (method == "MinMax" || method == "AllNodes"){

  # .out-Datei auslesen fuer alle Knoten (nodes_info$name)
  # und ausgewaehlten Schadstoff (pollutant bzw. var)
  list <- read_out(file=binary, iType=1,
                   object_name=nodes_info$name, vIndex=var)
  xts <- list[[1]][[1]] # Zusammenfuehren der xts-Objekte mit Schadstoffwerten
  for(i in 2:length(nodes_info$title)){
    xts <- merge(xts, list[[i]][[1]])
  }
  colnames(xts) <- nodes_info$title
  
  
  if(method == "AllNodes"){
    
    json_str <- sprintf('{
  "demo_data": %s,
  "data":
  [\n', tolower(demo))
    for(n in 1:ncol(xts)){
      data_obj <- toJSON(data.frame(time=time(xts[,n]),
                                    value=as.numeric(coredata(xts[,n]))))
      obj <- sprintf(
'    {
       "name" : "%s",
       "x" : "%s",
       "y" : "%s",
       "phenomenon" : "%s",
       "data" : %s
    }',
        colnames(xts[,n]),
        nodes_info$x[nodes_info$title==colnames(xts[,n])],
        nodes_info$y[nodes_info$title==colnames(xts[,n])],
        pollutant,
        data_obj)
      json_str <- paste0(json_str, "\n", obj)
    }
    json_str <- paste0(json_str, "\n]\n}")
    
    # Ausgabe: json-Datei erstellen
    table <- paste0("AllNodes_", pollutant, ".json") # Name des Output-File
    writeLines(json_str, table)

  }
  
  
  if(method == "MinMax"){
    
    json_str <- sprintf('{
  "demo_data":%s,
  "data":
  [\n', tolower(demo))
    for(n in 1:ncol(xts)){
      obj <- sprintf(
'  {
    "name" : "%s",
    "x" : "%s",
    "y" : "%s",
    "phenomenon" : "%s",
    "min" : %s,
    "max" :%s
  }',
        colnames(xts[,n]),
        nodes_info$x[nodes_info$title==colnames(xts[,n])],
        nodes_info$y[nodes_info$title==colnames(xts[,n])],
        pollutant,
        min(coredata(xts[,n])),
        max(coredata(xts[,n])))
      json_str <- ifelse(n != 1, paste0(json_str, ",\n", obj),
                         paste0(json_str, "\n", obj))
    }
    json_str <- paste0(json_str, '\n  ]\n}')
    
    # Ausgabe: json-Datei erstellen
    table <- paste0("AllNodes_", pollutant, "_MinMax.json") # Name des Output-File
    writeLines(json_str, table)
  }

}



# wps.out: id = result, type = text,
# title = Schadstoffwerte (json),
# abstract = Werte des/der ausgewaehlten Knoten fuer ausgewaehlten Schadstoff;