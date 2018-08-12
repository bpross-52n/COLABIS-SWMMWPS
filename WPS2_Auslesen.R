# wps.des: id = COLABIS_WPS2_Auslesen, title = SWMM-Binaerdatei auslesen,
# abstract = liest von COLABIS_WPS1_Simulation erstellte out-Datei aus;


# wps.res: SWMM/SWMM_project_original.inp;


# wps.in: id = binary, type = string,
# title = Name des out-File mit Dateiendung,
# abstract = Ergebnis der SWMM-Simulation (WPS1) mit/ohne Strassenreinigung;

# wps.in: id = method, type = string,
# title = Ausgabemodus (SingleNode / MinMax / AllNodes),
# abstract = Schadstoffkonzentrationen fuer bestimmten Knoten / minimale/maximale Schadstoffkonzentrationen fuer alle Knoten / Schadstoffkonzentrationen fuer alle Knoten;

# wps.in: id = name, type = string,
# title = Name des Knoten,
# abstract = Auswahl eines Knotens im SingleNode-Modus;

# wps.in: id = pollutant, type = string,
# title = Name des Schadstoffes,
# abstract = zum Beispiel CSB / NH4N;


# Variablen fuer Testlauf in RStudio
# wps.off;
  binary <- "eschdorf_v6_20141208_new.out"
  method <- "SingleNode"
  name <- "5"
  pollutant <- "CSB"
# wps.on;



# Anzupassende Variablen (siehe Doku) ---------------------------------------------
# Pfad zur .out-Datei (von WPS1_Simulation erstellt)
inp_path <- "D:/BueRO/SHK_TUD/COLABIS/Outputs_Test"
# ---------------------------------------------------------------------------------



# Bibliotheken
library(swmmr)
library(zoo)
library(sf)
library(jsonlite)


# vollstaendiger Pfad zu .out-File
binary_filename <- binary
binary <- paste0(inp_path,"/",binary)

# Codierung zur Abfrage von Schadstoffen
raw_inp <- readLines(paste0(unlist(strsplit(binary_filename,
                                            "_new"))[1], ".inp"))
pollutants_inp <- raw_inp[grep("[POLLUTANTS]",raw_inp, fixed=T)
                          :grep("[LOADINGS]",raw_inp, fixed=T)]
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

# Koordinaten umrechnen: GK Zone 5 (EPSG 31469) -> WGS84 (EPSG 4326)
nodes_sp <- st_as_sf(nodes_info, coords=c("x","y"), crs=31469)
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
  "data" : %s
}',
    nodes_info$title[nodes_info$name==name],
    nodes_info$x[nodes_info$name==name],
    nodes_info$y[nodes_info$name==name],
    pollutant,
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
    
    json_str <- "{\n"
    for(n in 1:ncol(xts)){
      data_obj <- toJSON(data.frame(time=time(xts[,n]),
                                    value=coredata(xts[,n])))
      obj <- sprintf(
'{
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
    json_str <- paste0(json_str, "\n}")
    
    # Ausgabe: json-Datei erstellen
    table <- paste0("AllNodes_", pollutant, ".json") # Name des Output-File
    writeLines(json_str, table)

  }
  
  
  if(method == "MinMax"){
    
    json_str <- "[{\n"
    for(n in 1:ncol(xts)){
      obj <- sprintf(
'{
  "name" : "%s",
  "x" : "%s",
  "y" : "%s",
  "phenomenon" : "%s",
  "min" : {
           "time" : %s,
           "value" : %s
          },
  "max" : {
           "time" : %s,
           "value" : %s
          },
}',
        colnames(xts[,n]),
        nodes_info$x[nodes_info$title==colnames(xts[,n])],
        nodes_info$y[nodes_info$title==colnames(xts[,n])],
        pollutant,
        time(xts[which(xts[,n]==min(xts[,n])),n])[1],
        min(coredata(xts[,n])),
        time(xts[which(xts[,n]==max(xts[,n])),n])[1],
        max(coredata(xts[,n])))
      json_str <- paste0(json_str, "\n", obj)
    }
    json_str <- paste0(json_str, "\n}]")
    
    # Ausgabe: json-Datei erstellen
    table <- paste0("AllNodes_", pollutant, "_MinMax.json") # Name des Output-File
    writeLines(json_str, table)
  }

}



# wps.out: id = table, type = text,
# title = Ausgabetabelle (json),
# abstract = Werte des/der ausgewaehlten Knoten fuer ausgewaehlten Schadstoff;