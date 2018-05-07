# wps.des: id = COLABIS_WPS2_Auslesen, title = SWMM-Binaerdatei auslesen,
# abstract = liest von COLABIS_WPS1_Simulation erstellte out-Datei aus;


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
inp_path <- "c:/WPS-support-files/swmm-output"
# Namen aller Knoten entsprechend [JUNCTIONS] und [OUTFALLS] im inp-File
node_names <- c(1:6, 8, 10:12, 14:27, 29:51, 53:95, 97, 98, 100, 102, 104:296, 299:343, "Out1")
# Namen der Schadstoffe in Codes umwandeln
# Ausfuehren von read_out() ohne Angabe des Parameters vIndex zeigt verfuegbare Elemente
#read_out(file=binary, iType=1, object_name=name)
if(pollutant == "CSB"){var <- 7-1}
if(pollutant == "NH4N"){var <- 8-1}
# ---------------------------------------------------------------------------------



# Bibliotheken
library(swmmr)
library(zoo)


# vollstaendiger Pfad zu .out-File
binary <- paste0(inp_path,"/",binary)

if(method == "SingleNode"){
  
  # .out-Datei auslesen fuer 1 Knoten (name)
  # und ausgewaehlten Schadstoff (pollutant bzw. var)
  list <- read_out(file=binary, iType=1, vIndex=var, object_name=name)
  title <- names(list) # Name des Knoten (vgl. name)
  var_name <- names(list[[title]]) # Name des Schadstoffes (vgl. pollutant)
  xts <- list[[1]][[1]] # xts-Objekt mit Schadstoffwerten
  
  # Ausgabe: csv-Datei erstellen
  table <- paste0(title, "_", var_name, ".csv") # Name des Output-File
  write.zoo(xts, file=table, sep=",",dec=".")
  
}


if (method == "MinMax" || method == "AllNodes"){
  
  # .out-Datei auslesen fuer alle Knoten (node_names)
  # und ausgewaehlten Schadstoff (pollutant bzw. var)
  list <- read_out(file=binary, iType=1, object_name=node_names, vIndex=var)
  title <- names(list) # Namen aller Knoten (vgl. node_names)
  var_name <- names(list[[title[1]]]) # Name des Schadstoffes (vgl. pollutant)
  xts <- list[[1]][[1]] # Zusammenfuehren der xts-Objekte mit Schadstoffwerten
  for(i in 2:length(node_names)){
    xts <- merge(xts, list[[i]][[1]])
  }
  colnames(xts) <- node_names
  
  if(method == "MinMax"){
    min_max <- range(xts[,1])
    for(i in 2:ncol(xts)){
      min_max <- cbind(min_max, range(xts[,i])) # Spalten = Knoten, Zeilen = Min/Max
    }
    # Ausgabe: csv-Datei erstellen
    table <- paste0("AllNodes_", var_name, "_MinMax.csv") # Name des Output-File
    write.table(min_max, file=table, sep=",", dec=".",
                row.names=c("Min", "Max"), col.names=node_names)
  }
  
  if(method == "AllNodes"){
    # Ausgabe: csv-Datei erstellen
    table <- paste0("AllNodes_", var_name, ".csv") # Name des Output-File
    write.zoo(xts, file=table, sep=",",dec=".") # Spalten = Knoten, Zeilen = Zeitschritte
  }
  
}



# wps.out: id = table, type = text,
# title = Ausgabetabelle (csv),
# abstract = Werte des/der ausgewaehlten Knoten fuer ausgewaehlten Schadstoff;