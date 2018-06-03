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
  method <- "MinMax"
  name <- "5"
  pollutant <- "CSB"
# wps.on;



# Anzupassende Variablen (siehe Doku) ---------------------------------------------
# Pfad zur .out-Datei (von WPS1_Simulation erstellt)
inp_path <- "D:/BueRO/SHK_TUD/COLABIS/Outputs_Test"
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
binary_filename <- binary
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
  
  # Namen der Knoten aus inp-File extrahieren
  raw_inp <- readLines(paste0(unlist(strsplit(binary_filename,
                                              "_new"))[1], ".inp"))
  junction_names <- raw_inp[grep("[JUNCTIONS]",raw_inp, fixed=T)
                            :grep("[OUTFALLS]",raw_inp, fixed=T)]
  outfall_names <- raw_inp[grep("[OUTFALLS]",raw_inp, fixed=T)
                           :grep("[CONDUITS]",raw_inp, fixed=T)]
  junctions <- character()
  for(j in 4:(length(junction_names)-2)){
    junctions[j-3] <- unlist(strsplit(junction_names[j]," +"))[1]
  }
  outfalls <- character()
  for(o in 4:(length(outfall_names)-2)){
    outfalls[o-3] <- unlist(strsplit(outfall_names[o]," +"))[1]
  }
  node_names <- c(junctions,outfalls)
  
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