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
library(jsonlite)


# vollstaendiger Pfad zu .out-File
binary_filename <- binary
binary <- paste0(inp_path,"/",binary)

# Namen + Koordinaten der Knoten aus inp-File extrahieren
raw_inp <- readLines(paste0(unlist(strsplit(binary_filename,
                                            "_new"))[1], ".inp"))
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


if(method == "SingleNode"){
  
  # .out-Datei auslesen fuer 1 Knoten (name)
  # und ausgewaehlten Schadstoff (pollutant bzw. var)
  list <- read_out(file=binary, iType=1, vIndex=var, object_name=name)
  xts <- list[[1]][[1]] # xts-Objekt mit Schadstoffwerten
  df <- data.frame(rep(nodes_info$title[nodes_info$name==name],nrow(xts)),
                   rep(nodes_info$x[nodes_info$name==name],nrow(xts)),
                   rep(nodes_info$y[nodes_info$name==name],nrow(xts)),
                   time(xts), coredata(xts[,1]))
  # Spalten: Knotenname, X-Koordinate, Y-Koordinate, Zeitstempel, Schadstoffwert
  colnames(df) <- c("name", "x", "y", "time", pollutant)

  # Ausgabe: json-Datei erstellen
  table <- paste0(nodes_info$title[nodes_info$name==name], "_",
                  pollutant, ".json") # Name des Output-File
  write(toJSON(df), file=table)

}


if (method == "MinMax" || method == "AllNodes"){

  # .out-Datei auslesen fuer alle Knoten (nodes_info$name)
  # und ausgewaehlten Schadstoff (pollutant bzw. var)
  list <- read_out(file=binary, iType=1, object_name=nodes_info$name, vIndex=var)
  xts <- list[[1]][[1]] # Zusammenfuehren der xts-Objekte mit Schadstoffwerten
  for(i in 2:length(nodes_info$title)){
    xts <- merge(xts, list[[i]][[1]])
  }
  colnames(xts) <- nodes_info$title
  
  
  if(method == "AllNodes"){
    
    # Spalten: Knotenname, X-Koordinate, Y-Koordinate, Zeitstempel, Schadstoffwert
    df <- data.frame()
    for(n in 1:ncol(xts)){
      node_temp <- colnames(xts[,n])
      node_df <- data.frame(rep(node_temp,nrow(xts)),
                            rep(nodes_info$x[nodes_info$title==node_temp],nrow(xts)),
                            rep(nodes_info$y[nodes_info$title==node_temp],nrow(xts)),
                            time(xts), coredata(xts[,n]))
      colnames(node_df) <- c("name", "x", "y", "time", pollutant)
      df <- rbind(df, node_df)
    }
    
    # Ausgabe: json-Datei erstellen
    table <- paste0("AllNodes_", pollutant, ".json") # Name des Output-File
    write(toJSON(df), file=table)
    
  }
  
  
  if(method == "MinMax"){
    
    # Spalten: Knotenname, X-Koordinate, Y-Koordinate, Zeitstempel, Minimum, Maximum
    df <- data.frame()
    for(n in 1:ncol(xts)){
      node_temp <- colnames(xts[,n])
      node_df <- data.frame(rep(node_temp,2),
                            rep(nodes_info$x[nodes_info$title==node_temp],2),
                            rep(nodes_info$y[nodes_info$title==node_temp],2),
                            c("min","max"), range(coredata(xts[,n])))
      colnames(node_df) <- c("name", "x", "y", "min_max", pollutant)
      df <- rbind(df, node_df)
    }
    
    # Ausgabe: json-Datei erstellen
    table <- paste0("AllNodes_", pollutant, "_MinMax.json") # Name des Output-File
    write(toJSON(df), file=table)
  }

  
}



# wps.out: id = table, type = text,
# title = Ausgabetabelle (json),
# abstract = Werte des/der ausgewaehlten Knoten fuer ausgewaehlten Schadstoff;