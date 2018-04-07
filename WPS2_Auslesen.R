# wps.des: id = WPS2_Auslesen, title = SWMM Binaerdatei auslesen,
# abstract = liest von WPS1_Simulation erstellte out_Datei (in Resources-Directory) aus;


# wps.res: SWMM/eschdorf_v6_20141208.out;


# wps.in: id = method, type = string,
# title = Methode,
# abstract = MinMax / SingleNode / AllNodes;

# wps.in: id = name, type = string,
# title = Name des Objektes (Subcatchments / Knoten / Links),
# abstract = Auswahl eines Subcatchments / Knotens / Links;

# wps.in: id = var, type = integer,
# title = Variable fuer gewaehlten Ergebnistyp bzw. Objekt,
# abstract = Auswahl einer Variablen je nach Ergebnistyp bzw. Objekt;



# Bibliotheken
library(swmmr)
library(zoo)

# Input-Variablen in Vektoren konvertieren
res <- as.vector(1) # 0: Subcatchments / 1: Nodes / 2: Links / 4: System Variables
name <- as.vector(name)
var <- as.vector(var)
method <- as.vector(method)
binary <- "eschdorf_v6_20141208.out"

if(method == "MinMax" || method == "SingleNode"){
  
  # .out-Datei auslesen entsprechend der Input-Variablen (1 Knoten)
  list <- read_out(file=binary, iType=res, vIndex=var, object_name=name)
  title <- names(list)
  var_name <- names(list[[title]])
  xts <- list[[1]][[1]]
  
  # Ausgabe: csv-Datei erstellen
  if(method == "MinMax"){
    values <- range(xts)
    table <- paste0(title, "_", var_name, "_MinMax.csv")
    write.zoo(values, file=table, sep=",",dec=".")
    
  } else if (method == "SingleNode"){
    table <- paste0(title, "_", var_name, ".csv")
    write.zoo(xts, file=table, sep=",",dec=".")
  }
  
  
} else if (method == "AllNodes"){
  
  # Namen aller Knoten (!= Indizes im interaktiven Modus)
  names <- c(1:6, 8, 10:12, 14:27, 29:51, 53:95, 97, 98,
             100, 102, 104:296, 299:343, "Out1")
  
  # .out-Datei auslesen entsprechend der Input-Variablen (alle Knoten)
  list <- read_out(file=binary, iType=res, object_name=names, vIndex=var)
  title <- names(list)
  var_name <- names(list[[title[1]]])
  xts <- list[[1]][[1]]
  for(i in 2:length(names)){
    xts <- merge(xts, list[[i]][[1]])
  }
  colnames(xts) <- names
  table <- paste0("AllNodes_", var_name, ".csv")
  write.zoo(xts, file=table, sep=",",dec=".")
  
}



# wps.out: id = table, type = text,
# title = Tabelle (csv),
# abstract = Werte der ausgewaehlten Variablen;