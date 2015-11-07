# ProtectedAreas_NDVI
#Sistema de seguimiento y alerta temprana de los cambios en el funcionamiento ecosistémico de áreas protegidas mediante teledetección
## Autor(es): Grettel Vargas Azofeifa.
## Versión: 1
## Nombre del código: AnomaliesInPark
## Ãšltima fecha de actualizaciòn: 15 de setiembre del 2014
## Desarrollado bajo R-3.0.2
## 
## 
############################################################
############################################################
## Obtener el PA_Code de la web -->
## También puede descargar manualmente el CSV y buscar el nombre deseado (http://ide.ugr.es/protectedareas.csv)
## 
## 
##======================================================================================================================
## M?DULO 1: ESTABLECER EL DIRECTORIO,INSTALACIÓN DE PAQUETES Y LEER DATOS DE LA NASA
##======================================================================================================================

#Step 0. Customization of personal settings

#Step 0b: Mirar en la web www.wdpa.com el c?digo del ?rea protegida que nos interesa
PA_Code <- "20956"
GlobalDirectory <-  "/Users/iecolab/ownCloud/3_Desarrollos/TFMs/GrettelVargas"

MODISBand <- "250m_16_days_EVI"

#Advanced settings
LevelOfDetail <- 8 # NUM OF COLS OF OUTPUT PNG FILE. integer to multiply the num of cols. Result must be usually >800 
NoDataValues <- -3000
DivColorBreaks <- c(-10000,-1600,-800,-300,-50,50,300,800,1600,10000) #Color ranges for anomaly maps in EVI units of change
libs <- .libPaths()[[1]]

### Step 1: Declarar/crear directorios y personalizar par?metros para el procesamiento

DirOfData <- file.path(GlobalDirectory, "data")
dir.create(DirOfData, showWarnings = FALSE)

DirOfMODOriginals <- file.path(GlobalDirectory, "modis")
dir.create(DirOfMODOriginals, showWarnings = FALSE)

DirOfMODStats <- file.path(GlobalDirectory, "stats")
dir.create(DirOfMODStats, showWarnings = FALSE)

DirOfMODKML <- file.path(GlobalDirectory, "kml")
dir.create(DirOfMODKML, showWarnings = FALSE)

## 
setwd(GlobalDirectory)

## Set parameters to raster files
parameters <- as.character("PROJCS[\"Sinusoidal\",GEOGCS[\"GCS_Undefined\",DATUM[\"Undefined\",SPHEROID[\"User_Defined_Spheroid\",6371007.181,0.0]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],PROJECTION[\"Sinusoidal\"],PARAMETER[\"False_Easting\",0.0],PARAMETER[\"False_Northing\",0.0],PARAMETER[\"Central_Meridian\",0.0],UNIT[\"Meter\",1.0]]")

### Step 2: install.packages("MODIS", buscar las actualizaciones en la p?gina repositorio)

cat("Cargando paquetes necesarios ... ")

## list of package and installations if needs
packages <- c("MODISTools", "rgdal", "raster", "plotKML", "RColorBrewer", "XML")

## comprobamos la version del MODISTools
if (length(setdiff(c("MODISTools"), rownames(installed.packages()))) == 0){
  if (packageVersion("MODISTools") < '0.94.4'){
    remove.packages("MODISTools", lib=libs)  
  }    
}
if (length(setdiff(c("plotKML"), rownames(installed.packages()))) == 0){
  if (packageVersion("plotKML") < '0.4.6'){
    remove.packages("plotKML", lib=libs)  
    ## instalamos la Ãºltima versiÃ³nSys.info()['sysname']
    if (Sys.info()['sysname'] == "Darwin"){ ## para Osx no estÃ¡ el binario
      install.packages("plotKML", repos=c("http://R-Forge.R-project.org"), lib=libs, type="source") 
    }else{
      install.packages("plotKML", repos=c("http://R-Forge.R-project.org"), lib=libs) 
    }
  }    
}

packages_pendientes <- setdiff(packages, rownames(installed.packages()))
if (length(packages_pendientes) > 0) {  
  ## instalamos los paquetes pendientes
  install.packages(packages_pendientes, dep=T, lib=libs)  
}

lapply(packages,function(x){library(x,character.only=TRUE, lib=libs)}) 

## colores 
NDivBreaks <- length(DivColorBreaks)-1 
DivColPalette <- colorRampPalette((brewer.pal(NDivBreaks, "RdBu")), space="Lab")
DivCols <- DivColPalette(NDivBreaks)

## cargamos una funcion extra
source("kml_raster_factor.R")

### Step 3: crear un fichero vac?o donde se almacenarán las imagenes que ya se han procesado, 
## si no existe (es la primera vez que se ejecuta el script)
cat("Cargando fechas anteriores ... ")
fileOfAlreadyProcessed <- file.path(DirOfData, "AlreadyProcessed.dat")
if (file.exists(fileOfAlreadyProcessed)){
  ## leemos la variable AlreadyProcess
  load(fileOfAlreadyProcessed)
}else{
  ## la creamos vacia
  AlreadyProcessed <- c()  
  ## guardamos el fichero para la siguiente vez que ejecutemos el script
  save(AlreadyProcessed, file=fileOfAlreadyProcessed )
}

##======================================================================================================================
## M?DULO 2:SELECCI?N DEL SHAPEFILE DE INTERES EN LA WORLD DATABASE PROTECTED AREAS.
##======================================================================================================================
## Step 4.

## leemos el csv
PathProtectedareasCsv <- file.path(DirOfData, "protectedareas.csv")
if (!file.exists(PathProtectedareasCsv)){
  ## descargamos de algun sitio, p.e. dropbox
  cat(paste("Descargando la informaciÃ³n sobre las areas protegidas ... "))
  download.file("http://ide.ugr.es/protectedareas.csv", PathProtectedareasCsv)
}

cat("Leyendo datos sobre las Ã¡reas protegidas")
allPAs <- read.csv(PathProtectedareasCsv, header=TRUE, sep=",")
## obtenemos los datos del PA
MyPa <- allPAs[allPAs$wdpaid==PA_Code,]

## Valores m?ximos y m?nimos del recuadro que abarca a Sierra Nevada.
xmin <- MyPa$x_min
ymin <- MyPa$y_min
xmax <- MyPa$x_max
ymax <- MyPa$y_max

# Step 6.Distancia del eje X para el shapefile de Sierra Nevada
LL <- cbind(c(xmin), c(ymin)) 
LR <- cbind(c(xmax), c(ymin))
pointDistance(LL, LR, lonlat=TRUE)
DistanceXDiv2 <- ceiling((pointDistance(LL, LR, lonlat=TRUE)/1000)/2)

#Distancia del eje Y para el shapefile de Sierra Nevada
UL <- cbind(c(xmin), c(ymin))
UR <- cbind(c(xmin), c(ymax))
pointDistance(UL, UR, lonlat=TRUE)
DistanceYDiv2 <- ceiling((pointDistance(UL, UR, lonlat=TRUE)/1000)/2)

# Step 8. C?lculo del centro del cuadrado que abarca a Sierra Nevada
long <- (xmin+xmax)/2
lat <- (ymin+ymax)/2

##======================================================================================================================
## M?DULO 3: CONSULTAR LOS DATOS DISPONIBLES EN LA NASA Y DESCARGA DE LAS NUEVAS IM?GENES
##======================================================================================================================

cat("Obteniendo fechas existentes en la NASA")
# Consultar cuales im?genes est?n disponibles en NASA.
AvailableDatesInNasa <- GetDates(lat, long, Product = "MOD13Q1")
#AvailableDatesInNasa

## Step 12. Calculamos la diferencia entre el contenido del fichero "AlreadyProcessed" y "AvailableDatesInNasa"
## como resultado vas a tener las ?nicas im?genes que tendr?as que procesar en el flujo
DatesToDownload <- setdiff(AvailableDatesInNasa, AlreadyProcessed)
cat(paste("Dates to download ",length(DatesToDownload)))

##======================================================================================================================
## M?DULO 4: DATOS DE EVI EN FORMATO ASCII PARA CADA COMPUESTO Y PARA LA EXTENSION DEL PARQUE NACIONAL DE SIERRA NEVADA 
#=======================================================================================================================
if (length(DatesToDownload) > 0){
  
# Step 13.Selecciona la primera y ?ltima fecha dispoble
j.start.date <- DatesToDownload[1]# fecha juliana disponible de inicio
j.end.date <- DatesToDownload[length(DatesToDownload)]# fecha juliana disponible final
j.end.date.day <- substr(j.end.date[length(j.end.date)],1, 9)

# Step 14.Primera y ?ltima fecha disponible en formato POSIXlt
start.year <- substr(j.start.date, 2, 5)
end.year <- substr(j.end.date, 2, 5)

# Step 15. Generar las variables necesarias para la funci?n MODISSubsets
PASubset <- SubsetExample
PASubset$lat <- lat
PASubset$long <- long
PASubset$start.date <- strptime(paste(substr(j.start.date[1], 2, 5), substr(j.start.date[1], 6, 8), sep = "-"), format = "%Y-%j")
end.datef <- strptime(paste(substr(j.end.date[length(j.end.date)], 2, 5), substr(j.end.date[length(j.end.date)], 6, 8), sep = "-"), format = "%Y-%j")
PASubset$end.date <- as.Date(as.POSIXlt(end.datef))+5 # Se le agrega un +1 porque no incluye la ?ltima fecha

cat("Descargando las imagenes necesarias ... (this may take from mins to hours)")
# Step 16.Obtencion de los datos EVI para las fechas disponibles con la funci?n MODISSubsets
MODISSubsets(LoadDat=PASubset, Product='MOD13Q1', Bands=MODISBand, Size = c(DistanceYDiv2,DistanceXDiv2), 
             SaveDir=DirOfMODOriginals, StartDate = TRUE)

# Step 17. Genera los archivos en formato grid para cada fecha disponible (se pueden visualizar en qgis)
## BUG package: tenemos que cambiar de directorio para poder procesar los GRID
cat("Generando ficheros en formato GRID ... ")
setwd(DirOfMODOriginals)
MODISNoDataValues <- c(NoDataValues)
names(MODISNoDataValues) <- c(MODISBand)
MODISGrid(SubDir=TRUE, NoDataValues=MODISNoDataValues)       
setwd(GlobalDirectory)     
}
##======================================================================================================================
## MÓDULO 5: CÁLCULO DE LAS ESTADÍSTICAS PARA CADA COMPUESTO (media, max, min histórico) Y CÁLCULO DEL #INTERVALO DE CONFIANZA (t-test para muestras menores de 30 años) 
##======================================================================================================================

## Step 18. Lee los datos en formato Grid creados en la nueva carpeta MODIS_GRID/1___MOD13Q1 y calcula las estad?sticas
#----------
FileListOriginals <- list.files(path=file.path(DirOfMODOriginals, "MODIS_GRID"), full.names = TRUE, recursive = TRUE, pattern = "*.asc")
FileListOriginalsSinExt <- strsplit(FileListOriginals, ".asc")
# function to search for all Julian days
julianDays <- unique(substr(FileListOriginalsSinExt, nchar(FileListOriginalsSinExt)-3+1, nchar(FileListOriginalsSinExt)))

years <- unique(substr(FileListOriginalsSinExt, nchar(FileListOriginalsSinExt)-7+1, nchar(FileListOriginalsSinExt)-3))
start.year <- min(years)
end.year <- max(years)

for (JulianDay in julianDays){
    cat(paste("Procesando dia juliano ", JulianDay, " ... \n", sep=""))  
  #list files by pattern of julian day
  FilesToRead<-list.files(path=file.path(DirOfMODOriginals, "MODIS_GRID"), full.names = TRUE, recursive = TRUE, pattern=paste("*",JulianDay,"\\.asc$", sep=""))
  #create raster stack
  FilesStack<-stack(FilesToRead)  
  #mean files
  meanRaster<-mean(FilesStack)
  #min files
  minRaster<-min(FilesStack)
  #max files
  maxRaster<-max(FilesStack)
  #sd files
  sdRaster <-calc(FilesStack, fun=sd) 
  #confidence interval 
  ConfintLevel <- 0.9
  years <-length(FilesToRead) 
  allMeans <- meanRaster # promedios de todos los compuestos
  allSD <- sdRaster # desviaciones standard de todos los compuesto
  n <- years # cantidad de a?os
  errorStandard <- qt(ConfintLevel,df=years-1)*allSD/sqrt(years) # error standard
  # UpperConfIntRaster
  UpperConfIntRaster <- (allMeans+errorStandard)
  # LowerConfintRaster
  LowerConfIntRaster <- (allMeans-errorStandard)
    #write new asci into output folder
    writeRaster(meanRaster, filename<-file.path(DirOfMODStats,paste("Modis_",start.year,"_",end.year,"_mean_JulDay_",JulianDay, sep="")),overwrite=T,"ascii") 
    writeRaster(minRaster, filename<-file.path(DirOfMODStats,paste("Modis_",start.year,"_",end.year,"_min_JulDay_",JulianDay, sep="")), overwrite=T,"ascii")
    writeRaster(maxRaster, filename<-file.path(DirOfMODStats,paste("Modis_",start.year,"_",end.year,"_max_JulDay_",JulianDay, sep="")), overwrite=T,"ascii")
    writeRaster(sdRaster, filename<-file.path(DirOfMODStats,paste("Modis_",start.year,"_",end.year,"_sd_JulDay_",JulianDay, sep="")), overwrite=T,"ascii")
    writeRaster(UpperConfIntRaster, filename<-file.path(DirOfMODStats,paste("Modis_",start.year,"_",end.year,"_UpperConfRaster_JulDay_",JulianDay, sep="")), overwrite=T,"ascii")
    writeRaster(LowerConfIntRaster, filename<-file.path(DirOfMODStats,paste("Modis_",start.year,"_",end.year,"_LowerConfRaster_JulDay_",JulianDay, sep="")), overwrite=T,"ascii") 
  
  #Crear los archivos de proyeccion para los compuestos
    prj_files = c(file.path(DirOfMODStats,paste("Modis_",start.year,"_",end.year,"_mean_JulDay_",JulianDay,".prj", sep="")),
                  file.path(DirOfMODStats,paste("Modis_",start.year,"_",end.year,"_min_JulDay_",JulianDay,".prj", sep="")),
                  file.path(DirOfMODStats,paste("Modis_",start.year,"_",end.year,"_max_JulDay_",JulianDay,".prj", sep="")),
                  file.path(DirOfMODStats,paste("Modis_",start.year,"_",end.year,"_sd_JulDay_",JulianDay,".prj", sep="")),
                  file.path(DirOfMODStats,paste("Modis_",start.year,"_",end.year,"_UpperConfRaster_JulDay_",JulianDay,".prj", sep="")),
                  filename<-file.path(DirOfMODStats,paste("Modis_",start.year,"_",end.year,"_LowerConfRaster_JulDay_",JulianDay,".prj", sep=""))
                  )
    lapply(prj_files, function(x){
      fileConn<-file(x)
      writeLines(c(parameters), fileConn)
      close(fileConn)      
    })    
}

##======================================================================================================================
## MÓDULO 6. CALCULO DE LAS ANOMALÍAS Y DETECCIÓN DE ALERTAS TEMPRANAS.
##======================================================================================================================

# Step 19. Cargar la fecha mas reciente y su media historica correspondiente
#----------
# Fecha mas reciente disponible
NewDateFilename <- FileListOriginals[length(FileListOriginals)]
NewDateFilenameJulianDay <- substr(NewDateFilename, nchar(NewDateFilename)-7+1, nchar(NewDateFilename)-4)
NewDateRaster <- raster(read.asciigrid(NewDateFilename, as.image = TRUE, plot.image = TRUE,
                                       colname = NewDateFilename, proj4string = CRS(as.character(NA))))

# Step 20. Crear el archivo KML para la nueva fecha
#----------
projection(NewDateRaster) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"           
setwd(DirOfMODKML)
kml_open("NewDateRaster.kml")
kml_layer.Raster(NewDateRaster, plot.legend = TRUE, metadata = NULL, NewDateRaster, 
                 png.width = ncol(NewDateRaster), png.height = nrow(NewDateRaster), 
                 min.png.width = ncol(NewDateRaster)*LevelOfDetail,#usually >800 
                 colour_scale = SAGA_pal[[1]] , 
                 raster_name="NewDateRaster.png")    
kml_close("NewDateRaster.kml")
setwd(GlobalDirectory)


# Media hist?rica correspondiente con la fecha mas reciente
pattMean <- paste("mean","_JulDay_",NewDateFilenameJulianDay, ".asc$", sep="") # patr?n para seleccionar el estad?stico de la media hist?rica
HistMeanOfLastImgFilename <- list.files(path=DirOfMODStats, pattern = pattMean, full.names = TRUE)
HistMeanOfLastImgRaster <- raster(read.asciigrid(HistMeanOfLastImgFilename, as.image = TRUE, plot.image = TRUE,
                                 colname = HistMeanOfLastImgFilename, proj4string = CRS(as.character(NA))))

#Step 21.  Comparaci?n de la fecha mas reciente con la media hist?rica correspondiente a ese compuesto
#----------
AnomalyRaster <- NewDateRaster - HistMeanOfLastImgRaster
plot(AnomalyRaster, useRaster=FALSE)
projection(AnomalyRaster) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"          

setwd(DirOfMODKML)
kml_open("AnomalyRaster.kml")
kml_layer.Raster(AnomalyRaster, plot.legend = TRUE, metadata = NULL, AnomalyRaster, 
                 png.width = ncol(AnomalyRaster), png.height = nrow(AnomalyRaster), 
                 min.png.width = ncol(AnomalyRaster)*LevelOfDetail,#usually >800 
                 colour_scale = SAGA_pal[[1]] , #buscar color intuitivo para la vieja marron o rojo a verde
                 raster_name="AnomalyRaster.png")    
kml_close("AnomalyRaster.kml")
setwd(GlobalDirectory)

# Step 22. Anomal?as significativas con respecto al Intervalo de Confianza
#----------
#L?mite superior del IC hist?ricO correspondiente con la fecha mas reciente
pattUpperConf <- paste("*UpperConfRaster","_JulDay_",NewDateFilenameJulianDay,"\\.asc$", sep="") # patr?n para seleccionar el l?mite m?ximo del IC
HistUpperConfLastImgFilename<- list.files(path=DirOfMODStats, pattern = pattUpperConf, full.names = TRUE)
HistUpperConfLastImgRaster <- raster(read.asciigrid(HistUpperConfLastImgFilename, as.image = TRUE, plot.image = TRUE,
                                     colname = HistUpperConfLastImgFilename, proj4string = CRS(as.character(NA))))

#L?mite inferior del IC hist?ricO correspondiente con la fecha mas reciente
pattLowerConf <- paste("*LowerConfRaster","_JulDay_",NewDateFilenameJulianDay,"\\.asc$", sep="") 
HistLowerConfLastImgFilename<-list.files(path=DirOfMODStats, pattern = pattLowerConf, full.names = TRUE)
HistLowerConfLastImgRaster <- raster(read.asciigrid(HistLowerConfLastImgFilename, as.image = TRUE, plot.image = TRUE,
                                    colname = HistLowerConfLastImgFilename, proj4string = CRS(as.character(NA))))

SignificantAnomalyRaster <- (NewDateRaster < HistLowerConfLastImgRaster)*
                            (NewDateRaster - HistLowerConfLastImgRaster)+
                            (NewDateRaster > HistUpperConfLastImgRaster)*
                            (NewDateRaster - HistUpperConfLastImgRaster)
projection(SignificantAnomalyRaster) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"           
hist(SignificantAnomalyRaster)
summary(SignificantAnomalyRaster[SignificantAnomalyRaster>0])
summary(SignificantAnomalyRaster[SignificantAnomalyRaster<0])
# This could be used to set automatic colour breaks in the legend based con quartiles of data distribution
#CombinedSummarySigAnRast<-rbind(summary(SignificantAnomalyRaster[SignificantAnomalyRaster>0]),
#    abs(summary(SignificantAnomalyRaster[SignificantAnomalyRaster<0])))
#MaxQuartile4ColBreaks<-max(apply(CombinedSummarySigAnRast, MARGIN=c(2), max)[c(2,5)])
#MinMax4ColBreaks<-max(apply(CombinedSummarySigAnRast, MARGIN=c(2), max)[c(1,6)])
#DivColorBreaks <- c(-MinMax4ColBreaks,-6*MinMax4ColBreaks,-3*MaxQuartile4ColBreaks,-MaxQuartile4ColBreaks,-50,
#                    50,MaxQuartile4ColBreaks,3*MaxQuartile4ColBreaks,6*MinMax4ColBreaks,MinMax4ColBreaks)
#png(file = "kk.png")
plot(SignificantAnomalyRaster, useRaster=FALSE,
  col=  DivCols, breaks=DivColorBreaks)
#dev.off()

setwd(DirOfMODKML)
kml_open("SignificantAnomalyRaster.kml")
kml_layer.Raster(SignificantAnomalyRaster, plot.legend = TRUE, metadata = NULL, SignificantAnomalyRaster, 
                 png.width = ncol(SignificantAnomalyRaster), png.height = nrow(SignificantAnomalyRaster), 
                 min.png.width = ncol(SignificantAnomalyRaster)*LevelOfDetail,#usually >800 
                 colour_scale =  SAGA_pal[[1]],#DivCols,# breaks=DivColorBreaks, #buscar color intuitivo para la vieja marron o rojo a verde
                 raster_name="SignificantAnomalyRaster.png")    
kml_close("SignificantAnomalyRaster.kml")
setwd(GlobalDirectory)


## Necesitamos que los raster estÃ©n proyectados
projection(NewDateRaster) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"           
projection(HistLowerConfLastImgRaster) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"           
projection(HistUpperConfLastImgRaster) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"           
NewDateRaster <- reproject(NewDateRaster)
HistLowerConfLastImgRaster <- reproject(HistLowerConfLastImgRaster)
HistUpperConfLastImgRaster <- reproject(HistUpperConfLastImgRaster)

# Anomalias significativas como clasificacion en Negativas, Cero y Positivas
SignificantAnomalyRasterClass <- (-1)*(NewDateRaster < HistLowerConfLastImgRaster)+
                                      (NewDateRaster > HistUpperConfLastImgRaster)
# -1: significant negative anomalies, 0: within CI, 1: significant positive anomalies
projection(SignificantAnomalyRasterClass) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"           

SignificantAnomalyRasterClass_rc <- cut(SignificantAnomalyRasterClass, breaks=c(-2, -0.1, 0.1, +2))
SignificantAnomalyRasterClass_rc <- as.factor(SignificantAnomalyRasterClass_rc)

setwd(DirOfMODKML)
kml_open("SignificantAnomalyRasterClass.kml")
kml_layer.RasterFactor(SignificantAnomalyRasterClass_rc, plot.legend = TRUE, 
                 colour_scale = DivColPalette(nrow(levels(SignificantAnomalyRasterClass_rc)[[1]])), #buscar color intuitivo para la vieja marron o rojo a verde
                 raster_name="SignificantAnomalyRasterClass.png", 
                 labels = c("menos uno", "cero", "mas uno"))    
kml_close("SignificantAnomalyRasterClass.kml")
setwd(GlobalDirectory)

# Step 23. Nuevos m?ximos y m?nimos extremos hist?ricos
#
# M?ximo hist?rico correspondiente con la fecha mas reciente
pattMax <- paste("*max","_JulDay_",NewDateFilenameJulianDay,"\\.asc$", sep="")
HistMaximLastImgFilename<- list.files(path=DirOfMODStats, pattern = pattMax, full.names = TRUE)
HistMaximLastImgRaster <- raster(read.asciigrid(HistMaximLastImgFilename, as.image = TRUE, plot.image = TRUE,
                                 colname = HistMaximLastImgFilename, proj4string = CRS(as.character(NA))))

# M?nimo hist?rico correspondiente con la fecha mas reciente
pattMin <- paste("*min","_JulDay_",NewDateFilenameJulianDay,"\\.asc$", sep="")  # patr?n para seleccionar el estad?stico del m?nimo hist?rico
HistMinLastImgFilename<- list.files(path=DirOfMODStats, pattern = pattMin, full.names = TRUE)
HistMinLastImgRaster <- raster(read.asciigrid(HistMinLastImgFilename, as.image = TRUE, plot.image = TRUE,
                               colname = HistMinLastImgFilename, proj4string = CRS(as.character(NA))))

NewHistXtremeRaster  <- (NewDateRaster < HistMinLastImgRaster)*
                        (NewDateRaster - HistMinLastImgRaster)+
                        (NewDateRaster > HistMaximLastImgRaster)*
                        (NewDateRaster - HistMaximLastImgRaster)
projection(NewHistXtremeRaster) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"           
hist(NewHistXtremeRaster)
summary(NewHistXtremeRaster[NewHistXtremeRaster>0])
summary(NewHistXtremeRaster[NewHistXtremeRaster<0])
plot(NewHistXtremeRaster, useRaster=FALSE,
     col=  DivCols, breaks=DivColorBreaks)

setwd(DirOfMODKML)
kml_open("NewHistXtremeRaster.kml")
kml_layer.Raster(NewHistXtremeRaster, plot.legend = TRUE, metadata = NULL, NewHistXtremeRaster, 
                 png.width = ncol(NewHistXtremeRaster), png.height = nrow(NewHistXtremeRaster), 
                 min.png.width = ncol(NewHistXtremeRaster)*LevelOfDetail,#usually >800 
                 colour_scale =  SAGA_pal[[1]],#DivCols,# breaks=DivColorBreaks, #buscar color intuitivo marron o rojo a verde
                 raster_name="NewHistXtremeRaster.png")    
kml_close("NewHistXtremeRaster.kml")
setwd(GlobalDirectory)


projection(HistMinLastImgRaster) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"           
projection(HistMaximLastImgRaster) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"           
HistMinLastImgRaster <- reproject(HistMinLastImgRaster)
HistMaximLastImgRaster <- reproject(HistMaximLastImgRaster)

NewHistXtremeRasterClass <- (-1)*(NewDateRaster < HistMinLastImgRaster)+
                                 (NewDateRaster > HistMaximLastImgRaster)
# -1: significant negative anomalies, 0: within CI, 1: significant positive anomalies
projection(NewHistXtremeRasterClass) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"           
NewHistXtremeRasterClass$rc <- cut(NewHistXtremeRasterClass, breaks=c(-2, -0.1, 0.1, +2))
NewHistXtremeRasterClass$rc <- as.factor(NewHistXtremeRasterClass$rc)

NewHistXtremeRasterClass <- 
NewHistXtremeRasterClass@data@isfactor <- TRUE
NewHistXtremeRasterClass@legend@names <- as.character(c("Neg", "0", "Pos"))
hist(NewHistXtremeRasterClass$rc)
plot(NewHistXtremeRasterClass$rc,useRaster=FALSE,
     col=  DivCols, breaks=DivColorBreaks)
setwd(DirOfMODKML)
kml_open("NewHistXtremeRasterClass.kml")
kml_layer.RasterFactor(NewHistXtremeRasterClass$rc, plot.legend = TRUE, 
                       colour_scale = DivColPalette(nrow(levels(SignificantAnomalyRasterClass$rc)[[1]])), #buscar color intuitivo para la vieja marron o rojo a verde
                       raster_name="NewHistXtremeRasterClass.png", 
                       labels = c("Neg", "0", "Pos"))    
kml_close("NewHistXtremeRasterClass.kml")
setwd(GlobalDirectory)

#======================================================================================================================
## M?DULO 7: GUARDAR LA VARIABLE DE FECHAS DISPONIBLES EN EL FICHERO "AlreadyProcessed", SOBREESCRIBIENDO EL 
#CONTENIDO DEL FICHERO
#======================================================================================================================

# Step 24.Guardar el archivo con las fechas disponibles para la siguiente consulta y descarga.
AlreadyProcessed <- AvailableDatesInNasa
save(AlreadyProcessed, file=fileOfAlreadyProcessed)
