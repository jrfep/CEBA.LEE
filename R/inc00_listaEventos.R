##source("inc00_listaEventos.R")

## leemos el archivo "lista" y lo grabamos como un objeto "fts"
fts <- data.frame()
for (j in dir(pattern="^lista.")) {
    fts <- rbind(fts,read.table(j,sep="\t",stringsAsFactors=F))
}
## colocamos nombres a las columnas de "fts", ordenamos la tabla
colnames(fts) <- c("filename","createdate","Directory","FileSize","ImageDescription","Make","CameraModelName","ShutterSpeedValue","ApertureValue","ExposureCompensation","LightSource","Flash","Aperture","ImageSize","ShutterSpeed")



## guardamos datos de la fecha y hora en un formato apropiado
fts$fecha <- chron(dates.=substr(fts$createdate,1,10),
                   times.=substr(fts$createdate,12,100),
                   format=c(dates="y:m:d", times="h:m:s"),
                   out.format=c(dates="Y-m-d",times="h:m:s"))
fts$hora <- substr(fts$createdate,12,100)
fts <- subset(fts,!is.na(fts$fecha))


## añadimos una columna con el codigo de la camara
fts$camara <- sapply(fts$Directory,function(x) paste(strsplit(x,"/")[[1]][5:7],collapse="."))
fts$bloque <- sapply(fts$Directory,function(x) strsplit(x,"/")[[1]][5])
fts$periodo <- sapply(fts$Directory,function(x) strsplit(x,"/")[[1]][6])

fts <- fts[order(fts$camara,fts$createdate),]

## agrupamos la lista de fotos en eventos, un "evento" es una secuencia de fotos separadas por menos de un minuto, de esta forma es posible analizar los datos de forma mas ordenada
fts$ev <- NA
fts$ev[1] <- 1
for (k in 2:nrow(fts)) {
    if (diff(fts$fecha[(k-1):k])<0.001) {
         fts$ev[k] <- fts$ev[k-1]
   } else {
    fts$ev[k] <- fts$ev[k-1]+1
   }
}

## para cada camara colocamos una numeracion independiente

for (cc in unique(fts$camara)) {
    ss <- fts$camara %in% cc
    fts$ev[ss] <- fts$ev[ss]-min(fts$ev[ss])+1
}

## el codigo del evento contiene el codigo de la camara, seguido por una "E" y el numero secuencial de eventos de esa camara

fts$evento <- paste(fts$camara,fts$ev,sep="_E")

length(unique(fts$evento))

## ahora hacemos una tabla con los datos resumidos por evento y lo grabamos en el archivo "tablaEventos.csv", ese archivo lo leemos despues en un programa de hojas de calculo y le agregamos la informacion adicional
dts <- data.frame()
for (mie in unique(fts$evento)) {
    SS <- subset(fts,evento==mie)

    dts <- rbind(dts,data.frame(camara=unique(SS$camara),
                                evento=unique(SS$evento),
                                fotos=nrow(SS),
                                dia=min(days(SS$fecha)),
                                mes=min(months(SS$fecha)),
                                año=min(years(SS$fecha)),
                                hora.ini=min(SS$hora),
                                hora.fin=max(SS$hora),
                                fecha1=min(SS$fecha),
                                fecha2=max(SS$fecha),
                                duracion=diff(range(SS$fecha))))

}

write.csv(file="tablaEventosGranSabanaIzza.csv",dts)

table(fts$bloque,fts$periodo)

## creat tabla en mysql
require(RMySQL)
archivo.sql <- "~/lib/sql/acceso.cnf"
mi.sql <- dbDriver("MySQL")
c.nm1 <- dbConnect(mi.sql, group="MV",default.file=archivo.sql)
tmp1 <- dbGetQuery(c.nm1,"SET NAMES 'utf8'")
tmp1 <- dbGetQuery(c.nm1,"SET CHARACTER SET utf8")
##dbWriteTable(c.nm1,"fotos",fts)
prg <- "SELECT bloque,periodo,evento,Directory,filename FROM fotos WHERE bloque = 'B02' && periodo = 'P2'"
rsp <- dbGetQuery(c.nm1,prg)
dbDisconnect(c.nm1)

