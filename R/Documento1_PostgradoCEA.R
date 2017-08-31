### Código R modificado de http://dx.doi.org/10.6084/m9.figshare.3394789
### Encoding: UTF-8

###################################################
### code chunk number 1: Documento1_PostgradoCEA.Rnw:70-84
###################################################
require(xml2)
require(rvest)
require(plotrix)

###################################################
### code chunk number 3: Descargar un archivo
###################################################
output <- "../output"

###################################################
### code chunk number 3: Descargar un archivo
###################################################
k <- 1973
if (!file.exists(sprintf("EgresadosAño%s.html",k)))
    download.file(sprintf("http://cea.ivic.gob.ve/?accion=consultaregresados&param=%s",k),sprintf("EgresadosAño%s.html",k))


###################################################
### code chunk number 4: Descargar varios archivos
###################################################
for (k in 1973:2017) {
    dst.file <- sprintf("EgresadosAño%s.html",k)
    if (!file.exists(dst.file))
        download.file(sprintf("http://cea.ivic.gob.ve/?accion=consultaregresados&param=%s",k),dst.file)

}



###################################################
### code chunk number 5: Verificar archivos descargados
###################################################
dir(pattern="EgresadosAño")



###################################################
### code chunk number 6: Leer archivo html
###################################################
require(rvest)

   pg <- read_html(sprintf("EgresadosAño%s.html",k))



###################################################
### code chunk number 7: Ubicar tablas en archivo html
###################################################
 html_nodes(pg,"table")


###################################################
### code chunk number 8: Verificar contenido de las tablas
###################################################
str(lapply(html_nodes(pg,"table"),xml_text))


###################################################
### code chunk number 9: Leer contenido de la tabla
###################################################
  rr <- html_table(html_nodes(pg,"table")[5],header=T,fill=T)[[1]]
  head(rr)



###################################################
### code chunk number 10: Combinar tablas de cada año
###################################################
rm(lista.CEA)
for (k in 1973:2017) {
    pg <- read_html(sprintf("EgresadosAño%s.html",k))
    rr <- html_table(html_nodes(pg,"table")[5],header=T,fill=T)[[1]]
    if (nrow(rr)>0) {
        rr$Año <- k
        if (!exists("lista.CEA")) {
            lista.CEA <- rr
        } else {
            lista.CEA <- rbind(lista.CEA[,colnames(rr)],rr)
        }
    }
}



###################################################
### code chunk number 11: Estructura del objeto
###################################################
str(lista.CEA)


###################################################
### code chunk number 12: Egresados por título otorgado
###################################################
table(lista.CEA$"Grado Académico")


###################################################
### code chunk number 13: Egresados por sexo
###################################################
table(lista.CEA$Sexo,useNA="always")


###################################################
### code chunk number 14: Normalización de la columna
###################################################
lista.CEA$Sexo <- tolower(lista.CEA$Sexo)
table(lista.CEA$Sexo,useNA="always")



###################################################
### code chunk number 15: Egresados por nacionalidad
###################################################
table(lista.CEA$Nacionalidad,useNA="always")


###################################################
### code chunk number 16: Egresados por áreas de estudio
###################################################
table(lista.CEA$"Area de Estudio")


###################################################
### code chunk number 17: Distancia y clasificación jerárquica
###################################################
d0 <- adist(lista.CEA$"Area de Estudio")
h0 <- hclust(as.dist(d0))


###################################################
### code chunk number 18: Ejemplo de aplicación de dist. de Levenshtein
###################################################
table(lista.CEA$"Area de Estudio",cutree(h0,h=3))[18:30,c(4:15,23)]



###################################################
### code chunk number 19: Velista.CEAión corregida del área de estudio
###################################################
c0 <- cutree(h0,h=3)
for (j in unique(c0)) {
    lista.CEA$area.corregida[c0 %in% j] <- names(  sort(table(lista.CEA[c0 %in% j,"Area de Estudio"]),decreasing=TRUE)[1])

}


###################################################
### code chunk number 20: Verificación del resultado
###################################################
table(lista.CEA$area.corregida)



###################################################
### code chunk number 21: Corregir tres errores en la tabla
###################################################
lista.CEA[grep("METALURGICA",lista.CEA$area.corregida),"area.corregida"] <- "METALURGICA Y CIENCIA DE LOS MATERIALES"
lista.CEA[lista.CEA$"Area de Estudio" %in% "BIOLOGIA","area.corregida"] <- "BIOLOGIA"
lista.CEA[lista.CEA$area.corregida %in% c("ESTUDIOS SOCIALES DE LA CIENCIA","ESTUDIOS DE LA CIENCIAS"),"area.corregida"] <- "ESTUDIOS SOCIALES DE LA CIENCIA"


###################################################
### code chunk number 22: EgresadosPorFecha
###################################################
dts <- table(lista.CEA$Año,lista.CEA$area.corregida)
ss <- colSums(dts)>19

svg(file=sprintf("%s/Egresados_IVIC_CEA.svg",output),width=8,height=8)
par(mar=c(5,4,3,7))

matplot(as.numeric(rownames(dts)), main="Postgrados del Centro de Estudios Avanzados, IVIC",
        apply(dts,2,cumsum)[,ss & !(colnames(dts) %in% "ARTICULO 9")],
                            type="l",lty=1,lwd=3,col=rainbow(14),
                            xlab="Año",ylab="Número acumulado de egresados por postgrado") 
axis(4,at=colSums(dts)[ss & !(colnames(dts) %in% "ARTICULO 9")],
     lab=sub("BIOLOGIA DE LA REPRODUCCION","BIO. REPRO.",
         sub("ESTUDIOS SOCIALES DE LA","EST. SOC.",
             colnames(dts)[ss & !(colnames(dts) %in% "ARTICULO 9")])),
     cex.axis=.5,las=2)

if (require(png) & exists("logo"))
    rasterImage(logo, 1975, 170, 1990, 250)

lines(as.numeric(rownames(dts)),cumsum(rowSums(dts[,!ss])),lty=3)
axis(4,at=sum(rowSums(dts[,!ss])),"Otras",cex.axis=.75,las=2)
dev.off()

###################################################
### code chunk number 23: PiramideEgresados
###################################################
require(plotrix)


piramide <- with(lista.CEA,tapply(Estudiante,list(Año,tolower(Sexo)),length))
pyramid.plot(piramide[,"femenino"],piramide[,"masculino"],
             labels=rownames(piramide),gap=4,##xlim=c(-35,-1,1,35),
             lxcol="orangered",rxcol="slateblue",
             top.labels=c("Femenino","Año","Masculino"),unit="Estudiantes",
             main="Graduados del Centro de Estudios Avanzados",labelcex=.65)



###################################################
### code chunk number 24: Documento1_PostgradoCEA.Rnw:310-311
###################################################
table(lista.CEA$"Grado Académico",lista.CEA$Sexo)


###################################################
### code chunk number 24: vamos a descargar ahora los datos del PEII
###################################################


PEII <- data.frame(id=c(120,115,110,108,100,109,102,111,117,118,99,113,116,98,103,106,121,101,104,105,97,112,119,107,114),
                   estado=c("Yaracuy","Portuguesa","Lara","Falcon",
                       "Apure","Guarico",
                       "Barinas","Merida","Tachira","Trujillo",
                       "Anzoategui","Monagas","Sucre",
                       "Amazonas","Bolivar","Delta Amacuro",
                       "Zulia",
                       "Aragua","Carabobo","Cojedes",
                       "Distrito Capital","Miranda","Vargas",
                       "Dependencias Federales","Nueva Esparta"))

for (k in 1:nrow(PEII)){
    dst.file <- sprintf("PEII_%s.html",gsub(" ","_",PEII$estado[k]))
    if (!file.exists(dst.file))
        download.file(sprintf("http://www.oncti.gob.ve/index.php/component/content/article?id=%s",PEII$id[k]),dst.file)
}

pg <- read_html(sprintf("PEII_%s.html",gsub(" ","_",PEII$estado[k])))

 html_nodes(pg,"table")
   rr <- html_table(html_nodes(pg,"table")[2],header=T,fill=T)[[1]]

rm(lista.PEII)
for (k in 1:nrow(PEII)) {
    pg <- read_html(sprintf("PEII_%s.html",gsub(" ","_",PEII$estado[k])))

    for (j in 1:length(html_nodes(pg,"table"))) {
        rr <- html_table(html_nodes(pg,"table")[j],header=T,fill=T)[[1]]
        if (any(c("NOMBRE","NOMBRES") %in% colnames(rr))) {
            if (nrow(rr)>0) {
                rr$estado <- PEII$estado[k]
                if (!exists("lista.PEII")) {
                    lista.PEII <- rr
                } else {
                    colnames(rr) <- colnames(lista.PEII)
                    lista.PEII <- rbind(lista.PEII,rr)
                }
            }
        }
    }
}

table(lista.PEII$estado)

table(subset(lista.PEII,CEDULA %in% lista.CEA$Cédula)$estado)

##¿306 o 280? existen varias personas con dos títulos (MSc y PhD)

table(lista.CEA$Cédula %in% lista.PEII$CEDULA)
require(RJSONIO)

cr.cea <- list()
autores.ttl <- data.frame()
for (j in 1:nrow(lista.CEA)) {
    print(j)
    buscar <- lista.CEA$Estudiante[j]
    
    bb <- strsplit(buscar," ")[[1]]
    
    crossref.1 <- fromJSON(sprintf("http://api.crossref.org/works?query=%s&rows=1000",gsub(" ","+",buscar)))
    
    autores <- data.frame()
    for (k in 1:1000) {
        aas <- unlist(sapply(crossref.1$message$items[[k]]$author,function(x) unlist(iconv(paste(x$family,x$given),"latin1","utf8"))))
        if (length(aas)>0) {
            autores <- rbind(autores,
                             data.frame(k,doi=crossref.1$message$items[[k]]$DOI,
                                        autor=as.character(aas),
                                        buscar=buscar))
        }
    }
    autores$m[gsub("-"," ",tolower(autores$autor)) %in% tolower(buscar)] <- "exacto"
    autores$m[gsub("-"," ",tolower(autores$autor)) %in% tolower(paste(bb[c(1,3)]))] <- "apellido nombre"
    autores$m[gsub("\\.","",gsub("-"," ",tolower(autores$autor))) %in% tolower(paste(bb[1],bb[2],substr(bb[3],1,1)))] <- "apellidos inicial"
    
    autores.ttl <- rbind(autores.ttl,subset(autores,!is.na(m)))
    
    cr.cea[[j]] <- crossref.1$message$items[subset(autores,!is.na(m))$k]
}
