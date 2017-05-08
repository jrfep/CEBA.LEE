### R code from vignette source '~/Dropbox/ceba/doc/100_Posgrado/Documento1_PostgradoCEA.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: Documento1_PostgradoCEA.Rnw:70-84
###################################################
require(xml2)
require(rvest)
require(plotrix)

paquetes <- (.packages())
paquetes <- paquetes[!(paquetes %in% c("stats", "graphics", "grDevices", "utils", "datasets", "methods", "base", "deldir", "DBI", "RMySQL"))]

luq <- function(x,contar.NA=FALSE) {
	if (contar.NA==F) {
	x <- x[!is.na(x)]
	}
 length(unique(x))
 }



###################################################
### code chunk number 2: citas paquetes
###################################################
cat(paste("\\texttt{",paquetes,"} \\citep{pqt::",paquetes,"}",sep="",collapse="; "))


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
rm(rs)
for (k in 1973:2017) {
    pg <- read_html(sprintf("EgresadosAño%s.html",k))
    rr <- html_table(html_nodes(pg,"table")[5],header=T,fill=T)[[1]]
    if (nrow(rr)>0) {
        rr$Año <- k
        if (!exists("rs")) {
            rs <- rr
        } else {
            rs <- rbind(rs[,colnames(rr)],rr)
        }
    }
}



###################################################
### code chunk number 11: Estructura del objeto
###################################################
str(rs)


###################################################
### code chunk number 12: Egresados por título otorgado
###################################################
table(rs$"Grado Académico")


###################################################
### code chunk number 13: Egresados por sexo
###################################################
table(rs$Sexo,useNA="always")


###################################################
### code chunk number 14: Normalización de la columna
###################################################
rs$Sexo <- tolower(rs$Sexo)
table(rs$Sexo,useNA="always")



###################################################
### code chunk number 15: Egresados por nacionalidad
###################################################
table(rs$Nacionalidad,useNA="always")


###################################################
### code chunk number 16: Egresados por áreas de estudio
###################################################
table(rs$"Area de Estudio")


###################################################
### code chunk number 17: Distancia y clasificación jerárquica
###################################################
d0 <- adist(rs$"Area de Estudio")
h0 <- hclust(as.dist(d0))


###################################################
### code chunk number 18: Ejemplo de aplicación de dist. de Levenshtein
###################################################
table(rs$"Area de Estudio",cutree(h0,h=3))[18:30,c(4:15,23)]



###################################################
### code chunk number 19: Versión corregida del área de estudio
###################################################
c0 <- cutree(h0,h=3)
for (j in unique(c0)) {
    rs$area.corregida[c0 %in% j] <- names(  sort(table(rs[c0 %in% j,"Area de Estudio"]),decreasing=TRUE)[1])

}


###################################################
### code chunk number 20: Verificación del resultado
###################################################
table(rs$area.corregida)



###################################################
### code chunk number 21: Corregir tres errores en la tabla
###################################################
rs[grep("METALURGICA",rs$area.corregida),"area.corregida"] <- "METALURGICA Y CIENCIA DE LOS MATERIALES"
rs[rs$"Area de Estudio" %in% "BIOLOGIA","area.corregida"] <- "BIOLOGIA"
rs[rs$area.corregida %in% c("ESTUDIOS SOCIALES DE LA CIENCIA","ESTUDIOS DE LA CIENCIAS"),"area.corregida"] <- "ESTUDIOS SOCIALES DE LA CIENCIA"


###################################################
### code chunk number 22: EgresadosPorFecha
###################################################
dts <- table(rs$Año,rs$area.corregida)
ss <- colSums(dts)>19
par(mar=c(5,4,0,7))

matplot(as.numeric(rownames(dts)),
        apply(dts,2,cumsum)[,ss & !(colnames(dts) %in% "ARTICULO 9")],
                            type="l",lty=1,lwd=3,col=rainbow(14),
                            xlab="Año",ylab="Acumulado de egresados") 
axis(4,at=colSums(dts)[ss & !(colnames(dts) %in% "ARTICULO 9")],
     lab=sub("BIOLOGIA DE LA REPRODUCCION","BIO. REPRO.",
         sub("ESTUDIOS SOCIALES DE LA","EST. SOC.",
             colnames(dts)[ss & !(colnames(dts) %in% "ARTICULO 9")])),
     cex.axis=.5,las=2)
lines(as.numeric(rownames(dts)),cumsum(rowSums(dts[,!ss])),lty=3)
axis(4,at=sum(rowSums(dts[,!ss])),"Otras",cex.axis=.75,las=2)


###################################################
### code chunk number 23: PiramideEgresados
###################################################
require(plotrix)
piramide <- with(rs,tapply(Estudiante,list(Año,tolower(Sexo)),length))
pyramid.plot(piramide[,"femenino"],piramide[,"masculino"],
             labels=rownames(piramide),gap=4,##xlim=c(-35,-1,1,35),
             lxcol="orangered",rxcol="slateblue",
             top.labels=c("Femenino","Año","Masculino"),unit="Estudiantes",
             main="Graduados del Centro de Estudios Avanzados",labelcex=.65)



###################################################
### code chunk number 24: Documento1_PostgradoCEA.Rnw:310-311
###################################################
table(rs$"Grado Académico",rs$Sexo)


