##R --vanilla
gis.data <- "/media/jferrer/Elements/SIG/gisdata/censos/CNE/"
setwd(gis.data)
system("wget 'http://www.cne.gov.ve/web/registro_electoral_descarga/abril2012/nacional.php'")
system("grep zip nacional.php > REestados.txt")
system("wget --continue --input-file=REestados.txt --force-html --base=http://www.cne.gov.ve/web/registro_electoral_descarga/abril2012/")
system("grep 'Ver Municipios' nacional.php > REmunicipios.txt")
system("wget --continue --input-file=REmunicipios.txt --force-html --base=http://www.cne.gov.ve/web/registro_electoral_descarga/abril2012/")

for (k in c(1:24,99)) {
    system(sprintf("mkdir estado%02d",k))
    system(sprintf("grep zip 'municipal.php?e=%s' > estado%02d/zip.txt",k,k))
    system(sprintf("grep 'Ver Parroquias' 'municipal.php?e=%s' > estado%02d/parroquias.txt",k,k))

}


for (k in c(1:24,99)) {
    setwd(sprintf("%sestado%02d",gis.data,k))
    for (fl in dir(pattern=".txt$")) {
        system(sprintf("wget --continue --input-file=%s --force-html --base=http://www.cne.gov.ve/web/registro_electoral_descarga/abril2012/",fl))
    }
}

##grep MUNICIPIO estado*/parroquial.php* > lista.municipios
##grep Descargue estado*/parroquial.php* > lista.parroquias



wget 'http://www.cne.gov.ve/web/registro_electoral_descarga/abril2012/municipal.php?e=22'
grep zip municipal.php\?e\=22 




require(ROpenOffice)

z <- read.ods("/media/jferrer/Elements/SIG/gisdata/censos/CNE/ListaParroquiasCNE.ods")

Z <- merge(merge(z[[1]],z[[3]],by="edo"),z[[2]],by=c("edo","mun"))


x <- read.csv("/media/jferrer/Elements/SIG/gisdata/censos/CNE/nacional.csv",as.is=T)
y <- read.csv("/media/jferrer/Elements/SIG/gisdata/censos/CNE/centros.csv",sep=";")

x$estado <- y$cod_estado[match(x$cod_centro,y$cod_centro)]

x$a1 <- sapply(x$primer_apellido,function(x) strsplit(x," ")[[1]][1])
x$a2 <- sapply(x$primer_apellido,function(x) strsplit(x," ")[[1]][2])
x$a2[!(x$segundo_apellido %in% "")] <- x$segundo_apellido[!(x$segundo_apellido %in% "")]

apellidos <- gsub("^DE ","",c(x$a1,x$a2))
estados <- z[[1]]$estado[match(c(x$estado,x$estado),z[[1]]$edo)]

ss <- (nchar(apellidos) %in% 0:1) |(nchar(apellidos)==2 & grepl("\\.",apellidos)) | apellidos %in% "" | estados %in% "EMBAJADA" 
table(ss)

apellidos <- apellidos[!ss]
estados <- estados[!ss]

mtz <- table(apellidos,as.character(estados))
head(mtz)
tail(mtz)

m0 <- mtz[rowSums(mtz)>ncol(mtz),]
m1 <- rbind(mtz[rowSums(mtz)>1000,],
            colSums(mtz[rowSums(mtz)<=1000,]))
head(m1)
tail(m1)
m2 <- apply(m1,2,function(x) x/sum(x))
m3 <- apply(m1,1,function(x) x/sum(x))
zulia <- data.frame(freq.Zulia=m1[,"ZULIA"],freq.Vzla=rowSums(m1),p.Zulia=m3["ZULIA",])
zulia <- subset(zulia,freq.Zulia>20000 & p.Zulia>(2*sum(m1[,"ZULIA"]))/sum(m1))
zulia[order(zulia$p.Zulia),]

require(cluster)
require(vegan)

d0 <- daisy(t(m2[-nrow(m2),]))
h0 <- hclust(d0)
cutree(h0,k=5)
table(cutree(h0,k=5))
sort(rowSums(m1[,cutree(h0,k=5) %in% 4,drop=F]))
sort(rowSums(m1[,"ZULIA",drop=F]))

plot(h0)

## k :=
plot(fanny(d0,k=9))

plot(hclust(d0))

plot(rda(m2))
screeplot(rda(m2))

mfit <- metaMDS(m2)

plot(mfit,type="n")
points(mfit,col="grey77")
points(mfit,"species",col=2,pch=3)
 text(mfit,"species",col="blue",cex=.7,srt=90)

##REF https://doi.org/10.1080/13658816.2011.591291
##https://doi.org/10.1016/j.geoforum.2011.02.001
## https://www.jstor.org/stable/41466196
##https://doi.org/10.1016/j.eeh.2014.12.002
##https://doi.org/10.1002/(SICI)1520-6300(200005/06)12:3<352::AID-AJHB5>3.0.CO;2-S # venezuela...
##https://www.tandfonline.com/doi/abs/10.1179/175622708X332860 ## spain
require(raster)
pq <- shapefile("/media/jferrer/Elements/SIG/gisdata/vectorial/uniSIG/parroq.shp")

for (ee in unique(pq@data$ESTADO)) {
    dd <- subset(pq,ESTADO %in% ee)@data
    for (mm in unique(dd$MUNICIPIO)) {
        dx <- subset(dd,MUNICIPIO %in% mm)
        for (pp in unique(dx$PARROQUIA)) {
            dz <- subset(dx,PARROQUIA %in% pp)
            if (nrow(Z[Z$estado %in% ee & grepl(mm,Z$municipio) & grepl(pp,Z$Parroquia),])==1) {
                Z[Z$estado %in% ee & grepl(mm,Z$municipio) & grepl(pp,Z$Parroquia),"ID"] <- unique(dz$ID)
            }   
        }
    }   
}


table(substr(y$centro_nuevo,0,6) %in% pq@data$ident)
table(x$cod_centro %in% y$cod_centro)
wget 'http://www.cne.gov.ve/web/registro_electoral_descarga/abril2012/municipal.php?e=22'


y <- read.csv("/media/jferrer/Elements/SIG/gisdata/censos/CNE/centros.csv",sep=";")
x <- read.csv("/media/jferrer/Elements/SIG/gisdata/censos/CNE/nacional.csv")

ss <- grep(" ",x$primer_apellido)
 dim(subset(x,primer_apellido %in% c("FERRER") | segundo_apellido %in% c("FERRER") ))
dim(subset(x,primer_apellido %in% c("PARIS") | segundo_apellido %in% c("PARIS") ))
head(rev(sort(table(x$primer_nombre))),20)

subset(x,primer_apellido %in% c("FERRER") | segundo_apellido %in% c("PARIS") )
subset(x,primer_apellido %in% c("FERRER") & segundo_apellido %in% c("PARIS") )
