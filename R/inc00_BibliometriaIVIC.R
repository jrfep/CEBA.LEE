##R --vanilla
require(RJSONIO)

###
## Código en R básico utilizado para análisis bibliométrico de publicaciones
## del IVIC: http://jrferrerparis.info/profesional/r-scripts/produccion-ivic-60-anos/
###

## con este:
qry.JP <- fromJSON("http://api.crossref.org/works?query.author=jon+paul+rodriguez&query.affiliation=Instituto+Venezolano+Investigaciones+Cientificas+Caracas+Miranda+Venezuela&rows=1000")

## Comparar este resultado:
qry.IVIC <- fromJSON("http://api.crossref.org/works?query.affiliation=Instituto+Venezolano+Investigaciones+Cientificas+Caracas+Miranda+Venezuela&rows=1000")

qry <- c()
for (k in 1:10) {
    qry0 <- fromJSON("http://api.crossref.org/works?query.affiliation=Instituto+Venezolano+Investigaciones+Cientificas+Caracas+Miranda+Venezuela&sample=100")
    qry <- c(qry,qry0$message$items)
}
length(qry) 

doi1 <- sapply(qry,function(x) x$DOI)
doi2 <- sapply(qry.IVIC$message$items,function(x) x$DOI)

qry <- c(qry,qry.IVIC$message$items,qry.JP$message$items)
aa <- data.frame()
bb <- data.frame()

for (w in 1:length(qry)) {
    z <- qry[[w]]
    if (!z$DOI %in% bb$DOI) {
        obra <- ifelse(is.null(z$"container-title"),NA,z$"container-title")
        editorial <- ifelse(is.null(z$publisher),NA,z$publisher)
        fecha <- ifelse(is.null(z$"published-print"),
                        ifelse(is.null(z$"published-online"),
                               ifelse(is.null(z$"deposited"),
                                      NA,z$deposited$`date-parts`[[1]][1]),
                               z$"published-online"$`date-parts`[[1]][1]),
                        z$"published-print"$"date-parts"[[1]][1])

	bb <- rbind(bb,data.frame(DOI=z$DOI,tipo=z$type,fecha,citas=z$"is-referenced-by-count",titulo=iconv(z$title,"latin1","utf8"),obra,editorial))
	y <- z$author
	for (k in 1:length(y)) {
	    apellidos <- iconv(y[[k]]$family,"latin1","utf8")
	    if (length(apellidos)>0) {
                iniciales <- ifelse(is.null(y[[k]]$given),NA,sapply(iconv(y[[k]]$given,"latin1","utf8"),function(x)
                    paste(substr(strsplit(x," ")[[1]],1,1),collapse=" ")))
		afiliacion <- ifelse(is.null(y[[k]]$affiliation),NA,iconv(unlist(y[[k]]$affiliation),"latin1","utf8"))
		aa <- rbind(aa,
                            data.frame(DOI=z$DOI,
                                       orig.given=ifelse(is.null(y[[k]]$given),NA,iconv(y[[k]]$given,"latin1","utf8")),
                                       orig.family=iconv(y[[k]]$family,"latin1","utf8"),
                                       iniciales,apellidos,
                                       nombre=toupper(paste(iniciales,apellidos)),
                                       afiliacion,row.names = NULL))
            }
        }
    }
}

aa$vzla <- (grepl("venezuela",tolower(gsub("[, ]+","",aa$afiliacion))))
aa$IVIC <- grepl("institutovenezolanode[li]nv[e]stiga[tc]i[oó]n[e]sci[ée]nt",tolower(gsub("[, ]+","",aa$afiliacion))) | grepl("institutovenezolanodeinvstigacionescientificas",tolower(gsub("[, ]+","",aa$afiliacion))) | grepl("venezuelaninstituteofscientificresearch",tolower(gsub("[, ]+","",aa$afiliacion))) | grepl("ivic",tolower(gsub("[, ]+","",aa$afiliacion)))


table(aa$vzla)
table(aa$IVIC)
rsm.autores <- with(aa,
 aggregate(data.frame(IVIC=IVIC,vzla=vzla,
   otro=!IVIC),list(DOI=DOI),sum))

dim(rsm.autores)
dim(subset(rsm.autores,IVIC>0))


qry.IVIC$message$"total-results"
qry.JP$message$"total-results"


