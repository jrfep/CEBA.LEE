##https://github.com/ropensci/rcrossref
##~/.Renviron con la línea crossref_email= "jose.ferrer@provitaonline.org"

##altmetrics api...

##R --vanilla
require(textcat)
require(rAltmetric)
require(RJSONIO)
require(rcrossref)
require(xml2)

mi.key <- ""

##require(ROpenOffice)
##RLEpubs <- read.ods("~/Descargas/RLE Publications - 2018 List.ods")
##RLEpubs <- read.csv("~/Descargas/RLE_Publications.csv")
##
## https://www.iucnredlist.org/resources/list
## http://www.keybiodiversityareas.org/publications

RLEpubs <- read.csv("~/mi.git/CEBA.LEE/data/IUCN_KP_Publications.csv")
mis.dois <- unique(as.character(RLEpubs$DOI))
mis.dois <- mis.dois[mis.dois!=""]


slc.RLE <- mis.dois %in% subset(RLEpubs,RLE)$DOI
slc.RLTS <- mis.dois %in% subset(RLEpubs,RLTS)$DOI
slc.KBA <- mis.dois %in% subset(RLEpubs,KBA)$DOI

table(slc.RLTS | slc.KBA, slc.RLE)

lst.qry <- vector("list",length(mis.dois))

art.info <- data.frame()
k <- 0

for (midoi in mis.dois) {
    k <- k+1

    qry <- try(altmetrics(doi=midoi,apikey=mi.key))
    if (any(class(qry) %in% "try-error")) {
        lst.qry[[k]] <- "error"
    } else {
        if (is.null(qry)) {
            lst.qry[[k]] <- NA
        } else {
            lst.qry[[k]] <- qry

            art.info <- rbind(art.info,
                              data.frame(doi=midoi,
                                         cr_cites=cr_citation_count(doi = midoi),
                                         altscore=qry$score,
                                         date=ifelse(is.null(qry$published_on),NA,1970+(qry$published_on/(365*60*60*24))),
                                         feeds=ifelse(is.null(qry$cited_by_feeds_count),NA,qry$cited_by_feeds_count),
                                         gplus=ifelse(is.null(qry$cited_by_gplus_count),NA,qry$cited_by_gplus_count),
                                         msm=ifelse(is.null(qry$cited_by_msm_count),NA,qry$cited_by_msm_count),
                                         policies=ifelse(is.null(qry$cited_by_policies_count),NA,qry$cited_by_policies_count),
                                         posts=ifelse(is.null(qry$cited_by_posts_count),NA,qry$cited_by_posts_count),
                                         rh=ifelse(is.null(qry$cited_by_rh_count),NA,qry$cited_by_rh_count),
                                         tweeters=ifelse(is.null(qry$cited_by_tweeters_count),NA,qry$cited_by_tweeters_count),
                                         wikipedia=ifelse(is.null(qry$cited_by_wikipedia_count),NA,qry$cited_by_wikipedia_count),
                                         fbwalls=ifelse(is.null(qry$cited_by_fbwalls_count),NA,qry$cited_by_fbwalls_count),
                                         accounts=ifelse(is.null(qry$cited_by_accounts_count),NA,qry$cited_by_accounts_count),
                                         connotea=ifelse(is.null(qry$readers.connotea),NA,qry$readers.connotea),
                                         citeulike=ifelse(is.null(qry$readers.citeulike),NA,qry$readers.citeulike),
                                         mendeley=ifelse(is.null(qry$readers.mendeley),NA,qry$readers.mendeley)))
        }       
    }
}

art.info$RLE <- art.info$doi %in% subset(RLEpubs,RLE)$DOI
art.info$RLTS <- art.info$doi %in% subset(RLEpubs,RLTS)$DOI
art.info$KBA <- art.info$doi %in% subset(RLEpubs,KBA)$DOI

plot(cr_cites~date,art.info,log="y",col=1+(RLE))
boxplot(sqrt(altscore)~RLE,art.info,notch=T)

plot(altscore~date,art.info,log="y",col=1+(RLE))
with(art.info,aggregate(msm,list(RLE),sum,na.rm=T))


dim(art.info)

boxplot(ccs~slc.RLE)

table(art.info$RLE)
with(art.info,aggregate((2020-date),list(RLE),sum,na.rm=T))

with(art.info,aggregate(data.frame(date),list(RLE),mean,na.rm=T))
with(art.info,aggregate(data.frame(cr_cites,altscore,msm,policies,posts,accounts,wikipedia,
                                   readers=as.numeric(as.character(citeulike))+as.numeric(as.character(connotea))+as.numeric(as.character(mendeley))),list(RLE),sum,na.rm=T))

## se podría hacer algo como una red de coautores... para ver el tamaño y las interconexiones más importantes... tal vez


## rh is research highlights
##unique(unlist(lapply(lst.qry,function(x) grep("cited_by",names(x),value=T))))


counts <- c("feeds","gplus","msm","policies","posts","rh","tweeters","wikipedia","fbwalls","accounts")
readers <- c("mendeley","connotea","citeulike")
ttls <- matrix(nrow=3,ncol=length(counts)+length(readers))
colnames(ttls) <- c(counts,readers)
rownames(ttls) <- c("RLE","RLTS","KBA")
for (k in counts) {
    ttls["RLE",k] <- sum(as.numeric(unlist(as.vector(sapply(lst.qry[slc.RLE],function(x) x[sprintf("cited_by_%s_count",k)])))),na.rm=T)
    ttls["RLTS",k] <- sum(as.numeric(unlist(as.vector(sapply(lst.qry[slc.RLTS],function(x) x[sprintf("cited_by_%s_count",k)])))),na.rm=T)
    ttls["KBA",k] <- sum(as.numeric(unlist(as.vector(sapply(lst.qry[slc.KBA],function(x) x[sprintf("cited_by_%s_count",k)])))),na.rm=T)
}
for (k in readers) {
   ttls["RLE",k] <- sum(as.numeric(unlist(as.vector(sapply(lst.qry[slc.RLE],function(x) x[sprintf("readers.%s",k)])))),na.rm=T)
    ttls["RLTS",k] <- sum(as.numeric(unlist(as.vector(sapply(lst.qry[slc.RLTS],function(x) x[sprintf("readers.%s",k)])))),na.rm=T)
    ttls["KBA",k] <- sum(as.numeric(unlist(as.vector(sapply(lst.qry[slc.KBA],function(x) x[sprintf("readers.%s",k)])))),na.rm=T)

}

aggregate(ccs,list(slc.RLE),median,na.rm=T)
aggregate(ccs,list(slc.RLE),mad,na.rm=T)

hist(ccs[slc.RLE],breaks=seq(0,2000,by=100))
hist(ccs[!slc.RLE],breaks=seq(0,2000,by=100))


asc <- unlist(lapply(lst.qry,function(x) ifelse(class(x) %in% "altmetric",x$score,NA)))
