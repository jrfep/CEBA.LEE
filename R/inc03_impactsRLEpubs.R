##https://github.com/ropensci/rcrossref
##~/.Renviron con la línea crossref_email= "jose.ferrer@provitaonline.org"

##altmetrics api...

##R --vanilla
require(textcat)
require(rAltmetric)
require(RJSONIO)
require(rcrossref)
require(xml2)
##require(dplyr)
require(tidyverse)
require(gdata)


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

ref.info <- cr_works(dois = mis.dois,.progress="text")

## Intro en https://poldham.github.io/abs/crossref.html
ref.info$data %>% count(subject, sort = TRUE)
ref.info$data %>% separate_rows(subject, sep = ",") %>% 
  count(subject=trim(subject), sort = TRUE) -> subjects  # output subjects

library(ggplot2)
subjects %>% 
  ggplot2::ggplot(aes(subject, n, fill = subject)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip()

library(stringr)
library(ggplot2)
library(plotly)

ref.info$data$year <- as.numeric(substr(ref.info$data$issued,0,4))
ref.info$data %>% 
    count(year) %>% 
        ggplot(aes(x = year, y = n, group = 1)) +
                geom_line() -> out # data out

out


library(tidyr)
# Remove null, unnest list column, create full_name

list.authors <- ref.info$data[ref.info$data$author != "NULL", ] %>% 
  tidyr::unnest(., author) %>%
  tidyr::unite(auth_full_name, c(family, given), sep = " ", remove = FALSE) %>% 
  rename("auth_family" = family, "auth_given" = given, "auth_aff" = affiliation.name)

# get initials...
list.authors$initials <- tolower(sapply(list.authors$auth_given,function(x) {
    y <- strsplit(x," ")[[1]]
    paste(substr(y,1,1),collapse="")
}))

## last name + initials
list.authors$valid_name <- trim(tolower(paste(list.authors$auth_family,list.authors$initials,sep=" ")))



library(dplyr)
library(stringr)

length(unique(list.authors$valid_name))
length(unique(list.authors$auth_full_name))


list.authors %>% count(valid_name, sort = TRUE)

list.authors$auth_alt2 <- list.authors$valid_name
list.authors$auth_alt3 <- list.authors$valid_name
list.authors$auth_alt4 <- list.authors$valid_name
mtz.names <- adist(list.authors$valid_name)

for (k in unique(list.authors$valid_name)) {
    idx <- first(match(k,list.authors$auth_alt2))
    if (length(idx)==1) {
        list.authors$auth_alt2[which(mtz.names[idx,]<2)] <- k
        list.authors$auth_alt3[which(mtz.names[idx,]<3)] <- k
        list.authors$auth_alt4[which(mtz.names[idx,]<4)] <- k
    }
}

list.authors %>% count(auth_alt2, sort = TRUE) ## poco
list.authors %>% count(auth_alt3, sort = TRUE) ## mejor
subset(list.authors,auth_alt3 %in% "butchart s")$valid_name

## error en Ma Keeping y Ma...
subset(list.authors,auth_alt3 %in% "ma z")$valid_name

## dos autores diferentes
subset(list.authors,auth_alt3 %in% "lei g")$valid_name
subset(list.authors,auth_alt3 %in% "keith da")$valid_name

for (bsc in c("ma z","lei g","keith da","murray nj","regan tj","wilson al")) {
    list.authors[list.authors$auth_alt3 %in% bsc,"auth_alt3"] <- list.authors[list.authors$auth_alt3 %in% bsc,"valid_name"]
}

list.authors %>% count(auth_alt4, sort = TRUE) ## demasiado
list.authors <- subset(list.authors,!valid_name %in% "na na")

length(unique(list.authors$auth_alt3))
length(unique(list.authors$valid_name))


mtz.LRE <- as.matrix(with(subset(list.authors,doi %in% subset(RLEpubs,RLE)$DOI),table(doi,auth_alt3)))


mtz.OTR <- as.matrix(with(subset(list.authors,doi %in% subset(RLEpubs,RLTS | KBA)$DOI),table(doi,auth_alt3)))
##save(file="~/mi.git/CEBA.LEE/Rdata/20190306_pubsLRE.rda",mtz.LRE,mtz.OTR)
save(file="~/mi.git/CEBA.LEE/Rdata/20190324_pubsLRE.rda",mtz.LRE,mtz.OTR,ref.info)

co_occurrence1 <- t(mtz) %*% mtz

co_occurrence2 <- t(mtz2) %*% mtz2
##mtz <- mtz[,colSums(mtz)>1]
##mtz <- mtz[rowSums(mtz)>1,]
##mtz <- mtz[,colSums(mtz)>1]

##http://kateto.net/netscix2016
require(igraph)

layout(matrix(c(1,1,3,3,
                1,1,3,3,
                2,2,4,4),ncol=4,byrow=T))
par(mar=c(0,0,1,0))

g1 <- graph.adjacency(co_occurrence1,
                         weighted=TRUE,
                         mode="undirected",
                         diag=FALSE)
deg <- degree(g1, mode="all")


md <- rev(sort(degree(g1)))[1:10]
mc <- rev(sort(closeness(g1)))[1:10]
mb <- rev(sort(betweenness(g1)))[1:10]

l1 <- layout_with_kk(g1)
n <- V(g1)$name

plot(g1, layout=l1,edge.arrow.size=.15, vertex.color=rgb(.2,.3,.4,.5), vertex.size=sqrt(deg)/2,vertex.label=NA) ## vertex.label=ifelse(n %in% c(names(md),names(mc),names(mb)),n,NA)

par(mar=c(4,4,0,0))
hist(deg, breaks=seq(0,vcount(g2),length=20),
     col=rgb(.2,.3,.4,.5),main="Histogram of node degree")

cfg <- cluster_fast_greedy(g1)
membership(cfg)
sizes(cfg)


par(mar=c(0,0,1,0))

g2 <- graph.adjacency(co_occurrence2,
                         weighted=TRUE,
                         mode="undirected",
                         diag=FALSE)
deg <- degree(g2, mode="all")

md <- rev(sort(degree(g2)))[1:10]
mc <- rev(sort(closeness(g2)))[1:10]
mb <- rev(sort(betweenness(g2)))[1:10]


l2 <- layout_with_kk(g2)
n <- V(g2)$name

plot(g2, layout=l2,edge.arrow.size=.15, vertex.color=rgb(.4,.3,.2,.5), vertex.size=sqrt(deg)/2,vertex.label=NA)
     ##vertex.label=ifelse(n %in% c(names(md),names(mc),names(mb)),n,NA))

par(mar=c(4,4,0,0))
hist(deg, breaks=seq(0,vcount(g2),length=20), main="Histogram of node degree")
centr_degree(g1, mode="all", normalized=T)
centr_degree(g2, mode="all", normalized=T)
centr_eigen(g1, directed=T, normalized=T)$centralization
centr_eigen(g2, directed=T, normalized=T)$centralization
centr_betw(g1, directed=T, normalized=T)$centralization
centr_betw(g2, directed=T, normalized=T)$centralization

## not well defined for disconnected graphs
##centr_clo(g1, mode="all", normalized=T) 
## number of clusters
clusters(g1)$no
clusters(g2)$no
graph.density(g1)
graph.density(g2)
mean_distance(g1)
mean_distance(g2)

transitivity(g1, type="average")
 transitivity(g2, type="average")

################
##

l <- layout_in_circle(g)
l <- layout_on_sphere(g)
               l <- layout_with_fr(g)
l <- layout_with_kk(g)

coords <- layout_(g, nicely())

plot(g, edge.arrow.size=.5, vertex.color="gold", vertex.size=15, 
     vertex.frame.color="gray", vertex.label.color="black", 
     vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2) 

plot(g, layout=coords,
main='Coauthor networks', vertex.label.dist=0.15, vertex.frame.color='maroon', vertex.label.color='black', vertex.label.font=2, vertex.label=V(g)$name, vertex.label.cex=.53)

 

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
