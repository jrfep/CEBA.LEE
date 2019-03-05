##R  --vanilla
require(WikipediR)
require(chron)
require(twitteR)
library(lattice)
require(RWordPress)
options(WordpressLogin = c(zuhe = "sierra nevada de M3rida"),
        WordpressURL = 'http://jrferrerparis.info/xmlrpc.php')
setwd("~/mi.git/CEBA.LEE/R/")

##download.file("http://blog.revolutionanalytics.com/downloads/calendarHeat.R","calendarHeat.R")
source("calendarHeat.R")

##~/Dropbox/ceba/bin/gitLogs.sh
##read.csv("~/mi.git/project-logs.csv")


contribs.en <- user_contributions("en", "wikipedia", username = "jrfep",limit=1000)
contribs.es <- user_contributions("es", "wikipedia", username = "jrfep",limit=1000)
##contribs.eva <- user_contributions(domain="wikieva.org.ve/index.php", username = "jrfep",properties = "ids", limit = 1)
fecha1 <- unlist(lapply(contribs.es$query[[1]],function(x) x$timestamp))
fecha2 <- unlist(lapply(contribs.en$query[[1]],function(x) x$timestamp))



cK <- "fmEceE7yi6ROw9JuNHq2Ug" ## consumer key
cS <- "xTLUGCxUBwttxsnHAQE2OFGJNopeLNpMtDp85xfPE" ## consumer secret
At <- "1951857650-YBM9SB1mIqy5uTjVXhaJuz5zxJGxwUQM933saQf"
As <- "keQl9kHLt8FCNKddusGBaRFf5viI0byNfF47SjetEc8"
setup_twitter_oauth(cK,cS,At,As)
tuser <- getUser('RSaurio')

ut <- userTimeline('NeoMapas', n=1000)
u2 <- userTimeline('Rsaurio', n=1000)
u3 <- userTimeline('jrfep', n=1000)


fecha3 <- c(unlist(lapply(ut,function(x) substr(as.character(x$created),1,10))),
            unlist(lapply(u2,function(x) substr(as.character(x$created),1,10))),
            unlist(lapply(u3,function(x) substr(as.character(x$created),1,10))))

prb <- RWordPress::getPosts()

fecha4 <- substr(prb$date_created_gmt,1,10)
fecha5 <- substr(read.csv("~/mi.git/project-logs.csv")$timestamp,1,10)

fechas <- chron(dates.=substr(c(fecha1,fecha2,fecha3,fecha4,fecha5),1,10),format="y-m-d")
fechas <- subset(fechas,years(fechas) %in% 2014:2019)


actividad <- data.frame(table(fechas))
actividad$cats <- as.numeric(cut(actividad$Freq,c(0,1,2,4,8,16,32,Inf)))

calendarHeat(actividad$fechas, actividad$Freq, 
             varname="en la web")
calendarHeat(actividad$fechas, actividad$cats, 
             varname="de JRFeP")
##,sub="(wikipedia, github, twitter, wordpress)")


