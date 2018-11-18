bkp <- "/media/jferrer/OneTouch4/GS_cameratraps/"
periodos <- dir(bkp)
names(periodos)[grep("2",periodos)] <- "P1"
names(periodos)[grep("3",periodos)] <- "P2"
names(periodos)[grep("4",periodos)] <- "P3"

##for (pp in names(periodos)) {
for (pp in c("P1","P2","P3")) {
    bloques <- dir(sprintf("%s%s",bkp,periodos[pp]))
    bloques <- grep("bloque",bloques,value=T)
    names(bloques) <- gsub("_","",gsub("bloque","B0",gsub("^[0-9\\._]+","",bloques)))
    ##  for (bb in names(bloques)) {
    for (bb in c("B01","B02","B03","B04","B05","B06")) {
        camaras <- dir(sprintf("%s%s/%s",bkp,periodos[pp],bloques[bb]))
        names(camaras) <- gsub("\\.","",gsub("_[0-9\\.\\-]+$","",camaras))
        names(camaras) <- toupper( names(camaras))

        for (cc in names(camaras)) {
            ##system(sprintf("mkdir -p ~/fototeca/%s/%s/%s", bb,pp,cc))
            lista <- sprintf("lista.%s.%s.%s.txt",bb,pp,cc)
            ##system(sprintf("cp -rnv %s%s/%s/%s/* ~/fototeca/%s/%s/%s",
            ##               bkp,periodos[pp],bloques[bb],camaras[cc],
            ##               bb,pp,cc))
            
            buscar <- sprintf('~/fototeca/%s/%s/%s',bb,pp,cc)
            if (!file.exists(lista))
                source("~/Dropbox/Mapoteca/doc/610_FotosGranSabana/inc00_datosexif.R")

        }
    }
}
