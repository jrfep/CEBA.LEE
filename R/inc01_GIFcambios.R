##http://www.imagemagick.org/Usage/compare/#difference
##http://www.imagemagick.org/Usage/photos/#removing

dstdir <- "OAECgif2"
SS <- subset(fts,evento==mie)
system(sprintf("convert %s -average promedio.jpg",paste(SS$Directory,SS$filename,sep="/",collapse=" ")))

for (k in 1:nrow(SS)) {
    system(sprintf("convert promedio.jpg %s -compose difference -composite -evaluate Pow 2 -separate -evaluate-sequence Add -evaluate Pow 0.5 -blur 0x20 -threshold 300 -negate tmp%s.gif",paste(SS$Directory[k],SS$filename[k],sep="/",collapse=" "),k))
    system(sprintf("composite -blend 25 tmp%s.gif %s compare%s.gif",k,paste(SS$Directory[k],SS$filename[k],sep="/",collapse=" "),k))
    system(sprintf("composite -geometry +1+1 ~/img/logos/logo.gif compare%s.gif tmpWM.gif",k))
   system(sprintf("convert tmpWM.gif -dither none  -colors 32  -scale 22%%  tmp%s.gif",k))
           wdt <- as.numeric(system(sprintf("identify -format %%w tmp%s.gif",k),intern=T))
        hgt <- system(sprintf("identify -format %%h tmp%s.gif",k),intern=T)
        cam <- unique(SS$camara)
        fchhr <- ifelse(cam == "100MFCAM",
                        ". Fecha y hora de las fotografias desajustadas, pendiente por corregir.",
                        ifelse(cam == "2Cuddeback2601",
                               sprintf(", %s de %s de %s. Hora de las fotografias desajustadas, pendiente por corregir.",unique(days(SS$fecha)),
                                       unique(months(SS$fecha)),
                                       unique(years(SS$fecha))),
                        sprintf(", %s de %s de %s entre %s y %s",
                                unique(days(SS$fecha)),
                                unique(months(SS$fecha)),
                                unique(years(SS$fecha)),
                                min(SS$hora),max(SS$hora))))

        fchhr <- sub(" Jan "," Ene ",fchhr)
        ## a cada foto le colocamos una etiqueta de ubicacion, identificacion del evento, fecha y hora
        system(sprintf("  convert -background '#1238' -fill white -gravity center -size %sx65 caption:'Jardín Botánico de Maracaibo, municipio San Francisco, estado Zulia. Captura fotográfica '%s' con %s imágenes%s' tmp%s.gif +swap -gravity north -composite  label%s.gif",wdt*.8,mie,
                       nrow(SS),fchhr,k,k))

}
if (nrow(SS)>1) {
    system(sprintf("convert -delay 100 -size %sx%s xc:Black %s -loop 0 +map %s/%s.gif", wdt,hgt,paste(sprintf("label%s.gif",1:nrow(SS)),collapse=" "),dstdir,mie))
} else {
    system(sprintf("mv label1.gif  %s/%s.gif", dstdir,mie))
}
 
system("rm promedio.jpg")
system("rm tmp*gif")
system("rm label*gif")
system("rm compare*gif")



    ##system(sprintf("compare %s  %s   compare%s.gif",    ## sobreestima
    ##system(sprintf("compare -metric AE -fuzz 5%% %s %s   compare%s.gif",##subestima
    ##                   paste(SS$Directory[k-1],SS$filename[k-1],sep="/"),
    ##                   paste(SS$Directory[k],SS$filename[k],sep="/"),
    ##                   k))

## otras opciones
##    system(sprintf("convert promedio.jpg %s -compose difference -composite -evaluate Pow 2 -separate -evaluate-sequence Add -evaluate Pow 0.5 -blur 0x20 -threshold 300 tmp%s.gif",paste(SS$Directory[k],SS$filename[k],sep="/",collapse=" "),k))
##  system(sprintf("composite -compose Divide tmp%s.gif %s compare%s.gif",k,paste(SS$Directory[k],SS$filename[k],sep="/",collapse=" "),k))



##    system(sprintf("convert %s %s -compose difference -composite -evaluate Pow 2 -separate -evaluate-sequence Add -evaluate Pow 0.5 -blur 0x20 -threshold 300 -negate tmp%s.gif",
##                 paste(SS$Directory[k-1],SS$filename[k-1],sep="/",collapse=" "),
##               paste(SS$Directory[k],SS$filename[k],sep="/",collapse=" "),
##               k))
##    system(sprintf("composite -blend 25 tmp%s.gif %s compare%s.gif",k,paste(SS$Directory[k],SS$filename[k],sep="/",collapse=" "),k))
