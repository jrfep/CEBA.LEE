##source("inc00_hacerGIFs.R")

##http://www.imagemagick.org/Usage/compare/#difference
##http://www.imagemagick.org/Usage/compare/#statistics  
##http://www.imagemagick.org/Usage/masking/#bg_remove
##http://www.imagemagick.org/Usage/anim_opt/#colortables

##
##cd ~/img/logos/
##convert ~/img/logos/logo_solo.ps  -scale 50% logo.gif
##cd

##dstdir <- ""
##for (mie in unique(fts$evento)) {
    ## para cada evento seleccionamos la lista de fotos
    SS <- subset(fts,evento==mie)

    for (k in 1:nrow(SS)) {
    ## cada foto la reducimos a un 22% del tamaño original y la guardamos en formato "gif"
        system(sprintf("composite -geometry +1+1 ~/img/logos/logo.gif %s tmpWM.jpg",paste(SS$Directory[k],SS$filename[k],sep="/",collapse=" ")))        

        ##reducir colores y escalar
        ##        system(sprintf("convert tmpWM.jpg -dither none  -colors 16  -scale 22%%  tmp%s.gif",k))
        ## solo escalar
        system(sprintf("convert tmpWM.jpg -scale 22%%  tmp%s.gif",k))


        ## guardamos la informacion de ancho (wdt) y altura (hgt)
        wdt <- as.numeric(system(sprintf("identify -format %%w tmp%s.gif",k),intern=T))
        hgt <- system(sprintf("identify -format %%h tmp%s.gif",k),intern=T)

        ## guardamos la informacion de la fecha y hora de la secuencia para colocarla en la etiqueta de la foto.
        cam <- unique(SS$camara)
        fchhr <- ifelse(cam == "2Cuddeback2601",
                        sprintf(", %s de %s de %s. Hora de las fotografias desajustadas, pendiente por corregir.",unique(days(SS$fecha)),
                                unique(months(SS$fecha)),
                                unique(years(SS$fecha))),
                        sprintf(", %s de %s de %s entre %s y %s",
                                unique(days(SS$fecha)),
                                unique(months(SS$fecha)),
                                unique(years(SS$fecha)),
                                min(SS$hora),max(SS$hora)))

        fchhr <- sub(" Jan "," Ene ",fchhr)
        ## a cada foto le colocamos una etiqueta de ubicacion, identificacion del evento, fecha y hora
        system(sprintf("  convert -background '#1238' -fill white -gravity center -size %sx65 caption:'%s. Captura fotográfica '%s' con %s imágenes%s' tmp%s.gif +swap -gravity north -composite  label%s.gif",wdt*.8,lugar,mie,
                       nrow(SS),fchhr,k,k))
    }
    
    ## por ultimo recogemos todas las fotos modificadas y hacemos una animacion en formato "gif"
    if (nrow(SS)>1) {
        system(sprintf("convert -delay 100 -size %sx%s xc:Black %s -loop 0 +map %s/%s.gif", wdt,hgt,paste(sprintf("label%s.gif",1:nrow(SS)),collapse=" "),dstdir,mie))
    } else {
        system(sprintf("mv label1.gif  %s/%s.gif", dstdir,mie))
    }
    ##  eliminamos los archivos temporales que ya no necesitamos
    system("rm tmp*gif")
    system("rm tmp*jpg")
    system("rm label*gif")
##}

