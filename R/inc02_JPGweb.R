##source("inc02_JPGweb.R")

## a cada foto le colocamos logos y una etiqueta del lab
system(sprintf("composite -geometry +1+1 ~/img/logos/logo.gif %s tmpWM.jpg",origen))

## solo escalar
system(sprintf("convert tmpWM.jpg -scale 60%%  tmpPQN.jpg"))


## guardamos la informacion de ancho (wdt) y altura (hgt)
wdt <- as.numeric(system(sprintf("identify -format %%w tmpPQN.jpg"),intern=T))
hgt <- as.numeric(system(sprintf("identify -format %%h tmpPQN.jpg"),intern=T))

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

##fchhr <- sub(" Jan "," Ene ",fchhr)

## a cada foto le colocamos una etiqueta de ubicacion, identificacion del evento, fecha y hora
system(sprintf("  convert -background '#1238' -fill white -gravity center -size %sx135 caption:'%s. Captura fotográfica '%s' con %s imágenes%s' tmpPQN.jpg +swap -gravity north -composite  %s",wdt*.8,lugar,SS$evento,
                       nrow(SS),fchhr,rslt))

    
    ##  eliminamos los archivos temporales que ya no necesitamos
    system("rm tmp*jpg")


