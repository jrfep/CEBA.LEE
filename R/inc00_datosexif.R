
## usamos el programa exiftool para leer la informacion de todas las imagenes en la carpeta "fotos" y la guardamos en un archivo llamado "lista"

for (fotos in dir(buscar,full.names=T)) {
    system(sprintf("exiftool -T -r -filename -createdate -Directory -FileSize -ImageDescription -Make -CameraModelName -ShutterSpeedValue -ApertureValue -ExposureCompensation -LightSource -Flash -Aperture -ImageSize -ShutterSpeed '%s' >> %s",fotos,lista))
}
