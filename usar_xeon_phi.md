# proceso de uso de las xeon phi
- hay que entrar en el pulqui antes de entrar a las placas.
desde *pulqui*, se puede correr las aplicaciones de control de las placas.
## miccheck
es una de las aplicaciones que te permite tener control de las placas, equivalente a *nvidia-smi*.
## micflash
te permite verificar el SO de cada placa, uso:
```sh
$ sudo micflash -devinfo -d 0/1
```
Necesitas ser sudo!!!

## micinfo
otra aplicación que te tira los datos de las placas instaladas.
Para algunos datos e necesario ser sudo, te da el status de la placas.
Es como miccheck pero a nivel de usuario.
```sh
$ micinfo -listDevices
```

## recursos
recursos de [intel]
[intel]:https://software.intel.com/en-us/mic-developer/tools-and-downloads


# compilar programas en pulqui
```sh
$ wget -c ftp://ftp.gnu.org/gnu/octave/octave-4.0.0.tar.gz
$ tar -xvf octave-4.0.0.tar.gz
$ cd octave-4.0.0/
$ ./configure --prefix=/opt/octave-4.0.0 CPPFLAGS=-I/usr/include/hdf5/serial LDFLAGS=-L/usr/lib/$(dpkg-architecture -qDEB_HOST_MULTIARCH)/hdf5/serial
make
```