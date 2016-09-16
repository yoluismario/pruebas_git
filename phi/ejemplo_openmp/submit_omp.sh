#!/bin/bash

### Las líneas #SBATCH configuran los recursos de la tarea
### (aunque parezcan estar comentadas)

### Nombre de la tarea
#SBATCH --job-name=test-phi-openmp

### Cola a usar
#SBATCH --partition=phi

### No queremos compartir xeon phis, poca memoria
#SBATCH --exclusive

### Un solo proceso para OpenMP
#SBATCH --ntasks=1

### Cores por proceso
#SBATCH --cpus-per-task=57
#SBATCH --threads-per-core=4

### Tiempo de ejecucion. Formato dias-horas:minutos. Maximo una semana.
#SBATCH --time 0-0:05

### Script que se ejecuta al arrancar el trabajo

### Cargar el entorno del usuario
### (en phi: MKL e Intel MPI)
### No tocar
. /etc/profile

### No tenemos módulos en los xeon phi
### Hasta 4 hilos por core
export OMP_NUM_THREADS=228
export MKL_NUM_THREADS=228

### Largar el programa
srun ./proctest

