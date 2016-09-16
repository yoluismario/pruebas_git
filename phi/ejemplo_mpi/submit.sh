#!/bin/bash

### Las líneas #SBATCH configuran los recursos de la tarea
### (aunque parezcan estar comentadas)

### Nombre de la tarea
#SBATCH --job-name=test-phi-mpi

### Cola a usar
#SBATCH --partition=phi

### No queremos compartir xeon phis, poca memoria
#SBATCH --exclusive

### Usar un xeon phi de 57 cores
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=57

### Cores por proceso (para MPI+OpenMP)
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
export OMP_NUM_THREADS=4
export MKL_NUM_THREADS=4

### Largar el programa
mpiexec -np $SLURM_NTASKS ./proctest

