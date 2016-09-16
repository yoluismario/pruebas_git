#include <stdio.h>
#include <omp.h>

#include "mpi.h"

int main(int argc, char ** argv) {

    MPI_Init(&argc, &argv);

    int commsize, commrank;
    MPI_Comm_size(MPI_COMM_WORLD, &commsize);
    MPI_Comm_rank(MPI_COMM_WORLD, &commrank);

    #pragma omp parallel
    {
        int omptid = omp_get_thread_num();
        int ompthreads = omp_get_num_threads();

        #pragma omp critical
        printf("MPI:%d/%d   OMP:%d/%d\n", commrank, commsize, omptid, ompthreads);
    }

    MPI_Finalize();
    return 0;
}

