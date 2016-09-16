#include <stdio.h>
#include <omp.h>

int main(int argc, char ** argv) {

    #pragma omp parallel
    {
        int omptid = omp_get_thread_num();
        int ompthreads = omp_get_num_threads();

        #pragma omp critical
        printf("OMP:%d/%d\n", omptid, ompthreads);
    }

    return 0;
}

