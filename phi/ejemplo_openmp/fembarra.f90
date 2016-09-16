! ifort -O2 -mmic -openmp -mkl=parallel random_number_contention.f90 -o number
Program Fembarra


    implicit none

    integer :: nelem, nnod, ic, j, errorflag, grlib, tiemp, Nropt, n, d, f ,grlibT,nnodxelem

    Double precision :: E, Le, G, RO, L, I, deltat, count, MU, pi, tc, Tsimul, a1, a2, a3, a4, a5, a6, beta, gamma

    Double precision , ALLOCATABLE, DIMENSION (:) :: Ut, Utmm1, U2pt, Ut0, U2pt0, Utm1, Ft0, Ft, U1pt,aux,&
     U1ptm1, B, U2ptmm1, U1ptmm1  ,Ftilde !mm=+1 t=0 m=-1

    Double precision , ALLOCATABLE, DIMENSION (:,:) :: Ke, Me, K, M, Minv, A ,Mtilde,invMtilde

    OPEN(1, FILE = 'deltat.txt', STATUS = 'old', ACTION = 'write')
    OPEN(2, FILE = 'Ut3.txt', STATUS = 'old', ACTION = 'write')
    OPEN(3, FILE = 'Ut1.txt', STATUS = 'old', ACTION = 'write')
    OPEN(4, FILE = 'Ut2.txt', STATUS = 'old', ACTION = 'write')
    OPEN(5, FILE = 'Ut4.txt', STATUS = 'old', ACTION = 'write')

    grlib=2                     !grado de libertad por nodo
    nelem = 1
    nnodxelem=2
    nnod = nelem + 1            !numero de nodos por elemento
    grlibT = nnodxelem*grlib    !grados de libertad totales por elemento
    n =nnod*grlib               !numero de elementos de las matrices ensambladas

    pi = acos(-1.)
    L = 200
    I = 1000
    E = 2 * 10**6
    Le = L/nelem
    MU = 0.007135

    Tsimul = 0.3
    deltat = 0.00001
    Nropt = Tsimul/deltat


    allocate(Ke(grlibT, grlibT))
    allocate(Me(grlibT, grlibT))
    allocate(K(n,n))
    allocate(M(n,n))
    allocate(Minv(n,n))
    allocate(A(n,n))
    allocate(B(n))
    allocate(Mtilde(n,n))
    allocate(invMtilde(n,n))
    allocate(aux(n))

    !viga flexion grados de libertad giro y desplazamiento para c/ elemento
    !funciones de forma , polinomios c£bicos
    !elemento


    Ke(1, 1) = (E * I/Le**3) * 12
    Ke(1, 2) = (E * I/Le**3) *(6 * Le )
    Ke(2, 1) = (E * I/Le**3) *(6 * Le)
    Ke(1, 3) = (E * I/Le**3)*(-12)
    Ke(3, 1) = (E * I/Le**3)*(-12)
    Ke(1, 4) = (E * I/Le**3)*(6 * Le)
    Ke(4, 1) = (E * I/Le**3)*(6 * Le)
    Ke(2, 2) = (E * I/Le**3) * 4 * Le**2
    Ke(2, 3) = (E * I/Le**3)*(-6 * Le)
    Ke(3, 2) = (E * I/Le**3)*(-6 * Le)
    Ke(2, 4) = (E * I/Le**3)*(2 * Le**2)
    Ke(4, 2) = (E * I/Le**3)*(2 * Le**2)
    Ke(3, 3) = (E * I/Le**3) * 12
    Ke(3, 4) = (E * I/Le**3)*(-6 * Le)
    Ke(4, 3) = (E * I/Le**3)*(-6 * Le)
    Ke(4, 4) = (E * I/Le**3)*(4 * Le**2)

    Me(1, 1) = (MU * Le/420) * 156
    Me(1, 2) = (MU * Le/420) * 22 * Le
    Me(2, 1) = (MU * Le/420) * 22 * Le
    Me(1, 3) = (MU * Le/420) * 54
    Me(3, 1) = (MU * Le/420) * 54
    Me(1, 4) = (MU * Le/420)*(-13 * Le)
    Me(4, 1) = (MU * Le/420)*(-13 * Le)
    Me(2, 2) = (MU * Le/420) * 4 * Le**2
    Me(2, 3) = (MU * Le/420) * 13 * Le
    Me(3, 2) = (MU * Le/420) * 13 * Le
    Me(2, 4) = (MU * Le/420)*(-3 * Le**2)
    Me(4, 2) = (MU * Le/420)*(-3 * Le**2)
    Me(3, 3) = (MU * Le/420) * 156
    Me(3, 4) = (MU * Le/420)*(-22 * Le)
    Me(4, 3) = (MU * Le/420)*(-22 * Le)
    Me(4, 4) = (MU * Le/420)*(4 * Le**2)


    
    
    K(:,:)=0
    M(:,:)=0
    Mtilde(:,:)=0
    Minv(:,:)=0
    invMtilde(:,:)=0

    !ensamble de masa y rigidez
    !MASA
    do j = 1, nelem
        M(j*grlibT-((j-1)*grlibT/2)-(grlibT-1):j*grlibT-(j-1)*grlibT/2 , &
        j*grlibT-((j-1)*grlibT/2)-(grlibT-1):j*grlibT-(j-1)*grlibT/2 ) = Me
    end do


    do j = 1, nelem - 1

    do d= 1,grlibT/2
    do f=1,grlibT/2
        M(j*grlibT-(j-1)*grlibT/2-(grlibT/2-f),j*grlibT-(j-1)*grlibT/2-(grlibT/2-d)) = &
        Me(f, d) + Me(f+grlibT/2, d+grlibT/2)

    end do
    end do
    
    end do

    !RIGIDEZ

    do j = 1, nelem
        K(j*grlibT-((j-1)*grlibT/2)-(grlibT-1):j*grlibT-(j-1)*grlibT/2 , &
        j*grlibT-((j-1)*grlibT/2)-(grlibT-1):j*grlibT-(j-1)*grlibT/2 ) = Ke
    end do


    do j = 1, nelem - 1

    do d= 1,grlibT/2
    do f=1,grlibT/2
        K(j*grlibT-(j-1)*grlibT/2-(grlibT/2-f),j*grlibT-(j-1)*grlibT/2-(grlibT/2-d)) = &
        Ke(f, d) + Ke(f+grlibT/2, d+grlibT/2)

    end do
    end do

    end do
    
    !print*,K(:,10)

  CALL FindInv(M, Minv, n , ErrorFlag)


    !condiciones iniciales y de borde

    allocate(Ut0(n))
    allocate(U2pt0(n))
    allocate(Ft0(n))
    allocate(U1pt(n))
    allocate(Ut(n))
    allocate(U2pt(n))
    allocate(Ft(n))
    allocate(Ftilde(n))
    allocate(U1ptm1(n))
    allocate(Utm1(n))
    allocate(Utmm1(n))
    allocate(U2ptmm1(n))
    allocate(U1ptmm1(n))

    !CCCCCCCCCCCCCCCCCCCCCCCCCCCCC  comun a todos

    Ut0(:) = 0
    Ft(:) = 0
    Ftilde(:)= 0
    
    !CCCCCCCCCCCCCCCCCCCCCCCCCCCCC  esquma 1

    U2pt0=matmul(Minv,Ft-matmul(K,Ut0))  !aceleracion inicial esquema 1
    Utm1 = Ut0 - (deltat**2/2)*U2pt0      !desplazamiento ficticio
    Mtilde=M/deltat**2
    Ut(:)=0
    
    !CCCCCCCCCCCCCCCCCCCCCCCCCCCCC  esquma 2
    
     !U1pt(:) = 0 !velocidad inicial (segundo esquema)
     !Ut(:) = 0
     !Ft(:)= 0


    !CCCCCCCCCCCCCCCCCCCCCCCCCCCCC  esquma 3

    !U2pt(:) = 0 !aceleracion inicial esquema Newmark


     !inversa matriz de masas


     CALL FindInv(Mtilde, invMtilde, n , ErrorFlag)

      !print*,invMtilde(9,8)


    !integraci¢n en el tiempo esquema centrado Y NEWMARK

    do tiemp = 1, Nropt

        tc = tc + deltat
        Ft(n-1) = 100 !Le/2*sin(200*tc) !F(t)

        !CCCCCCCCCCCCCCCCCCCC esquema centrado 1

        !Ftilde=Ft-matmul((K-(2/deltat**2)*M),Ut)-matmul((1/deltat**2)*M,Utm1)

       ! if(tiemp.eq.1)print*,Ftilde

        !Utmm1=matmul(invMtilde,Ftilde)

        !Utmm1 = matmul(Minv,Ft-matmul(K,Ut))*deltat**2+2*Ut-Utm1

        !U2pt = (Utmm1-2*Ut+Utm1)/deltat**2


        !Utm1=Ut
        !Ut=Utmm1


        !Ut(1)=0     !condicion de borde
        !Ut(2)=0
        !Utmm1(1)=0
        !Utmm1(2)=0



        !CCCCCCCCCCCCCCCCCCCC esquema centrado 2

         aux=Ft-matmul(K,Ut)
         U2pt=matmul(Minv,aux)
        ! 
         U2pt(1)=0     !condicion de borde
         U2pt(2)=0

         U1ptmm1=U1pt + U2pt*deltat
         Utmm1=Ut+U1ptmm1*deltat

         U1pt=U1ptmm1
         Ut=Utmm1

        !CCCCCCCCCCCCCCCCCCCCC M‚todo de Newmark

        !beta = 0.25
        !gamma = 0.5
        !a1 = 1/(beta * deltat**2)
        !a2 = gamma/(beta * deltat)
        !a3 = 1/(beta * deltat)
        !a4 = (1 - gamma/beta)
        !a5 = (1/(2 * beta) - 1)
        !a6 = (1 - (gamma/(2 * beta))) * deltat

        !A = a1 * M + K

        !B = Ft + matmul((a1 * M), Ut) + matmul((a3 * M), U1pt) + matmul((a5 * M), U2pt)

       ! print*, n

        !call triang(A, n, B, Utmm1)

        !A = a1 * M + K !reacomodo A

       ! U2ptmm1 = a1 * (Utmm1 - Ut) - a3 * U1pt - a5 * U2pt
       ! U1ptmm1 = a2 * (Utmm1 - Ut) + a4 * U1pt + a6 * U2pt

       ! U2pt(1) = 0 !condicion de borde en c paso d tiempo
       ! U2pt(2) = 0

       ! Ut = Utmm1
       ! U1pt = U1ptmm1
       ! U2pt = U2ptmm1


        write(1, *) tc
        write(2, *) Ut(n-1)
        !write(3,*)Ut(1)
        !write(4,*)Ut(2)
        write(5, *) Ut(n)

    end do

close(1)
close(2)
close(3)
close(4)
close(5)

end program



!Subroutine to find the inverse of a square matrix
!Author : Louisda16th a.k.a Ashwith J. Rego
!Reference : Algorithm has been well explained in:
!http://math.uww.edu/~mcfarlat/inverse.htm
!http://www.tutor.ms.unimelb.edu.au/matrix/matrix_inverse.html
SUBROUTINE FINDInv(matrix, inverse, n, errorflag)

    IMPLICIT NONE
    !Declarations
    INTEGER, INTENT(IN) :: n
    INTEGER, INTENT(OUT) :: errorflag !Return error status. -1 for error, 0 for normal
    Double precision , INTENT(IN), DIMENSION(n, n) :: matrix !Input matrix
    Double precision , INTENT(OUT), DIMENSION(n, n) :: inverse !Inverted matrix

    LOGICAL :: FLAG = .TRUE.
    INTEGER :: i, j, k, l
    Double precision :: m
    Double precision , DIMENSION(n, 2 * n) :: augmatrix !augmented matrix

    !Augment input matrix with an identity matrix
    DO i = 1, n
        DO j = 1, 2 * n
            IF (j <= n) THEN
                augmatrix(i, j) = matrix(i, j)
            ELSE IF ((i + n) == j) THEN
                augmatrix(i, j) = 1
            Else
                augmatrix(i, j) = 0
            ENDIF
        END DO
    END DO

    !Reduce augmented matrix to upper traingular form
    DO k = 1, n - 1
        IF (augmatrix(k, k) == 0) THEN
            FLAG = .FALSE.
            DO i = k + 1, n
                IF (augmatrix(i, k) /= 0) THEN
                    DO j = 1, 2 * n
                        augmatrix(k, j) = augmatrix(k, j) + augmatrix(i, j)
                    END DO
                    FLAG = .TRUE.
                    EXIT
                ENDIF
                IF (FLAG .EQV. .FALSE.) THEN
                    PRINT*, "Matrix is non - invertible"
                    inverse = 0
                    errorflag = -1
                    return
                ENDIF
            END DO
        ENDIF
        DO j = k + 1, n
            m = augmatrix(j, k)/augmatrix(k, k)
            DO i = k, 2 * n
                augmatrix(j, i) = augmatrix(j, i) - m * augmatrix(k, i)
            END DO
        END DO
    END DO

    !Test for invertibility
    DO i = 1, n
        IF (augmatrix(i, i) == 0) THEN
            PRINT*, "Matrix is non - invertible"
            inverse = 0
            errorflag = -1
            return
        ENDIF
    END DO

    !Make diagonal elements as 1
    DO i = 1, n
        m = augmatrix(i, i)
        DO j = i, (2 * n)
            augmatrix(i, j) = (augmatrix(i, j) / m)
        END DO
    END DO

    !Reduced right side half of augmented matrix to identity matrix
    DO k = n - 1, 1, -1
        DO i = 1, k
            m = augmatrix(i, k + 1)
            DO j = k, (2 * n)
                augmatrix(i, j) = augmatrix(i, j) - augmatrix(k + 1, j) * m
            END DO
        END DO
    END DO

    !store answer
    DO i = 1, n
        DO j = 1, n
            inverse(i, j) = augmatrix(i, j + n)
        END DO
    END DO
    errorflag = 0

END SUBROUTINE FINDinv


Subroutine triang(A, n, B, X)

    INTENT(IN) :: n


    DOUBLE PRECISION, DIMENSION(n, n), INTENT(INOUT) :: A

    DOUBLE PRECISION, DIMENSION(n), INTENT(INOUT) :: B, X

    DOUBLE PRECISION, DIMENSION (n) :: ipiv1, ipiv2

    DOUBLE PRECISION :: ipiv3, ipiv4, f

    integer :: i, j, u

    !triangulaci¢n


    do i = 1, n - 1

        ipiv1(1:n) = 0
        ipiv2(1:n) = 0
        ipiv3 = 0
        ipiv4 = 0

        do j = i + 1, n


            if (i .eq. j - 1)then

                do k = i + 1, n

                    if (abs(A(i, i)) .lt. abs(A(k, i)))then

                        ipiv1(1:n) = A(i, 1:n)
                        ipiv2(1:n) = A(k, 1:n)
                        A(i, 1:n) = ipiv2(1:n)
                        A(k, 1:n) = ipiv1(1:n)

                        ipiv3 = B(i)
                        ipiv4 = B(k)
                        B(i) = ipiv4
                        B(k) = ipiv3

                    end if

                end do

            end if

            f = A(j, i)/A(i, i)

            A(j, i) = A(j, i) - f * A(i, i)

            B(j) = B(j) - f * B(i)

            do u = i + 1, n

                A(j, u) = A(j, u) - f * A(i, u)

            end do

        end do

    end do

    !Resolucion     SUSTITUCION REVERSIVA
    X(n) = B(n)/A(n, n)

    do i = n - 1, 1, -1

        s = 0

        do j = i + 1, n

            s = s + A(i, j) * X(j)

        end do

        X(i) = (B(i) - s)/A(i, i)


    end do


    return
end

