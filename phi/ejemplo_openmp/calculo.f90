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

	close(1)
	close(2)
	close(3)
	close(4)
	close(5)

	end program



subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    06 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len = 8 ) ampm
  integer d
  integer h
  integer m
  integer mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer n
  integer s
  integer values(8)
  integer y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end

