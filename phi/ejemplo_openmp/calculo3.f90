Program calculo
	
	use omp_lib	
	implicit none
	! Definiciones
	INTEGER :: i,j,k,l
	INTEGER :: gldn, nod, nel,nnod 
	DOUBLE PRECISION :: E, theta, alfa, wtime
	! Alocables 
	DOUBLE PRECISION, ALLOCATABLE, DIMENSION (:,:) :: Ke, M
	
	gldn = 2
	
	call timestamp ( )
	
	nel = 100
	allocate(Ke(nel,nel))
	allocate(M(nel,nel))
	
	call RANDOM_NUMBER(Ke)
	call RANDOM_NUMBER(M)
	
	matmul(Ke,M)
	
	
	
	call timestamp ( )	
	
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