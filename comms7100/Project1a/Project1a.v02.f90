program Project1a
	implicit none
	
	!print *, 'Hello World'
	
      ! AV : I suggest that for now you use real(8), not real(4)
      
	!Variables
	character(100) :: buffer
	real(8) :: temp !temperature assumed Kelvin
	real(8), allocatable :: guess(:), deltaGuess(:) !paramenters
	real(8), allocatable :: dataPoints(:,:) !2d array with the shape [(x1,y1),...,(xm,ym)]
      ! AV : use integers for counters
	integer :: M, N !number of parameters & data points
	character, dimension(2,20) :: units !(unitX,unitY)
	real(8) :: lambda = 0.0001 !starting value of lambda
	real(8) :: error, newError, deltaError !Can I really declare them this way?
	real(8), allocatable :: beta(:), alpha(:,:), alphaMod(:,:), alphaSolve(:) 
	!a reset variable maybe
	real(8) :: variance
	real(8), allocatable :: standDev(:)
	logical :: flag
	integer :: i = 0
      
      ! AV: we known in advance the kind of units we can have
      character punits(7)*7, vunits(4)*8
      data punits / 'pa     ', 'bar    ', 'kilobar', 'megapa ', 'atm    ', 'torr   ', 'mmhg   ' /
      data vunits / 'm^3/mol ', 'dm^3/mol', 'l/mol   ', 'cm^3/mol' /
      integer :: ipunit, ivunit
	
	
	!command line argument test
	if (COMMAND_ARGUMENT_COUNT() >= 1) then
		call GET_COMMAND_ARGUMENT(1, buffer)
	end if
	
	!this is a test function 
	if (buffer == '-echo') then
		call get_command_argument(2, buffer)
		print *, buffer
		stop
	end if
	
	!opening the file
	inquire(file = buffer, Exist = flag)
	if (flag) then
		open(1, file = buffer, status = 'old')
	else if(flag .eqv. .false.) then
		print *, 'That file name will not do. Exiting.'
		stop
	end if
	
	!reading the data
	read(1, '(A)') buffer
	print *, buffer
	
	read(1, *) buffer, M 
	print *, buffer, M
	allocate(guess(M))
	allocate(deltaGuess(M))
	read(1, *) guess
	print *, guess
	read(1, *) buffer, temp, buffer
	print *, 'temp: ', temp, 'K '
      
      ! AV: there is a problem reading two character variables using free format
      !     which is why you are getting weird output
	!read(1, *) units !something is coming in ut it doesn't make sense?
	!print *, units(1,:), units(2,:)
      ! AV: here is one possible solution:
      ! first, read entire line into a variable using 'fixed' format
      read(1,'(a)') buffer
      ! convert it to lower case 
      call lower_case(buffer)
      ! now scan for each unit in 'buffer'
      ! first, let's do volume
      ivunit = 0
      do i = 1, size( vunits )
        if( index( buffer, trim(vunits(i)) ) /= 0 ) then
          ivunit = i
          exit
        endif
      enddo
      if( ivunit == 0 ) then
        write(*,*) ' could not recognize Vm unit in ',trim(buffer)
        stop
      endif
      ! now, let's do pressure
      ipunit = 0
      do i = 1, size( punits )
        if( index( buffer, trim(punits(i)) ) /= 0 ) then
          ipunit = i
          exit
        endif
      enddo
      if( ipunit == 0 ) then
        write(*,*) ' could not recognize p unit in ',trim(buffer)
        stop
      endif
      ! now, print out the units
      write(*,*) ' Vm unit = ',trim(vunits(ivunit)),', p unit = ',trim(punits(ipunit))
      ! reinitialize variable i
      i = 0
	
	N = 0
1     read(1, '(A)', end = 2 ) buffer
		N = N + 1
		!print *, buffer
	      goto 1
2     rewind 1
	print *,'There are ', N, ' data points'
	allocate( dataPoints(2,N) )
	do i=1, 5
		read(1, '(A)') buffer
		!print *, buffer
	end do
	read(1, *) dataPoints
	do i=1, N
		print *, dataPoints(1,i), "  ", dataPoints(2, i)
	end do
	
	!clean up
	close(1)
	
end program Project1a

! AV: http://computer-programming-forum.com/49-fortran/4075a24f74fcc9ce.ht
subroutine lower_case(word)
! convert a word to lower case
character (len=*) , intent(in out) :: word
integer                            :: i,ic,nlen
nlen = len(word)
do i=1,nlen
   ic = ichar(word(i:i))
   if (ic >= 65 .and. ic < 90) word(i:i) = char(ic+32)
end do
end subroutine lower_case
