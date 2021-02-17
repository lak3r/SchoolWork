program Project1a
	implicit none
	
	!print *, 'Hello World'
	
	!Variables
	character(100) :: buffer	
	real :: temp !temperature assumed Kelvin
	real, allocatable :: guess(:), deltaGuess(:) !paramenters
	real, allocatable :: dataPoints(:,:) !2d array with the shape [(x1,y1),...,(xm,ym)]
	integer :: M, N !number of paramenters 
	character(10) :: unitsX, unitsY, funcs !(unitX,unitY)
	real :: lambda = 0.0001 !starting value of lambda
	real :: error, newError, deltaError !Can I really declare them this way?
	real, allocatable :: beta(:), alpha(:,:), alphaMod(:,:), alphaSolve(:) 
	!a reset variable maybe
	real :: variance
	real, allocatable :: standDev(:)
	logical :: flag
	integer :: i = 0
	
	
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
	
	!paramenters
	if(index(buffer, 'virial') /= 0) then
		read(1, *) buffer, M
	else
		read(1, *) buffer
		M = 2
	end if
	print *, buffer, M
	allocate(guess(M))
	allocate(deltaGuess(M))
	read(1, *) guess
	print *, guess
	
	!temperature
	read(1, *) buffer, temp, buffer
	print *, 'temp: ', temp, 'K '
	
	!units
	read(1, '(a)') buffer
	call lowerCase(buffer)
	print *, buffer
	!come fix this
	
	!the data points
	N = 0
	do while(i == 0)
		read(1, '(A)', IOstat = i) buffer
		N = N + 1
		!print *, buffer
	end do
	rewind 1
	print *,'There are ', N - 1, ' data points'
	allocate(dataPoints(2, N - 1))
	do i=0, 4
		read(1, '(A)') buffer
		!print *, buffer
	end do
	read(1, *) dataPoints
	do i=1, 10
		print *, dataPoints(1,i), "  ", dataPoints(2, i)
	end do
	
	!clean up
	close(1)
	
end program Project1a

	! AV: http://computer-programming-forum.com/49-fortran/4075a24f74fcc9ce.ht
subroutine lowerCase(word)
! convert a word to lower case
	character (len=*) , intent(in out) :: word
	integer                            :: i,ic,nlen
	nlen = len(word)
	do i=1,nlen
	   ic = ichar(word(i:i))
	   if (ic >= 65 .and. ic < 90) word(i:i) = char(ic+32)
	end do
end subroutine lowerCase

!the functions
function fit(func, temp, guess, M, volume, version) result(pressure)
	implicit none
	character(15), intent(in) :: func
	real, intent(in) :: temp, volume
	integer, intent(in) :: M, version
	real, intent(in) :: guess(M)
	real :: pressure
	real :: gasR = 8.31447
	
	pressure = 0
	
	select case (func)
		case ('vdw')
			select case (version)
				case (0) !just the function
					pressure = ((gasR * temp)/(volume - guess(2))) - (guess(1)/(volume ** 2))
				case (1) !partial wrt param 1
					pressure = -1 / (volume ** 2)
				case (2) !partial wrt param 2
					pressure = (gasR * temp) / ((volume - guess(2))**2)
			end select
		case ('rk')
			select case (version)
				case (0) !just the function
					pressure = ((gasR * temp)/(volume - guess(2))) - (guess(1)/((temp**0.5) * volume * (volume + guess(2))))
				case (1) !partial wrt param 1
					pressure = -1 / ((temp ** 0.5) * volume * (volume + guess(2)))
				case (2) !partial wrt param 2
					pressure = ((gasR * temp)/((volume - guess(2))**2) + (guess(1)/((temp**2) * volume * ((volume + guess(2))**2)))
			end select
		case ('dieterici')
			select case (version)
				case (0) !just the function
					pressure = (gasR * temp * exp((-1 * guess(1))/(gasR * temp * volume)))/(volume - guess(2))
				case (1) !partial wrt param 1
					pressure = (-1 * exp((-1 * guess(1))/(gasR * temp * volume))) / (volume * (volume - guess(2)))
				case (2) !partial wrt param 2
					pressure = (gasR * temp * exp((-1 * guess(1))/(gasR * temp * volume)))/((volume - guess(2))**2)
			end select
		case ('berthelot')
			select case (version)
				case (0) !just the function
					pressure = ((gasR * temp)/(volume - guess(2))) - (guess(1)/(temp * volume * volume))
				case (1) !partial wrt param 1
					pressure = -1 / (temp * volume * volume)
				case (2) !partial wrt param 2
					pressure = (gasR * temp) / ((volume - guess(2))**2)
			end select
		case ('virial')
			select case (version)
				case (0) !just the function
				case (1:) !partial wrt param 1
			end select
	end select
end function fit
	