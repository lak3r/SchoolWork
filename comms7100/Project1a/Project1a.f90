program Project1a
	use helper
	implicit none
	
	
	!print *, 'Hello World'
	
	!Variables
	character(100) :: buffer	
	real :: temp !temperature assumed Kelvin
	real, allocatable :: guess(:), deltaGuess(:) !paramenters
	real, allocatable :: dataPoints(:,:) !2d array with the shape [(x1,y1),...,(xm,ym)]
	integer :: M, N !number of paramenters 
	character(10) :: unitsX, unitsY, funcs !(unitX,unitY)
	real :: lambda = 0.001 !starting value of lambda
	real :: error, newError, deltaError !Can I really declare them this way?
	real, allocatable :: beta(:), alpha(:,:), alphaMod(:,:), alphaSolve(:) 
	!a reset variable maybe
	real :: variance
	real, allocatable :: standDev(:)
	logical :: flag
	integer :: i, j
	
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
	
	!paramenters and function name
	if(index(buffer, 'virial') /= 0) then
		read(1, *) funcs, M
	else
		read(1, *) funcs
		M = 2
	end if
	call lowerCase(funcs)
	print *, funcs, M
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
	i = 0
	do while(i == 0)
		read(1, '(A)', IOstat = i) buffer
		N = N + 1
		!print *, buffer, " N: ", N
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
	
	!other allocations
	allocate(beta(M))
	allocate(alpha(M,M))
	allocate(alphaMod(M,M))
	allocate(alphaSolve(M))
	!the good stuff
	flag = .true.
	do i=0, 5
		print *, '-------------------------------------------------------------------------------------'
		print *, 'cycle: ', i
		print *, 'lambsa: ', lambda
		if (flag) then
			error = findError(funcs, dataPoints, N, temp, guess, M)
			print *, 'The Error is ', error
			
			!beta
			beta = makeBeta(funcs, dataPoints, N, temp, guess, M)
			print *, 'The beta array is: ', beta
			
			!alpha
			alpha = makeAlpha(funcs, dataPoints, N, temp, guess, M)
		end if
		
		print *, 'The alpha array:'
		do j=1, M
			print *, alpha(j, :)
		end do
		
		!alpha prime
		alphaMod = modAlpha(alpha, M, lambda)
		print *, 'The modified alpha array:'
		do j=1, M
			print *, alphaMod(j, :)
		end do
		
		
		
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

