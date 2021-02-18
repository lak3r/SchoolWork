program Project1a
	use helper
	implicit none
	
	
	!print *, 'Hello World'
	
	!Variables
	character(100) :: buffer	
	real(16) :: temp !temperature assumed Kelvin
	real(16), allocatable :: guess(:), deltaGuess(:) !paramenters
	real(16), allocatable :: dataPoints(:,:)
	integer :: M, N !number of paramenters 
	character(10) :: unitsX, unitsY, funcs !(unitX,unitY)
	real(16) :: lambda = 0.001 !starting value of lambda
	real(16) :: error, newError, deltaError !Can I real(16)ly declare them this way?
	real(16), allocatable :: beta(:), alpha(:,:), alphaMod(:,:), alphaSolve(:) 
	!a reset variable maybe
	real(16) :: variance, corCoef
	real(16), allocatable :: standDev(:)
	logical :: flag
	integer :: i, j, cnt
	
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
	N = N-1
	print *,'There are ', N, ' data points'
	allocate(dataPoints(2, N))
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
	allocate(standDev(M))
	
	!the good stuff
	flag = .true.
	cnt = 0
	i = 1
	do while(cnt < 3)
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
		
		!linear solve
		alphaSolve = linSolv(alphaMod, M, beta)
		print *, 'The change in the parameters is: '
		print *, alphaSolve
		
		deltaGuess = guess + alphaSolve
		print *, 'old paramenters: ', guess
		print *, 'new paramenters: ', deltaGuess
		
		newError = findError(funcs, dataPoints, N, temp, deltaGuess, M)
		deltaError = error - newError
		print *, 'old error: ', error
		print *, 'new error: ', newError
		print *, 'change in error: ', deltaError
		
		if (deltaError <= 0) then
			lambda = lambda * 10
			flag = .false.
		else if (deltaError > 0) then
			lambda = lambda / 10
			flag = .true.
			guess = deltaGuess
			print *, 'New paramenters accepted'
		else
			print *, 'There were only two options...how did you get here?'
		end if
		
		!stopping condition
		if (abs(deltaError) < 0.0001) then
			cnt = cnt + 1
		end if
		!failsafe
		if (i>=50) then 
			cnt = 5
		end if
		
		i = i + 1
	end do
	
	!Final Statistics
	print *, "-----------------------------------------------"
	print *, 'Final Statistics'
	
	print *, 'chi square: ', newError
	
	variance = findVariance(funcs, dataPoints, N, temp, guess, M)
	print *, 'sample variance: ', variance
	
	print *, 'variance-covariance matric C:'
	alphaMod = invert(alphaMod, M)
	do i=1, M
		print *, alphaMod(i,:)
	end do
	
	print *, 'Standard Deviation:'
	do i=1, M
		standDev(i) = (variance * alphaMod(i,i))**0.5
	end do
	print *, standDev
	
	if( M > 1) then
		print *, 'correlation coefficient: '
		corCoef = 1
		do i=1, M
			corCoef = corCoef * standDev(i)
		end do
		corCoef = (variance * alphaMod(1,2))/corCoef
		print *, corCoef
	end if
	
	print *, 'coefficient of determination R squared: ', 1 - (newError / sumSquared(dataPoints, N))
	print *, 'R bar squared: ', 1 - (newError * (real(N, 16) - 1))/(sumSquared(dataPoints, N) * real(N -M -1, 16))
	print *, 'R-Factor: ', 100 * findRFact(funcs, dataPoints, N, temp, guess, M), '%'
	
	print *, 'Final paramenters: '
	print *, guess
	!end final paramenters
	
	!output file for graphing
	
	
	
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

