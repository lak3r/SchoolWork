program Project1a
	implicit none
	
	!print *, 'Hello World'
	
	!Variables
	character(100) :: buffer	
	real :: temp !temperature assumed Kelvin
	real, allocatable :: guess(:), deltaGuess(:) !paramenters
	real, dimension(2, 1000) :: dataPoints!(:,:) !2d array with the shape [(x1,y1),...,(xm,ym)]
	real :: M !number of paramenters (real for math reasons) ((not sure if real is necisarry)
	character, dimension(2,10) :: units !(unitX,unitY)
	real :: lambda = 0.0001 !starting value of lambda
	real :: error, newError, deltaError !Can I really declare them this way?
	real, allocatable :: beta(:), alpha(:,:), alphaMod(:,:), alphaSolve(:) 
	!a reset variable maybe
	real :: variance
	real, allocatable :: standDev(:)
	logical :: flag;
	
	
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
	allocate(guess(int(M)))
	allocate(deltaGuess(int(M)))
	read(1, *) guess
	print *, guess
	read(1, *) buffer, temp, buffer
	print *, 'temp: ', temp, 'K '
	read(1, *) units !something is coming in ut it doesn't make sense?
	print *, units(1,:), units(2,:)
	read(1, *) dataPoints
	print *, dataPoints
	
	
	!clean up
	close(1)
	
end program Project1a