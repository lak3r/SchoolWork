program Project2
	!use statements here
	use helpP2
	use linAlg
	implicit none
	
!Variables

	!needed for smooth run
	character(100) :: buffer
	logical :: flag
	integer :: i, j, k
	character(1) :: c !single character holder
	
	!actually related to the problem at hand
	real(8), dimension(1:6) :: cell !a, b, c (all in angstroms, Å), and alpha, beta, gamma (all in degrees, ).
	real(8), allocatable :: hklData(:,:) 
	integer :: N !number of data points
	real(8) :: step !in anxtroms
	real(8), dimension(1:3, 1:3) :: G !metric tensor
	real(8) :: Vc !Volume of the unit cell
	real(8), dimension(3,3) :: toCart, toFrac
	
!initial settup and verifications
	
	!get command line input
	if (command_argument_count() >= 1) then
		call get_command_argument(1, buffer)
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
		print *, 'That file name does not exist. Exiting'
		stop
	end if
	

!reading the data
	
	!trivial comment line. Just needs to be printed.
	read(1, '(A)') buffer
	print *, buffer
	
	!Cell line
	read(1, *) buffer, cell
	print *, buffer
	print *, cell
	
	!hkl data
	read(1, '(A)') buffer !header line for data
	print *, buffer
	N = 0 !set up variables for input
	i = 0
	do while(i == 0) !find out how much data lines there are
		read(1, '(A)', IOstat = i) buffer
		N = N +1
	end do
	rewind 1 !reset to top of file
	N = N - 1
	print *, 'There are ', N, ' lines of data'
	allocate(hklData(6, N)) 
	do i=1, 3 !cycle lines to start of data
		read(1, '(A)') buffer
		!print *, buffer
	end do
	read(1, *) hklData !get data
	do i=1, 10 !print first 10 data lines
		print *, hklData(:, i)
	end do
	
	
!The Good Stuff
	
	!Calculate the Volume
	G = makeG(cell) !make the G matrix
	Vc = abs(det(G, 3))** 0.5
	print *, "the metric tensor G: "
	do i=1, 3
		print *, G(i, :)
	end do
	print *, "The Volume cell if " , Vc
	
	!Orthonormalization
	toCart = findM(cell)
	print *, 'Fracional to Cartesian matrix: '
	do i=1, 3
		print *, toCart(i, :)
	end do
	
	toFrac = invert(toCart, 3)
	
	print *, 'Cartesian to Fracional matrix:'
	do i=1, 3
		print *, toFrac(i, :)
	end do
	
	
!clean up
	close(1)


end program Project2