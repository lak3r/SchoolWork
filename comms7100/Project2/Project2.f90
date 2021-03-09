program Project2
	!use statements here
	use helpP2
	use linAlg
	implicit none
	
!Variables

	!needed for smooth run
	character(100) :: buffer
	logical :: flag
	integer :: i, j, k, l 
	character(1) :: c !single character holder
	
	!actually related to the problem at hand
	real(8), dimension(6) :: cell !a, b, c (all in angstroms, Å), and alpha, beta, gamma (all in degrees, ).
	real(8), allocatable :: hklData(:,:), checkListCords(:,:)
	character(5), allocatable :: checkListNames(:)
	integer :: N, numInCheckList, stepNum, numPoints, step, peakNum, truePeakIndex !number of data points
	real(8) :: stepSizeMax !in anxtroms
	real(8), dimension(3,3) :: G, hes
	real(8) :: Vc, rho, gradNorm
	real(8), dimension(3,3) :: toCart, toFrac
	real(8), dimension(3) :: Xf, Xc, grad, h, legnth 
	real(8), dimension(3) :: gridNum, gridStep, currentPointFrac, currentPointCart
	real(8), allocatable :: gridPoints
	real(8), dimension(6,1000) :: peak !(Xc, Yc, Zc, distance to peak, rho, gradient)
	real(8) :: distanceToTruePeak, hold
	
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
	
	!get the checkList data
	if (command_argument_count() >= 2) then
		call get_command_argument(2, buffer)
	else
		print *, "No fractional list of coordinates provided"
		stop
	end if
	inquire(file = buffer, Exist = flag)
	if (flag) then
		open(2, file = buffer, status = 'old')
	else if(flag .eqv. .false.) then
		print *, 'That file name does not exist. Exiting'
		stop
	end if
	!read the checklist in
	i=0
	numInCheckList = 0
	do while(i == 0)
		read(2, '(A)', IOstat = i) buffer
		numInCheckList = numInCheckList + 1
	end do
	numInCheckList = numInCheckList -1
	rewind 2
	allocate(checkListCords(3,numInCheckList))
	allocate(checkListNames(numInCheckList))
	do i=1, numInCheckList 
		read(2, *) checkListNames(i),checkListCords(:,i)
		!print *, checkListNames(i), checkListCords(:,i)
	end do
	
	
!reading the data
	
	!trivial comment line. Just needs to be printed.
	read(1, '(A)') buffer
	print *, buffer
	
	!Cell line
	read(1, *) buffer, cell
	print *, buffer
	print "(6(es10.3, 3X))", cell
	
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
	!do i=1, 10 !print first 10 data lines
		!print "(6(es10.3, 3X))", hklData(:,i)
		!print *, hklData(:, i)
	!end do
	
	
!The Good Stuff
	
	!Calculate the Volume
	G = makeG(cell) !make the G matrix
	Vc = abs(det(G, 3))** 0.5
	print "(/,A)", "the metric tensor G: "
	do i=1, 3
		print "(3(es10.3, 3X))", G(i, :)
	end do
	print "(/,A,f10.3)", "The Volume cell is " , Vc
	
	!Orthonormalization
	toCart = findM(cell)
	print "(/,A)", 'Fracional to Cartesian matrix: '
	do i=1, 3
		print "(3(f10.6, 3X))", toCart(i, :)
	end do
	toFrac = invert(toCart, 3)
	print "(/,A)", 'Cartesian to Fracional matrix:'
	do i=1, 3
		print "(3(f10.6, 3X))", toFrac(i, :)
	end do
	
	!Defins a grid
	!suggested not larger that 0.4 anst in fractional coordinates
	!	0.1 <= Xf <= 0.9
	!	0.1 <= Yf <= 0.9
	!	0.1 <= Zf <= 0.9
	
	stepSizeMax = 0.25
	
	!The initial coordinates in fractional coordinates
	Xf(1) = 0.1000000000000000000000000 !0.393240
	Xf(2) = 0.1000000000000000000000000 !0.377510
	Xf(3) = 0.1000000000000000000000000 !0.690940
	Xc = matmul(toCart, Xf)
	
	
	legnth(1) = cell(1) * 0.8
	legnth(2) = cell(2) * 0.8
	legnth(3) = cell(3) * 0.8
	
	do i=1, 3
		gridNum(i) = ceiling(legnth(i) / 0.4 ) + 1
	end do
	print "(/,A)", "The Number of grid points along crystal axes"
	print "(3(i5, 3x))", int(gridNum)
	
	print "(/,A, es10.3)", "0.1 <= a <= 0.9,   legnth =  ", legnth(1)
	print "(A, es10.3)", "0.1 <= a <= 0.9,   legnth =  ", legnth(2)
	print "(A, es10.3)", "0.1 <= a <= 0.9,   legnth =  ", legnth(3)
	
	do i=1, 3
		gridStep(i) = 0.4 / cell(i)
		!print *, gridStep(i)
	end do
	print "(/,A)", "Grid step size (frac) along crystal coordinates axes"
	print "(3(f10.5, 3x))", gridStep !this is priting wrong
	
	numPoints = gridNum(1) * gridNum(2) * gridNum(3)
	print *
	print *, "There are ", numPoints, " points on the grid"
	
	
	!print "(/,A)", "The coordinates in fractional form: "
	!print *, Xf
	!print "(/,A)", "The coordinates in Cartesian form: "
	!print *, Xc
	
	peakNum = 1
	step = 0
	do k=1, int(gridNum(1))
		Xf(1) = Xf(1) + gridStep(1)
		do j=1, int(gridNum(2))
			Xf(2) = Xf(2) + gridStep(2)
			do l=1, int(gridNum(3))
				print "(/,A)", "--------------------------------------------------------------"
				print *, "--------------------------------------------------------------"
				print *, "Starting point ", step
				
				Xf(3) = Xf(3) + gridStep(3)
				Xc = matmul(toCart, Xf)
				currentPointFrac = Xf
				currentPointCart = Xc
				
				print "(/,A)", "---Step 0"
				
				print "(/,A)", "The Fracional Coordinates: "
				print "(3(f10.6, 3x))", currentPointFrac
				print *, "The Cartesian Coordinates: "
				print "(3(f10.6, 3x))", currentPointCart
				
				rho = density(hklData, N, Xf, Vc)
				print "(/,A,f7.3)", "The density is", rho
				
				grad = gradient(hklData, N, Xf, Vc, toFrac)
				!print "(/,A)", "The gradient is: "
				!print "(3(es10.3, 3X))", grad
				gradNorm = norm(grad, 3)
				print "(/,A,es10.3)", "The norm is: ", gradNorm
				
				!print "(/,A)", "The Hessian matrix is: "
				hes = hessian(hklData, N, Xf, Vc, toFrac)
				do i=1, 3
					!print "(3(es10.3, 3x))", hes(i,:)
				end do
				
				stepNum = 1
				
				do while(gradNorm > 0.00001 .and. stepNum < 15)
					!print *, "---------------------------------------------------------------"
					!print *, "This is step number: ", stepNum
					!advnace the point
					hes = invert(hes, 3)
					h = (-1) * matmul(hes, grad)
					!print "(/,A)", "The h is: "
					!print "(3(es10.3, 3x))", h
					!print *, norm(h,3)
					if( norm(h, 3) > stepSizeMax) then
						h = 0.25 * h/norm(h,3)
					end if
					
					currentPointCart = currentPointCart + h
					currentPointFrac = matmul(toFrac, currentPointCart)
					
					
					rho = density(hklData, N, currentPointFrac, Vc)
					!print *
					!print *, "The density is", rho
					
					grad = gradient(hklData, N, currentPointFrac, Vc, toFrac)
					!print "(/,A)", "The gradient is: "
					!print "(3(es10.3, 3X))", grad
					gradNorm = norm(grad, 3)
					
					
					hes = hessian(hklData, N, currentPointFrac, Vc, toFrac)
					
					stepNum = stepNum + 1
				end do
				
				print "(/,A,i3)", "---Step ", stepNum
				!print "(/,A)", "The h is: "
				!print "(3(es10.3, 3x))", h
				print "(/,A)", "The Fracional Coordinates: "
				print "(3(f10.6, 3x))", currentPointFrac
				print *, "The Cartesian Coordinates: "
				print "(3(f10.6, 3x))", currentPointCart
				
				print "(/,A,f7.3)", "The density is", rho
				print "(/,A,es10.3)", "The gradient is: ", gradNorm
				print "(/,A)", "-------"
				
				
				
				if(stepNum >= 15) then
					print *, "Too many steps. Peak not found"
				else if(gradNorm <= 0.00001) then
					print *, "Gradient is small. Found Stationary point. Peak? TBD."
					print "(A, f18.16,A)", "The distance from starting point is: ", norm(Xc-currentPointCart, 3), " angstroms"
					if(rho > 2) then
						print *, "Peak found!!!!"
						if(findPeak(peak(:,1:peakNum),peakNum, currentPointCart) == -1) then
							print *, "It's a new peak. Yippee!"
							
							Peak(1:3, peakNum) = currentPointCart
							Peak(4, peakNum) = norm(Xc-currentPointCart, 3)
							peak(5, peakNum) = rho
							peak(6, peakNum) = gradNorm
							peakNum = peakNum + 1
						else
							print *, "Duplicate peak..."
						end if
					else
						print *, "density is too small... :("
					end if
				end if
				
				step = step + 1
			end do
			Xf(3) = 0.10000000
		end do
		Xf(2) = 0.1000000
	end do
	
	print "(/,/,2A,/)", "-----------------------------------------------------------------------------------"
	print "(A,i5)", "Number of peaks found: ", peakNum - 1
	print *, "#  -------------xyz(FRA)-------------     -------------xyz(Car)-------------       rho        gradient    &
				dist to closest atom"
	do i=1, peakNum -1
		Xc = peak(1:3,i)
		Xf = matmul(toFrac, Xc)
		distanceToTruePeak = 50000
		do j=1, numInCheckList
			hold = abs(norm(checkListCords(:,j) - Xf, 3))
			if(hold < distanceToTruePeak) then
				distanceToTruePeak = hold
				truePeakIndex = j
			end if		
		end do
		print "(i2, 6(f10.6, 3x), f10.5, 3x, es10.3, f10.4)", i, Xf, Xc, peak(5,i), peak(6,i)
	end do
	
	
	
!clean up
	close(1)


end program Project2