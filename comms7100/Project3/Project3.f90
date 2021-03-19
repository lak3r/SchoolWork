program Project3
	!use numMeathods
	use helpP3
	use linAlg
	
	implicit none
	
!Variables
	
	!General smooth programming
	character(100) :: buffer
	logical :: flag
	integer :: i, j, k, l 
	character(1) :: c !single character holder
	real(16) :: temp
	real(8) :: startTime, finishTime
	integer :: sysTimeStart, sysTimeStop
	
	!problem specific 
	real(16), allocatable :: r(:,:), v(:,:)
	real(16) :: G, pi, mSun, gSun !constants
	real(16) :: deltT, Tj, mass
	real(16) :: semimajorAxis, orbitPeriod, eccentricity, meanVelocity
	integer :: N
	real(16), dimension(6) :: peri, ap !(rx, ry, rnorm, vx, vy, vnorm)
	real(16), dimension(2) :: rHalf, vHalf
	
	!timing
	call cpu_time(startTime)
	call system_clock(sysTimeStart)
	
!setup
	!get command line input
	if (command_argument_count() >= 1) then
		call get_command_argument(1, buffer)
	end if
	
	!this is a test function
	if (buffer == '-test') then
		print *, "Hello World!"
		stop
	end if
	
	!deal with input later
	Tj = 87.97 * 24 * 60 * 60!Mercury orbit in seconds
	mass = 0.3301 * 10**24 !Mercury in kg
	
	!Allocate some things
	N = 1000000
	pi = 3.1415927410125732421875 
	G = 6.67384 * 10**(-20) !given gravitation constant (wrong though?)
	mSun = 1.98855 * 10**30 !kg
	gSun = 1.327126453 * 10**11 !G * mSun
	allocate(r(2,N))
	allocate(v(2,N))
	r(1,1) = 46 * 10**6
	r(2,1) = 0
	v(1,1) = 0
	v(2,1) = 58.98
	deltT = Tj / (N - 1) !in seconds
	print *, "Time step: ", deltT
	
	peri(1:2) = r(:,1)
	peri(3) = norm(peri(1:2),2)
	peri(4:5) = v(:,1)
	peri(6) = norm(peri(4:5),2)
	ap = peri
	meanVelocity = norm(v(:,1),2)
	!euler
	do i=2, N
		r(:,i) = r(:,i-1) + v(:,i-1) * deltT
		v(:,i) = v(:,i-1) - ((gSun)/(norm(r(1:2,i-1),2)**3)) * r(:,i-1) * deltT
		
		meanVelocity = meanVelocity + norm(v(:,i),2)
		if(norm(v(:,i),2) > peri(6)) then
			peri(1:2) = r(:,i)
			peri(3) = norm(peri(1:2),2)
			peri(4:5) = v(:,i)
			peri(6) = norm(peri(4:5),2)
		else if(norm(v(:,i),2) < ap(6)) then
			ap(1:2) = r(:,i)
			ap(3) = norm(ap(1:2),2)
			ap(4:5) = v(:,i)
			ap(6) = norm(ap(4:5),2)
		end if
		
	end do
	!Orbit = deltT * i / ( 24 * 60 * 60 )
	print *, "----------------------------------------------------------------------------"
	print "(/,A)", "Euler method results"
	print "(/,A)", "Perihelion of Mercury"
	print "(2(A,2x,es10.3,2x))", "Distance: ", peri(3), " Speed: ", peri(6)
	print *, "Aphelion of Mercury"
	print "(2(A,2x,es10.3,2x))", "Distance: ", ap(3), " Speed: ", ap(6)
	!print "(/,A, f10.3)", "Sidereal orbit period (days): ", orbit
	semimajorAxis = (abs(peri(3)) + abs(ap(3))) / 2
	print "(/,A,es10.3)", "Semimajor axis: ", semimajorAxis
	orbitPeriod = sqrt(semimajorAxis**3 / gSun) / (24 * 60 * 60)
	print "(/,A,f10.3)", "Sidereal Orbit period: ", orbitPeriod
	eccentricity = (semimajorAxis - peri(3)) / semimajorAxis
	print "(/,A,f10.5)", "Orbit eccentricity: ", eccentricity
	meanVelocity = meanVelocity / N
	print "(/,A,f10.3)", "Mean orbital velocity: ", meanVelocity
	

	peri(1:2) = r(:,1)
	peri(3) = norm(peri(1:2),2)
	peri(4:5) = v(:,1)
	peri(6) = norm(peri(4:5),2)
	ap = peri
	meanVelocity = norm(v(:,1),2)
	!cromer
	do i=2, N
		v(:,i) = v(:,i-1) - ((gSun)/(norm(r(1:2,i-1),2)**3)) * r(:,i-1) * deltT
		r(:,i) = r(:,i-1) + v(:,i) * deltT
		
		meanVelocity = meanVelocity + norm(v(:,i),2)
		if(norm(v(:,i),2) > peri(6)) then
			peri(1:2) = r(:,i)
			peri(3) = norm(peri(1:2),2)
			peri(4:5) = v(:,i)
			peri(6) = norm(peri(4:5),2)
		else if(norm(v(:,i),2) < ap(6)) then
			ap(1:2) = r(:,i)
			ap(3) = norm(ap(1:2),2)
			ap(4:5) = v(:,i)
			ap(6) = norm(ap(4:5),2)
		end if
		
	end do
	
	print *, "----------------------------------------------------------------------------"
	print "(/,A)", "Cromer method results"
	print "(/,A)", "Perihelion of Mercury"
	print "(2(A,2x,es10.3,2x))", "Distance: ", peri(3), " Speed: ", peri(6)
	print *, "Aphelion of Mercury"
	print "(2(A,2x,es10.3,2x))", "Distance: ", ap(3), " Speed: ", ap(6)	
	semimajorAxis = (abs(peri(3)) + abs(ap(3))) / 2
	print "(/,A,es10.3)", "Semimajor axis: ", semimajorAxis
	eccentricity = (semimajorAxis - peri(3)) / semimajorAxis
	print "(/,A,f10.5)", "Orbit eccentricity: ", eccentricity
	meanVelocity = meanVelocity / N
	print "(/,A,f10.3)", "Mean orbital velocity: ", meanVelocity
	
	
	peri(1:2) = r(:,1)
	peri(3) = norm(peri(1:2),2)
	peri(4:5) = v(:,1)
	peri(6) = norm(peri(4:5),2)
	ap = peri
	meanVelocity = norm(v(:,1),2)
	!Runge-Kutta
	do i=2, N
		!take a "trial" step
		rHalf = r(:,i-1) + v(:,i-1) * (deltT / 2)
		vHalf = v(:,i-1) - ((gSun)/(norm(r(1:2,i-1),2)**3)) * r(:,i-1) * (deltT / 2)
		
		!Take full step with half a
		r(:,i) = r(:,i-1) + vHalf * deltT
		v(:,i) = v(:,i-1) - ((gSun)/(norm(rHalf,2)**3)) * rHalf * deltT
		
		meanVelocity = meanVelocity + norm(v(:,i),2)
		if(norm(v(:,i),2) > peri(6)) then
			peri(1:2) = r(:,i)
			peri(3) = norm(peri(1:2),2)
			peri(4:5) = v(:,i)
			peri(6) = norm(peri(4:5),2)
		else if(norm(v(:,i),2) < ap(6)) then
			ap(1:2) = r(:,i)
			ap(3) = norm(ap(1:2),2)
			ap(4:5) = v(:,i)
			ap(6) = norm(ap(4:5),2)
		end if
		
	end do
	
	print *, "----------------------------------------------------------------------------"
	print "(/,A)", "Runge-Kutta method results"
	print "(/,A)", "Perihelion of Mercury"
	print "(2(A,2x,es10.3,2x))", "Distance: ", peri(3), " Speed: ", peri(6)
	print *, "Aphelion of Mercury"
	print "(2(A,2x,es10.3,2x))", "Distance: ", ap(3), " Speed: ", ap(6)	
	semimajorAxis = (abs(peri(3)) + abs(ap(3))) / 2
	print "(/,A,es10.3)", "Semimajor axis: ", semimajorAxis
	eccentricity = (semimajorAxis - peri(3)) / semimajorAxis
	print "(/,A,f10.5)", "Orbit eccentricity: ", eccentricity
	meanVelocity = meanVelocity / N
	print "(/,A,f10.3)", "Mean orbital velocity: ", meanVelocity
	
	
	
	!For graphing
	open(2, file = "mercury.csv")
	write(2,'(*(G0.6,:,","))') 'X', 'Y'
	do i=1, N
		write(2,'(*(G0.6,:,","))') r(1,i), r(2,i)
	end do
	print "(/,/,A)", "written."
	close(2)

!cleanup
	!timing
	call cpu_time(finishTime)
	call system_clock(sysTimeStop)
	print "(/,/,A, f10.5)", "CPU time: ", finishTime - startTime
	print "(A, i10)", "System time: ", sysTimeStop - sysTimeStart

end program Project3