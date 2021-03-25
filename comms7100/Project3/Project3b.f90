program Project3b
	use linAlg
	use ogpf  !for graphing (hopefully)
	
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
	integer, parameter :: N = 10000
	character(10), allocatable :: names(:)
	real(16), allocatable :: deets(:,:) !(Rx0, Ry0, Vx0, Vy0, Tj)
	real(16) :: r(2,N), v(2,N)
	real(16) :: G, pi, mSun, gSun !constants
	real(16) :: deltT, Tj, mass
	real(16) :: semimajorAxis, orbitPeriod, eccentricity, meanVelocity
	integer :: numPlanets
	real(16), dimension(6) :: peri, ap !(rx, ry, rnorm, vx, vy, vnorm)
	real(16), dimension(2) :: rHalf, vHalf
	
	!plotting
	TYPE(gpf) :: gp
	real(wp) :: x(N)
	real(wp) :: y(N)
	
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
	

	!opening the file
	inquire(file = "input_p3.txt", Exist = flag)
	if(flag) then
		open(1, file = "input_p3.txt", status = 'old')
	else if(flag .eqv. .false.) then
		print *, 'That file name does not exist. Exiting'
		stop
	end if
	
	!get the data
	i = 0
	numPlanets = -1
	do while(i == 0)
		read(1, '(A)', IOstat = i) buffer
		numPlanets = numPlanets + 1
	end do
	numPlanets = numPlanets - 1
	print *,"numPlanets = ", numPlanets
	rewind 1
	read(1, '(a)')
	allocate(names(numPlanets))
	allocate(deets(5,numPlanets))
	print *, "planet      X inital    Y inital    Vx inital   Vy inital   Tj given"
	do i=1, numPlanets
		read(1, *) names(i), deets(:,i)
		print '(A,5(2x,es10.3))', names(i), deets(:,i)
	end do
	
	
	!Allocate some things
	pi = 3.1415927410125732421875 
	G = 6.67384 * 10**(-20) !given gravitation constant (wrong though?)
	mSun = 1.98855 * 10**30 !kg
	gSun = 1.327126453 * 10**11 !G * mSun


	
!The math	
	print "(/,A)", "----------------------------------------------------------------------------"
	print "(A,/)", "----------------------------------------------------------------------------"
	print *, names(1)
	r(:,1) = deets(1:2,1) * 10**6
	v(:,1) = deets(3:4,1) 
	Tj = deets(5,1) * 24 * 60 * 60
	deltT = Tj / (N - 1) !in seconds
	print *, "Time step: ", deltT
	
	peri(1:2) = r(:,1)
	peri(3) = norm(peri(1:2),2)
	peri(4:5) = v(:,1)
	peri(6) = norm(peri(4:5),2)
	ap = peri
	meanVelocity = norm(v(:,1),2)
	
	!start animation
	call gp%animation_start(1)
	call gp%axis([-200000000.0_wp,200000000.0_wp,-200000000.0_wp,200000000.0_wp])
	call gp%options('set grid')
	
	do i=2, N	
		!Runge-Kutta
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
			
		!For graphing
		x = r(1,:)
		y = r(2,:)
		if(mod(i,100) == 0) then
			call gp%plot(x(1:i), y(1:i), 'w lines lc "red" lw 2')!, x2=x(i:i), y2=y(i:i),'w points ps 3 pt 7 lc "red"')
		end if
	end do
	print *, "out of loop"
	call gp%animation_show()
	
	
	print "(/,A)", "----------------------------------------------------------------------------"
	print "(/,A)", "Runge-Kutta method results"
	print "(/,A)", "Perihelion"
	print "(2(A,2x,es10.3,2x))", "Distance: ", peri(3), " Speed: ", peri(6)
	print *, "Aphelion"
	print "(2(A,2x,es10.3,2x))", "Distance: ", ap(3), " Speed: ", ap(6)	
	semimajorAxis = (abs(peri(3)) + abs(ap(3))) / 2
	print "(/,A,es10.3)", "Semimajor axis: ", semimajorAxis
	orbitPeriod = 2 * pi * sqrt(semimajorAxis**3 / gSun) / (24 * 60 * 60)
	print "(/,A,f10.3)", "Sidereal Orbit period: ", orbitPeriod
	eccentricity = (semimajorAxis - peri(3)) / semimajorAxis
	print "(/,A,f10.5)", "Orbit eccentricity: ", eccentricity
	meanVelocity = meanVelocity / N
	print "(/,A,f10.3)", "Mean orbital velocity: ", meanVelocity


!cleanup
	close(1)
	
	!timing
	call cpu_time(finishTime)
	call system_clock(sysTimeStop)
	print "(/,/,A, f10.5)", "CPU time: ", finishTime - startTime
	print "(A, i10)", "System time: ", sysTimeStop - sysTimeStart
end program Project3b
