program Project3
	use numMeathods
	use helpP3
	use linAlg
	
	implicit none
	
!Variables
	
	!General smooth programming
	character(100) :: buffer
	logical :: flag
	integer :: i, j, k, l 
	character(1) :: c !single character holder
	real(8) :: startTime, finishTime
	integer :: sysTimeStart, sysTimeStop
	
	!problem specific 
	real(8), allocatable :: r(:,:), v(:,:)
	real(8) :: G, pi, mSun !constants
	real(8) :: deltT, Tj, mass
	integer :: N
	real(8), dimension(6) :: peri, ap !(rx, ry, rnorm, vx, vy, vnorm)
	
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
	Tj = 87.97 / (24 * 60 * 60)!Mercury orbit in seconds
	mass = 0.3301 * 10**24 !Mercury in kg
	
	!Allocate some things
	N = 1000000
	pi = 3.1415927410125732421875 
	G = 6.67384 * 10**(-11) !given gravitation constant 
	mSun = 1.98855 * 10**30 !kg
	allocate(r(2,N))
	allocate(v(2,N))
	r(1,1) = 46 * 10**6
	r(2,1) = 0
	v(1,1) = 0
	v(2,1) = -58.98
	deltT = Tj / N !in seconds
	
	!euler
	do i=2, N
		r(:,i) = v(:,i-1) - ((G * mSun)/(norm(r(:,i-1),2)**3)) * r(:,i-1) * deltT
	end do


!cleanup
	!timing
	call cpu_time(finishTime)
	call system_clock(sysTimeStop)
	print "(/,/,A, f10.5)", "CPU time: ", finishTime - startTime
	print "(/,/,A, i10)", "System time: ", sysTimeStop - sysTimeStart

end program Project3