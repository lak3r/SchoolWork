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
	real(8) :: deltT, Tj, mass
	integer :: N
	
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
	Tj = 87.97 !Mercury orbit in days
	mass = 0.3301 * 10**24 !Mercury in kg
	allocate(r(2,N))
	allocate(v(2,N))
	
	!Allocate some things
	N = 1000000
	


!cleanup
	!timing
	call cpu_time(finishTime)
	call system_clock(sysTimeStop)
	print "(/,/,A, f10.5)", "CPU time: ", finishTime - startTime
	print "(/,/,A, i10)", "System time: ", sysTimeStop - sysTimeStart

end program Project3