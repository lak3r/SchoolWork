program Project3
	use numMeathods
	
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
	
	!timing
	call cpu_time(startTime)
	call system_clock(sysTimeStart)
	
!setup



!cleanup
	!timing
	call cpu_time(finishTime)
	call system_clock(sysTimeStop)
	print "(/,/,A, f10.5)", "CPU time: ", finishTime - startTime
	print "(/,/,A, i10)", "System time: ", sysTimeStop - sysTimeStart

end program Project3