module helper
	implicit none
	
	contains 
	!the functions
		function fit(func, temp, guess, M, volume, version) result(pressure)
			implicit none
			character(10), intent(in) :: func
			real, intent(in) :: temp, volume
			integer, intent(in) :: M, version
			real, intent(in) :: guess(M)
			real :: pressure
			real :: gasR = 8.31447
			integer :: i
			
			pressure = 0
			
			select case (func)
				case ('vdw')
					select case (version)
						case (0) !just the function
							pressure = ((gasR * temp)/(volume - guess(2))) - (guess(1)/(volume ** 2))
						case (1) !partial wrt param 1
							pressure = -1 / (volume ** 2)
						case (2) !partial wrt param 2
							pressure = (gasR * temp) / ((volume - guess(2))**2)
					end select
				case ('rk')
					select case (version)
						case (0) !just the function
							pressure = ((gasR * temp)/(volume - guess(2))) - (guess(1)/((temp**0.5) * volume * (volume + guess(2))))
						case (1) !partial wrt param 1
							pressure = -1 / ((temp ** 0.5) * volume * (volume + guess(2)))
						case (2) !partial wrt param 2
							pressure = ((gasR * temp)/((volume - guess(2))**2)) + (guess(1)/((temp**2) * volume * ((volume + guess(2))**2)))
					end select
				case ('dieterici')
					select case (version)
						case (0) !just the function
							pressure = (gasR * temp * exp((-1 * guess(1))/(gasR * temp * volume)))/(volume - guess(2))
						case (1) !partial wrt param 1
							pressure = (-1 * exp((-1 * guess(1))/(gasR * temp * volume))) / (volume * (volume - guess(2)))
						case (2) !partial wrt param 2
							pressure = (gasR * temp * exp((-1 * guess(1))/(gasR * temp * volume)))/((volume - guess(2))**2)
					end select
				case ('berthelot')
					select case (version)
						case (0) !just the function
							pressure = ((gasR * temp)/(volume - guess(2))) - (guess(1)/(temp * volume * volume))
						case (1) !partial wrt param 1
							pressure = -1 / (temp * volume * volume)
						case (2) !partial wrt param 2
							pressure = (gasR * temp) / ((volume - guess(2))**2)
					end select
				case ('virial')
					select case (version)
						case (0) !just the function
							pressure = (gasR * temp) / volume
							do i=1, M
								pressure = pressure + (gasR * temp * guess(i))/(volume**i)
							end do
						case (1:) !partial wrt param 1
							pressure = (gasR * temp) / (volume ** version)
					end select
			end select
		end function fit
	
	!the maths
		function findError(func, dataPoints, N, temp, guess, M) result(error)
			character(10), intent(in) :: func
			integer, intent(in) :: N, M
			real, intent(in) :: dataPoints(2,N), guess(M)
			real, intent(in) :: temp
			real :: error
			integer :: i
			error = 0
			
			do i=1, N
				error = error + (dataPoints(2,i) - fit(func, temp, guess, M, dataPoints(1,i), 0))**2
			end do
			
		end function findError
		
		function makeBeta(func, dataPoints, N, temp, guess, M) result(beta)
			character(10), intent(in) :: func
			integer, intent(in) :: N, M
			real, intent(in) :: dataPoints(2,N), guess(M)
			real, intent(in) :: temp
			real, dimension(M) :: beta
			integer :: i, j
			
			do i=1, M
				beta(i) = 0
				do j=0, N
					beta(i) = beta(i) + ((dataPoints(2,j) - fit(func, temp, guess, M, dataPoints(1,j), 0)) &
						* fit(func, temp, guess, M, dataPoints(1,j), i))
				end do
			end do
			
		end function makeBeta
	
		function makeAlpha(func, dataPoints, N, temp, guess, M) result(alpha)
			character(10), intent(in) :: func
			integer, intent(in) :: N, M
			real, intent(in) :: dataPoints(2,N), guess(M)
			real, intent(in) :: temp
			real, dimension(M,M) :: alpha
			integer :: i, j, k
			
			do i=1, M
				do j=1, M
					alpha(i,j) = 0
					do k=1, N
						alpha(i,j) = alpha(i,j) &
									+	fit(func, temp, guess, M, dataPoints(1,k), i) &
									*	fit(func, temp, guess, M, dataPoints(1,k), j)
					end do
				end do
			end do
			
		end function makeAlpha
		
		function modAlpha(alpha, M, lambda) result(alphaMod)
			integer, intent(in) :: M
			real, intent(in) :: alpha(M,M)
			real, intent(in) :: lambda
			real, dimension(M,M) :: alphaMod
			integer :: i, j
			
			do i=1, M
				do j=1, M
					alphaMod(i,j) = alpha(i,j)
					if(i == j) then
						alphaMod(i,j) = alphaMod(i,j) * (1 + lambda)
					end if
				end do
			end do
			
		end function modAlpha
		
		function linSolv(A, n, y) result(x)
			integer, intent(in) :: n 
			real, intent(in) :: A(n,n), y(n)
			real, dimension(n) :: x
			real, dimension(n,n+1) :: augmented
			integer :: i, j, k
			
			!make augmented matrix
			do i=1, n
				do j=1, n+1
					if(j <=n) then
						augmented(i,j) = A(i,j)
					else
						augmented(i,j) = y(i)
					end if
				end do
			end do
			
			!forward ellemnation
			do i=1, n
				do j=i, n
					augmented(j,:) = augmented(j, :) / augmented(i,i)
				end do
				
				do j=i+1, n
					augmented(j,:) = augmented(j,:) - augmented(i, :)
				end do
			end do
			
			do i=n, 1
				do j=i-1, 1
					
				end do
			end do
			
		end function linSolv
end module helper