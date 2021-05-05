module helper
	implicit none
	
	contains 
	!the functions
		function fit(func, temp, guess, M, volume, version) result(pressure)
			implicit none
			integer, parameter :: qp = selected_real_kind(33, 4931)
			character(10), intent(in) :: func
			real(16), intent(in) :: temp, volume
			integer, intent(in) :: M, version
			real(16), intent(in) :: guess(M)
			real(16) :: pressure
			real(16) :: gasR = 8.31447_qp
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
							pressure = ((gasR * temp)/(volume - guess(2))) - (guess(1)/(sqrt(temp) * volume * (volume + guess(2))))
						case (1) !partial wrt param 1
							pressure = -1 / (sqrt(temp) * volume * (volume + guess(2)))
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
								pressure = pressure + (gasR * temp * guess(i))/(volume**(i+1))
							end do
						case (1:) !partial wrt param 1
							pressure = (gasR * temp) / (volume ** (version+1))
					end select
			end select
		end function fit
	
	!the maths
		function findError(func, dataPoints, N, temp, guess, M) result(error)
			character(10), intent(in) :: func
			integer, intent(in) :: N, M
			real(16), intent(in) :: dataPoints(2,N), guess(M)
			real(16), intent(in) :: temp
			real(16) :: error
			integer :: i
			error = 0
			
			do i=1, N
				error = error + (dataPoints(2,i) - fit(func, temp, guess, M, dataPoints(1,i), 0))**2
			end do
		end function findError
		
		function makeBeta(func, dataPoints, N, temp, guess, M) result(beta)
			character(10), intent(in) :: func
			integer, intent(in) :: N, M
			real(16), intent(in) :: dataPoints(2,N), guess(M)
			real(16), intent(in) :: temp
			real(16), dimension(M) :: beta
			integer :: i, j
			
			do i=1, M
				beta(i) = 0
				do j=1, N
					beta(i) = beta(i) + ((dataPoints(2,j) - fit(func, temp, guess, M, dataPoints(1,j), 0)) &
						* fit(func, temp, guess, M, dataPoints(1,j), i))
				end do
			end do
			
		end function makeBeta
	
		function makeAlpha(func, dataPoints, N, temp, guess, M) result(alpha)
			character(10), intent(in) :: func
			integer, intent(in) :: N, M
			real(16), intent(in) :: dataPoints(2,N), guess(M)
			real(16), intent(in) :: temp
			real(16), dimension(M,M) :: alpha
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
			real(16), intent(in) :: alpha(M,M)
			real(16), intent(in) :: lambda
			real(16), dimension(M,M) :: alphaMod
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
		
		function linSolv1(A, n, y) result(x)
			integer, intent(in) :: n
			real(16), intent(in) :: A(n,n), y(n)
			real(16), dimension(n) :: x
			real(16), dimension(n,n+1) :: augmented
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
					augmented(j,:) = augmented(j, :) / augmented(j,i)
				end do
				
				do j=i+1, n
					augmented(j,:) = augmented(j,:) - augmented(i, :)
				end do
			end do
			
			!backward ellimnation
			do i=n, 2, -1
				do j=i-1, 1, -1
					augmented(j,:) = augmented(j,:) - (augmented(i,:) * augmented(j, i))
				end do
			end do
			
			do i=1, n
				x(i) = augmented(i, n+1)
			end do
			
		end function linSolv1
		
		function findVariance(func, dataPoints, N, temp, guess, M) result(vari)
			character(10), intent(in) :: func
			integer, intent(in) :: N, M
			real(16), intent(in) :: dataPoints(2,N), guess(M)
			real(16), intent(in) :: temp	
			real(16) :: vari
			integer :: i
			
			vari = 0
			do i=1, N
				vari = vari + (dataPoints(2,i) - fit(func, temp, guess, M, dataPoints(1,i), 0))**2
			end do
			
			vari = vari * (1/real((N - M), 16))
			
		end function findVariance
		
		function invert1(A, n) result(x)
			integer, intent(in) :: n
			real(16), intent(in) :: A(n,n)
			real(16), dimension(n, n) :: x
			real(16), dimension(n,n*2) :: augmented
			integer :: i, j, k
			
			!make augmented matrix
			do i=1, n
				do j=1, n*2
					if(j <=n) then
						augmented(i,j) = A(i,j)
					else if(j == i + n) then
						augmented(i,j) = 1
					else
						augmented(i,j) = 0
					end if
				end do
			end do
			
			!forward ellemnation
			do i=1, n
				do j=i, n
					augmented(j,:) = augmented(j, :) / augmented(j,i)
				end do
				
				do j=i+1, n
					augmented(j,:) = augmented(j,:) - augmented(i, :)
				end do
			end do
			
			!backward ellimnation
			do i=n, 2, -1
				do j=i-1, 1, -1
					augmented(j,:) = augmented(j,:) - (augmented(i,:) * augmented(j, i))
				end do
			end do
			
			do i=1, n
				do j=1, n
					x(i,j) = augmented(i, j+ n)
				end do
			end do
			
		end function invert1
		
		function sumSquared(dataPoints, N) result(sumSquare)
			integer, intent(in) :: N
			real(16), intent(in) :: dataPoints(2,N)
			real(16) :: sumSquare, mean
			integer :: i
			
			mean = sum(dataPoints(2,:)) / real(N, 16)
			
			sumSquare = 0
			do i=1, N
				sumSquare = sumSquare + (dataPoints(2, i) - mean)**2
			end do
		end function sumSquared
		
		function findRFact(func, dataPoints, N, temp, guess, M) result(rFact)
			character(10), intent(in) :: func
			integer, intent(in) :: N, M
			real(16), intent(in) :: dataPoints(2,N), guess(M)
			real(16), intent(in) :: temp	
			real(16) :: rFact, tot
			integer :: i
			
			rFact = 0.d-0
			tot = 0.d-0
			do i=1, N
				rFact = rFact + abs(dataPoints(2,i) - fit(func, temp, guess, M, dataPoints(1,i), 0))
				tot = tot + abs(dataPoints(2,i))
			end do
			
			rFact = rFact / tot
			
		end function findRFact
end module helper
