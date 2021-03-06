module linAlg
	implicit none
	
	contains
	!determinant
		recursive function det(mat, N) result(determinant)
			integer, intent(in) :: N
			real(8), intent(in) :: mat(N,N)
			real(8), dimension(N-1,N-1) :: litMat
			real(8) :: determinant, holder
			integer :: i, j, k
			
			determinant = 0;
			if(N <= 2) then
				determinant = (mat(1,1) * mat(2,2)) - (mat(1,2) * mat(2,1))
			else 
				do i=1, N
					!construct little matrix
					k=1
					do j=1, N
						if(j /= i) then
							litMat(:,k) = mat(2:N,j)
							k = k + 1
						end if
					end do
					
					holder = mat(1,i) * det(litMat, N-1)
					if(mod(i,2) > 0) then
						holder = holder * (-1)
					end if
					
					determinant = determinant + holder
				end do
			end if
			
		end function det
	
		function invert(A, n) result(x)
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
			
		end function invert
	!cross product
	



end module linAlg