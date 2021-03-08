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
	
	!invert a matrix
		function invert(A, n) result(x)
			integer, intent(in) :: n
			real(8), intent(in) :: A(n,n)
			real(8), dimension(n, n) :: x
			real(8), dimension(n,n*2) :: augmented
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
					if(augmented(j,i) /= 0) then
						augmented(j,:) = augmented(j, :) / augmented(j,i)
					end if
				end do
				
				do j=i+1, n
					if(augmented(j,i) /= 0) then
						augmented(j,:) = augmented(j,:) - augmented(i, :)
					end if
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
		
	!Transpose a matrix
		function trans(A, n, m) result(At)
			integer, intent(in) :: n, m !matrix dimensions
			real(8), intent(in) :: A(n,m)
			real(8), dimension(m,n) :: At
			integer :: i
			
			do i=1, n 
				At(:,i) = A(i,:)
			end do
		
		end function trans
	
	!Solve a linear system
		function linSolv(A, n, y) result(x)
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
					if(augmented(j,i) /= 0) then
						augmented(j,:) = augmented(j, :) / augmented(j,i)
					end if
				end do
				
				do j=i+1, n
					if(augmented(j,i) /= 0) then
						augmented(j,:) = augmented(j,:) - augmented(i, :)
					end if
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
			
		end function linSolv
	
	!The norm of a vector
		function norm(A, n) result(nrm)
			integer, intent(in) :: n 
			real(8), intent(in) :: A(n)
			real(8) :: nrm
			integer :: i
		
			nrm = sum(A**2)
			nrm = sqrt(nrm)
			
		end function norm
	
	!next line
end module linAlg