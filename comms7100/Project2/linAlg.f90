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
	
	
	!cross product
	



end module linAlg