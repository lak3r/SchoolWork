module helpP2
	implicit none
	
	contains
	!general purpose 
		! AV: http://computer-programming-forum.com/49-fortran/4075a24f74fcc9ce.ht
		subroutine lowerCase(word)
		! convert a word to lower case
			character (len=*) , intent(in out) :: word
			integer                            :: i,ic,nlen
			nlen = len(word)
			do i=1,nlen
			   ic = ichar(word(i:i))
			   if (ic >= 65 .and. ic < 90) word(i:i) = char(ic+32)
			end do
		end subroutine lowerCase
		
	!maths
		function makeG(cell) result(G)
			!This is hardcoded
			real(8), intent(in) :: cell(6)
			real(8), dimension(3,3) :: G
			real(8) :: pi
			
			pi = 3.1415927410125732421875
			
			G(1,1) = cell(1) * cell(1) * cos(0.0)
			G(1,2) = cell(1) * cell(2) * cos(cell(6)*(pi/180))
			G(1,3) = cell(1) * cell(3) * cos(cell(5)*(pi/180))
			
			G(2,1) = cell(2) * cell(1) * cos(cell(6)*(pi/180))
			G(2,2) = cell(2) * cell(2) * cos(0.0)
			G(2,3) = cell(2) * cell(3) * cos(cell(4)*(pi/180))
			
			G(3,1) = cell(3) * cell(1) * cos(cell(5)*(pi/180))
			G(3,2) = cell(3) * cell(2) * cos(cell(4)*(pi/180))
			G(3,3) = cell(3) * cell(3) * cos(0.0)
			
		end function makeG
		
		function findM(cell) result(M)
			real(8), intent(in) :: cell(6)
			real(8), dimension(3,3) :: M
			real(8) :: pi, toRad
			
			pi = 3.1415927410125732421875
			toRad = pi/180
			
			M(1,1) = cell(1)
			M(1,2) = cell(2) * cos(cell(6) * toRad)
			M(1,3) = cell(3) * cos(cell(5) * toRad)
			
			M(2,1) = 0.0
			M(2,2) = cell(2) * sin(cell(6) * toRad)
			M(2,3) = (cell(3)*(cos(cell(4)*toRad)-(cos(cell(5)*toRad)*cos(cell(6)*toRad))))/sin(cell(6)*toRad)
			
			M(3,1) = 0.0
			M(3,2) = 0.0
			M(3,3) = (cell(3) * ( &
						sin(cell(6) * toRad) **2 - &
						( cos(cell(4)*toRad)**2 + cos(cell(5)*toRad)**2 - &
						2 * cos(cell(4)*toRad) * cos(cell(5)*toRad) * cos(cell(6)*toRad)))**0.5 )&
						/ sin(cell(6) * toRad)
			
		end function findM
		
		function density(hklData, N, Xf, Vc) result(rho)
			integer, intent(in) :: N
			real(8), intent(in) :: hklData(6,N)
			real(8), intent(in) :: Xf(3)
			real(8), intent(in) :: Vc
			real(8) :: rho
			real(8) :: pi
			
			pi = 3.1415927410125732421875		
			
			rho = (2/Vc) * sum( &
					hklData(5,:) * & !hklData(1,:) * hklData(2,:) * hklData(3,:) * &
					cos(2 * pi *( &
						(hklData(1,:) * Xf(1)) + &
						(hklData(2,:) * Xf(2)) + &
						(hklData(3,:) * Xf(3)) ) &
					) + &
					hklData(6,:) * & !hklData(1,:) * hklData(2,:) * hklData(3,:) * &
					sin(2 * pi * ( &
						(hklData(1,:) * Xf(1)) + &
						(hklData(2,:) * Xf(2)) + &
						(hklData(3,:) * Xf(3)) )) &
					)
			
		end function density
		
		function gradient(hklData, N, Xf, Vc, toFrac) result(grad)
			integer, intent(in) :: N
			real(8), intent(in) :: hklData(6,N)
			real(8), intent(in) :: Xf(3), toFrac(3,3)
			real(8), intent(in) :: Vc
			real(8), dimension(3) :: grad
			real(8) :: pi
			integer :: i, j, k
			
			pi = 3.1415927410125732421875
			
			do i=1, 3
				grad(i) = (2/Vc) * sum( &
					hklData(6,:) * & !hklData(1,:) * hklData(2,:) * hklData(3,:) * &
					cos(2 * pi *( &
						(hklData(1,:) * Xf(1)) + &
						(hklData(2,:) * Xf(2)) + &
						(hklData(3,:) * Xf(3)) ) &
					) * 2 * pi *( &
						(hklData(1,:) * toFrac(1,i)) + &
						(hklData(2,:) * toFrac(2,i)) + &
						(hklData(3,:) * toFrac(3,i))  &
					) - &
					hklData(5,:) * & !hklData(1,:) * hklData(2,:) * hklData(3,:) * &
					sin(2 * pi * ( &
						(hklData(1,:) * Xf(1)) + &
						(hklData(2,:) * Xf(2)) + &
						(hklData(3,:) * Xf(3)) ) &
					) * 2 * pi *( &
						(hklData(1,:) * toFrac(1,i)) + &
						(hklData(2,:) * toFrac(2,i)) + &
						(hklData(3,:) * toFrac(3,i)) &
					) )
			end do
			
		end function gradient
		
		function hessian(hklData, N, Xf, Vc, toFrac) result(Ho)
			integer, intent(in) :: N
			real(8), intent(in) :: hklData(6,N)
			real(8), intent(in) :: Xf(3), toFrac(3,3)
			real(8), intent(in) :: Vc
			real(8), dimension(3,3) :: Ho
			real(8) :: pi
			integer :: i, j, k
			
			pi = 3.1415927410125732421875
			
			do i=1, 3
				do j=1, 3
					Ho(i,j) = (-2/Vc) * sum( &
						hklData(5,:) * & !hklData(1,:) * hklData(2,:) * hklData(3,:) * &
						cos(2 * pi *( &
							(hklData(1,:) * Xf(1)) + &
							(hklData(2,:) * Xf(2)) + &
							(hklData(3,:) * Xf(3)) ) &
						) * 2 * pi *( &
							(hklData(1,:) * toFrac(1,i)) + &
							(hklData(2,:) * toFrac(2,i)) + &
							(hklData(3,:) * toFrac(3,i))  &
						) * 2 * pi *( &
							(hklData(1,:) * toFrac(1,j)) + &
							(hklData(2,:) * toFrac(2,j)) + &
							(hklData(3,:) * toFrac(3,j))  &
						) + &
						hklData(6,:) * & !hklData(1,:) * hklData(2,:) * hklData(3,:) * &
						sin(2 * pi * ( &
							(hklData(1,:) * Xf(1)) + &
							(hklData(2,:) * Xf(2)) + &
							(hklData(3,:) * Xf(3)) ) &
						) * 2 * pi *( &
							(hklData(1,:) * toFrac(1,i)) + &
							(hklData(2,:) * toFrac(2,i)) + &
							(hklData(3,:) * toFrac(3,i)) &
						) * 2 * pi *( &
							(hklData(1,:) * toFrac(1,j)) + &
							(hklData(2,:) * toFrac(2,j)) + &
							(hklData(3,:) * toFrac(3,j))  &
						) )
				end do
			end do
			
		end function hessian
		
end module helpP2