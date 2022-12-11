module vg
implicit none
real    :: G, DP, MedG, sigma,s
!real,dimension(:,:)     :: A
end module
!_______________________________________________________
program main
implicit none

!call leitura
!call anal
call Desvio_Padrao

end program main
!_______________________________________________________
subroutine leitura
use vg
implicit none
integer ::i

open(1,file="dados.txt",status='OLD')
  do i=1,4
    read(1,*) G
    write(2,*) G
    write(*,*) G
  end do
close(1)
close(2)

end subroutine leitura
!_______________________________________________________
!subroutine anal
!use vg
!implicit none
!integer :: j

!MedG=0.0

!open(2,file="dados.txt",status='OLD')
!do j=1,10
!   MedG=MedG+G/10
  ! DP =sqrt((G/10)-(MedG/10)**2)
  ! A(10,10)=MedG
  ! write(*,*) A(10,10)
!   write(*,*) DP
!end do
!close(2)

!end subroutine anal
!_______________________________________________________
subroutine Desvio_Padrao
use vg
implicit none
integer ::k,l,m
s=0.0
sigma=0.0
MedG=0.0
open(4,file="dados.txt",status='OLD')
do k=1,4
   read(4,*) G
   MedG=(MedG+G)
end do

MedG=MedG/4

write(*,*)"media",  MedG
   do l=1,4
     s=((G)**2 -(2*G*MedG) + (MedG)**2)
     write(*,*) "vairancia", s
 !    DP=sqrt(sigma)
   end do
   do m=1,4 
  sigma=(sigma+s)
  sigma=sigma/4
          write(*,*) sigma
  end do
close(4)
write(*,*) "media e algo mais" ,  MedG


end subroutine Desvio_Padrao
