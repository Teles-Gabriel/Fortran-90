program num_primos
implicit none 
integer :: i,j,k_div, N

write(*,*)'Insira um numero inteiro'
write(*,*)'serao calculados todos os numeros primos inteiros do intervalo [0,N)'
read(*,*) N

do i=1,N ! intervalo dos numeros primos
  k_div=0  ! o numero que comeca as divisoes
  do  j=1,i  ! um valor que comeca no "i" + 1
    if (j*(i/j)-i .eq. 0)then ! o if com a expressaom
       k_div=k_div+1 ! a conta em si 
    end if 
  end do
  if(k_div .eq. 2)then    ! o numero de possibilidades para ser inteiro
    write(*,*) i
  end if
end do

end program num_primos
