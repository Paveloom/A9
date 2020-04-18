submodule ( preorb_do_m ) preorb_calc_factor_even_s
implicit none

     contains

     ! Функция для вычисления факториала четных чисел
     module procedure preorb_calc_factor_even

          integer(JP) :: i ! Счетчик
          integer(JP) :: res ! Результат

          res = 1_JP
          do i = 2_JP, n, 2_JP

               res = res * i

          enddo

          preorb_calc_factor_even = real(res, kind = RP)

     end procedure preorb_calc_factor_even

end submodule preorb_calc_factor_even_s