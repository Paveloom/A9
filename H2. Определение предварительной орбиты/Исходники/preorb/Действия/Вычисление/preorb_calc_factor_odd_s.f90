submodule ( preorb_do_m ) preorb_calc_factor_odd_s
implicit none

     contains

     ! Функция для вычисления факториала нечетных чисел
     module procedure preorb_calc_factor_odd

          integer(JP) :: i ! Счетчик
          integer(JP) :: res ! Результат

          res = 1_JP
          do i = 1_JP, n, 2_JP

               res = res * i

          enddo

          preorb_calc_factor_odd = real(res, kind = RP)

     end procedure preorb_calc_factor_odd

end submodule preorb_calc_factor_odd_s