submodule ( preorb_do_m ) preorb_calc_cross_s
implicit none

     contains

     ! Функция для вычисления векторного произведения двух векторов
     module procedure preorb_calc_cross

          c(1) = a(2) * b(3) - a(3) * b(2)
          c(2) = a(3) * b(1) - a(1) * b(3)
          c(3) = a(1) * b(2) - a(2) * b(1)

     end procedure preorb_calc_cross

end submodule preorb_calc_cross_s