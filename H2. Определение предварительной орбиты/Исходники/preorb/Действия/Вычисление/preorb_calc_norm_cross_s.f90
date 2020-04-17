submodule ( preorb_do_m ) preorb_calc_norm_cross_s
implicit none

     contains

     ! Функция для вычисления нормы векторного произведения двух векторов
     module procedure preorb_calc_norm_cross

          real(RP), dimension(3) :: c

          call preorb_calc_cross(a, b, c)

          preorb_calc_norm_cross = norm2(c)

     end procedure preorb_calc_norm_cross

end submodule preorb_calc_norm_cross_s