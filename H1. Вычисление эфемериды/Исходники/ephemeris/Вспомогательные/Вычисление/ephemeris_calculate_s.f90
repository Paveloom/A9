submodule ( ephemeris ) ephemeris_calculate_s
implicit none

     contains

     ! Вспомогательная процедура для вычисления эфемериды
     module procedure ephemeris_calculate

          call ephemeris_do_calculate(ephemeris%input, ephemeris%result)

     end procedure ephemeris_calculate

end submodule ephemeris_calculate_s