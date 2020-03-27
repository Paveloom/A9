submodule ( ephemeris ) ephemeris_calculate_s
implicit none

     contains

     ! Вспомогательная процедура для вычисления эфемериды
     module procedure ephemeris_calculate

          ! Проверка значения verbose
          if ( .not. present(verbose) ) then

               call ephemeris_do_calculate_non_verbose(ephemeris%input, ephemeris%result)

          else if ( verbose ) then

               call ephemeris_do_calculate_verbose(ephemeris%input, ephemeris%result)

          else

               call ephemeris_do_calculate_non_verbose(ephemeris%input, ephemeris%result)

          endif

     end procedure ephemeris_calculate

end submodule ephemeris_calculate_s