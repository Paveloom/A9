submodule ( preorb ) preorb_calculate_s
implicit none

     contains

     ! Вспомогательная процедура для определения параметров
     ! предварительной орбиты по трем наблюдениям
     module procedure preorb_calculate

          if ( present(verbose) ) then

               if ( verbose ) then

                    call preorb_do_calculate_verbose(preorb%input, preorb%result)

               else

                    call preorb_do_calculate_non_verbose(preorb%input, preorb%result)

               endif

          else

               call preorb_do_calculate_non_verbose(preorb%input, preorb%result)

          endif

     end procedure preorb_calculate

end submodule preorb_calculate_s