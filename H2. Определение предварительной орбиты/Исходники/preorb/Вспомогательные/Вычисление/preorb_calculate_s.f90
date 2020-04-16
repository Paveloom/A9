submodule ( preorb ) preorb_calculate_s
implicit none

     contains

     ! Вспомогательная процедура для определения параметров
     ! предварительной орбиты по трем наблюдениям
     module procedure preorb_calculate

          call preorb_do_calculate_verbose(preorb%input, preorb%result)

     end procedure preorb_calculate

end submodule preorb_calculate_s