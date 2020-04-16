module preorb ! Модуль, описывающий интерфейс для
              ! определения параметров предварительной
              ! орбиты по трем наблюдениям
use preorb_input_m, only : input_type   ! Тип, определяющий входные данные
use preorb_result_m, only : result_type ! Тип, определяющий результат
use preorb_do_m, only : preorb_do_calculate_verbose ! Процедура для определения параметров предварительной
                                                    ! орбиты по трем наблюдениям (с дополнительным выводом)
implicit none

     private
     public :: preorb_API ! API модуля

     type preorb_API

          type( input_type ) :: input   ! Входные данные
          type( result_type ) :: result ! Результат

          contains

          ! Вспомогательная процедура для общего освобождения памяти
          procedure :: deallocate => preorb_deallocate

          ! Вспомогательная процедура для определения параметров
          ! предварительной орбиты по трем наблюдениям
          procedure :: calc => preorb_calculate

     end type preorb_API

     interface

          ! Вспомогательная процедура для общего освобождения памяти
          module subroutine preorb_deallocate(preorb)
          implicit none

               class( preorb_API ), intent(inout) :: preorb ! Экземпляр API модуля

          end subroutine preorb_deallocate

          ! Вспомогательная процедура для определения параметров
          ! предварительной орбиты по трем наблюдениям
          module subroutine preorb_calculate(preorb)
          implicit none

               class( preorb_API ), intent(inout) :: preorb ! Экземпляр API модуля

          end subroutine preorb_calculate

     end interface

end module preorb