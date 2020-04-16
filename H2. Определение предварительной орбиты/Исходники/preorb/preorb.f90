module preorb
use preorb_input_m, only : input_type ! Тип, определяющий входные данные
implicit none

     private
     public :: preorb_API ! API модуля

     type preorb_API

          type( input_type ) :: input ! Входные данные

          contains

          ! Вспомогательная процедура для общего освобождения памяти
          procedure :: deallocate => preorb_deallocate

     end type preorb_API

     interface

          ! Вспомогательная процедура для общего освобождения памяти
          module subroutine preorb_deallocate(preorb)
          implicit none

               class( preorb_API ), intent(inout) :: preorb ! Экземпляр API модуля

          end subroutine preorb_deallocate

     end interface

end module preorb