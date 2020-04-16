module preorb_input_m ! Модуль, описывающий входные данные и
                         ! операции, связанные с ними
use prec_m, only : RP, & ! Точность вещественных чисел, используемых в программе
                 & JP, & ! Точность целого числа счетчика и индекса
                 & SP, & ! Точность целого числа статусной переменной
                 & UP    ! Точность целого числа номера дескриптора файла
use, intrinsic :: iso_fortran_env ! Стандартный модуль содержащий
                                  ! информацию об окружении Fortran
implicit none

     private
     public :: input_type ! Тип, определяющий входные данные

     ! Тип, определяющий входные данные
     type input_type

          real(RP), dimension(:), allocatable :: dates ! Массив дат (юлианские дни)

          real(RP), dimension(:), allocatable :: alpha ! Массив прямых восхождений
          real(RP), dimension(:), allocatable :: delta ! Массив склонений

          real(RP), dimension(:), allocatable :: X ! Массив значений координат X Солнца
          real(RP), dimension(:), allocatable :: Y ! Массив значений координат Y Солнца
          real(RP), dimension(:), allocatable :: Z ! Массив значений координат Z Солнца

          contains

          ! Процедура для освобождения памяти (входные данные)
          procedure :: deallocate => preorb_input_deallocate

          ! Процедура для считывания входных данных
          procedure :: read => preorb_input_read

          ! Процедура для вывода ошибок (входные данные)
          procedure, private :: log => preorb_input_log_error

     end type input_type

     interface

          ! Процедура для вывода ошибок (входные данные)
          module impure subroutine preorb_input_log_error(input, error_code, file)
          implicit none

               class( input_type ), intent(inout) :: input ! Входные данные

               character(*), intent(in) :: error_code ! Код ошибки
               character(*), intent(in), optional :: file ! Имя файла

          end subroutine preorb_input_log_error

          ! Процедура для освобождения памяти (входные данные)
          module impure subroutine preorb_input_deallocate(input)
          implicit none

               class( input_type ), intent(inout) :: input ! Входные данные

          end subroutine preorb_input_deallocate

          ! Процедура для считывания входных данных
          module impure subroutine preorb_input_read(input, file)
          implicit none

               class( input_type ), intent(inout) :: input ! Входные данные
               character(*), intent(in), optional :: file ! Имя файла для считывания

          end subroutine preorb_input_read

     end interface

end module preorb_input_m