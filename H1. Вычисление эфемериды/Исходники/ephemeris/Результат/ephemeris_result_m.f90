module ephemeris_result_m ! Модуль, описывающий результат и
                          ! операции, связанные с ним
use prec_m, only : RP, & ! Точность вещественных чисел, используемых в программе
                 & RF, & ! Формат вывода вещественных чисел
                 & SP, & ! Точность целого числа статусной переменной
                 & UP    ! Точность целого числа номера дескриптора файла
implicit none

     private
     public :: result_type ! Тип, определяющий результат

     ! Тип, определяющий результат
     type result_type

          real(RP), dimension(:), allocatable :: dates ! Массив дат

          real(RP), dimension(:), allocatable :: alpha ! Массив значений прямых восхождений
          real(RP), dimension(:), allocatable :: delta ! Массив значений склонений

          contains

          ! Процедура для освобождения памяти (результат)
          procedure :: deallocate => ephemeris_result_deallocate

          ! Процедура для записи результата в файл
          procedure :: write => ephemeris_result_write

     end type result_type

     interface

          ! Процедура для вывода ошибок (результат)
          module impure subroutine ephemeris_result_log_error(error_code, file)
          implicit none

               character(*), intent(in) :: error_code ! Код ошибки
               character(*), intent(in), optional :: file ! Имя файла

          end subroutine ephemeris_result_log_error

          ! Процедура для освобождения памяти (результат)
          module impure subroutine ephemeris_result_deallocate(result)
          implicit none

               class( result_type ), intent(inout) :: result ! Результат

          end subroutine ephemeris_result_deallocate

          ! Процедура для записи результата в файл
          module impure subroutine ephemeris_result_write(result, file)
          implicit none

               class( result_type ), intent(inout) :: result ! Результат
               character(*), intent(in), optional :: file ! Имя файла для записи

          end subroutine ephemeris_result_write

     end interface

end module ephemeris_result_m