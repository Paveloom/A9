module ephemeris_result_m ! Модуль, описывающий результат и
                          ! операции, связанные с ним
use prec_m, only : RP, & ! Точность вещественных чисел, используемых в программе
                 & RF, & ! Формат вывода вещественных чисел
                 & SP, & ! Точность целого числа статусной переменной
                 & JP, & ! Точность целого числа счетчика и индекса
                 & UP    ! Точность целого числа номера дескриптора файла
use ephemeris_conversion_m, only : ephemeris_conversion_DD, & ! Функция для конвертации из радианной меры в градусную
                                 & ephemeris_conversion_DMS   ! Функция для конвертации из радианной меры в часовую
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
          module impure subroutine ephemeris_result_write(result, file, conversion)
          implicit none

               class( result_type ), intent(inout) :: result ! Результат
               character(*), intent(in), optional :: file ! Имя файла для записи

               ! Тумблер конвертации
               logical(kind(.true.)), optional, intent(in) :: conversion

          end subroutine ephemeris_result_write

     end interface

end module ephemeris_result_m