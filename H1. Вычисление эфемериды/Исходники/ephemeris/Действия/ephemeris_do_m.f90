module ephemeris_do_m ! Модуль, содержащий процедуру для вычисления эфемериды
use prec_m, only : RP, & ! Точность вещественных чисел, используемых в программе
                 & IP, & ! Точность целых чисел, используемых в программе
                 & SP, & ! Точность целого числа статусной переменной
                 & JP    ! Точность целого числа счетчика и индекса
use ephemeris_input_m, only : input_type ! API для взаимодействия с входными данными
use ephemeris_result_m, only : result_type ! API для взаимодействия с результатом
implicit none
     
     private
     public :: ephemeris_do_calculate ! Процедура для вычисления эфемериды
     
     interface

          ! Процедура для вычисления эфемериды
          module subroutine ephemeris_do_calculate(input, result)
          implicit none
          
               type( input_type ), intent(in) :: input ! Входные данные
               type( result_type ), intent(inout) :: result ! Результат 
          
          end subroutine ephemeris_do_calculate

          ! Процедура для вывода ошибок (действия)
          module impure subroutine ephemeris_do_log_error(error_code)
          implicit none
               
               character(*), intent(in) :: error_code ! Код ошибки

          end subroutine ephemeris_do_log_error
     
     end interface
     
end module ephemeris_do_m