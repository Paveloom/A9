module ephemeris_do_m ! Модуль, содержащий процедуру для вычисления эфемериды
use prec_m, only : RP, & ! Точность вещественных чисел, используемых в программе
                 & IP, & ! Точность целых чисел, используемых в программе
                 & SP, & ! Точность целого числа статусной переменной
                 & JP, & ! Точность целого числа счетчика и индекса
                 & RF    ! Формат вывода вещественных чисел
use ephemeris_input_m, only : input_type ! API для взаимодействия с входными данными
use ephemeris_result_m, only : result_type ! API для взаимодействия с результатом
use ephemeris_conversion_m, only : ephemeris_conversion_DD, & ! Функция для конвертации из радианной меры в градусную
                                 & ephemeris_conversion_DMS   ! Функция для конвертации из радианной меры в часовую
implicit none

     private
     public :: ephemeris_do_calculate_verbose, &  ! Процедура для вычисления эфемериды (с дополнительным выводом)
             & ephemeris_do_calculate_non_verbose ! Процедура для вычисления эфемериды (без дополнительного вывода)

     ! Постоянная из уравнения движения
     real(RP), parameter :: chi = 0.017202_RP

     ! Постоянная малости для итераций уравнения Кеплера
     real(RP), parameter :: eps_K = 1e-8_RP

     ! Число 2 * pi
     real(RP), parameter :: pi_2 = 8._RP * atan(1._RP)

     ! Число pi / 180
     real(RP), parameter :: pi_180 = pi_2 / 360._RP

     ! Наклон экватора к эклиптике
     real(RP), parameter :: eps =  pi_180 * &
                            & (23._RP + 26._RP / 60._RP + 12.27_RP / 3600._RP)

     ! Вспомогательные переменные для вычисления
     ! координат в экваториальной системе координат
     real(RP), parameter :: sin_eps = sin(eps)
     real(RP), parameter :: cos_eps = cos(eps)

     interface

          ! Процедура для вычисления эфемериды (с дополнительным выводом)
          module impure subroutine ephemeris_do_calculate_verbose(input, result)
          implicit none

               type( input_type ), intent(in) :: input ! Входные данные
               type( result_type ), intent(inout) :: result ! Результат

          end subroutine ephemeris_do_calculate_verbose

          ! Процедура для вычисления эфемериды (без дополнительного вывода)
          module impure subroutine ephemeris_do_calculate_non_verbose(input, result)
          implicit none

               type( input_type ), intent(in) :: input ! Входные данные
               type( result_type ), intent(inout) :: result ! Результат

          end subroutine ephemeris_do_calculate_non_verbose

          ! Процедура для вывода ошибок (действия)
          module impure subroutine ephemeris_do_log_error(error_code)
          implicit none

               character(*), intent(in) :: error_code ! Код ошибки

          end subroutine ephemeris_do_log_error

     end interface

end module ephemeris_do_m