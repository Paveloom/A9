module preorb_do_m ! Модуль, содержащий процедуры для определения
                   ! параметров предварительной орбиты по трем наблюдениям
use prec_m, only : RP, & ! Точность вещественных чисел, используемых в программе
                 & IP, & ! Точность целых чисел, используемых в программе
                 & SP, & ! Точность целого числа статусной переменной
                 & JP, & ! Точность целого числа счетчика и индекса
                 & RF    ! Формат вывода вещественных чисел
use preorb_input_m, only : input_type ! API для взаимодействия с входными данными
use preorb_result_m, only : result_type ! API для взаимодействия с результатом
! use preorb_conversion_m, only : preorb_conversion_DD, & ! Функция для конвертации из радианной меры в градусную
                              ! & preorb_conversion_DMS   ! Функция для конвертации из радианной меры в часовую
implicit none

     private
     public :: preorb_do_calculate_verbose!, &  ! Процедура для определения параметров предварительной
                                               ! орбиты по трем наблюдениям (с дополнительным выводом)
          !    & preorb_do_calculate_non_verbose ! Процедура для определения параметров предварительной
                                               ! орбиты по трем наблюдениям (без дополнительного вывода)

     ! Постоянная из уравнения движения
     real(RP), parameter :: chi_sq = 0.017202_RP ** 2._RP

     ! Постоянная малости для итераций
     real(RP), parameter :: eps_e = 1e-8_RP

     ! Число pi
     real(RP), parameter :: pi = 4._RP * atan(1._RP)

     ! Число pi / 12
     real(RP), parameter :: pi_12 = pi / 12._RP

     ! Число pi / 180
     real(RP), parameter :: pi_180 = pi / 180._RP

     ! Наклон экватора к эклиптике
     real(RP), parameter :: eps =  pi_180 * &
                            & (23._RP + 26._RP / 60._RP + 12.27_RP / 3600._RP)

     interface

          ! Процедура для определения параметров предварительной
          ! орбиты по трем наблюдениям (с дополнительным выводом)
          module impure subroutine preorb_do_calculate_verbose(input, result)
          implicit none

               type( input_type ), intent(inout) :: input ! Входные данные
               type( result_type ), intent(inout) :: result ! Результат

          end subroutine preorb_do_calculate_verbose

          ! Процедура для определения параметров предварительной
          ! орбиты по трем наблюдениям (без дополнительного вывода)
          ! module impure subroutine preorb_do_calculate_non_verbose(input, result)
          ! implicit none

          !      type( input_type ), intent(in) :: input ! Входные данные
          !      type( result_type ), intent(inout) :: result ! Результат

          ! end subroutine preorb_do_calculate_non_verbose

          ! Процедура для вывода ошибок (действия)
          module impure subroutine preorb_do_log_error(input, error_code)
          implicit none

               type( input_type ), intent(inout) :: input ! Входные данные

               character(*), intent(in) :: error_code ! Код ошибки

          end subroutine preorb_do_log_error

     end interface

end module preorb_do_m