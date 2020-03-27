module ephemeris_conversion_m ! Модуль, содержащий процедуры для конвертаций
                              ! из радианной меры угла в градусную и часовую
use prec_m, only : RP, & ! Точность вещественных чисел, используемых в программе
                 & IP    ! Точность целых чисел, используемых в программе
implicit none

     private
     public :: ephemeris_conversion_DD, & ! Функция для конвертации из радианной меры в градусную
             & ephemeris_conversion_DMS   ! Функция для конвертации из радианной меры в часовую

     ! Число pi
     real(RP), parameter :: pi = 4._RP * atan(1._RP)

     ! Размер вспомогательных строк
     integer(kind(1)), parameter :: chars = 40

     interface

          ! Функция для конвертации из радианной меры в градусную
          ! (с форматированием)
          module pure function ephemeris_conversion_DD(value) result(out)
          implicit none

               real(RP), intent(in) :: value ! Число в радианной мере
               character(:), allocatable :: out ! Строка-результат

          end function ephemeris_conversion_DD

          ! Функция для конвертации из радианной меры в часовую
          module pure function ephemeris_conversion_DMS(value) result(out)
          implicit none

               real(RP), intent(in) :: value ! Число в радианной мере
               character(:), allocatable :: out ! Строка-результат

          end function ephemeris_conversion_DMS

     end interface

end module ephemeris_conversion_m