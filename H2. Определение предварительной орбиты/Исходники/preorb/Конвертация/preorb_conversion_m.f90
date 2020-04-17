module preorb_conversion_m ! Модуль, содержащий процедуры для конвертаций
                           ! из радианной меры угла в градусную и часовую
use prec_m, only : RP, & ! Точность вещественных чисел, используемых в программе
                 & IP, & ! Точность целых чисел, используемых в программе
                 & RF    ! Формат вывода вещественных чисел
implicit none

     private
     public :: preorb_conversion_DD, & ! Функция для конвертации из радианной меры в градусную
             & preorb_conversion_DMS, & ! Функция для конвертации из радианной меры в часовую
             & chars ! Размер вспомогательных строк

     ! Число pi
     real(RP), parameter :: pi = 4._RP * atan(1._RP)

     ! Размер вспомогательных строк
     integer(kind(1)), parameter :: chars = 40

     interface

          ! Функция для конвертации из радианной меры в градусную
          ! (с форматированием)
          module pure function preorb_conversion_DD(value) result(out)
          implicit none

               real(RP), intent(in) :: value ! Число в радианной мере
               character(:), allocatable :: out ! Строка-результат

          end function preorb_conversion_DD

          ! Функция для конвертации из радианной меры в часовую
          ! (с форматированием)
          module pure function preorb_conversion_DMS(value) result(out)
          implicit none

               real(RP), intent(in) :: value ! Число в радианной мере
               character(:), allocatable :: out ! Строка-результат

          end function preorb_conversion_DMS

     end interface

end module preorb_conversion_m