module preorb_result_m ! Модуль, описывающий результат и
                       ! операции, связанные с ним
use prec_m, only : RP, & ! Точность вещественных чисел, используемых в программе
                 & RF, & ! Формат вывода вещественных чисел
                 & SP, & ! Точность целого числа статусной переменной
                 & JP, & ! Точность целого числа счетчика и индекса
                 & UP    ! Точность целого числа номера дескриптора файла
implicit none

     private
     public :: result_type ! Тип, определяющий результат

     ! Тип, определяющий результат
     type result_type

          real(RP) :: a ! Большая полуось
          real(RP) :: e ! Эксцентриситет орбиты
          real(RP) :: i ! Наклонение плоскости орбиты к плоскости эклиптики (градусная мера)
          real(RP) :: small_omega ! Угловое расстояние перигелия от восходящего узла (градусная мера)
          real(RP) :: capital_omega ! Гелиоцентрическая долгота восходящего узла (градусная мера)
          real(RP) :: M_0 ! Момент прохождения через перигелий (градусная мера)

          contains

          ! ! Процедура для записи результата в файл
          ! procedure :: write => preorb_result_write

          ! ! Процедуры для вывода ошибок (результат)
          ! procedure, private :: log => preorb_result_log_error

     end type result_type

     interface

          ! ! Процедура для вывода ошибок (результат)
          ! module impure subroutine preorb_result_log_error(result, error_code, file)
          ! implicit none

          !      class( result_type ), intent(inout) :: result ! Результат

          !      character(*), intent(in) :: error_code ! Код ошибки
          !      character(*), intent(in), optional :: file ! Имя файла

          ! end subroutine preorb_result_log_error

     end interface

end module preorb_result_m