module ephemeris_input_m ! Модуль, описывающий входные данные и
                         ! операции, связанные с ними
use prec_m, only : RP, & ! Точность вещественных чисел, используемых в программе
                 & JP, & ! Точность целого числа счетчика и индекса
                 & SP, & ! Точность целого числа статусной переменной
                 & UP    ! Точность целого числа номера дескриптора файла
implicit none
     
     private
     public :: input_type ! Тип, определяющий входные данные
     
     ! Тип, определяющий входные данные
     type input_type

          integer(JP) :: N ! Размер массива дат (индекс)      

          real(RP), dimension(:), allocatable :: dates ! Массив дат (юлианские дни)

          real(RP) :: date ! Опорная дата (юлианские дни)

          real(RP) :: a ! Большая полуось
          real(RP) :: e ! Эксцентриситет орбиты
          real(RP) :: i ! Наклонение плоскости орбиты к плоскости эклиптики
          real(RP) :: small_omega ! Угловое расстояние перигелия от восходящего узла
          real(RP) :: capital_omega ! Гелиоцентрическая долгота восходящего узла
          real(RP) :: M_0 ! Момент прохождения через перигелий

          real(RP), dimension(:), allocatable :: X ! Массив координаты X Солнца (по массиву дат)
          real(RP), dimension(:), allocatable :: Y ! Массив координаты Y Солнца (по массиву дат)
          real(RP), dimension(:), allocatable :: Z ! Массив координаты Z Солнца (по массиву дат)

          contains

          ! Процедура для считывания входных данных
          procedure :: read => ephemeris_input_read

          ! Процедура для освобождения памяти (входные данные)
          procedure :: deallocate => ephemeris_input_deallocate

     end type input_type

     interface
     
          ! Процедура для считывания входных данных
          module impure subroutine ephemeris_input_read(input, file)
          implicit none
          
               class( input_type ), intent(inout) :: input ! Входные данные
               character(*), intent(in), optional :: file ! Имя файла для считывания
          
          end subroutine ephemeris_input_read

          ! Процедура для вывода ошибок (входные данные)
          module impure subroutine ephemeris_input_log_error(error_code, file)
          implicit none
               
               character(*), intent(in) :: error_code ! Код ошибки
               character(*), intent(in), optional :: file ! Имя файла

          end subroutine ephemeris_input_log_error

          ! Процедура для освобождения памяти (входные данные)
          module impure subroutine ephemeris_input_deallocate(input)
          implicit none
               
               class( input_type ), intent(inout) :: input ! Входные данные

          end subroutine ephemeris_input_deallocate
     
     end interface
     
end module ephemeris_input_m