module ephemeris ! Модуль, определяющий интерфейс для
                 ! вычисления эфемерид малых планет
use ephemeris_input_m, only : input_type ! API для взаимодействия с входными данными
use ephemeris_do_m, only : ephemeris_do_calculate ! Процедура для вычисления эфемериды
use ephemeris_result_m, only : result_type ! API для взаимодействия с результатом
implicit none

     private
     public :: ephemeris_API ! Тип, определяющий API модуля

     ! Тип, определяющий API модуля
     type ephemeris_API

          type( input_type ) :: input ! Экземпляр API для взаимодействия с входными данными
          type( result_type ) :: result ! Экземпляр API для взаимодействия с результатом

          contains

          ! Вспомогательная процедура для вычисления эфемериды
          procedure :: calculate => ephemeris_calculate

          ! Вспомогательная процедура для общего освобождения памяти
          procedure :: deallocate => ephemeris_deallocate

     end type ephemeris_API

     interface

          ! Вспомогательная процедура для вычисления эфемериды
          module subroutine ephemeris_calculate(ephemeris)
          implicit none

               class( ephemeris_API ) :: ephemeris ! Экземпляр API модуля

          end subroutine ephemeris_calculate

          ! Вспомогательная процедура для общего освобождения памяти
          module subroutine ephemeris_deallocate(ephemeris)
          implicit none

               class( ephemeris_API ) :: ephemeris ! Экземпляр API модуля

          end subroutine ephemeris_deallocate

     end interface

end module ephemeris