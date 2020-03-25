module ephemeris ! Модуль, определяющий интерфейс для
                 ! вычисления эфемерид малых планет
use ephemeris_input_m, only : input_type ! API для взаимодействия с входными данными
implicit none
     
     private
     public :: ephemeris_API ! Тип, определяющий API модуля
     
     ! Тип, определяющий API модуля
     type ephemeris_API
          
          type ( input_type ) :: input ! Экземпляр API для работы с входными данными 
          
     end type ephemeris_API
     
end module ephemeris