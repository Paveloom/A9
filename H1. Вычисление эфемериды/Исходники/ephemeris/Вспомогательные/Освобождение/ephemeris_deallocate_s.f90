submodule ( ephemeris ) ephemeris_deallocate_s
implicit none
     
     contains
     
     ! Вспомогательная процедура для общего освобождения памяти
     module procedure ephemeris_deallocate
          
          call ephemeris%input%deallocate() ! Освобождение памяти из-под входных данных
          call ephemeris%result%deallocate() ! Освобождение памяти из-под результата
           
     end procedure ephemeris_deallocate
     
end submodule ephemeris_deallocate_s