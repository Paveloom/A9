submodule ( preorb ) preorb_deallocate_s
implicit none

     contains

     ! Вспомогательная процедура для общего освобождения памяти
     module procedure preorb_deallocate

          call preorb%input%deallocate() ! Освобождение памяти из-под входных данных
          ! call preorb%result%deallocate() ! Освобождение памяти из-под результата

     end procedure preorb_deallocate

end submodule preorb_deallocate_s