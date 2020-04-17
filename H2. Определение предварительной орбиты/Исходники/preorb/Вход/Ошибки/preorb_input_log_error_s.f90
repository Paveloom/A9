submodule ( preorb_input_m ) preorb_input_log_error_s
implicit none

     contains

     ! Процедура для вывода ошибок (входные данные)
     module procedure preorb_input_log_error

          select case (error_code) ! Проверка кода ошибки

               case ('WO')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_input_log_error:', 'Не удалось получить доступ к файлу '//file//'&
                                                                                 & для чтения или записи.'
                    call input%deallocate()
                    stop

               case ('EOF')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_input_log_error:', 'Неожиданный конец файла ('//file//').'
                    call input%deallocate()
                    stop

               case ('WR_dates')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_input_log_error:', 'Не удалось считать значения массива дат в&
                                                                                 & файле '//file//'. Проверьте правильность&
                                                                                 & введенных данных.'
                    call input%deallocate()
                    stop

               case ('WR_alpha')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_input_log_error:', 'Не удалось считать значения массива&
                                                                                 & прямых восхождений в файле '//file//'.&
                                                                                 & Проверьте правильность введенных данных.'
                    call input%deallocate()
                    stop

               case ('WR_delta')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_input_log_error:', 'Не удалось считать значения массива&
                                                                                 & склонений в файле '//file//'.&
                                                                                 & Проверьте правильность введенных данных.'
                    call input%deallocate()
                    stop

               case ('WR_X')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_input_log_error:', 'Не удалось считать значения массива&
                                                                                 & значений координат X Солнца в файле '//file//'.&
                                                                                 & Проверьте правильность введенных данных.'
                    call input%deallocate()
                    stop

               case ('WR_Y')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_input_log_error:', 'Не удалось считать значения массива&
                                                                                 & значений координат Y Солнца в файле '//file//'.&
                                                                                 & Проверьте правильность введенных данных.'
                    call input%deallocate()
                    stop

               case ('WR_Z')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_input_log_error:', 'Не удалось считать значения массива&
                                                                                 & значений координат Z Солнца в файле '//file//'.&
                                                                                 & Проверьте правильность введенных данных.'
                    call input%deallocate()
                    stop

               case ('WA_dates')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_input_log_error:', 'Не удалось выделить память под массив дат&
                                                                                 & для объекта типа input_type.'
                    call input%deallocate()
                    stop

               case ('WA_alpha')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_input_log_error:', 'Не удалось выделить память под массив прямых&
                                                                                 & восхождений для объекта типа input_type.'
                    call input%deallocate()
                    stop

               case ('WA_delta')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_input_log_error:', 'Не удалось выделить память под массив&
                                                                                 & склонений для объекта типа input_type.'
                    call input%deallocate()
                    stop

               case ('WA_X')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_input_log_error:', 'Не удалось выделить память под массив значений&
                                                                                 & координат X Солнца для объекта типа input_type.'
                    call input%deallocate()
                    stop

               case ('WA_Y')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_input_log_error:', 'Не удалось выделить память под массив значений&
                                                                                 & координат Y Солнца для объекта типа input_type.'
                    call input%deallocate()
                    stop

               case ('WA_Z')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_input_log_error:', 'Не удалось выделить память под массив значений&
                                                                                 & координат Z Солнца для объекта типа input_type.'
                    call input%deallocate()
                    stop

               case ('WD_dates')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_input_log_error:', 'Не удалось освободить память из-под&
                                                                                 & массива дат объекта&
                                                                                 & типа input_type.'

               case ('WD_alpha')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_input_log_error:', 'Не удалось освободить память из-под&
                                                                                 & массива прямых восхождений объекта&
                                                                                 & типа input_type.'

               case ('WD_delta')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_input_log_error:', 'Не удалось освободить память из-под&
                                                                                 & массива склонений объекта&
                                                                                 & типа input_type.'

               case ('WD_X')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_input_log_error:', 'Не удалось освободить память из-под&
                                                                                 & массива значений координат X Солнца&
                                                                                 & объекта типа input_type.'

               case ('WD_Y')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_input_log_error:', 'Не удалось освободить память из-под&
                                                                                 & массива значений координат Y Солнца&
                                                                                 & объекта типа input_type.'

               case ('WD_Z')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_input_log_error:', 'Не удалось освободить память из-под&
                                                                                 & массива значений координат Z Солнца&
                                                                                 & объекта типа input_type.'

               case ('WС')

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_input_log_error:', 'Не удалось закрыть файл '//file//'&
                                                                                 & для чтения или записи.'
                    call input%deallocate()
                    stop

               case default

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'preorb_input_log_error:', 'Неизвестный код ошибки ('//error_code//').'

          end select

     end procedure preorb_input_log_error

end submodule preorb_input_log_error_s