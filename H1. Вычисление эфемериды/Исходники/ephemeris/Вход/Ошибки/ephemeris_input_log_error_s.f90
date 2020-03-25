submodule ( ephemeris_input_m ) ephemeris_input_log_error_s
implicit none
     
     contains
     
     ! Процедура для вывода ошибок (входные данные)
     module procedure ephemeris_input_log_error
          
          select case (error_code) ! Проверка кода ошибки

               case ('WO') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Не удалось получить доступ к файлу '//file//'&
                                                                                    & для чтения или записи.'
                    stop

               case ('WR_N') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Не удалось считать значение размера массива дат в&
                                                                                    & файле '//file//'. Проверьте правильность&
                                                                                    & введенных данных.'
                    stop

               case ('WR_dates') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Не удалось считать значения массива дат в&
                                                                                    & файле '//file//'. Проверьте правильность&
                                                                                    & введенных данных.'
                    stop

               case ('WR_date') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Не удалось считать значение опорной даты в&
                                                                                    & файле '//file//'. Проверьте правильность&
                                                                                    & введенных данных.'
                    stop

               case ('WR_a') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Не удалось считать значение большой полуоси в&
                                                                                    & файле '//file//'. Проверьте правильность&
                                                                                    & введенных данных.'
                    stop

               case ('WR_e') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Не удалось считать значение эксцентриситета орбиты в&
                                                                                    & файле '//file//'. Проверьте правильность&
                                                                                    & введенных данных.'
                    stop

               case ('WR_i') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Не удалось считать значение наклонения плоскости&
                                                                                    & орбиты к плоскости эклиптики в&
                                                                                    & файле '//file//'. Проверьте правильность&
                                                                                    & введенных данных.'
                    stop

               case ('WR_small_omega') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Не удалось считать значение углового расстояния&
                                                                                    & перигелия от восходящего узла в&
                                                                                    & файле '//file//'. Проверьте правильность&
                                                                                    & введенных данных.'
                    stop

               case ('WR_capital_omega') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Не удалось считать значение гелиоцентрической&
                                                                                    & долготы восходящего узла в&
                                                                                    & файле '//file//'. Проверьте правильность&
                                                                                    & введенных данных.'
                    stop

               case ('WR_M_0') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Не удалось считать значение момента&
                                                                                    & прохождения через перигелий в&
                                                                                    & файле '//file//'. Проверьте правильность&
                                                                                    & введенных данных.'
                    stop

               case ('WR_X') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Не удалось считать значения массива&
                                                                                    & координаты X Солнца в&
                                                                                    & файле '//file//'. Проверьте правильность&
                                                                                    & введенных данных.'
                    stop

               case ('WR_Y') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Не удалось считать значения массива&
                                                                                    & координаты Y Солнца в&
                                                                                    & файле '//file//'. Проверьте правильность&
                                                                                    & введенных данных.'
                    stop

               case ('WR_Z') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Не удалось считать значения массива&
                                                                                    & координаты Z Солнца в&
                                                                                    & файле '//file//'. Проверьте правильность&
                                                                                    & введенных данных.'
                    stop

               case ('WA_dates') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Не удалось выделить память под массив дат&
                                                                                    & для объекта типа input_type.'
                    stop

               case ('WA_X') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Не удалось выделить память под массив координаты X Солнца&
                                                                                    & для объекта типа input_type.'
                    stop

               case ('WA_Y') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Не удалось выделить память под массив координаты Y Солнца&
                                                                                    & для объекта типа input_type.'
                    stop

               case ('WA_Z') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Не удалось выделить память под массив координаты Z Солнца&
                                                                                    & для объекта типа input_type.'
                    stop

               case ('WD_dates') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Не удалось освободить память из-под&
                                                                                    & массива дат объекта&
                                                                                    & типа input_type.'

               case ('WD_X') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Не удалось освободить память из-под&
                                                                                    & массива значений координаты X Солнца объекта&
                                                                                    & типа input_type.'

               case ('WD_Y') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Не удалось освободить память из-под&
                                                                                    & массива значений координаты Y Солнца объекта&
                                                                                    & типа input_type.'

               case ('WD_Z') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Не удалось освободить память из-под&
                                                                                    & массива значений координаты Z Солнца объекта&
                                                                                    & типа input_type.'

               case ('WС') 

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Не удалось закрыть файл '//file//'&
                                                                                    & для чтения или записи.'
                    stop

               case default

                    write(*,'(/, 5x, a, /, 5x, a, /)') 'ephemeris_input_log_error:', 'Неизвестный код ошибки ('//error_code//').'

          end select
          
     end procedure ephemeris_input_log_error
     
end submodule ephemeris_input_log_error_s