submodule ( preorb_input_m ) preorb_input_read_s
implicit none

     contains

     ! Процедура для считывания входных данных
     module procedure preorb_input_read

          integer(SP) :: stat ! Статусная переменная
          integer(UP) :: unit ! Номер дескриптора файла

          ! Открытие файла
          open( newunit = unit, file = file, action = 'read', status = 'old', iostat = stat)
          if ( stat .ne. 0_SP ) call input%log('WO', file)

          ! Пропуск строки
          read( unit = unit, fmt = '()', iostat = stat )
          if ( stat .eq. iostat_end) call input%log('EOF', file)

          ! Проверка, выделена ли память под массив дат
          if ( .not. allocated(input%dates) ) then

               ! Выделение памяти под массив дат
               allocate( input%dates(3), stat = stat )
               if ( stat .ne. 0_SP ) call input%log('WA_dates')

          endif

          ! Считывание массива дат
          read( unit = unit, fmt = *, iostat = stat ) input%dates
          if ( stat .ne. 0_SP .and. stat .ne. iostat_end ) call input%log('WR_dates', file)
          if ( stat .eq. iostat_end ) call input%log('EOF', file)

          ! Пропуск двух строк
          read( unit = unit, fmt = '(/)', iostat = stat )
          if ( stat .eq. iostat_end ) call input%log('EOF', file)

          ! Проверка, выделена ли память под массив прямых восхождений
          if ( .not. allocated(input%alpha) ) then

               ! Выделение памяти под массив прямых восхождений
               allocate( input%alpha(3), stat = stat )
               if ( stat .ne. 0_SP ) call input%log('WA_alpha')

          endif

          ! Считывание массива прямых восхождений
          read( unit = unit, fmt = *, iostat = stat ) input%alpha
          if ( stat .ne. 0_SP .and. stat .ne. iostat_end ) call input%log('WR_alpha', file)
          if ( stat .eq. iostat_end ) call input%log('EOF', file)

          ! Пропуск двух строк
          read( unit = unit, fmt = '(/)', iostat = stat )
          if ( stat .eq. iostat_end ) call input%log('EOF', file)

          ! Проверка, выделена ли память под массив склонений
          if ( .not. allocated(input%delta) ) then

               ! Выделение памяти под массив склонений
               allocate( input%delta(3), stat = stat )
               if ( stat .ne. 0_SP ) call input%log('WA_delta')

          endif

          ! Считывание массива склонений
          read( unit = unit, fmt = *, iostat = stat ) input%delta
          if ( stat .ne. 0_SP .and. stat .ne. iostat_end ) call input%log('WR_delta', file)

          ! Пропуск двух строк
          read( unit = unit, fmt = '(/)', iostat = stat )
          if ( stat .eq. iostat_end ) call input%log('EOF', file)

          ! Проверка, выделена ли память под массив значений координат X Солнца
          if ( .not. allocated(input%X) ) then

               ! Выделение памяти под массив значений координат X Солнца
               allocate( input%X(3), stat = stat )
               if ( stat .ne. 0_SP ) call input%log('WA_X')

          endif

          ! Считывание массива склонений
          read( unit = unit, fmt = *, iostat = stat ) input%X
          if ( stat .ne. 0_SP .and. stat .ne. iostat_end ) call input%log('WR_X', file)

          ! Пропуск двух строк
          read( unit = unit, fmt = '(/)', iostat = stat )
          if ( stat .eq. iostat_end ) call input%log('EOF', file)

          ! Проверка, выделена ли память под массив значений координат Y Солнца
          if ( .not. allocated(input%Y) ) then

               ! Выделение памяти под массив значений координат Y Солнца
               allocate( input%Y(3), stat = stat )
               if ( stat .ne. 0_SP ) call input%log('WA_Y')

          endif

          ! Считывание массива склонений
          read( unit = unit, fmt = *, iostat = stat ) input%Y
          if ( stat .ne. 0_SP .and. stat .ne. iostat_end ) call input%log('WR_Y', file)

          ! Пропуск двух строк
          read( unit = unit, fmt = '(/)', iostat = stat )
          if ( stat .eq. iostat_end ) call input%log('EOF', file)

          ! Проверка, выделена ли память под массив значений координат Z Солнца
          if ( .not. allocated(input%Z) ) then

               ! Выделение памяти под массив значений координат Z Солнца
               allocate( input%Z(3), stat = stat )
               if ( stat .ne. 0_SP ) call input%log('WA_Z')

          endif

          ! Считывание массива склонений
          read( unit = unit, fmt = *, iostat = stat ) input%Z
          if ( stat .ne. 0_SP .and. stat .ne. iostat_end ) call input%log('WR_Z', file)

          ! Закрытие файла
          close( unit = unit, iostat = stat )
          if ( stat .ne. 0_SP ) call input%log('WC', file)

     end procedure preorb_input_read

end submodule preorb_input_read_s