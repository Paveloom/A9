submodule ( ephemeris_do_m ) ephemeris_do_calculate_non_verbose_s
implicit none

     contains

     ! Процедура для вычисления эфемериды (с дополнительным выводом)
     module procedure ephemeris_do_calculate_non_verbose

          ! Значение числа sqrt( (1+e) / (1-e) )
          real(RP) :: sqrt_v

          ! Некоторые элементы орбиты (в радианах)
          real(RP) :: i_r ! Наклонение плоскости орбиты к плоскости эклиптики (радианы)
          real(RP) :: small_omega_r ! Угловое расстояние перигелия от восходящего узла (радианы)
          real(RP) :: capital_omega_r ! Гелиоцентрическая долгота восходящего узла (радианы)
          real(RP) :: M_0_r ! Момент прохождения через перигелий (радианы)

          real(RP) :: n ! Средняя угловая скорость
          real(RP) :: M ! Средняя аномалия
          real(RP) :: EA ! Эксцентрическая аномалия
          real(RP) :: EA_prev ! Предыдущее значение эксцентрической аномалии

          ! Орбитальные координаты
          real(RP) :: theta ! Истинная аномалия
          real(RP) :: r ! Расстояние

          real(RP) :: u ! Аргумент широты
          real(RP) :: xc, yc, zc ! Координаты в эклиптической системе координат
          real(RP) :: yce, zce ! Координаты в экваториальной системе координат

          ! Вспомогательные переменные для вычисления координат
          real(RP) :: r_cos_u ! Значение r * cos(u)
          real(RP) :: r_sin_u ! Значение r * sin(u)
          real(RP) :: cos_capital_omega ! Значение cos(capital_omega)
          real(RP) :: sin_capital_omega ! Значение sin(capital_omega)
          real(RP) :: cos_i ! Значение cos(i)
          real(RP) :: sin_i ! Значение sin(i)

          real(RP) :: xcg, ycg, zcg !  Геоцентрические координаты
          real(RP) :: ro ! Радиус-вектор в геоцентрических координатах

          integer(JP) :: j ! Счетчики
          integer(SP) :: stat ! Статусная переменная

          ! Проверка, выделена ли память под массивы во входных данных
          if ( .not. allocated(input%dates) ) call ephemeris_do_log_error('NA_dates')
          if ( .not. allocated(input%X) ) call ephemeris_do_log_error('NA_X')
          if ( .not. allocated(input%Y) ) call ephemeris_do_log_error('NA_Y')
          if ( .not. allocated(input%Z) ) call ephemeris_do_log_error('NA_Z')

          ! Распаковка входных данных
          associate( N_JP => input%N, &
                   & dates => input%dates, &
                   & date => input%date, &
                   & a => input%a, &
                   & e => input%e, &
                   & i => input%i, &
                   & small_omega => input%small_omega, &
                   & capital_omega => input%capital_omega, &
                   & M_0 => input%M_0, &
                   & X => input%X, &
                   & Y => input%Y, &
                   & Z => input%Z )

          ! Проверка, выделена ли память под массив дат
          if ( allocated(result%dates) ) then

               if ( .not. (size(result%dates, kind=JP) .eq. N_JP) ) then

                    ! Освобождение памяти из-под массива дат
                    deallocate( result%dates, stat = stat )
                    if ( stat .ne. 0_SP ) call ephemeris_do_log_error('WD_dates')

                    ! Выделение памяти под массив дат
                    allocate( result%dates(1_JP:N_JP), stat = stat )
                    if ( stat .ne. 0_SP ) call ephemeris_do_log_error('WA_dates')

               endif

          else

               ! Выделение памяти под массив дат
               allocate( result%dates(1_JP:N_JP), stat = stat )
               if ( stat .ne. 0_SP ) call ephemeris_do_log_error('WA_dates')

          endif

          ! Проверка, выделена ли память под массив значений прямых восхождений
          if ( allocated(result%alpha) ) then

               if ( .not. (size(result%alpha, kind=JP) .eq. N_JP) ) then

                    ! Освобождение памяти из-под массива значений прямых восхождений
                    deallocate( result%alpha, stat = stat )
                    if ( stat .ne. 0_SP ) call ephemeris_do_log_error('WD_alpha')

                    ! Выделение памяти под массив значений прямых восхождений
                    allocate( result%alpha(1_JP:N_JP), stat = stat )
                    if ( stat .ne. 0_SP ) call ephemeris_do_log_error('WA_alpha')

               endif

          else

               ! Выделение памяти под массив значений прямых восхождений
               allocate( result%alpha(1_JP:N_JP), stat = stat )
               if ( stat .ne. 0_SP ) call ephemeris_do_log_error('WA_alpha')

          endif

          ! Проверка, выделена ли память под массив значений склонений
          if ( allocated(result%delta) ) then

               if ( .not. (size(result%delta, kind=JP) .eq. N_JP) ) then

                    ! Освобождение памяти из-под массива значений склонений
                    deallocate( result%delta, stat = stat )
                    if ( stat .ne. 0_SP ) call ephemeris_do_log_error('WD_alpha')

                    ! Выделение памяти под массив значений склонений
                    allocate( result%delta(1_JP:N_JP), stat = stat )
                    if ( stat .ne. 0_SP ) call ephemeris_do_log_error('WA_alpha')

               endif

          else

               ! Выделение памяти под массив значений склонений
               allocate( result%delta(1_JP:N_JP), stat = stat )
               if ( stat .ne. 0_SP ) call ephemeris_do_log_error('WA_alpha')

          endif

          ! Вычисление числа sqrt( (1+e) / (1-e) )
          sqrt_v = sqrt( ( 1._RP + e )/( 1._RP - e ) )

          ! Перевод некоторых элементов орбиты
          ! из градусной меры в радианную
          i_R = pi_180 * i
          small_omega_r = pi_180 * small_omega
          capital_omega_r = pi_180 * capital_omega
          M_0_r = pi_180 * M_0

          ! Вычисление некоторых вспомогательных
          ! переменных для вычисления координат
          cos_capital_omega = cos(capital_omega_r)
          sin_capital_omega = sin(capital_omega_r)
          cos_i = cos(i_r)
          sin_i = sin(i_r)

          ! Вычисление средней угловой скорости
          n = chi * a ** (-3._RP / 2._RP)

          do j = 1_JP, N_JP

               ! [ Определение орбитальных координат ]

               ! Вычисление средней аномалии
               M = M_0_r + n * (dates(j) - date)

               ! Определение начального значения эксцентрической аномалии
               EA = M
               EA_prev = M + 2._RP * eps_K

               ! Вычисление эксцентрической аномалии
               do while ( abs(EA - EA_prev) .gt. eps_K )

                    EA_prev = EA
                    EA = M + e * sin(EA_prev)

               enddo

               ! Вычисление истинной аномалии и расстояния
               theta = 2._RP * atan(sqrt_v * tan(EA / 2._RP))
               r = a * (1._RP - e * e) / (1._RP + e * cos(theta))

               ! [ Вычисление гелиоцентрических координат ]

               ! Вычисление аргумента широты
               u = theta + small_omega_r

               ! Вычисление некоторых вспомогательных
               ! переменных для вычисления координат
               r_cos_u = r * cos(u)
               r_sin_u = r * sin(u)

               ! Вычисление координат в эклиптической системе координат
               xc = r_cos_u * cos_capital_omega - r_sin_u * sin_capital_omega * cos_i
               yc = r_cos_u * sin_capital_omega + r_sin_u * cos_capital_omega * cos_i
               zc = r_sin_u * sin_i

               ! Вычисление координат в экваториальной системе координат
               yce = yc * cos_eps - zc * sin_eps
               zce = yc * sin_eps + zc * cos_eps

               ! [ Вычисление геоцентрических координат ]
               xcg = xc + X(j)
               ycg = yce + Y(j)
               zcg = zce + Z(j)

               ! Вычисление значения радиус-вектора
               ro = sqrt(xcg * xcg + ycg * ycg + zcg * zcg)

               ! Вычисление прямого восхождения
               result%alpha(j) = atan(ycg / xcg) + pi_2

               ! Вычисление склонения
               result%delta(j) = asin(zcg / ro)

          enddo

          ! [ Сохранение дат в результате ]
          result%dates(:) = dates(:)

          end associate

     end procedure ephemeris_do_calculate_non_verbose

end submodule ephemeris_do_calculate_non_verbose_s