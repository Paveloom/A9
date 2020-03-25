submodule ( ephemeris_do_m ) ephemeris_do_calculate_s
implicit none
     
     contains
     
     ! Процедура для вычисления эфемериды
     module procedure ephemeris_do_calculate
          
          ! Постоянная из уравнения движения
          real(RP), parameter :: chi = 0.017202_RP

          ! Постоянная малости для итераций уравнения Кеплера
          real(RP), parameter :: eps_K = 1e-8_RP

          ! Наклон экватора к эклиптике
          real(RP), parameter :: eps = 4._RP * atan(1._RP) * &
                                 & (23._RP + 26._RP / 60._RP + 12.27_RP / 3600._RP) / 180._RP

          ! Вспомогательные переменные для вычисления 
          ! координат в экваториальной системе координат
          real(RP), parameter :: sin_eps = sin(eps)
          real(RP), parameter :: cos_eps = cos(eps)

          ! Значение числа sqrt( (1+e) / (1-e) )
          real(RP) :: sqrt_v

          real(RP) :: n ! Средняя угловая скорость
          real(RP) :: M ! Средняя аномалия
          real(RP) :: EA ! Эксцентрическая аномалия
          real(RP) :: EA_prev ! Предыдущее значение эксцентрической аномалии

          ! Орбитальные координаты
          real(RP) :: theta
          real(RP) :: r

          real(RP) :: u ! Аргумент широты
          real(RP) :: xc, yc, zc ! Прямоугольные координаты
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

          ! Вспомогательная переменная при вычислении 
          ! прямых восхождений и склонений
          real(RP) :: zcg_sq ! Значение zcg * zcg

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

          ! Вычисление некоторых вспомогательных
          ! переменных для вычисления координат
          cos_capital_omega = cos(capital_omega)
          sin_capital_omega = sin(capital_omega)
          cos_i = cos(i)
          sin_i = sin(i)

          ! Вычисление средней угловой скорости
          n = chi * a ** (-3._RP / 2._RP)

          do j = 1_JP, N_JP

               ! [ Определение орбитальных координат ]

               ! Вычисление средней аномалии
               M = M_0 + n * (dates(j) - date)

               ! Определение начального значения эксцентрической аномалии
               EA = M
               EA_prev = M + 2._RP * eps_K

               ! Вычисление эксцентрической аномалии
               do while ( abs(EA - EA_prev) .gt. eps_K )

                    EA_prev = EA
                    EA = M + e * sin(EA_prev)

               enddo

               ! Вычисление орбитальных координат
               theta = 2._RP * atan(sqrt_v * tan(EA / 2._RP))
               r = a * (1._RP - e * e) / (1._RP + e * cos(theta))

               ! [ Вычисление гелиоцентрических координат ]

               ! Вычисление аргумента широты
               u = theta + small_omega

               ! Вычисление некоторых вспомогательных
               ! переменных для вычисления координат
               r_cos_u = r * cos(u)
               r_sin_u = r * sin(u)

               ! Вычисление прямоугольных координат
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

               ! [ Вычисление прямых восхождений и склонений ]
               
               ! Вычисление квадрата zcg
               zcg_sq = zcg * zcg

               ! Вычисление значения радиус-вектора
               ro = sqrt(xcg * xcg + ycg * ycg + zcg_sq)

               ! Вычисление прямого восхождения
               result%alpha(j) = asin(xcg / sqrt(ro * ro - zcg_sq))

               ! Вычисление склонения
               result%delta(j) = asin(zcg / ro)

          enddo

          end associate

     end procedure ephemeris_do_calculate
     
end submodule ephemeris_do_calculate_s