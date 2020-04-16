submodule ( preorb_do_m ) preorb_do_calculate_verbose_s
implicit none

     contains

     ! Процедура для определения параметров предварительной
     ! орбиты по трем наблюдениям (с дополнительным выводом)
     module procedure preorb_do_calculate_verbose

          real(RP) :: tau_1, tau_2 ! Разницы между средним моментом и другими моментами
          real(RP) :: tau ! Сумма значений tau_1 и tau_2

          ! Вспомогательные значения
          real(RP) :: n_1_0, n_2_0
          real(RP) :: c_0, c_1, c_2

          ! Значения прямых восхождений и склонений в радианах
          real(RP), dimension(3) :: alpha_r
          real(RP), dimension(3) :: delta_r

          ! Вспомогательный массив для хранения cos(detla_r)
          real(RP), dimension(3) :: cos_delta_r

          ! Направляющие косинусы
          real(RP), dimension(3) :: lambda
          real(RP), dimension(3) :: mu
          real(RP), dimension(3) :: nu

          ! Вспомогательные значения
          real(RP) :: lambda_13, mu_13, nu_13
          real(RP), dimension(3) :: U
          real(RP) :: D, Dr
          real(RP) :: P, Q, C
          real(RP) :: R_2

          ! Геоцентрические и гелиоцентрические расстояния
          real(RP), dimension(3) :: ro, r

          ! Вспомогательные переменные
          real(RP) :: ro_prev
          real(RP) :: r_m3
          real(RP) :: n_1, n_2

          ! Коэффициенты для поиска геоцентрического
          ! и гелиоцентрического расстояний первого
          ! и третьего моментов наблюдений
          real(RP) ko_a_1, ko_b_1, ko_c_1
          real(RP) ko_a_2, ko_b_2, ko_c_2
          real(RP) ko_a, ko_b, ko_c
          real(RP) ko_d_1, ko_d_2, ko_d_3

          associate( dates => input%dates, &
                   & alpha => input%alpha, &
                   & delta => input%delta, &
                   & X => input%X,         &
                   & Y => input%Y,         &
                   & Z => input%Z          )

          ! Вычисление tau_1 и tau_2
          tau_1 = dates(2) - dates(1)
          tau_2 = dates(3) - dates(2)

          ! Вычисление tau
          tau = dates(3) - dates(1)

          ! Вычисление n_1_0 и n_2_0
          n_1_0 = tau_2 / tau
          n_2_0 = tau_1 / tau

          ! Вычисление c_1 и c_2
          c_0 = 1._RP / 6._RP * chi_sq * tau_1 * tau_2
          c_1 = c_0 * (1._RP + n_1_0)
          c_2 = c_0 * (1._RP + n_2_0)

          ! Перевод прямых восхождений и склонений в радианы
          alpha_r = alpha * pi_12
          delta_r = delta * pi_180

          ! Вычисление значений направляющих косинусов
          cos_delta_r = cos(delta_r)
          lambda = cos_delta_r * cos(alpha_r)
          mu = cos_delta_r * sin(alpha_r)
          nu = sin(delta_r)

          ! Вычисление lambda_13, mu_13, nu_13
          lambda_13 = mu(1) * nu(3) - mu(3) * nu(1)
          mu_13 = nu(1) * lambda(3) - nu(3) * lambda(1)
          nu_13 = lambda(1) * mu(3) - lambda(3) * mu(1)

          ! Вычисление U
          U = X * lambda_13 + Y * mu_13 + Z * nu_13

          ! Вычисление Dr
          D = lambda(2) * lambda_13 + mu(2) * mu_13 + nu(2) * nu_13
          Dr = 1._RP / D

          ! Вычисление P, Q, C
          P = Dr * ( U(2) - n_1_0 * U(1) - n_2_0 * U(3) )
          Q = Dr * ( c_1 * U(1) + c_2 * U(3) )
          C = - ( lambda(2) * X(2) + mu(2) * Y(2) + nu(2) * Z(2) )

          ! Вычисление R^2
          R_2 = X(2) * X(2) + Y(2) * Y(2) + Z(2) * Z(2)

          ! Вычисление геоцентрического и гелиоцентрического
          ! расстояний для среднего момента времени
          ro(2) = P
          ro_prev = P - 2._RP * eps_e

          do while ( abs( ro(2) - ro_prev ) .ge. eps_e )

               ! Сохранение предыдущего значения ro
               ro_prev = ro(2)

               ! Вычисление значения r
               r(2) = sqrt( ro(2) * ro(2) + 2._RP * C * ro(2) + R_2 )

               ! Вычисление значения ro
               ro(2) = ro_prev - ( ro_prev - P + Q * r(2) ** (-3._RP) ) / ( 1._RP - 3._RP * Q * ( ro_prev + C ) * r(2) ** (-5._RP) )

          enddo

          ! Вычисление значения r
          r(2) = sqrt( ro(2) * ro(2) + 2._RP * C * ro(2) + R_2 )

          ! Вычисление n_1 и n_2
          r_m3 = r(2) ** (-3._RP)
          n_1 = n_1_0 + c_1 * r_m3
          n_2 = n_2_0 + c_2 * r_m3

          ! Вычисление коэффициентов для поиска
          ! геоцентрического и гелиоцентрического
          ! расстояний первого и третьего
          ! моментов наблюдений

          ko_a_1 = n_1 * lambda(1)
          ko_b_1 = n_1 * mu(1)
          ko_c_1 = n_1 * nu(1)

          ko_a_2 = n_2 * lambda(3)
          ko_b_2 = n_2 * mu(3)
          ko_c_2 = n_2 * nu(3)

          ko_a = lambda(2) * ro(2)
          ko_b = mu(2) * ro(2)
          ko_c = nu(2) * ro(2)

          ko_d_1 = n_1 * X(1) - X(2) + n_2 * X(3)
          ko_d_2 = n_1 * Y(1) - Y(2) + n_2 * Y(3)
          ko_d_3 = n_1 * Z(1) - Z(2) + n_2 * Z(3)

          ! Вычисление геоцентрического и гелиоцентрического
          ! расстояний для первого и третьего моментов времени
          ro(3) = ( ko_b_1 * ( ko_d_1 + ko_a ) - ko_a_1 * ( ko_d_2 + ko_b ) ) &
              & / ( ko_a_2 * ko_b_1 - ko_a_1 * ko_b_2 )

          ro(1) = ( ko_d_3 - ko_c_2 * ro(3) + ko_c ) / ko_c_1

          end associate

     end procedure preorb_do_calculate_verbose

end submodule preorb_do_calculate_verbose_s