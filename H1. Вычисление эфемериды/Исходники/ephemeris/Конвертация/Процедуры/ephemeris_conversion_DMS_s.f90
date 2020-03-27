submodule ( ephemeris_conversion_m ) ephemeris_conversion_DMS_s
implicit none

     contains

     ! Функция для конвертации из радианной меры в часовую
     module procedure ephemeris_conversion_DMS

          real(RP) :: value_D ! Число в градусной мере

          real(RP) :: degrees_RP ! Градусы (вещественное)
          integer(IP) :: degrees ! Градусы (целое)
          real(RP) :: minutes_RP ! Угловые минуты (вещественное)
          integer(IP) :: minutes ! Угловые минуты (целое)
          real(RP) :: seconds_RP ! Угловые секунды (вещественное)

          real(RP) :: tmp ! Временная переменная

          real(RP) :: hours_RP ! Часы (вещественное)
          integer(IP) :: hours ! Часы (целое)
          real(RP) :: h_minutes_RP ! Минуты (вещественное)
          integer(IP) :: h_minutes ! Минуты (целое)
          real(RP) :: h_seconds_RP ! Секунды (вещественное)
          integer(IP) :: h_seconds ! Секунды (целое)

          ! Вспомогательные строки
          character(chars) :: f1, f2, f3, f4

          ! Перевод числа в градусную меру
          value_D = value * 180._RP / pi

          ! Вычисление градусов
          degrees = floor(abs(value_D))
          degrees_RP = real(degrees, kind=RP)

          ! Вычитание градусов и переход к минутам
          tmp = (value_D - degrees_RP) * 60._RP

          ! Вычисление минут
          minutes = floor(tmp)
          minutes_RP = real(minutes, kind=RP)

          ! Вычитание минут и переход к секундам
          seconds_RP = (tmp - minutes_RP) * 60._RP

          ! Вычисление числа секунд в часовой мере
          tmp = degrees_RP * 240._RP + minutes_RP * 4._RP + seconds_RP / 15._RP

          ! Вычисление часов
          hours = floor(tmp / 3600._RP)
          hours_RP = real(hours, kind=RP)

          ! Перевод часов
          do while (hours .ge. 24_IP)

               hours = hours - 24_IP

          enddo

          do while (hours .lt. 0_IP)

               hours = hours + 24_IP

          enddo

          ! Вычитание часов и переход к минутам
          tmp = tmp - hours_RP * 3600._RP

          ! Вычисление минут
          h_minutes = floor(tmp / 60._RP)
          h_minutes_RP = real(h_minutes, kind=RP)

          ! Вычитание минут и переход к секундам
          tmp = tmp - h_minutes_RP * 60._RP

          ! Вычисление секунд
          h_seconds = floor(tmp)
          h_seconds_RP = real(h_seconds, kind=RP)

          ! Вычитание секунд
          tmp = tmp - h_seconds_RP

          ! Запись результатов во внутренние файлы
          write(f1, *) hours
          write(f2, *) h_minutes
          write(f3, *) h_seconds
          write(f4, '('//RF//')') tmp

          f4 = adjustl(f4)
          allocate(out, source = trim(adjustl(f1))//'h '//trim(adjustl(f2))//'m '//trim(adjustl(f3))//'s'//trim(f4(2:)))

     end procedure ephemeris_conversion_DMS

end submodule ephemeris_conversion_DMS_s