program main ! Программа, демонстрирующая использование модуля
             ! для вычисления эфемерид малых планет
use ephemeris ! Модуль для вычисления эфемерид малых планет
implicit none

     type ( ephemeris_API ) :: e ! Экземпляр API модуля

     call e%input%read('Файлы/input') ! Считывание входных данных

     call e%calculate() ! Вычисление эфемериды

     call e%result%write('Файлы/result') ! Запись результата в файл

     call e%deallocate() ! Общее освобождение памяти

end program main