#!/bin/bash

# Вывод названия скрипта
printf "\nЗапущен скрипт, проверяющий, активен ли флаг отладки\n\n"

# Определение начального числа ошибок
ERROR_COUNT=0

# Описание функции для проверки make-файлов
function check_debugging {

   printf "Проверяется make-файл $1 ...\n\n"

   if grep "debugging := " "$1" | grep -q "true"; then

      printf "[!] Флаг отладки активирован в этом make-файле.\n"
      ERROR_COUNT=$((ERROR_COUNT+1))

   else

      printf "Флаг отладки НЕ активирован в этом make-файле.\n"

   fi

}

# Проверка make-файлов
check_debugging "H1. Вычисление эфемериды/Makefile"

# Проверка числа ошибок
if [ "$ERROR_COUNT" -gt 0 ]; then

     printf "Число ошибок: $ERROR_COUNT\n\n"
     exit 1

else

     printf "\nВсё в порядке.\n"

fi