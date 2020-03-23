
     ## Это шаблон* make-файла для публикации кода на GitHub.

     ## * Изменен для данного проекта.

     ## Репозиторий на GitHub: https://github.com/Paveloom/B1
     ## Документация: https://www.notion.so/paveloom/B1-fefcaf42ddf541d4b11cfcab63c2f018

     ## Версия релиза: 2.1.3
     ## Версия документации: 2.1.0

     ## Автор: Павел Соболев (http://paveloom.tk)

     ## Для корректного отображения содержимого
     ## символ табуляции должен визуально иметь
     ## ту же длину, что и пять пробелов.

     # Настройки make-файла

     ## Имя координатора
     make_name := make

     ## Указание оболочки
     SHELL := /bin/bash

     ## Указание make-файлу выполнять все правила в одном вызове оболочки
     .ONESHELL : 

     ## Заглушка на вывод сообщений указанными правилами
     ## (без указания имён подавляет вывод со стороны make-файла у всех правил)

     .SILENT :

     ## Правила-псевдоцели
     .PHONY : git, git-am, new, del

     ## Правило, выполняющееся при вызове координатора без аргументов
     ALL : git



     # Блок правил для разработки и публикации кода на GitHub

     ## Имя пользователя на GitHub
     username := Paveloom

	## Имя ветки изменений
     FEATURE_BRANCH := feature

     ## Правило для создания и публикации коммита

     git :
	      git add -A
	      git commit -e

	      # Проверка, был ли создан коммит
	      if [ $$? -eq 0 ]; then
	           git push
	      fi

     ## Правило для создания ветки изменений

     new :
	      git checkout -q master
	      git checkout -b ${FEATURE_BRANCH}
	      git push -u origin ${FEATURE_BRANCH}

	## Правило для удаления текущей ветки изменений локально

     del :
	      git checkout -q master
	      git branch -D ${FEATURE_BRANCH}

     ## Правило для обновления последнего коммита до текущего состояния локального репозитория

     git-am :
	         git add -A
	         git commit --amend

	         # Проверка, был ли создан коммит
	         if [ $$? -eq 0 ]; then
	              git push --force-with-lease
	         fi
