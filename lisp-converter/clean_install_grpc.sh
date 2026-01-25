#!/bin/bash
set -e

# Пути
QL_LOCAL="$HOME/quicklisp/local-projects"
PROJECT_DIR="$HOME/projects/grpc"

echo "=== 1. Очистка ошметков старых версий ==="
# Удаляем всё, что может конфликтовать в Quicklisp
if [ -d "$QL_LOCAL/grpc" ]; then
    echo "Removing old grpc from local-projects..."
    rm -rf "$QL_LOCAL/grpc"
fi

if [ -d "$QL_LOCAL/cl-grpc" ]; then
    echo "Removing old cl-grpc from local-projects..."
    rm -rf "$QL_LOCAL/cl-grpc"
fi

echo "=== 2. Сборка C++ ядра в $PROJECT_DIR ==="
if [ ! -d "$PROJECT_DIR" ]; then
    echo "❌ Ошибка: Папка $PROJECT_DIR не найдена. Убедись, что репозиторий qitab/grpc скачан."
    exit 1
fi

cd "$PROJECT_DIR"
echo "Выполняю make clean..."
make clean > /dev/null
echo "Выполняю make..."
make

if [ ! -f "grpc.so" ]; then
    echo "❌ Ошибка: grpc.so не был создан."
    exit 1
fi
echo "✔ grpc.so успешно собран."

echo "=== 3. Линковка проекта в Quicklisp ==="
# Создаем симлинк, чтобы Quicklisp подхватил проект из ~/projects
ln -s "$PROJECT_DIR" "$QL_LOCAL/grpc"
echo "✔ Создан симлинк: $QL_LOCAL/grpc -> $PROJECT_DIR"

echo "=== 4. Финальная проверка в SBCL ==="
sbcl --noinform --load "$HOME/quicklisp/setup.lisp" \
     --eval '(format t "~%--- Обновление кэша проектов ---~%")' \
     --eval '(ql:register-local-projects)' \
     --eval '(format t "--- Попытка загрузки системы :grpc ---~%")' \
     --eval '(handler-case (ql:quickload :grpc) (error (c) (format t "ERROR: ~A" c) (sb-ext:exit :code 1)))' \
     --eval '(format t "~%--- Путь к системе (должен быть в ~/projects/grpc) ---~%")' \
     --eval '(print (asdf:system-source-directory (asdf:find-system :grpc)))' \
     --quit

echo ""
echo "✅ Готово! Библиотека очищена, пересобрана и зарегистрирована."
