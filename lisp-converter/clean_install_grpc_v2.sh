#!/bin/bash
set -e

# Пути
QL_LOCAL="$HOME/quicklisp/local-projects"
PROJECT_DIR="$HOME/projects/grpc"

echo "=== 1. Очистка ошметков старых версий ==="
# Удаляем всё, что может конфликтовать в Quicklisp
rm -rf "$QL_LOCAL/grpc"
rm -rf "$QL_LOCAL/cl-grpc"
echo "✔ Старые симлинки удалены."

echo "=== 2. Исправление Makefile в $PROJECT_DIR ==="
if [ ! -d "$PROJECT_DIR" ]; then
    echo "❌ Ошибка: Папка $PROJECT_DIR не найдена. Сначала скачайте репозиторий."
    exit 1
fi

cd "$PROJECT_DIR"

# Перезаписываем Makefile исправленной версией (чиним target clean)
cat << 'MAKEFILE_EOF' > Makefile
# Copyright 2021 Google LLC
GRPC_ROOT ?= /usr/local
LIBS = -lgrpc -lgpr
CXXFLAGS = $(shell pkg-config grpc --cflags) -I$(GRPC_ROOT)/include -fPIC
LDFLAGS = -L$(GRPC_ROOT)/lib $(LIBS)
OFILES = client.o client_auth.o server.o

default_target: grpc.so

.PHONY : default_target clean

grpc.so: ${OFILES}
	$(CXX)  -pthread -shared -Wl,--no-undefined ${OFILES} -o $@ $(LDFLAGS)

clean:
	rm -f ${OFILES} grpc.so

client.o: client.cc
client_auth.o: client_auth.cc
server.o: server.cc
MAKEFILE_EOF
echo "✔ Makefile исправлен."

echo "=== 3. Сборка C++ ядра ==="
make clean
make
if [ ! -f "grpc.so" ]; then
    echo "❌ Ошибка: grpc.so не был создан."
    exit 1
fi
echo "✔ grpc.so успешно собран."

echo "=== 4. Линковка проекта в Quicklisp ==="
ln -s "$PROJECT_DIR" "$QL_LOCAL/grpc"
echo "✔ Создан симлинк: $QL_LOCAL/grpc -> $PROJECT_DIR"

echo "=== 5. Финальная проверка в SBCL ==="
sbcl --noinform --load "$HOME/quicklisp/setup.lisp" \
     --eval '(format t "~%--- Обновление кэша проектов ---~%")' \
     --eval '(ql:register-local-projects)' \
     --eval '(format t "--- Попытка загрузки системы :grpc ---~%")' \
     --eval '(handler-case (ql:quickload :grpc) (error (c) (format t "ERROR: ~A" c) (sb-ext:exit :code 1)))' \
     --eval '(format t "~%--- Путь к системе (должен быть в ~/projects/grpc) ---~%")' \
     --eval '(print (asdf:system-source-directory (asdf:find-system :grpc)))' \
     --quit

echo ""
echo "✅ Готово! Теперь grpc установлен корректно."
