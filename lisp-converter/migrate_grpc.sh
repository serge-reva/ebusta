#!/bin/bash
set -e

PROJECT_ROOT="$HOME/projects/ebusta"
OLD_GRPC="$HOME/projects/grpc"
NEW_GRPC="$PROJECT_ROOT/grpc"

echo "📂 Переходим в $PROJECT_ROOT..."
cd "$PROJECT_ROOT"

# 1. Проверяем, инициализирован ли git
if [ ! -d ".git" ]; then
    echo "⚙️ Инициализирую git репозиторий..."
    git init
fi

# 2. Бэкапим старый рабочий grpc (нам нужен Makefile оттуда)
if [ -d "$OLD_GRPC" ]; then
    echo "💾 Сохраняю рабочий Makefile..."
    cp "$OLD_GRPC/Makefile" /tmp/grpc_makefile_fix
    mv "$OLD_GRPC" "$OLD_GRPC.bak"
fi

# 3. Добавляем submodule
if [ -d "grpc" ]; then
    echo "⚠️ Папка grpc уже есть в ebusta. Пропускаю добавление submodule."
else
    echo "🔗 Добавляю qitab/grpc как submodule..."
    git submodule add https://github.com/qitab/grpc grpc
    git submodule update --init --recursive
fi

# 4. Возвращаем фикс Makefile
echo "🔧 Применяю фикс для Makefile..."
cp /tmp/grpc_makefile_fix "$NEW_GRPC/Makefile"

# 5. Сборка на новом месте
echo "🚀 Пересборка grpc.so внутри ebusta..."
cd "$NEW_GRPC"
make clean
make

if [ -f "grpc.so" ]; then
    echo "✅ grpc.so успешно собран в $NEW_GRPC"
else
    echo "❌ Ошибка сборки!"
    exit 1
fi

# 6. Обновляем симлинки в Quicklisp (теперь они должны указывать внутрь ebusta)
echo "🔄 Обновляю ссылки в Quicklisp..."
rm -rf "$HOME/quicklisp/local-projects/grpc"
ln -s "$NEW_GRPC" "$HOME/quicklisp/local-projects/grpc"

echo "✅ Миграция завершена."
