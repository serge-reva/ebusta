#!/bin/bash

# Проверяем статус Git
echo "🔍 Checking status..."
git status

# Добавляем все изменения (кроме игнорируемых)
echo "➕ Adding files..."
git add .

# Формируем сообщение коммита
COMMIT_MSG="Feat: unified proto, smart AST-based search, and Prometheus metrics (v1.0-stable)"

echo "💾 Committing with message: $COMMIT_MSG"
git commit -m "$COMMIT_MSG"

# Пушим в текущую ветку
CURRENT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
echo "🚀 Pushing to $CURRENT_BRANCH..."
git push origin "$CURRENT_BRANCH"

if [ $? -eq 0 ]; then
    echo "✅ Success! Code is now on the remote server."
else
    echo "❌ Error: Push failed. Check your connection or permissions."
fi
