mkdir -p ~/projects
cd ~/projects

if [ -d "grpc" ]; then
    echo "⚠️ Папка ~/projects/grpc уже существует. Переименовываю старую в grpc_old..."
    mv grpc grpc_old_$(date +%s)
fi

echo "🚀 Клонирую qitab/grpc..."
git clone https://github.com/qitab/grpc
cd grpc
git submodule update --init --recursive

echo "✅ Репозиторий скачан в $(pwd)"
