Компонент,Роль,Описание,Входящие (In),Исходящие (Out)
Web-Adapter,API Gateway,"Точка входа. Принимает HTTP-запросы, генерирует Trace-ID и управляет цепочкой вызовов.",HTTP :8080 (/input?msg=...),"gRPC -> Message-Converter, gRPC -> Processor"
Message-Converter,DSL Parser,Превращает сырой текст в структурированное дерево (AST). Использует internal/parser.,gRPC :50052 (Convert),Нет
Processor,Orchestrator,"Ядро системы. Реализует логику AST Walker: получает дерево, запрашивает данные и фильтрует их.",gRPC :50053 (HandleCommand),gRPC -> Data-Manager (GetData)
Data-Manager,Data Provider,Хранилище. Загружает books.json и отдает сырой список книг для дальнейшей фильтрации.,gRPC :50051 (GetData),Файловая система (books.json)
CLI,UI Client,Интерактивная консоль пользователя с поддержкой истории команд (readline).,User Input,HTTP -> Web-Adapter
Client,Debug Tool,Утилита для прямой проверки доступности Data-Manager в обход шлюзов.,Manual Run,gRPC -> Data-Manager
