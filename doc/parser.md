;;; ===========================================================================
;;; ЯДРО ПАРСЕРА MERCURY (DSL -> S-Expression)
;;; Реализация алгоритма Shunting-yard для UR 1.1 - UR 3.3
;;; ===========================================================================

;; 1. ЛЕКСЕР (TOKENIZER)
;; Разрезает строку на атомы. Правила регулярки:
;; - \"[^\"]+\"    : строки в кавычках (UR 2.1)
;; - [a-zA-Z0-9]+: : префиксы полей (например, author:)
;; - AND|OR|NOT    : логические операторы
;; - \(|\)         : скобки для группировки
;; - /             : маркер регулярных выражений (UR 2.2)
;; - \S+           : любые другие непробельные символы
(defun tokenize (str)
  (cl-ppcre:all-matches-as-strings "(\"[^\"]+\"|[a-zA-Z0-9_]+:|AND|OR|NOT|\\(|\\)|/|\\S+)" str))

;; 2. ТАБЛИЦА ПРИОРИТЕТОВ (PRECEDENCE)
;; Определяет порядок выполнения операций согласно UR 3.3.
;; Чем выше число, тем быстрее оператор "заберет" себе операнды.
(defun get-priority (op)
  (cond ((string-equal op "NOT") 3) ; Самый высокий приоритет
        ((string-equal op "AND") 2)
        ((string-equal op "OR") 1)  ; Самый низкий приоритет
        (t 0)))                     ; Не является оператором

;; 3. СБОРКА ДЕРЕВА ИЗ ПОСТФИКСНОЙ ЗАПИСИ (RPN BUILDER)
;; На вход идет список типа (A B AND). На выходе (:AND A B).
(defun build-tree-from-rpn (rpn)
  (let (stack)
    (dolist (token rpn (car stack)) ; В конце в стеке останется один корень дерева
      (if (and (listp token) (eq (car token) :field))
          (push token stack) ; Если это уже узел данных, просто кладем в стек
          (let ((op (string-upcase (string token))))
            (cond 
              ;; Унарный NOT: берет один аргумент из стека
              ((string= op "NOT") 
               (push `(:not ,(pop stack)) stack))
              ;; Бинарные AND/OR: берут два аргумента (правый и левый)
              ((member op '("AND" "OR") :test #'string=)
               (let* ((right (pop stack))
                      (left (pop stack))
                      (key (if (string= op "AND") :and :or)))
                 (push `(,key ,left ,right) stack)))))))))

;; 4. ОБРАБОТКА ПОЛЕЙ И ЗНАЧЕНИЙ (FIELD SCOPING)
;; Логика разделения "поле:значение". Учитывает UR 2.1 (жадность).
(defun process-field (token rest)
  (let* ((pos (position #\: token))
         (field (subseq token 0 pos))      ; Берем всё до двоеточия (author)
         (val-part (subseq token (1+ pos)))) ; Хвост сразу после двоеточия
    (if (string/= "" val-part)
        ;; Случай "field:value" (без пробела)
        (values (make-field-node field val-part) rest)
        ;; Случай "field: value1 value2" — собираем всё до ближайшего оператора
        (let (collected)
          (loop while (and (car rest) 
                           (zerop (get-priority (car rest))) 
                           (not (member (car rest) '("(" ")") :test #'string-equal)))
                do (push (pop rest) collected))
          (values (make-field-node field (format nil "~{~A~^ ~}" (nreverse collected))) 
                  rest)))))

;; 5. ШИНТИНГ-ЯРД (ГЛАВНЫЙ АЛГОРИТМ)
;; Превращает инфиксную запись (A AND B) в постфиксную (A B AND)
(defun parse-raw-to-sexp (str)
  (let ((token-list (tokenize str)) (output nil) (stack nil))
    (loop while token-list do
      (let ((token (pop token-list)))
        (cond 
          ;; Если это оператор (AND/OR/NOT)
          ((> (get-priority token) 0)
           ;; Выталкиваем из стека в выход те операторы, у которых приоритет выше или равен текущему
           (loop while (and stack (> (get-priority (car stack)) 0) 
                            (>= (get-priority (car stack)) (get-priority token)))
                 do (push (pop stack) output))
           (push token stack))
          
          ;; Скобки: управляем вложенностью
          ((string= token "(") (push token stack))
          ((string= token ")")
           ;; При закрывающей скобке выталкиваем всё до ближайшей открывающей
           (loop while (and stack (string/= (car stack) "(")) do (push (pop stack) output))
           (pop stack)) ; Удаляем саму "(" из стека
          
          ;; Поля: если в токене есть двоеточие — это начало Scope-фильтра
          ((find #\: token)
           (multiple-value-bind (node remaining) (process-field token token-list)
             (push node output) 
             (setf token-list remaining))) ; Обновляем список токенов после захвата значения
          
          ;; Обычные слова: трактуются как поиск по всем полям (UR 1.1)
          (t (push (make-field-node "any" token) output)))))
    
    ;; Выталкиваем остатки из стека в выход и строим финальное дерево
    (build-tree-from-rpn (nreverse (append output stack)))))
