package errutil

import (
    "os"
    "sync"

    "gopkg.in/yaml.v3"
)

var (
    messages     *ErrorMessages
    messagesOnce sync.Once
    messagesErr  error
)

// ErrorMessages — структура файла errors.yaml
type ErrorMessages struct {
    UserErrors   map[string]string `yaml:"user_errors"`
    SystemErrors map[string]string `yaml:"system_errors"`
}

// LoadMessages загружает сообщения из errors.yaml
// Потокобезопасно, загружает только один раз
func LoadMessages(path string) error {
    messagesOnce.Do(func() {
        data, err := os.ReadFile(path)
        if err != nil {
            messagesErr = err
            return
        }

        var m ErrorMessages
        if err := yaml.Unmarshal(data, &m); err != nil {
            messagesErr = err
            return
        }

        messages = &m
    })

    return messagesErr
}

// GetMessages возвращает загруженные сообщения
func GetMessages() *ErrorMessages {
    return messages
}

// GetUserMessage возвращает сообщение для пользователя по ключу
func GetUserMessage(key string) string {
    if messages == nil {
        return key
    }
    if msg, ok := messages.UserErrors[key]; ok {
        return msg
    }
    return key
}

// GetSystemMessage возвращает системное сообщение по ключу
func GetSystemMessage(key string) string {
    if messages == nil {
        return key
    }
    if msg, ok := messages.SystemErrors[key]; ok {
        return msg
    }
    return key
}

// GetMessage возвращает сообщение по ключу (сначала system, потом user)
func GetMessage(key string) string {
    if messages == nil {
        return key
    }
    if msg, ok := messages.SystemErrors[key]; ok {
        return msg
    }
    if msg, ok := messages.UserErrors[key]; ok {
        return msg
    }
    return key
}
