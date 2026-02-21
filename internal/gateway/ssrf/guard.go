package ssrf

import (
    "net"
    "net/url"
    "strings"
)

var (
    // Приватные IP диапазоны, которые нужно блокировать
    privateCIDRs = []string{
        "127.0.0.0/8",      // localhost
        "10.0.0.0/8",       // private
        "172.16.0.0/12",    // private
        "192.168.0.0/16",   // private
        "169.254.0.0/16",   // link-local
        "::1/128",          // localhost IPv6
        "fc00::/7",         // unique local addr IPv6
        "fe80::/10",        // link-local IPv6
    }
    
    // Запрещённые хосты
    blockedHosts = []string{
        "localhost",
        "127.0.0.1",
        "::1",
        "metadata.google.internal",
        "169.254.169.254", // AWS metadata
    }
)

type Guard struct {
    privateNets []*net.IPNet
}

func NewGuard() (*Guard, error) {
    g := &Guard{}
    
    for _, cidr := range privateCIDRs {
        _, ipnet, err := net.ParseCIDR(cidr)
        if err != nil {
            return nil, err
        }
        g.privateNets = append(g.privateNets, ipnet)
    }
    
    return g, nil
}

func (g *Guard) IsAllowed(rawURL string) bool {
    parsed, err := url.Parse(rawURL)
    if err != nil {
        return false
    }
    
    host := parsed.Hostname()
    
    // Проверка запрещённых хостов
    for _, blocked := range blockedHosts {
        if strings.EqualFold(host, blocked) {
            return false
        }
    }
    
    // Пытаемся распарсить как IP
    ip := net.ParseIP(host)
    if ip != nil {
        // Проверяем, не в приватном ли диапазоне
        for _, net := range g.privateNets {
            if net.Contains(ip) {
                return false
            }
        }
        return true
    }
    
    // Если это домен - разрешаем (можно добавить белый список)
    return true
}

// ValidateURL проверяет URL и возвращает ошибку если небезопасно
func (g *Guard) ValidateURL(rawURL string) error {
    if !g.IsAllowed(rawURL) {
        return ErrBlockedURL
    }
    return nil
}

var ErrBlockedURL = &BlockedURLError{}

type BlockedURLError struct{}

func (e *BlockedURLError) Error() string {
    return "URL is blocked for security reasons"
}
