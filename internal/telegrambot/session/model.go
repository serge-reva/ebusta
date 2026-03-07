package session

import (
	"time"

	"ebusta/internal/presenter"
)

type Session struct {
	UserID            string
	Query             string
	PageSize          int
	CurrentPage       int
	LastResult        *presenter.PresenterResult
	LastListMessageID int
	LastBookMessageID int
	LastMessageID     int
	UpdatedAt         time.Time
}
