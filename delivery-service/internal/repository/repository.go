package repository

import (
	"context"
	"errors"

	"delivery-service/internal/models"
)

var (
	ErrObjectNotFound = errors.New("object not found")
)

type DeliveryRepo interface {
	Add(ctx context.Context, delivery models.Delivery) (int64, error)
	GetById(ctx context.Context, id int64) (*models.Delivery, error)
	Get(ctx context.Context) ([]*models.Delivery, error)
	Update(ctx context.Context, delivery models.Delivery) (bool, error)
	Delete(ctx context.Context, id int64) (bool, error)
}

type OrderRepo interface {
	Add(ctx context.Context, order models.Order) (int64, error)
	GetById(ctx context.Context, id int64) (*models.Order, error)
	Get(ctx context.Context) ([]*models.Order, error)
	Update(ctx context.Context, order models.Order) (bool, error)
	Delete(ctx context.Context, id int64) (bool, error)
}
