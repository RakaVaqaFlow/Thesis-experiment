package postgresql

import (
	"context"
	"database/sql"
	"errors"

	"delivery-service/internal/models"
	"delivery-service/internal/repository"
)

type OrderRepo struct {
	db dbOps
}

func NewOrderRepo(db dbOps) *OrderRepo {
	return &OrderRepo{db: db}
}

// Add specific order
func (r *OrderRepo) Add(ctx context.Context, order *models.Order) (int64, error) {
	var id int64
	err := r.db.ExecQueryRow(ctx,
		`INSERT INTO orders(name) VALUES ($1) RETURNING id`,
		order.Name).Scan(&id)
	return id, err
}

// GetById Get order info by id
func (r *OrderRepo) GetById(ctx context.Context, id int64) (*models.Order, error) {
	var order models.Order
	err := r.db.Get(ctx, &order,
		`SELECT id,name FROM orders WHERE id=$1`, id)
	if errors.Is(err, sql.ErrNoRows) {
		return nil, repository.ErrObjectNotFound
	}
	return &order, err
}

// Get random 100 orders
func (r *OrderRepo) Get(ctx context.Context) ([]*models.Order, error) {
	orders := make([]*models.Order, 0)
	err := r.db.Get(ctx, &orders, `SELECT id,name FROM orders`)
	if errors.Is(err, sql.ErrNoRows) {
		return nil, repository.ErrObjectNotFound
	}
	return orders, err
}

// Update order info
func (r *OrderRepo) Update(ctx context.Context, order *models.Order) (bool, error) {
	res, err := r.db.Exec(ctx,
		`UPDATE orders SET name = $1 WHERE id=$2`,
		order.Name, order.ID)
	return res.RowsAffected() > 0, err
}

// Delete order by id
func (r *OrderRepo) Delete(ctx context.Context, id int64) (bool, error) {
	res, err := r.db.Exec(ctx, "DELETE FROM orders WHERE id=$1", id)
	return res.RowsAffected() > 0, err
}
