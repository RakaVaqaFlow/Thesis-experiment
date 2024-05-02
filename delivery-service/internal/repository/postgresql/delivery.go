package postgresql

import (
	"context"
	"database/sql"
	"errors"

	"delivery-service/internal/models"
	"delivery-service/internal/repository"

	"github.com/jackc/pgconn"
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
)

type dbOps interface {
	Select(ctx context.Context, dest interface{}, query string, args ...interface{}) error
	Get(ctx context.Context, dest interface{}, query string, args ...interface{}) error
	Exec(ctx context.Context, query string, args ...interface{}) (pgconn.CommandTag, error)
	ExecQueryRow(ctx context.Context, query string, args ...interface{}) pgx.Row
	GetPool(ctx context.Context) *pgxpool.Pool
}

type DeliveryRepo struct {
	db dbOps
}

func NewDeliveryRepo(db dbOps) *DeliveryRepo {
	return &DeliveryRepo{db: db}
}

// Add specific delivery
func (r *DeliveryRepo) Add(ctx context.Context, delivery models.Delivery) (int64, error) {
	var id int64
	err := r.db.ExecQueryRow(ctx,
		`INSERT INTO deliveries(name) VALUES ($1) RETURNING id`,
		delivery.Name).Scan(&id)
	return id, err
}

// GetById Get delivery info by id
func (r *DeliveryRepo) GetById(ctx context.Context, id int64) (*models.Delivery, error) {
	var delivery models.Delivery
	err := r.db.Get(ctx, &delivery,
		`SELECT id,name FROM deliveries WHERE id=$1`, id)
	if errors.Is(err, sql.ErrNoRows) {
		return nil, repository.ErrObjectNotFound
	}
	return &delivery, err
}

// Get random 100 deliveries
func (r *DeliveryRepo) Get(ctx context.Context) ([]*models.Delivery, error) {
	deliveries := make([]*models.Delivery, 0)
	err := r.db.Select(ctx, &deliveries, `SELECT id,name FROM deliveries ORDER BY random() LIMIT 100`)
	if errors.Is(err, sql.ErrNoRows) {
		return nil, repository.ErrObjectNotFound
	}
	return deliveries, err
}

// Update delivery info
func (r *DeliveryRepo) Update(ctx context.Context, delivery models.Delivery) (bool, error) {
	res, err := r.db.Exec(ctx,
		`UPDATE deliveries SET name = $1 WHERE id=$2`,
		delivery.Name, delivery.ID)
	return res.RowsAffected() > 0, err
}

// Delete delivery by id
func (r *DeliveryRepo) Delete(ctx context.Context, id int64) (bool, error) {
	res, err := r.db.Exec(ctx, "DELETE FROM deliveries WHERE id=$1", id)
	return res.RowsAffected() > 0, err
}
