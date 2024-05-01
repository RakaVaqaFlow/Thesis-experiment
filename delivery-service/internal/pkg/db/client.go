package db

import (
	"context"
	"fmt"

	"github.com/jackc/pgx/v4/pgxpool"
)

//const (
//	host     = "localhost"
//	port     = 5432
//	user     = "test"
//	password = "test"
//	dbname   = "test"
//)

// NewDB return instance of Database
func NewDB(ctx context.Context, host string, port int, user, password, dbname string) (*Database, error) {
	dsn := generateDsn(host, port, user, password, dbname)
	pool, err := pgxpool.Connect(ctx, dsn)
	if err != nil {
		return nil, err
	}

	return newDatabase(pool), nil
}

func generateDsn(host string, port int, user, password, dbname string) string {
	return fmt.Sprintf("host=%s port=%d user=%s password=%s dbname=%s sslmode=disable", host, port, user, password, dbname)
}
