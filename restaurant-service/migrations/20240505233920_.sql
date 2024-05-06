-- +goose Up
-- +goose StatementBegin
CREATE TABLE IF NOT EXISTS orders(
    id BIGSERIAL PRIMARY KEY, 
    name VARCHAR(100) NOT NULL DEFAULT ''
);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE orders;
-- +goose StatementEnd