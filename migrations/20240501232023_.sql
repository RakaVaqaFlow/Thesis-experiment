-- +goose Up
-- +goose StatementBegin
CREATE TABLE IF NOT EXISTS deliveries (
    id BIGSERIAL PRIMARY KEY NOT NULL, 
    name VARCHAR(100) NOT NULL DEFAULT ''
);

CREATE TABLE IF NOT EXISTS orders(
    id BIGSERIAL PRIMARY KEY NOT NULL, 
    name VARCHAR(100) NOT NULL DEFAULT ''
);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE deliveries;
DROP TABLE orders;
-- +goose StatementEnd