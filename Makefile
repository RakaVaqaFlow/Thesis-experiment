# Параметры для подключения к базе данных PostgreSQL
PG_HOST=${DB_HOST}
PG_PORT=${DB_PORT}
PG_USER=${DB_USER}
PG_PASSWORD=${DB_PASSWORD}
PG_DATABASE=${DB_NAME}

ifeq ($(POSTGRES_SETUP_TEST),)
	POSTGRES_SETUP_TEST := user=$(PG_USER) password=$(PG_PASSWORD) dbname=$(PG_DATABASE) host=$(PG_HOST) port=$(PG_PORT) sslmode=disable
endif

MIGRATION_FOLDER=$(CURDIR)/migrations

.PHONY: migration-create
migration-create:
	goose -dir "$(MIGRATION_FOLDER)" create "$(name)" sql

.PHONY: test-migration-up
test-migration-up:
	goose -dir "$(MIGRATION_FOLDER)" postgres "$(POSTGRES_SETUP_TEST)" up

.PHONY: test-migration-down
test-migration-down:
	goose -dir "$(MIGRATION_FOLDER)" postgres "$(POSTGRES_SETUP_TEST)" down

	
.PHONY: compose-up
compose-up:
	docker compose build
	docker compose up -d postgres
	docker compose up -d delivery-service

.PHONY: compose-rm
compose-rm:
	docker compose down