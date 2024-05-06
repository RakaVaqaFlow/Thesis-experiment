ifeq ($(DEL_POSTGRES_SETUP),)
	DEL_POSTGRES_SETUP := user=$(DEL_DB_USER) password=$(DEL_DB_PASSWORD) dbname=$(DEL_DB_NAME) host=$(DEL_DB_HOST) port=$(DEL_DB_PORT) sslmode=disable
endif

ifeq ($(RES_POSTGRES_SETUP),)
	RES_POSTGRES_SETUP := user=$(RES_DB_USER) password=$(RES_DB_PASSWORD) dbname=$(RES_DB_NAME) host=$(RES_DB_HOST) port=$(RES_DB_PORT) sslmode=disable
endif

DEL_MIGRATION_FOLDER=$(CURDIR)/delivery-service/migrations
RES_MIGRATION_FOLDER=$(CURDIR)/restaurant-service/migrations

.PHONY: del-migration-create
del-migration-create:
	goose -dir "$(DEL_MIGRATION_FOLDER)" create "$(name)" sql

.PHONY: res-migration-create
res-migration-create:
	goose -dir "$(RES_MIGRATION_FOLDER)" create "$(name)" sql

.PHONY: del-migration-up
del-migration-up:
	goose -dir "$(DEL_MIGRATION_FOLDER)" postgres "$(DEL_POSTGRES_SETUP)" up

.PHONY: del-migration-down
del-migration-down:
	goose -dir "$(DEL_MIGRATION_FOLDER)" postgres "$(DEL_POSTGRES_SETUP)" down

.PHONY: res-migration-up
res-migration-up:
	goose -dir "$(RES_MIGRATION_FOLDER)" postgres "$(RES_POSTGRES_SETUP)" up

.PHONY: res-migration-down
res-migration-down:
	goose -dir "$(RES_MIGRATION_FOLDER)" postgres "$(RES_POSTGRES_SETUP)" down

	
.PHONY: compose-up
compose-up:
	docker compose build
	docker compose up -d delivery-postgres
	docker compose up -d delivery-service
	docker compose up -d restaurant-postgres
	docker compose up -d restaurant-service

.PHONY: compose-rm
compose-rm:
	docker compose down

.PHONY up-all:
up-all:
	make compose-up
	make del-migration-up
	make res-migration-up

