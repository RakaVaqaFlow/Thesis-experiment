# How to use

inside root of the repositoty

1. Setup values. Example:

```
export DEL_DB_HOST=127.0.0.1
export DEL_DB_PORT=5432
export DEL_DB_USER=test
export DEL_DB_PASSWORD=test
export DEL_DB_NAME=test
export RES_DB_HOST=127.0.0.1
export RES_DB_PORT=5430
export RES_DB_USER=test
export RES_DB_PASSWORD=test
export RES_DB_NAME=test
```

2. Run `make up-all` to build, run docker containers and migrate tables to databases
3. Now you can use it, to check run `curl -X POST "http://localhost:8080/api/delivery?name=some"` 
