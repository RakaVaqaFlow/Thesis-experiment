# How to use

inside root of the repositoty

1. Run `make compose-up` to build and run docker containers
2. Run `make create-migration-up` to add tables in postgresql
3. Now you can use it, to check run `curl -X POST "http://localhost:8080/api/delivery?name=some"` 
