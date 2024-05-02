FROM golang:latest

ENV SERVICE_PATH=delivery-service

WORKDIR /app

COPY ${SERVICE_PATH}/go.mod ${SERVICE_PATH}/go.sum ./

RUN go mod download

COPY ${SERVICE_PATH}/ . 

RUN go build -o main cmd/main.go

EXPOSE 8080

CMD ["./main"]
