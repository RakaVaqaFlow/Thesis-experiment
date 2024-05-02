package main

import (
	"context"
	"fmt"
	"os"

	"delivery-service/internal/controllers"
	"delivery-service/internal/pkg/db"
	"delivery-service/internal/repository/postgresql"

	"github.com/gin-gonic/gin"
)

func main() {
	dbHost := os.Getenv("DB_HOST")
	dbPort := os.Getenv("DB_PORT")
	dbUser := os.Getenv("DB_USER")
	dbPass := os.Getenv("DB_PASSWORD")
	dbName := os.Getenv("DB_NAME")

	ctx := context.Background()

	dbOps, err := db.NewDB(ctx, dbHost, dbPort, dbUser, dbPass, dbName)
	if err != nil {
		fmt.Println(err.Error())
		return
	}

	delivery := postgresql.NewDeliveryRepo(dbOps)
	order := postgresql.NewOrderRepo(dbOps)
	deliveryController := controllers.NewController(delivery, order)
	router := gin.Default()
	router.POST("/api/delivery", deliveryController.PostDelivery)

	err = router.Run(":8080")
	if err != nil {
		fmt.Println("error while running service")
		return
	}
}
