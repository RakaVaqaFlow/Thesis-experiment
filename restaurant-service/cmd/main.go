package main

import (
	"context"
	"fmt"
	"os"

	"restaurant-service/internal/controllers"
	"restaurant-service/internal/pkg/db"
	"restaurant-service/internal/repository/postgresql"

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

	order := postgresql.NewOrderRepo(dbOps)
	orderController := controllers.NewController(order)
	router := gin.Default()
	router.POST("/api/order", orderController.PostOrder)
	router.POST("/api/internal/order", orderController.PostOrder)
	router.GET("/api/order", orderController.GetOrder)
	router.GET("/api/internal/order", orderController.GetOrder)

	err = router.Run(":8081")
	if err != nil {
		fmt.Println("error while running service")
		return
	}
}
