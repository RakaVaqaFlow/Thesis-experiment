package main

import (
	"context"
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"

	"restaurant-service/internal/controllers"
	"restaurant-service/internal/pkg/db"
	"restaurant-service/internal/repository/postgresql"

	"github.com/gin-gonic/gin"
	"github.com/hashicorp/consul/api"
	"github.com/hashicorp/consul/api/watch"
	"golang.org/x/time/rate"
)

var limiter *rate.Limiter

func initRateLimiter(rate float64, burst int) {
	limiter = rate.NewLimiter(rate.Limit(rate), burst)
}

func setupWatch(client *api.Client, key string, handler func(interface{})) {
	plan, err := watch.Parse(map[string]interface{}{
		"type": "key",
		"key":  key,
	})
	if err != nil {
		log.Fatalf("Failed to create watch plan for %s: %v", key, err)
	}

	plan.Handler = handler

	go func() {
		if err := plan.Run(client.Address()); err != nil {
			log.Fatalf("Failed to run watch plan: %v", err)
		}
	}()
}

func main() {
	config := api.DefaultConfig()
	consulClient, err := api.NewClient(config)
	if err != nil {
		log.Fatalf("Error creating Consul client: %v", err)
	}

	setupWatch(consulClient, "rate-limiter/rate", func(data interface{}) {
		if kv, ok := data.(*api.KVPair); ok && kv != nil {
			newRate, err := strconv.ParseFloat(string(kv.Value), 64)
			if err == nil {
				initRateLimiter(newRate, int(limiter.Burst()))
			}
		}
	})

	setupWatch(consulClient, "rate-limiter/burst", func(data interface{}) {
		if kv, ok := data.(*api.KVPair); ok && kv != nil {
			newBurst, err := strconv.Atoi(string(kv.Value))
			if err == nil {
				initRateLimiter(float64(limiter.Limit()), newBurst)
			}
		}
	})

	dbHost := os.Getenv("RES_DB_HOST")
	dbPort := os.Getenv("RES_DB_PORT")
	dbUser := os.Getenv("RES_DB_USER")
	dbPass := os.Getenv("RES_DB_PASSWORD")
	dbName := os.Getenv("RES_DB_NAME")

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
	router.POST("/api/internal/order", RateLimitMiddleware(), orderController.PostOrder)
	router.GET("/api/order", orderController.GetOrder)
	router.GET("/api/internal/order", RateLimitMiddleware(), orderController.GetOrder)

	err = router.Run(":8081")
	if err != nil {
		fmt.Println("error while running service")
		return
	}
}

func RateLimitMiddleware() gin.HandlerFunc {
	return func(c *gin.Context) {
		if !limiter.Allow() {
			c.AbortWithStatusJSON(http.StatusTooManyRequests, gin.H{"error": "Too many requests"})
			return
		}
		c.Next()
	}
}
