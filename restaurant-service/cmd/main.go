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
var consulAddress = "127.0.0.1:8500"

func initRateLimiter(newRate float64, burst int) {
	limiter = rate.NewLimiter(rate.Limit(newRate), burst)
}

func setupWatch(address string, key string, handler func(uint64, interface{})) {
	plan, err := watch.Parse(map[string]interface{}{
		"type": "key",
		"key":  key,
	})
	if err != nil {
		log.Fatalf("Failed to create watch plan for %s: %v", key, err)
	}

	plan.Handler = watch.HandlerFunc(func(idx uint64, data interface{}) {
		handler(idx, data)
	})

	go func() {
		if err := plan.Run(address); err != nil {
			log.Fatalf("Failed to run watch plan: %v", err)
		}
	}()
}

func main() {
	config := api.DefaultConfig()
	config.Address = consulAddress
	_, err := api.NewClient(config)
	if err != nil {
		log.Fatalf("Error creating Consul client: %v", err)
	}

	setupWatch(consulAddress, "rate-limiter/rate", func(idx uint64, data interface{}) {
		if kv, ok := data.(*api.KVPair); ok && kv != nil {
			newRate, err := strconv.ParseFloat(string(kv.Value), 64)
			if err == nil {
				initRateLimiter(newRate, int(limiter.Burst()))
			}
		}
	})

	setupWatch(consulAddress, "rate-limiter/burst", func(idx uint64, data interface{}) {
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
