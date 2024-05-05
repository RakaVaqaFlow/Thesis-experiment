package controllers

import (
	"net/http"
	"strconv"

	"restaurant-service/internal/models"
	"restaurant-service/internal/repository"

	"github.com/gin-gonic/gin"
)

type OrderController struct {
	orderRepo repository.OrderRepo
}

func NewController(orderRepo repository.OrderRepo) *OrderController {
	return &OrderController{
		orderRepo: orderRepo,
	}
}

func (o *OrderController) PostOrder(c *gin.Context) {
	orderName := c.Query("name")
	if orderName == "" {
		c.JSON(http.StatusBadRequest, gin.H{"error": "Name parameter is required"})
		return
	}

	newOrder := models.Order{
		Name: orderName,
	}
	_, err := o.orderRepo.Add(c, newOrder)
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}
	c.JSON(http.StatusOK, nil)
}

func (o *OrderController) GetOrder(c *gin.Context) {
	orders, err := o.orderRepo.Get(c)
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}
	c.JSON(http.StatusOK, gin.H{"orders": orders})
}

func (o *OrderController) UpdateOrder(c *gin.Context) {
	orderName := c.Query("name")
	if orderName == "" {
		c.JSON(http.StatusBadRequest, gin.H{"error": "Name parameter is required"})
		return
	}
	_, err := o.orderRepo.Update(c, models.Order{
		Name: orderName,
	})
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}
	c.JSON(http.StatusOK, nil)
}

func (o *OrderController) DeleteOrder(c *gin.Context) {
	id := c.Query("id")
	if id == "" {
		c.JSON(http.StatusBadRequest, gin.H{"error": "Id parameter is required"})
		return
	}
	validID, err := strconv.ParseInt(id, 10, 64)
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{"error": "Id parameter should be integer"})
	}
	_, err = o.orderRepo.Delete(c, validID)
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}
	c.JSON(http.StatusOK, nil)
}
