package controllers

import (
	"net/http"

	"delivery-service/internal/models"
	"delivery-service/internal/repository"

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
	_, err := o.orderRepo.Get(c)
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}
	c.JSON(http.StatusOK, nil)
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
	_, err := o.orderRepo.Delete(c, id)
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}
	c.JSON(http.StatusOK, nil)
}
