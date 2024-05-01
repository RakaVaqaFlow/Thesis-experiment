package controllers

import (
	"net/http"

	"delivery-service/internal/models"
	"delivery-service/internal/repository"

	"github.com/gin-gonic/gin"
)

type DeliveryController struct {
	deliveryRepo repository.DeliveryRepo
	orderRepo    repository.OrderRepo
}

func NewController(deliveryRepo repository.DeliveryRepo, orderRepo repository.OrderRepo) *DeliveryController {
	return &DeliveryController{
		deliveryRepo: deliveryRepo,
		orderRepo:    orderRepo,
	}
}

func (d *DeliveryController) PostDelivery(c *gin.Context) {
	deliveryName := c.Query("name")
	if deliveryName == "" {
		c.JSON(http.StatusBadRequest, gin.H{"error": "Name parameter is required"})
		return
	}
	_, err := d.deliveryRepo.Add(c, models.Delivery{
		Name: deliveryName,
	})
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
	}

	newOrder := models.Order{
		Name: deliveryName,
	}
	_, err = d.orderRepo.Add(c, newOrder)
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
	}

	_, err = d.orderRepo.Get(c)
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
	}
	// create order, call post and get methods from order repo
	c.JSON(http.StatusOK, nil)
}
