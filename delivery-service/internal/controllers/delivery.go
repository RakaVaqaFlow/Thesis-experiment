package controllers

import (
	"net/http"

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
	// put delivery to database
	// create order, call post and get methods from order repo

}
