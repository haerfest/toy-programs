package main

import (
	"fmt"
	"math"
)

type Square struct {
	width int
}

type Rectangle struct {
	width int
	height int
}

type Circle struct {
	radius int
}

type Shape interface {
	area() float64
}

func (this *Square) area() float64 {
	return float64(this.width * this.width)
}

func (this *Rectangle) area() float64 {
	return float64(this.width * this.height)
}

func (this *Circle) area() float64 {
	return math.Pi * float64(this.radius * this.radius)
}

func totalArea(shapes ...Shape) float64 {
	var area float64
	for _, shape := range shapes {
		area += shape.area()
	}
	return area
}

func main() {
	square    := Square{ width: 10 }
	rectangle := Rectangle{ width: 10, height: 20 }
	circle    := Circle{ radius: 10 }

	fmt.Println(totalArea(&square, &rectangle, &circle))
}
