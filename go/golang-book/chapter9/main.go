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

func (this *Square) area() float64 {
	return float64(this.width * this.width)
}

func (this *Rectangle) area() float64 {
	return float64(this.width * this.height)
}

func (this *Circle) area() float64 {
	return math.Pi * float64(this.radius * this.radius)
}

func main() {
	square := Square{ 10 }
	rectangle := Rectangle{ 10, 20 }
	circle := Circle{ 10 }

	fmt.Println(square.area())
	fmt.Println(rectangle.area())
	fmt.Println(circle.area())
}
