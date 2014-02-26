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
	description() string
	area() float64
	perimeter() float64
}

// -------------------------------------------------------------------
//  Square
// -------------------------------------------------------------------
func (this *Square) description() string {
	return fmt.Sprintf("Square{ width: %d }", this.width)
}

func (this *Square) area() float64 {
	return float64(this.width * this.width)
}

func (this *Square) perimeter() float64 {
	return float64(4 * this.width)
}

// -------------------------------------------------------------------
//  Rectangle
// -------------------------------------------------------------------
func (this *Rectangle) description() string {
	return fmt.Sprintf("Rectangle{ width: %d, height: %d }", this.width, this.height)
}

func (this *Rectangle) area() float64 {
	return float64(this.width * this.height)
}

func (this *Rectangle) perimeter() float64 {
	return float64(2 * (this.width + this.height))
}

// -------------------------------------------------------------------
//  Circle
// -------------------------------------------------------------------
func (this *Circle) description() string {
	return fmt.Sprintf("Circle{ radius: %d }", this.radius)
}

func (this *Circle) area() float64 {
	return math.Pi * float64(this.radius * this.radius)
}

func (this *Circle) perimeter() float64 {
	return math.Pi * float64(2 * this.radius)
}

// -------------------------------------------------------------------
//  Main
// -------------------------------------------------------------------
func totalArea(shapes ...Shape) float64 {
	var area float64
	for _, shape := range shapes {
		area += shape.area()
	}
	return area
}

func printStats(shape Shape) {
	fmt.Println(shape.description(), "area:", shape.area(), "perimeter:", shape.perimeter())
}

func main() {
	square    := Square{ width: 10 }
	rectangle := Rectangle{ width: 10, height: 20 }
	circle    := Circle{ radius: 10 }

	printStats(&square)
	printStats(&rectangle)
	printStats(&circle)

	fmt.Println("Total area", totalArea(&square, &rectangle, &circle))
}
