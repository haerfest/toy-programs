package math

import "testing"

type testpair struct {
	values []float64
	expectation float64
}

var averageTests = []testpair{
	{ []float64{ 1, 2 },                1.5 },
	{ []float64{ 1, 1, 1, 1, 1, 1, 1 },   1 },
	{ []float64{ -1, 1 },                 0 },
}

var minTests = []testpair{
	{ []float64{ 1, 2 },              1 },
	{ []float64{ 2, 1 },              1 },
	{ []float64{ 10, 3, -20, 100 }, -20 },
}

var maxTests = []testpair{
	{ []float64{ 1, 2 },              2 },
	{ []float64{ 2, 1 },              2 },
	{ []float64{ 10, 3, -20, 100 }, 100 },
}

type testableFunc func([]float64) float64

func test(t *testing.T, f testableFunc, tests []testpair) {
	for _, pair := range tests {
		v := f(pair.values)
		if v != pair.expectation {
			t.Error(
				"For", pair.values,
				"expected", pair.expectation,
				"got", v,
			)
		}
	}
}

func TestAverage(t *testing.T) {
	test(t, Average, averageTests)
}

func TestMin(t *testing.T) {
	test(t, Min, minTests)
}

func TestMax(t *testing.T) {
	test(t, Max, maxTests)
}
