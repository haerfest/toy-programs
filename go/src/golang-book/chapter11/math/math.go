package math

func Average(xs []float64) float64 {
	total := float64(0)
	for _, x := range xs {
		total += x
	}
	return total / float64(len(xs))
}

type reduceFunc func(float64, float64) float64

func reduce(xs []float64, reducer reduceFunc) float64 {
	reduction := xs[0]
	for _, x := range xs {
		reduction = reducer(reduction, x)
	}
	return reduction
}

func Min(xs []float64) float64 {
	reducer := func(reduction, x float64) float64 {
		if x < reduction {
			return x
		}
		return reduction
	}
	return reduce(xs, reducer)
}

func Max(xs []float64) float64 {
	reducer := func(reduction, x float64) float64 {
		if x > reduction {
			return x
		}
		return reduction
	}
	return reduce(xs, reducer)
}
