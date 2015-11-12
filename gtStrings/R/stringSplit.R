StringSplit <- function(data, state, inputs, outputs, normalization =TRUE)
	inputs <- substitute(inputs)
	check.exprs(inputs, FALSE)
	inputs <- convert.exprs(inputs)

	outputs <- substitute(outputs)
	check.atts(outputs, FALSE)
	outputs <- convert.atts(outputs)

	if(length(inputs) != 1)
		stop("Must be exactly 1 input")
	if(length(outputs) != 1)
		stop("Must be exactly 1 output")

	gt <- GT(statistics::Nearest_Neighbor, normalization = normalization)
	
	Transform(data, gt, inputs, outputs, list(state))
}
