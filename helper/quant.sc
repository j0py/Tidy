JSQuant {
	classvar <>quant=1;

	*quantize {
		var now = thisThread.beats;
		((now + quant).div(quant) * quant - now).wait;
	}
}
