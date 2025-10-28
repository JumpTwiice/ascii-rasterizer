mod Options {
    use Options.asciiChars
    pub def dimensions(): (Int32, Int32) = (128, 64)

    pub def viewportDimensions(): (Int32, Int32) = (128, 64)

    pub def focalLength(): Float64 = 128.0

    pub def asciiChars(): String = " .\'`^\",:;Il!i><~+_-?][}{1)(|\\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$"
    
    pub def asciiGradientIncrement(): Float64 = 
        let gradientLen = String.length(asciiChars());
        255.0 / Int32.toFloat64(gradientLen)

}