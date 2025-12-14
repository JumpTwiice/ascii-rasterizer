mod Options {
    pub def dimensions(): (Int32, Int32) = (128, 64)

    pub def viewportDimensions(): (Int32, Int32) = (16, 9)

    pub def asciiChars(): String = " .`'^\",:;-_~!|/\\()[]{}<>iIl1ftjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$"
    
    pub def asciiGradientIncrement(): Float64 = 
        let gradientLen = String.length(asciiChars());
        255.0 / Int32.toFloat64(gradientLen - 1)

    pub def zNear(): Float64 = 0.5

    pub def debugShowBackfaces(): Bool = false
    
    pub def debugBackfaceColor(): (Int32, Int32, Int32) = (255, 0, 255)

    pub def debugBackfaceCullPoint(): Option[Math.Vec3] = None

}
