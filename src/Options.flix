mod Options {
    pub def dimensions(): (Int32, Int32) = (128, 64)

    pub def viewportDimensions(): (Int32, Int32) = (16, 9)

    pub def asciiChars(): String = " .`'^\",:;-_~!|/\\()[]{}<>iIl1ftjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$"
    
    pub def asciiGradientIncrement(): Float64 = 
        let gradientLen = String.length(asciiChars());
        255.0 / Int32.toFloat64(gradientLen - 1)

    pub def zNear(): Float64 = 0.5

    pub def lights(): List[Light] = List#{
        Light.Point(0.7, Math.Vec3.Vec3(0.0, 0.0, 0.5)),
        Light.Point(0.7, Math.Vec3.Vec3(1.5, 0.0, 2.5)),
        Light.Point(0.7, Math.Vec3.Vec3(-1.5, 0.0, 2.5))
    }

    pub def shadingMode(): Shading = Shading.Phong
}
