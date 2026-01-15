mod Options {
    pub def asciiMode(): Bool = false

    pub def dimensions(): (Int32, Int32) = (128, 64)

    pub def viewportDimensions(): (Int32, Int32) = (2,1)

    pub def asciiChars(): String = " ░▒▓█"
    
    pub def asciiGradientIncrement(): Float64 = 
        let gradientLen = String.length(asciiChars());
        255.0 / Int32.toFloat64(gradientLen - 1)

    pub def zNear(): Float64 = 0.5

    pub def lights(): List[Light] = List#{
        Light.Point(0.6, Math.Vec3.Vec3(0.0, 0.0, 0.0))
    }

    pub def shadingMode(): Shading = Shading.Phong
}
