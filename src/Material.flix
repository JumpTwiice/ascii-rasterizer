type alias Color = (Int32, Int32, Int32)

enum Material {
    case Solid(Color)
    case Textured(Texture)
}

mod Material {

    def wrap01(x: Float64): Float64 =
        let f = x - Float64.floor(x);
        if (f < 0.0) f + 1.0 else f

    def clamp01(x: Float64): Float64 =
        if (x < 0.0) 0.0 else if (x > 1.0) 1.0 else x

    pub def getColorAt(material: Material, u: Float64, v: Float64): Color = match material {
        case Solid(color) => color
        case Textured(texture) => {
            let (width, height) = Texture.dimensions(texture);
            let uu = wrap01(u);
            let vv = clamp01(v);
            let clamp = Float64.clampToInt32(min = 0, max = width - 1, nanValue = 0);
            let x = clamp(Float64.floor(uu * Int32.toFloat64(width)));
            let y = clamp(Float64.floor(vv * Int32.toFloat64(height)));
            let (r, g, b, _) = Texture.getTexel(texture, x, y);
            (Int16.toInt32(r), Int16.toInt32(g), Int16.toInt32(b))
        }
    }
}