pub enum Texture {
    case Texture(Int32, Int32, Vector[Int16]) // width, height, pixels
}

mod Texture {

    pub def getTexel(t: Texture, x: Int32, y: Int32): (Int16, Int16, Int16, Int16) = match t {
        case Texture(width, _, pixels) => {
            let idx = (y * width + x) * 4;
            (
                Vector.get(idx, pixels),
                Vector.get(idx + 1, pixels),
                Vector.get(idx + 2, pixels),
                Vector.get(idx + 3, pixels)
            )
        }
    }
    
    pub def dimensions(t: Texture): (Int32, Int32) = match t {
        case Texture(width, height, _) => (width, height)
    }
}