mod ImageLoader {
    use Math.Vec3

    pub def loadCheckerboardTexture(width: Int32, height: Int32, checkerSize: Int32): Texture = {
        let pixels = Vector.range(0, width * height * 4) |> Vector.map(i -> {
            let x = (i / 4) `Int32.modulo` width;
            let y = (i / 4) / width;
            let checkerX = x / checkerSize;
            let checkerY = y / checkerSize;
            let isWhite = (checkerX + checkerY) `Int32.modulo` 2 == 0;
            let colorValue: Int16 = if (isWhite) 255i16 else 0i16;

            match i `Int32.modulo` 4 {
                case 0 => colorValue // R
                case 1 => colorValue // G
                case 2 => colorValue // B
                case 3 => 255i16 // A
                case _ => unreachable!()
            }
        });
        Texture.Texture(width, height, pixels)
    }

    pub def readTextureFromFile(path: String): Texture \ FileRead = {
        let bytesRaw = FileRead.readBytes(path);
        let bytes = Vector.map(b -> {
            let x = Int8.toInt16(b);
            if (x < 0i16) x + 256i16 else x
        }, bytesRaw);
        let (width, height, pixels) = parseRawRGBA(bytes);
        Texture.Texture(width, height, pixels)
    }

    def parseRawRGBA(data: Vector[Int16]): (Int32, Int32, Vector[Int16]) = {
        def u32le(offset: Int32): Int32 = {
            let b0 = Int16.toInt32(Vector.get(offset, data));
            let b1 = Int16.toInt32(Vector.get(offset + 1, data));
            let b2 = Int16.toInt32(Vector.get(offset + 2, data));
            let b3 = Int16.toInt32(Vector.get(offset + 3, data));
            Int32.bitwiseOr(
                b0,
                Int32.bitwiseOr(
                    Int32.leftShift(b1, 8),
                    Int32.bitwiseOr(Int32.leftShift(b2, 16), Int32.leftShift(b3, 24))
                )
            )
        };

        if (Vector.length(data) < 8) unreachable!() else {
            let width = u32le(0);
            let height = u32le(4);
            let pixelBytes = width * height * 4;
            let expectedLen = 8 + pixelBytes;
            if (width <= 0 or height <= 0 or Vector.length(data) < expectedLen) unreachable!() else {
                let pixels = Vector.slice(start = 8, end = expectedLen, data);
                (width, height, pixels)
            }
        }
    }
}
