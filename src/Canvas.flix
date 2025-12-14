struct Canvas[r: Region] {
    width: Int32,
    height: Int32,
    mut raster: Canvas.Buffer[Int32, r],
    mut zBuffer: Canvas.Buffer[Float64, r],
    rc: Region[r]
}

mod Canvas {
    @Internal
    pub type alias Buffer[v: Type, r: Region] = Array[Array[v, r], r]

    pub def dimensions(c: Canvas[r]): (Int32, Int32) = (c->width, c->height)

    pub def empty(rc: Region[r], w: Int32, h: Int32): Canvas[r] \ r =
        new Canvas @ rc { 
            width = w, 
            height = h, 
            raster = mkBuffer(rc, w, h, 0), 
            zBuffer = mkBuffer(rc, w, h, 0.0), 
            rc = rc 
        }

    pub def display(c: Canvas[r]): Unit \ Console + r = 
        _getTerminalFrame(c) |> Console.println

    pub def flush(c: Canvas[r]): Unit \ r = {
        foreach (
            i <- Range.Range(0, c->height);
            j <- Range.Range(0, c->width)
        ) {
            Array.get(i, c->raster) |> Array.put(0, j);
            Array.get(i, c->zBuffer) |> Array.put(0.0, j)
        }
    }

    def mkBuffer(rc: Region[r], w: Int32, h: Int32, v: v): Buffer[v, r] \ r  = 
        Array.init(rc, _ -> Array.repeat(rc, w, v), h)

    def _getTerminalFrame(c: Canvas[r]): String \ r = region rc {
        let h2 = (c->height + 1) / 2;
        let width = c->width;
        let frameRows = Array.empty(rc, h2);

        foreach (i2 <- Range.Range(0, h2)) {
            let yTop = i2 * 2;
            let yBot = yTop + 1;

            let rowVec =
                Vector.init(x -> {
                    let top = getValueAt(yTop, x, c->raster);
                    let bot = getValueAt(yBot, x, c->raster);
                    RichString.text("▀")
                        |> RichString.withColor(unpackRGBRich(top))
                        |> RichString.withBgColor(unpackRGBRich(bot))
                        |> RichString.toString
                }, width);

            Array.put(Vector.join("", rowVec), i2, frameRows)
        };

        Array.join("\n", frameRows)
    }

    def _getASCIITerminalFrameHalved(c: Canvas[r]): String \ r = region rc {
        let width = c->width;
        let height = c->height / 2;
        let frameRows = Array.empty(rc, height);

        foreach (i <- Range.Range(0, height)) {
            let rowVec =
                Vector.init(j -> {
                    let top = getValueAt(i * 2, j, c->raster);
                    let bot = getValueAt(i * 2 + 1, j, c->raster);
                    let avg = (top + bot) / 2;
                    let (r, g, b) = unpackRGB(avg);
                    let gray = grayScaleFromRGB(r, g, b);
                    asciiFromGrayScale(gray)
                }, width);

            Array.put(Vector.join("", rowVec), i, frameRows)
        };

        Array.join("\n", frameRows)
    }

    def _getASCIITerminalFrame(c: Canvas[r]): String \ r = region rc {
        let frameRows = Array.empty(rc, c->height);
        foreach (i <- Range.Range(0, c->height)) {
            let rowVec =
                Vector.init(j -> {
                    let color = getValueAt(i, j, c->raster);
                    let (r, g, b) = unpackRGB(color);
                    let gray = grayScaleFromRGB(r, g, b);
                    asciiFromGrayScale(gray)
                }, c->width);

            Array.put(Vector.join("", rowVec), i, frameRows)
        };

        Array.join("\n", frameRows)
    }

    def asciiFromGrayScale(gray: Float64): Char =
        let idx = Float64.clampToInt32(min = 0, max = 255, nanValue = -1, gray / Options.asciiGradientIncrement());
        Options.asciiChars() |> String.charAt(idx)

    def grayScaleFromRGB(r: Int32, g: Int32, b: Int32): Float64 = 
        (0.299 * Int32.toFloat64(r)) + (0.587 * Int32.toFloat64(g)) + (0.114 * Int32.toFloat64(b))

    def unpackRGBRich(color: Int32): RichString.Color =
        let (r, g, b) = unpackRGB(color);
        RichString.Color.Rgb(r, g, b)
        
    pub def drawTriangle(x0: Int32, y0: Int32, x1: Int32, y1: Int32, x2: Int32, y2: Int32, color: (Int32, Int32, Int32), c: Canvas[r]): Unit \ r = {
        drawLine(x0, y0, x1, y1, color, c);
        drawLine(x1, y1, x2, y2, color, c);
        drawLine(x2, y2, x0, y0, color, c)
    }

    pub def drawFilledTriangle(x0: Int32, y0: Int32, x1: Int32, y1: Int32, x2: Int32, y2: Int32, color: (Int32, Int32, Int32), c: Canvas[r]): Unit \ r = {
        let points = List#{(x0, y0), (x1, y1), (x2, y2)} |> List.sortBy(snd);
        let ((ax0, ay0), (ax1, ay1), (ax2, ay2)) = match points {
            case top :: mid :: bot :: Nil => (top, mid, bot)
            case _ => unreachable!()
        };
        let x01 = interpolate(ay0, ax0, ay1, ax1);
        let x12 = interpolate(ay1, ax1, ay2, ax2);
        let x02 = interpolate(ay0, ax0, ay2, ax2);
        let x012 = Vector.append(Vector.dropRight(1, x01), x12);
        let m = Vector.length(x012) / 2;
        let (xLeft, xRight) = if (Vector.get(m, x02) < Vector.get(m, x012)) (x02, x012) else (x012, x02);
        foreach (y <- Range.Range(ay0, ay2 + 1))
            foreach (x <- Range.Range(Vector.get(y - ay0, xLeft), Vector.get(y - ay0, xRight) + 1))
                drawPoint(x, y, color, c)
    }

    pub def drawShadedTriangle(
        x0: Int32, y0: Int32,
        x1: Int32, y1: Int32,
        x2: Int32, y2: Int32,
        h0: Float64, h1: Float64, h2: Float64,
        z0: Float64, z1: Float64, z2: Float64,        
        color: (Int32, Int32, Int32), 
        c: Canvas[r]
    ): Unit \ r = {
        let (r, g, b) = rgbAsFloat(color);
        let points = List#{(x0, y0, h0, z0), (x1, y1, h1, z1), (x2, y2, h2, z2)} |> List.sortBy(match (_, y, _, _) -> y);
        let ((ax0, ay0, ah0, az0), (ax1, ay1, ah1, az1), (ax2, ay2, ah2, az2)) = match points {
            case top :: mid :: bot :: Nil => (top, mid, bot)
            case _ => unreachable!()
        };
        let x01 = interpolate(ay0, ax0, ay1, ax1);
        let h01 = interpolateSmooth(ay0, ah0, ay1, ah1);
        let z01 = interpolateSmooth(ay0, az0, ay1, az1);
        let x12 = interpolate(ay1, ax1, ay2, ax2);
        let h12 = interpolateSmooth(ay1, ah1, ay2, ah2);
        let z12 = interpolateSmooth(ay1, az1, ay2, az2);
        let x02 = interpolate(ay0, ax0, ay2, ax2);
        let h02 = interpolateSmooth(ay0, ah0, ay2, ah2);
        let z02 = interpolateSmooth(ay0, az0, ay2, az2);
        let x012 = Vector.append(Vector.dropRight(1, x01), x12);
        let h012 = Vector.append(Vector.dropRight(1, h01), h12);
        let z012 = Vector.append(Vector.dropRight(1, z01), z12);
        let m = Vector.length(x012) / 2;
        let (xLeft, xRight, hLeft, hRight, zLeft, zRight) = 
            if (Vector.get(m, x02) < Vector.get(m, x012)) 
                (x02, x012, h02, h012, z02, z012) 
            else 
                (x012, x02, h012, h02, z012, z02);
        foreach (y <- Range.Range(ay0, ay2))
            let xL = Vector.get(y - ay0, xLeft);
            let xR = Vector.get(y - ay0, xRight);
            let hL = Vector.get(y - ay0, hLeft);
            let hR = Vector.get(y - ay0, hRight);
            let zL = Vector.get(y - ay0, zLeft);
            let zR = Vector.get(y - ay0, zRight);
            let hSeg = interpolateSmooth(xL, hL, xR, hR);
            let zSeg = interpolateSmooth(xL, zL, xR, zR);
            
            foreach (x <- Range.Range(xL, xR)) {
                let z = 1.0 / Vector.get(x - xL, zSeg);
                let currentZ = getZAtPoint(x, y, c);
                if (z > currentZ) {
                    let h = Vector.get(x - xL, hSeg);
                    let f = Float64.clampToInt32(min = 0, max = 255, nanValue = -1);
                    let sh = (f(r * h), f(g * h), f(b * h));
                    drawPoint(x, y, sh, c);
                    putZValue(x, y, z, c)
                } else ()
            }
    }

    pub def drawLine(x0: Int32, y0: Int32, x1: Int32, y1: Int32, color: (Int32, Int32, Int32), c: Canvas[r]): Unit \ r = {
        let isSteep = Int32.abs(y1 - y0) > Int32.abs(x1- x0);

        let (ax0, ay0, ax1, ay1) = if (isSteep) (y0, x0, y1, x1) else (x0, y0, x1, y1);
        let (sx0, sy0, sx1, sy1) = if (ax0 > ax1) (ax1, ay1, ax0, ay0) else (ax0, ay0, ax1, ay1);

        let ys = interpolate(sx0, sy0, sx1, sy1);

        foreach (x <- Range.Range(sx0, sx1 + 1)) {
            let y = Vector.get(x - sx0, ys);
            if (isSteep) 
                drawPoint(y, x, color, c)
            else
                drawPoint(x, y, color, c)
        }
    }
        
    pub def drawPoint(x: Int32, y: Int32, color: (Int32, Int32, Int32), c: Canvas[r]): Unit \ r = {
        let xBounded = 0 <= x and x < c->width;
        let yBounded = 0 <= y and y < c->height;
        if (xBounded and yBounded) 
            putValueAt(y, x, packRGB(color), c->raster)
        else ()
    }

    pub def putZValue(x: Int32, y: Int32, z: Float64, c: Canvas[r]): Unit \ r = {
        let xBounded = 0 <= x and x < c->width;
        let yBounded = 0 <= y and y < c->height;
        if (xBounded and yBounded) 
            Array.get(y, c->zBuffer) |> Array.put(z, x)
        else ()
    }

    def getZAtPoint(x: Int32, y: Int32, c: Canvas[r]): Float64 \ r = 
        let xBounded = 0 <= x and x < c->width;
        let yBounded = 0 <= y and y < c->height;
        if (xBounded and yBounded) 
            Array.get(y, c->zBuffer) |> Array.get(x)
        else UpperBound.maxValue()

    pub def interpolate(i0: Int32, d0: Int32, i1: Int32, d1: Int32): Vector[Int32] = {
        if (i0 == i1) {
            Vector#{d0}
        } else region rc {
            let a = (Int32.toFloat64(d1) - Int32.toFloat64(d0)) / (Int32.toFloat64(i1) - Int32.toFloat64(i0));
            let l = MutList.empty(rc);
            def loop(i, d) = {
                if (i <= i1) match Float64.round(d) |> Float64.tryToInt32 {
                    case Some(v) => 
                        MutList.push(v, l);
                        loop(i + 1, d + a)
                    case None => unreachable!()
                } else ()
            };
            loop(i0, Int32.toFloat64(d0));
            MutList.toVector(l)
        }
    }

    pub def interpolateSmooth(i0: Int32, d0: Float64, i1: Int32, d1: Float64): Vector[Float64] = {
        if (i0 == i1) {
            Vector#{d0}
        } else region rc {
            let a = (d1 - d0) / (Int32.toFloat64(i1) - Int32.toFloat64(i0));
            let l = MutList.empty(rc);
            def loop(i, d) = {
                if (i <= i1) {
                    MutList.push(d, l);
                    loop(i + 1, d + a)
                } else ()
            };
            loop(i0, d0);
            MutList.toVector(l)
        }
    }

    def getValueAt(i: Int32, j: Int32, raster: Buffer[v, r]): v \ r = 
        Array.get(i, raster) |> Array.get(j)

    def putValueAt(i: Int32, j: Int32, v: v, raster: Buffer[v, r]): Unit \ r = 
        Array.get(i, raster) |> Array.put(v, j)

    def <<(x: Int32, y: Int32): Int32 = Int32.leftShift(x, y)

    def >>(x: Int32, y: Int32): Int32 = Int32.rightShift(x, y)

    def packRGB(color: (Int32, Int32, Int32)): Int32 = 
        let (r, g, b) = color;
        Int32.bitwiseOr(r << 16, Int32.bitwiseOr(g << 8, b))

    def unpackRGB(color: Int32): (Int32, Int32, Int32) =
        let r = Int32.bitwiseAnd(color >> 16, 0xFF);
        let g = Int32.bitwiseAnd(color >> 8, 0xFF);
        let b = Int32.bitwiseAnd(color, 0xFF);
        (r, g, b)

    def rgbAsFloat(color: (Int32, Int32, Int32)): (Float64, Float64, Float64) =
        let (r, g, b) = color;
        let f = Int32.toFloat64;
        (f(r), f(g), f(b))


}
