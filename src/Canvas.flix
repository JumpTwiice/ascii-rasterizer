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
        new Canvas @ rc { width = w, height = h, raster = mkBuffer(rc, w, h), zBuffer = mkBuffer(rc, w, h), rc = rc }

    pub def display(c: Canvas[r]): Unit \ Console + r = 
        _getTerminalFrameSimple(c) |> Console.println

    pub def flush(c: Canvas[r]): Unit \ r = c->raster = mkBuffer(c->rc, c->width, c->height)

    def mkBuffer(rc: Region[r], w: Int32, h: Int32): Buffer[v, r] \ r  = 
        Array.init(rc, _ -> Array.empty(rc, w), h)
    
    def _getTerminalFrame(c: Canvas[r]): String \ r = region rc {
        let h2 = (c->height + 1) / 2;
        let width = c->width;
        let frameRows = Array.empty(rc, h2);

        foreach (i2 <- Range.Range(0, h2)) {
            let yTop = i2 * 2;
            let yBot = yTop + 1;
            
            let sTop = getPixel(yTop, 0, c->raster);
            let sBot = getPixel(yBot, 0, c->raster);

            let (row, seg, _, _) = (("", "", sTop, sBot), Vector.range(1, c->width))
                ||> Vector.foldLeft(match (accRow, currSeg, currTop, currBot) -> x -> {
                    let top = getPixel(yTop, x, c->raster);
                    let bot = getPixel(yBot, x, c->raster);
                    if (top != currTop or bot != currBot) {
                        let flushedSeg = if (currSeg != "") {
                            RichString.text(currSeg)
                                |> RichString.withColor(unpackRGBRich(currTop))
                                |> RichString.withBgColor(unpackRGBRich(currBot))
                                |> RichString.toAnsiString
                        } else "";
                        (accRow + flushedSeg, "▀", top, bot)
                    } else {
                        (accRow, currSeg + "▀", currTop, currBot)
                    }
                });
            let rest = if (seg != "") {
                let top = getPixel(yTop, width - 1, c->raster);
                let bot = getPixel(yBot, width - 1, c->raster);
                RichString.text(seg)
                    |> RichString.withColor(unpackRGBRich(top))
                    |> RichString.withBgColor(unpackRGBRich(bot))
                    |> RichString.toAnsiString
            } else "";
            Array.put(row + rest, i2, frameRows)
        };
        Array.join("\n", frameRows)
    }

    def _getTerminalFrameSimple(c: Canvas[r]): String \ r = region rc {
        let h2 = (c->height + 1) / 2;
        let width = c->width;
        let frameRows = Array.empty(rc, h2);

        foreach (i2 <- Range.Range(0, h2)) {
            let yTop = i2 * 2;
            let yBot = yTop + 1;

            let rowVec =
                Vector.init(x -> {
                    let top = getPixel(yTop, x, c->raster);
                    let bot = getPixel(yBot, x, c->raster);
                    RichString.text("▀")
                        |> RichString.withColor(unpackRGBRich(top))
                        |> RichString.withBgColor(unpackRGBRich(bot))
                        |> RichString.toAnsiString
                }, width);

            Array.put(Vector.join("", rowVec), i2, frameRows)
        };

        Array.join("\n", frameRows)
    }

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
        foreach (y <- Range.Range(y0, y2 + 1))
            foreach (x <- Range.Range(Vector.get(y - y0, xLeft), Vector.get(y - y0, xRight) + 1))
                drawPoint(x, y, color, c)
    }

    pub def drawShadedTriangle(
        x0: Int32, y0: Int32, 
        x1: Int32, y1: Int32, 
        x2: Int32, y2: Int32, 
        h0: Int32, h1: Int32, h2: Int32,
        color: (Int32, Int32, Int32), 
        c: Canvas[r]
    ): Unit \ r = {
        let (r, g, b) = color;
        let points = List#{(x0, y0), (x1, y1), (x2, y2)} |> List.sortBy(snd);
        let ((ax0, ay0), (ax1, ay1), (ax2, ay2)) = match points {
            case top :: mid :: bot :: Nil => (top, mid, bot)
            case _ => unreachable!()
        };
        let x01 = interpolate(ay0, ax0, ay1, ax1);
        let h01 = interpolate(ay0, h0, ay1, h1);
        let x12 = interpolate(ay1, ax1, ay2, ax2);
        let h12 = interpolate(ay1, h1, ay2, h2);
        let x02 = interpolate(ay0, ax0, ay2, ax2);
        let h02 = interpolate(ay0, h0, ay2, h2);
        let x012 = Vector.append(Vector.dropRight(1, x01), x12);
        let h012 = Vector.append(Vector.dropRight(1, h01), h12);
        let m = Vector.length(x012) / 2;
        let (xLeft, xRight, hLeft, hRight) = 
            if (Vector.get(m, x02) < Vector.get(m, x012)) 
                (x02, x012, h02, h012) 
            else 
                (x012, x02, h012, h02);
        foreach (y <- Range.Range(y0, y2 + 1))
            let xL = Vector.get(y - y0, xLeft);
            let xR = Vector.get(y - y0, xRight);
            let hL = Vector.get(y - y0, hLeft);
            let hR = Vector.get(y - y0, hRight);
            let hSeg = interpolate(xL, hL, xR, hR);
            
            foreach (x <- Range.Range(xL, xR + 1)) {
                let h = Vector.get(x - xL, hSeg);
                let sh = (r * h / 100, g * h / 100, b * h / 100);
                drawPoint(x, y, sh, c)
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
            drawPixel(y, x, packRGB(color), c->raster)
        else ()
    }

    pub def interpolate(i0: Int32, d0: Int32, i1: Int32, d1: Int32): Vector[Int32] = {
        if (i0 == i1) {
            Vector#{d0}
        } else region rc {
            let a = (Int32.toFloat64(d1) - Int32.toFloat64(d0)) / (Int32.toFloat64(i1) - Int32.toFloat64(i0));
            let l = MutList.empty(rc);
            def loop(i, d) = {
                if (i <= i1) match Float64.tryToInt32(d) {
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

    def getPixel(i: Int32, j: Int32, raster: Buffer[v, r]): v \ r = 
        Array.get(i, raster) |> Array.get(j)

    def drawPixel(i: Int32, j: Int32, v: v, raster: Buffer[v, r]): Unit \ r = 
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

}
