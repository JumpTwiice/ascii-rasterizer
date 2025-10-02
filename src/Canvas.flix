struct Canvas[r: Region] {
    width: Int32,
    height: Int32,
    mut raster: Canvas.Raster[r],
    rc: Region[r]
}

mod Canvas {
    @Internal
    pub type alias Raster[r: Region] = Array[Array[Int32, r], r]

    pub def empty(rc: Region[r], w: Int32, h: Int32): Canvas[r] \ r =
        new Canvas @ rc { width = w, height = h, raster = mkRaster(rc, w, h), rc = rc }

    pub def display(c: Canvas[r]): Unit \ Console + r = 
        toString(c) |> Console.println

    pub def flush(c: Canvas[r]): Unit \ r = c->raster = mkRaster(c->rc, c->width, c->height)

    pub def toString(c: Canvas[r]): String \ r =
        ("", shrinkRaster(c)) ||>
            Array.foldLeft(acc -> row -> {
                let rowString = Array.joinWith(brightnessToChar >> ToString.toString, "", row); 
                acc + "\n" + rowString
            })

    def mkRaster(rc: Region[r], w: Int32, h: Int32): Raster[r] \ r = 
        Array.init(rc, _ -> Array.repeat(rc, w, 0), h)
    
    def brightnessToChar(value: Int32): Char = Vector.get(value, Options.colors())

    def shrinkRaster(c: Canvas[r]): Raster[r] \ r =
        let shrinkedRaster = mkRaster(c->rc, c->width, c->height / 2);
        def setShrinkedPixel(i, j) = {
            let top = getPixel(i, j, c->raster);
            let bot = getPixel(i + 1, j, c->raster);
            let avg = (top + bot) / 2;
            drawPixel(i / 2, j, avg, shrinkedRaster)
        };
        foreach (i <- Range.Range(0, c->height - 1))
            foreach (j <- Range.Range(0, c->width - 1)) 
                setShrinkedPixel(i, j);
        shrinkedRaster

    pub def drawTriangle(x0: Int32, y0: Int32, x1: Int32, y1: Int32, x2: Int32, y2: Int32, brightness: Int32, c: Canvas[r]): Unit \ r = {
        drawLine(x0, y0, x1, y1, brightness, c);
        drawLine(x1, y1, x2, y2, brightness, c);
        drawLine(x2, y2, x0, y0, brightness, c)
    }

    pub def drawLine(x0: Int32, y0: Int32, x1: Int32, y1: Int32, brightness: Int32, c: Canvas[r]): Unit \ r = {
        let isSteep = Int32.abs(y1 - y0) > Int32.abs(x1- x0);

        let (ax0, ay0, ax1, ay1) = if (isSteep) (y0, x0, y1, x1) else (x0, y0, x1, y1);
        let (sx0, sy0, sx1, sy1) = if (ax0 > ax1) (ax1, ay1, ax0, ay0) else (ax0, ay0, ax1, ay1);

        let ys = interpolate(sx0, sy0, sx1, sy1);

        foreach (x <- Range.Range(sx0, sx1 + 1)) {
            let y = Vector.get(x - sx0, ys);
            if (isSteep) 
                drawPoint(y, x, brightness, c)
            else
                drawPoint(x, y, brightness, c)
        }
    }

    pub def drawPoint(x: Int32, y: Int32, brightness: Int32, c: Canvas[r]): Unit \ r = {
        let xBounded = 0 < x and x < c->width;
        let yBounded = 0 < y and y < c->height;
        if (xBounded and yBounded) 
            drawPixel(y, x, brightness, c->raster)
        else ()
    }

    pub def interpolate(i0: Int32, d0: Int32, i1: Int32, d1: Int32): Vector[Int32] = {
        if (i0 == i1) {
            Vector#{d0}
        } else {
            let a = (Int32.toFloat64(d1) - Int32.toFloat64(d0)) / (Int32.toFloat64(i1) - Int32.toFloat64(i0));
            def loop(i, d) = {
                if (i <= i1) match Float64.tryToInt32(d) {
                    case Some(v) => v :: loop(i + 1, d + a)
                    case None => unreachable!()
                } else Nil
            };
            loop(i0, Int32.toFloat64(d0)) |> List.toVector
        }
    }

    def getPixel(i: Int32, j: Int32, raster: Raster[r]): Int32 \ r = 
        Array.get(i, raster) |> Array.get(j)

    def drawPixel(i: Int32, j: Int32, b: Int32, raster: Raster[r]): Unit \ r = 
        Array.get(i, raster) |> Array.put(b, j)

}
