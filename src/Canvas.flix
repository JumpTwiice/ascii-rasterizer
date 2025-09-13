struct Canvas[r: Region] {
    width: Int32,
    height: Int32,
    mut image: Array[Array[Char, r], r],
    rc: Region[r]
}

mod Canvas {

    pub def empty(rc: Region[r], w: Int32, h: Int32): Canvas[r] \ r =
        new Canvas @ rc { width = w, height = h, image = mkImage(rc, w, h), rc = rc }

    pub def flush(c: Canvas[r]): Unit \ r =
        c->image = mkImage(c->rc, c->width, c->height)

    pub def shrink(rc: Region[r0], c: Canvas[r1]): Canvas[r0] \ r0 + r1 =
        let height = c->height / 2;
        let image = mkImage(rc, c->width, height);
        def setShrinkedPixel(i, j) = {
            let top = Array.get(i, c->image) |> Array.get(j);
            let bot = Array.get(i + 1, c->image) |> Array.get(j);
            let dstRow = Array.get(i / 2, image);
            match (top, bot) {
                case ('*', ' ') => Array.put('\'', j, dstRow)
                case ('*', '*') => Array.put('*', j, dstRow)
                case (' ', '*') => Array.put('.', j, dstRow)
                case (' ', ' ') => Array.put(' ', j, dstRow)
                case _ => ???
            }
        };
        foreach (i <- Range.Range(0, c->height - 1))
            foreach (j <- Range.Range(0, c->width - 1)) 
                setShrinkedPixel(i, j);
        new Canvas @ rc { width = c->width, height = height, image = image, rc = rc }

    def mkImage(rc: Region[r], w: Int32, h: Int32): Array[Array[Char, r], r] \ r = 
        Array.init(rc, _ -> Array.repeat(rc, w, ' '), h)

    pub def toString(c: Canvas[r]): String \ r =
        Array.foldLeft(acc -> row -> acc + "\n" + Array.join("", row),"", c->image)

    pub def drawTriangle(x0: Int32, y0: Int32, x1: Int32, y1: Int32, x2: Int32, y2: Int32, c: Canvas[r]): Unit \ r = {
        drawLine(x0, y0, x1, y1, c);
        drawLine(x1, y1, x2, y2, c);
        drawLine(x2, y2, x0, y0, c)
    }

    pub def drawLine(x0: Int32, y0: Int32, x1: Int32, y1: Int32, c: Canvas[r]): Unit \ r = {
        if (Int32.abs(y1 - y0) < Int32.abs(x1 - x0)) {
            if (x0 > x1) 
                drawLineLow(x1, y1, x0, y0, c)
            else
                drawLineLow(x0, y0, x1, y1, c)
        } else {
            if (y0 > y1) 
                drawLineHigh(x1, y1, x0, y0, c)
            else
                drawLineHigh(x0, y0, x1, y1, c)
        }
    }

    pub def fill(x: Int32, y: Int32, char: Char, c: Canvas[r]): Unit \ r = {
        let xBounded = 0 < x and x < c->width;
        let yBounded = 0 < y and y < c->height;
        if (xBounded and yBounded) 
            Array.get(y, c->image) |> Array.put(char, x)
        else ()
    }

    def drawLineHigh(x0: Int32, y0: Int32, x1: Int32, y1: Int32, c: Canvas[r]): Unit \ r = {
        let (dx, dy, xi) = {
            let dx = x1 - x0;
            let dy = y1 - y0;
            if (dx < 0) (-dx, dy, -1) else (dx, dy, 1)
        };
        def f(x, y, d) = {
            if (y <= y1) {
                fill(x, y, '*', c);
                if (d > 0) 
                    f(x + xi, y + 1, d + 2 * (dx - dy))
                else 
                    f(x, y + 1, d + 2 * dx)
            } else ()
        };
        f(x0, y0, 2 * dx - dy)
    }

    def drawLineLow(x0: Int32, y0: Int32, x1: Int32, y1: Int32, c: Canvas[r]): Unit \ r = {
        let (dx, dy, yi) = {
            let dx = x1 - x0;
            let dy = y1 - y0;
            if (dy < 0) (dx, -dy, -1) else (dx, dy, 1)
        };
        def f(x, y, d) = {
            if (x <= x1) {
                fill(x, y, '*', c);
                if (d > 0) 
                    f(x + 1, y + yi, d + 2 * (dy - dx))
                else 
                    f(x + 1, y, d + 2 * dy)
            } else ()
        };
        f(x0, y0, 2 * dy - dx)
    }

}
