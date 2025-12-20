struct Canvas[r: Region] {
    width: Int32,
    height: Int32,
    mut raster: Canvas.Buffer[Int32, r],
    mut zBuffer: Canvas.Buffer[Float64, r],
    rc: Region[r]
}

mod Canvas {
    use Math.{Vec2, Vec3}
    use Math.Vec2.Vec2
    use Math.Vec3.Vec3
    pub type alias Buffer[v: Type, r: Region] = Array[Array[v, r], r]

    pub def dimensions(c: Canvas[r]): (Int32, Int32) = (c->width, c->height)

    pub def empty(rc: Region[r], w: Int32, h: Int32): Canvas[r] \ r =
        new Canvas @ rc { 
            width = w, 
            height = h, 
            raster = mkBuffer(rc, w, h, 0), 
            zBuffer = mkBuffer(rc, w, h, LowerBound.minValue()), 
            rc = rc 
        }

    pub def fillWithTexture(c: Canvas[r], t: Texture): Unit \ r = {
        let (tw, th) = Texture.dimensions(t);
        foreach (
            i <- Range.Range(0, c->height);
            j <- Range.Range(0, c->width)
        ) {
            let texX = (j * tw) / c->width;
            let texY = (i * th) / c->height;
            let (r, g, b, _) = Texture.getTexel(t, texX, texY);
            putValueAt(i, j, packRGB((Int16.toInt32(r), Int16.toInt32(g), Int16.toInt32(b))), c->raster)
        }
    }

    pub def display(c: Canvas[r]): Unit \ Console + r = 
        _getTerminalFrame(c) |> Console.println

    pub def flush(c: Canvas[r]): Unit \ r = {
        foreach (
            i <- Range.Range(0, c->height);
            j <- Range.Range(0, c->width)
        ) {
            Array.get(i, c->raster) |> Array.put(0, j);
            Array.get(i, c->zBuffer) |> Array.put(LowerBound.minValue(), j)
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

    def asciiFromGrayScale(gray: Float64): Char = {
        let idx = Float64.clampToInt32(min = 0, max = 255, nanValue = -1, gray / Options.asciiGradientIncrement());
        Options.asciiChars() |> String.charAt(idx)
    }

    def grayScaleFromRGB(r: Int32, g: Int32, b: Int32): Float64 = 
        (0.299 * Int32.toFloat64(r)) + (0.587 * Int32.toFloat64(g)) + (0.114 * Int32.toFloat64(b))

    def unpackRGBRich(color: Int32): RichString.Color = {
        let (r, g, b) = unpackRGB(color);
        RichString.Color.Rgb(r, g, b)
    }
        
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

    def edgeFunction(ax: Float64, ay: Float64, bx: Float64, by: Float64, px: Float64, py: Float64): Float64 =
        (px - ax) * (by - ay) - (py - ay) * (bx - ax)

    def isTopLeftEdge(ax: Float64, ay: Float64, bx: Float64, by: Float64): Bool =
        (ay < by) or (ay == by and ax > bx)

    def ceilToInt32(v: Float64): Int32 = match Float64.ceil(v) |> Float64.tryToInt32 {
        case Some(i) => i
        case None => unreachable!()
    }

    def floorToInt32(v: Float64): Int32 = match Float64.floor(v) |> Float64.tryToInt32 {
        case Some(i) => i
        case None => unreachable!()
    }

    def boundingBox(
        x0: Float64, y0: Float64, 
        x1: Float64, y1: Float64, 
        x2: Float64, y2: Float64, 
        c: Canvas[r]
    ): (Int32, Int32, Int32, Int32) = {
        let minXf = Float64.min(x0, Float64.min(x1, x2));
        let maxXf = Float64.max(x0, Float64.max(x1, x2));
        let minYf = Float64.min(y0, Float64.min(y1, y2));
        let maxYf = Float64.max(y0, Float64.max(y1, y2));

        let minX0 = ceilToInt32(minXf - 0.5);
        let maxX0 = floorToInt32(maxXf - 0.5);
        let minY0 = ceilToInt32(minYf - 0.5);
        let maxY0 = floorToInt32(maxYf - 0.5);

        let minX = Int32.max(0, minX0);
        let maxX = Int32.min(c->width - 1, maxX0);
        let minY = Int32.max(0, minY0);
        let maxY = Int32.min(c->height - 1, maxY0);
        (minX, maxX, minY, maxY)
    }

    def barycentricEdgeValues(
        px: Float64, py: Float64, 
        x0: Float64, y0: Float64, 
        x1: Float64, y1: Float64, 
        x2: Float64, y2: Float64
    ): (Float64, Float64, Float64) = {
        let w0e = edgeFunction(x1, y1, x2, y2, px, py);
        let w1e = edgeFunction(x2, y2, x0, y0, px, py);
        let w2e = edgeFunction(x0, y0, x1, y1, px, py);
        (w0e, w1e, w2e)
    }

    def insideTriangle(w0e: Float64, w1e: Float64, w2e: Float64, tl01: Bool, tl12: Bool, tl20: Bool): Bool =
        (w0e > 0.0 or (w0e == 0.0 and tl12)) and
        (w1e > 0.0 or (w1e == 0.0 and tl20)) and
        (w2e > 0.0 or (w2e == 0.0 and tl01))

    def barycentricWeights(w0e: Float64, w1e: Float64, w2e: Float64, invArea: Float64): (Float64, Float64, Float64) =
        (w0e * invArea, w1e * invArea, w2e * invArea)

    def perspectiveHeight(
        w0: Float64, w1: Float64, w2: Float64, 
        h0: Float64, h1: Float64, h2: Float64, 
        invZ0: Float64, invZ1: Float64, invZ2: Float64
    ): (Float64, Float64) = {
        let invZ = w0 * invZ0 + w1 * invZ1 + w2 * invZ2;
        let hOverZ = w0 * (h0 * invZ0) + w1 * (h1 * invZ1) + w2 * (h2 * invZ2);
        let h = if (invZ == 0.0) 0.0 else (hOverZ / invZ);
        (invZ, h)
    }

    def shadeColor(h: Float64, rf: Float64, gf: Float64, bf: Float64): (Int32, Int32, Int32) = {
        let clamp255 = Float64.clampToInt32(min = 0, max = 255, nanValue = 0);
        (clamp255(rf * h), clamp255(gf * h), clamp255(bf * h))
    }

    pub def drawShadedTriangle(
        x0: Float64, y0: Float64,
        x1: Float64, y1: Float64,
        x2: Float64, y2: Float64,
        h0: Float64, h1: Float64, h2: Float64,
        invZ0: Float64, invZ1: Float64, invZ2: Float64,
        color: (Int32, Int32, Int32),
        c: Canvas[r]
    ): Unit \ r = {
        let area = edgeFunction(x0, y0, x1, y1, x2, y2);
        if (area != 0.0) {
            let invArea = 1.0 / area;
            let (minX, maxX, minY, maxY) = boundingBox(x0, y0, x1, y1, x2, y2, c);
            let tl01 = isTopLeftEdge(x0, y0, x1, y1);
            let tl12 = isTopLeftEdge(x1, y1, x2, y2);
            let tl20 = isTopLeftEdge(x2, y2, x0, y0);

            let (rf, gf, bf) = rgbAsFloat(color);

            foreach (y <- Range.Range(minY, maxY + 1)) {
                let py = Int32.toFloat64(y) + 0.5;

                foreach (x <- Range.Range(minX, maxX + 1)) {
                    let px = Int32.toFloat64(x) + 0.5;

                    let (w0e, w1e, w2e) = barycentricEdgeValues(px, py, x0, y0, x1, y1, x2, y2);

                    if (insideTriangle(w0e, w1e, w2e, tl01, tl12, tl20)) {
                        let (w0, w1, w2) = barycentricWeights(w0e, w1e, w2e, invArea);
                        let (invZ, h) = perspectiveHeight(w0, w1, w2, h0, h1, h2, invZ0, invZ1, invZ2);

                        let currentInvZ = getZAtPoint(x, y, c);
                        if (invZ >= currentInvZ) {
                            let sh = shadeColor(h, rf, gf, bf);
                            drawPoint(x, y, sh, c);
                            putZValue(x, y, invZ, c)
                        }
                    } 
                }
            }
        }
    }

    pub def drawPhongTriangle(
        x0: Float64, y0: Float64,
        x1: Float64, y1: Float64,
        x2: Float64, y2: Float64,
        p0: Vec3, p1: Vec3, p2: Vec3,
        n0: Vec3, n1: Vec3, n2: Vec3,
        uv0: Vec2, uv1: Vec2, uv2: Vec2,
        invZ0: Float64, invZ1: Float64, invZ2: Float64,
        specular: Float64,
        lights: List[Light],
        camPos: Vec3,
        material: Material,
        c: Canvas[r]
    ): Unit \ r = {
        let area = edgeFunction(x0, y0, x1, y1, x2, y2);
        if (area != 0.0) {
            let invArea = 1.0 / area;
            let (minX, maxX, minY, maxY) = boundingBox(x0, y0, x1, y1, x2, y2, c);
            let tl01 = isTopLeftEdge(x0, y0, x1, y1);
            let tl12 = isTopLeftEdge(x1, y1, x2, y2);
            let tl20 = isTopLeftEdge(x2, y2, x0, y0);

            foreach (y <- Range.Range(minY, maxY + 1)) {
                let py = Int32.toFloat64(y) + 0.5;

                foreach (x <- Range.Range(minX, maxX + 1)) {
                    let px = Int32.toFloat64(x) + 0.5;

                    let (w0e, w1e, w2e) = barycentricEdgeValues(px, py, x0, y0, x1, y1, x2, y2);

                    if (insideTriangle(w0e, w1e, w2e, tl01, tl12, tl20)) {
                        let (w0, w1, w2) = barycentricWeights(w0e, w1e, w2e, invArea);
                        let invZ = w0 * invZ0 + w1 * invZ1 + w2 * invZ2;

                        let currentInvZ = getZAtPoint(x, y, c);
                        if (invZ >= currentInvZ and invZ != 0.0) {
                            let w0p = w0 * invZ0;
                            let w1p = w1 * invZ1;
                            let w2p = w2 * invZ2;
                            let sumW = w0p + w1p + w2p;

                            let (bw0, bw1, bw2) = if (sumW == 0.0) (0.0, 0.0, 0.0) else (w0p / sumW, w1p / sumW, w2p / sumW);

                            let pos = Vec3.scale(p0, bw0) + Vec3.scale(p1, bw1) + Vec3.scale(p2, bw2);
                            let normal = Vec3.normalize(Vec3.scale(n0, bw0) + Vec3.scale(n1, bw1) + Vec3.scale(n2, bw2));

                            let uv = Vec2.scale(uv0, bw0) + Vec2.scale(uv1, bw1) + Vec2.scale(uv2, bw2);
                            let Vec2(u, v) = uv;
                            let (rf, gf, bf) = rgbAsFloat(Material.getColorAt(material, u, v));

                            let h = Light.lightingForPoint(pos, normal, camPos - pos, specular, lights);
                            let sh = shadeColor(h, rf, gf, bf);
                            drawPoint(x, y, sh, c);
                            putZValue(x, y, invZ, c)
                        }
                    } 
                }
            }
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
                if (i <= i1) match Float64.floor(d) |> Float64.tryToInt32 {
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
