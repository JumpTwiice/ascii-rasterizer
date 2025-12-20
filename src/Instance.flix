struct Instance[r] {
    model: Model,
    baseSphere: (Math.Vec3, Float64),
    mut position: Math.Vec3,
    mut scale: Math.Vec3,
    mut rotation: Math.Vec3
}

mod Instance {
    use Math.{Mat4x4, Mat3x4, Vec3, Vec4}
    use Math.Vec4.Vec4
    use Math.Vec3.Vec3

    pub def fromModel(
        rc: Region[r], 
        model: Model, 
        t: { position = Vec3 },
        r: { rotation = Vec3 },
        s: { scale = Vec3 }
    ): Instance[r] \ r =  new Instance @ rc { 
        model = model, 
        baseSphere = Model.boundingSphere(model),
        scale = s#scale, 
        rotation = r#rotation, 
        position = t#position 
    } 

    pub def render(canvas: Canvas[r0], camera: Camera[r1], i: Instance[r2]): Unit \ r0 + r1 + r2 = {
        let Model.Model(_, vertices, triangles, specular) = i->model;
        let model = modelMatrix(i);
        let view = Camera.viewMatrix(camera);
        let camMatrix = view * model;
        let camVertices = Vector.map(Mat4x4.mulVec3(camMatrix), vertices);
        let transformNormal = n -> {
            let Vec3(nx, ny, nz) = n;
            let v = Vec4.Vec4(nx, ny, nz, 0.0);
            Mat4x4.mulVec4(camMatrix, v) |> Vec4.toVec3 |> Vec3.normalize
        };

        let (centerWorld, radius) = worldSphere(model, i);
        let centerCam = Mat4x4.mulVec3(view, centerWorld);
        let boundingSphereCam = (centerCam, radius);

        let planes = Camera.frustumPlanes(camera);

        def clipAll(ps, tris) = match ps {
            case Nil => tris
            case p :: rest => clipAll(rest, clipAgainstPlane(p, tris, boundingSphereCam))
        };

        let camLights = List.map(Light.toCameraSpace(view), Options.lights());
        let camPos = Vec3.zero();

        foreach ((p1, p2, p3, n1, n2, n3, drawColor) <- triangles) {
            let a = Vector.get(p1, camVertices);
            let b = Vector.get(p2, camVertices);
            let c = Vector.get(p3, camVertices);
            let normal = (c - a) * (b - a);
            let tn1 = transformNormal(n1);
            let tn2 = transformNormal(n2);
            let tn3 = transformNormal(n3);
            let (h0, h1, h2) = match Options.shadingMode() {
                case Shading.Flat => {
                    let centroid = Vec3.scale(a + b + c, 0.333);
                    let h = Light.lightingForPoint(centroid, normal, camPos - centroid, specular, camLights);
                    (h, h, h)
                }
                case Shading.Gouraud => {
                    let h0 = Light.lightingForPoint(a, tn1, camPos - a, specular, camLights);
                    let h1 = Light.lightingForPoint(b, tn2, camPos - b, specular, camLights);
                    let h2 = Light.lightingForPoint(c, tn3, camPos - c, specular, camLights);
                    (h0, h1, h2)
                }
                case _ => (0.0, 0.0, 0.0)
            };
            let viewDir = camPos - a;
            let isBackface = Vec3.dot(normal, viewDir) <= 0.0;
            if (not isBackface) {
                let clipped = clipAll(planes, Vector#{(a, b, c)});
                foreach ((c0, c1, c2) <- clipped) {
                    let Vec3(_, _, z0) = c0;
                    let Vec3(_, _, z1) = c1;
                    let Vec3(_, _, z2) = c2;
                    let isValid = z -> not Float64.isNan(z) and z > 0.0;
                    if (isValid(z0) and isValid(z1) and isValid(z2)) {
                        let (x0, y0) = projectVertex(camera, Vec4.fromVec3(c0));
                        let (x1, y1) = projectVertex(camera, Vec4.fromVec3(c1));
                        let (x2, y2) = projectVertex(camera, Vec4.fromVec3(c2));
                        
                        match Options.shadingMode() {
                            case Shading.Flat => 
                                Canvas.drawShadedTriangle(
                                    x0, y0, x1, y1, x2, y2,
                                    h0, h1, h2,
                                    1.0 / z0, 1.0 / z1, 1.0 / z2,
                                    drawColor, canvas
                                )
                            case Shading.Gouraud => 
                                Canvas.drawShadedTriangle(
                                    x0, y0, x1, y1, x2, y2,
                                    h0, h1, h2,
                                    1.0 / z0, 1.0 / z1, 1.0 / z2,
                                    drawColor, canvas
                                )
                            case Shading.Phong => 
                                Canvas.drawPhongTriangle(
                                    x0, y0, x1, y1, x2, y2,
                                    a, b, c,
                                    tn1, tn2, tn3,
                                    1.0 / z0, 1.0 / z1, 1.0 / z2,
                                    specular,
                                    camLights,
                                    camPos,
                                    drawColor,
                                    canvas
                                )
                        }                            
                    }
                }
            }
        }
    }

    def clipAgainstPlane(
        plane: (Vec3, Float64), 
        triangles: Vector[(Vec3, Vec3, Vec3)],
        boundingSphere: (Vec3, Float64)
    ): Vector[(Vec3, Vec3, Vec3)] = {
        let (center, radius) = boundingSphere;
        let d = signedDistance(plane, center);
        if (d > radius) {
            triangles
        } else if (d < -radius) {
            Vector.empty()
        } else {
            clipTrianglesAgainstPlane(plane, triangles)
        }
    }

    def modelMatrix(i: Instance[r]): Mat4x4 \ r = {
        let Vec3(s1, s2, s3) = i->scale;
        let Vec3(r1, r2, r3) = i->rotation;
        let Vec3(t1, t2, t3) = i->position; 
        Mat4x4.translation(t1, t2, t3) * Mat4x4.rotationZ(r3) * Mat4x4.rotationY(r2) * Mat4x4.rotationX(r1) * Mat4x4.scale(s1, s2, s3)
    }

    pub def translate(translation: Vec3, i: Instance[r]): Unit \ r = 
        i->position = i->position + translation

    pub def rotateX(angle: Float64, i: Instance[r]): Unit \ r = 
        i->rotation = i->rotation + Vec3.Vec3(angle, 0.0, 0.0)

    pub def rotateY(angle: Float64, i: Instance[r]): Unit \ r = 
        i->rotation = i->rotation + Vec3.Vec3(0.0, angle, 0.0)
    
    pub def rotateZ(angle: Float64, i: Instance[r]): Unit \ r = 
        i->rotation = i->rotation + Vec3.Vec3(0.0, 0.0, angle)
    
    pub def scale(scale: Float64, i: Instance[r]): Unit \ r =
        i->scale = Vec3.scale(i->scale, scale)

    pub def getPosition(i: Instance[r]): Vec3 \ r = i->position

    def worldSphere(modelMatrix: Mat4x4, i: Instance[r]): (Vec3, Float64) \ r = {
        let Vec3(sx, sy, sz) = i->scale;
        let s = Float64.abs(sx) `Float64.max` Float64.abs(sy) `Float64.max` Float64.abs(sz);
        let (cModel, rModel) = i->baseSphere;
        let cWorld = Mat4x4.mulVec3(modelMatrix, cModel);
        (cWorld, rModel * s)
    }

    def projectVertex(cam: Camera[r], v: Vec4): (Float64, Float64) \ r = {
        let (cw, ch) = Options.dimensions();
        let (vw, vh) = Options.viewportDimensions();
        let f = Int32.toFloat64(vh) / (2.0 * Math.tan(Camera.fovY(cam) / 2.0));
        let Vec3(x, y, z) = Mat3x4.mulVec4(viewportToCanvasMatrix(cw, ch, vw, vh, f), v);
        (x / z + Int32.toFloat64(cw / 2), Int32.toFloat64(ch / 2) - y / z)
    }

    def clipTrianglesAgainstPlane(plane: (Vec3, Float64), triangles: Vector[(Vec3, Vec3, Vec3)]): Vector[(Vec3, Vec3, Vec3)] = {
        triangles |> Vector.map(clipTriangle(plane)) |> Vector.flatten
    }

    def clipTriangle(plane: (Vec3, Float64), triangle: (Vec3, Vec3, Vec3)): Vector[(Vec3, Vec3, Vec3)] = {
        let (v0, v1, v2) = triangle;
        let d0 = signedDistance(plane, v0);
        let d1 = signedDistance(plane, v1);
        let d2 = signedDistance(plane, v2);

        let intersect = va -> vb -> da -> db -> {
            let denom = da - db;
            let t = if (denom == 0.0) 0.0 else da / denom;
            va + Vec3.scale(vb - va, t)
        };
        let onlyOnePositive = () -> 
            (d0 > 0.0 and d1 <= 0.0 and d2 <= 0.0) or
            (d0 <= 0.0 and d1 > 0.0 and d2 <= 0.0) or
            (d0 <= 0.0 and d1 <= 0.0 and d2 > 0.0);
            
        if (d0 > 0.0 and d1 > 0.0 and d2 > 0.0) {
            Vector#{triangle}
        } else if (d0 < 0.0 and d1 < 0.0 and d2 < 0.0) {
            Vector.empty()
        } else if (onlyOnePositive()) {
            if (d0 > 0.0) {
                let i1 = intersect(v0, v1, d0, d1);
                let i2 = intersect(v0, v2, d0, d2);
                Vector#{(v0, i1, i2)}
            } else if (d1 > 0.0) {
                let i0 = intersect(v1, v0, d1, d0);
                let i2 = intersect(v1, v2, d1, d2);
                Vector#{(v1, i2, i0)}
            } else {
                let i0 = intersect(v2, v0, d2, d0);
                let i1 = intersect(v2, v1, d2, d1);
                Vector#{(v2, i0, i1)}
            }
        } else {
            if (d0 <= 0.0) {
                let i1 = intersect(v1, v0, d1, d0);
                let i2 = intersect(v2, v0, d2, d0);
                Vector#{(v1, v2, i2), (v1, i2, i1)}
            } else if (d1 <= 0.0) {
                let i0 = intersect(v0, v1, d0, d1);
                let i2 = intersect(v2, v1, d2, d1);
                Vector#{(v2, v0, i0), (v2, i0, i2)}
            } else {
                let i0 = intersect(v0, v2, d0, d2);
                let i1 = intersect(v1, v2, d1, d2);
                Vector#{(v0, v1, i1), (v0, i1, i0)}
            }
        }
    }

    def signedDistance(plane: (Vec3, Float64), v: Vec3): Float64 = {
        let (Vec3(nx, ny, nz), offset) = plane;
        let Vec3(vx, vy, vz) = v;
        vx*nx + vy*ny + vz*nz + offset
    }

    def viewportToCanvasMatrix(cw: Int32, ch: Int32, vw: Int32, vh: Int32, f: Float64): Mat3x4 = {
        let cwf = Int32.toFloat64(cw);
        let chf = Int32.toFloat64(ch);
        let vwf = Int32.toFloat64(vw);
        let vhf = Int32.toFloat64(vh);
        Mat3x4.Mat3x4(Vector#{
            f * cwf / vwf, 0.0, 0.0, 0.0,
            0.0, f * chf / vhf, 0.0, 0.0,
            0.0, 0.0, 1.0, 0.0
        })
    }

}
