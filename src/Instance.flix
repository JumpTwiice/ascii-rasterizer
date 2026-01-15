struct Instance[r] {
    model: Model,
    baseSphere: (Math.Vec3, Float64),
    mut position: Math.Vec3,
    mut scale: Math.Vec3,
    mut rotation: Math.Vec3
}

mod Instance {
    use Math.{Mat4x4, Vec2, Vec3, Vec4}
    use Math.Vec4.Vec4
    use Math.Vec3.Vec3
    use Math.Vec2.Vec2

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

    pub def render(canvas: Canvas[r0], camera: Camera[r1], lights: List[Light], i: Instance[r2]): Unit \ r0 + r1 + r2 = {
        let Model.Model(_, vertices, triangles, specular) = i->model;
        let model = modelMatrix(i);
        let view = Camera.viewMatrix(camera);
        let camMatrix = view * model;
        let camToCanvas = cameraToCanvasProjectionMatrix(camera);
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

        let camLights = List.map(Light.toCameraSpace(view), lights);
        let camPos = Vec3.zero();

        foreach ((p1, p2, p3, n1, n2, n3, tx1, tx2, tx3, material) <- triangles) {
            let a = Vector.get(p1, camVertices);
            let b = Vector.get(p2, camVertices);
            let c = Vector.get(p3, camVertices);
            let tn1 = transformNormal(n1);
            let tn2 = transformNormal(n2);
            let tn3 = transformNormal(n3);
            let clipped = clipAll(planes, Vector#{((a, tn1, tx1), (b, tn2, tx2), (c, tn3, tx3))});
            foreach (((c0, cn0, cuv0), (c1, cn1, cuv1), (c2, cn2, cuv2)) <- clipped) {
                let normal = (c2 - c0) * (c1 - c0);
                let viewDir = camPos - c0;
                let isBackface = Vec3.dot(normal, viewDir) <= 0.0;
                if (not isBackface) {
                    let Vec3(_, _, z0) = c0;
                    let Vec3(_, _, z1) = c1;
                    let Vec3(_, _, z2) = c2;
                    let isValid = z -> not Float64.isNan(z) and z > 0.0;
                    if (isValid(z0) and isValid(z1) and isValid(z2)) {
                        let (x0, y0) = projectVertex(camToCanvas, c0);
                        let (x1, y1) = projectVertex(camToCanvas, c1);
                        let (x2, y2) = projectVertex(camToCanvas, c2);
                        
                        match Options.shadingMode() {
                            case Shading.Flat => {
                                let centroid = Vec3.scale(c0 + c1 + c2, 0.333);
                                let h = Light.lightingForPoint(centroid, normal, camPos - centroid, specular, camLights);
                                Canvas.drawShadedTriangle(
                                    x0, y0, x1, y1, x2, y2,
                                    h, h, h,
                                    1.0 / z0, 1.0 / z1, 1.0 / z2,
                                    ???, canvas
                                )
                            }
                            case Shading.Gouraud => {
                                let h0 = Light.lightingForPoint(c0, cn0, camPos - c0, specular, camLights);
                                let h1 = Light.lightingForPoint(c1, cn1, camPos - c1, specular, camLights);
                                let h2 = Light.lightingForPoint(c2, cn2, camPos - c2, specular, camLights);
                                Canvas.drawShadedTriangle(
                                    x0, y0, x1, y1, x2, y2,
                                    h0, h1, h2,
                                    1.0 / z0, 1.0 / z1, 1.0 / z2,
                                    ???, canvas
                                )
                            }
                            case Shading.Phong => {
                                Canvas.drawPhongTriangle(
                                    x0, y0, x1, y1, x2, y2,
                                    c0, c1, c2,
                                    cn0, cn1, cn2,
                                    cuv0, cuv1, cuv2,
                                    1.0 / z0, 1.0 / z1, 1.0 / z2,
                                    specular,
                                    camLights,
                                    camPos,
                                    material,
                                    canvas
                                )
                            }
                        }                            
                    }
                }
            }
        }
    }

    def clipAgainstPlane(
        plane: (Vec3, Float64), 
        triangles: Vector[((Vec3, Vec3, Vec2), (Vec3, Vec3, Vec2), (Vec3, Vec3, Vec2))],
        boundingSphere: (Vec3, Float64)
    ): Vector[((Vec3, Vec3, Vec2), (Vec3, Vec3, Vec2), (Vec3, Vec3, Vec2))] = {
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

    def cameraToCanvasProjectionMatrix(cam: Camera[r]): Mat4x4 \ r = {
        let (cw, ch) = Options.dimensions();
        let (vw, vh) = Options.viewportDimensions();
        let f = Int32.toFloat64(vh) / (2.0 * Math.tan(Camera.fovY(cam) / 2.0));
        let cwf = Int32.toFloat64(cw);
        let chf = Int32.toFloat64(ch);
        let vwf = Int32.toFloat64(vw);
        let vhf = Int32.toFloat64(vh);
        let cwHalf = Int32.toFloat64(cw / 2);
        let chHalf = Int32.toFloat64(ch / 2);
        let sx = f * cwf / vwf;
        let sy = f * chf / vhf;
        Mat4x4.Mat4x4(Vector#{
            sx, 0.0, cwHalf, 0.0,
            0.0, -sy, chHalf, 0.0,
            0.0, 0.0, 1.0, 0.0,
            0.0, 0.0, 1.0, 0.0
        })
    }

    def projectVertex(camToCanvas: Mat4x4, v: Vec3): (Float64, Float64) = {
        let Vec4(x, y, _, w) = Mat4x4.mulVec4(camToCanvas, Vec4.fromVec3(v));
        (x / w, y / w)
    }

    def clipTrianglesAgainstPlane(plane: (Vec3, Float64), triangles: Vector[((Vec3, Vec3, Vec2), (Vec3, Vec3, Vec2), (Vec3, Vec3, Vec2))]): Vector[((Vec3, Vec3, Vec2), (Vec3, Vec3, Vec2), (Vec3, Vec3, Vec2))] = {
        triangles |> Vector.map(clipTriangle(plane)) |> Vector.flatten
    }

    def clipTriangle(plane: (Vec3, Float64), triangle: ((Vec3, Vec3, Vec2), (Vec3, Vec3, Vec2), (Vec3, Vec3, Vec2))): Vector[((Vec3, Vec3, Vec2), (Vec3, Vec3, Vec2), (Vec3, Vec3, Vec2))] = {
        let ((p0, n0, tx0), (p1, n1, tx1), (p2, n2, tx2)) = triangle;
        let d0 = signedDistance(plane, p0);
        let d1 = signedDistance(plane, p1);
        let d2 = signedDistance(plane, p2);

        let intersect = va -> vb -> da -> db -> {
            let denom = da - db;
            let t = if (denom == 0.0) 0.0 else da / denom;
            let (pa, na, uva) = va;
            let (pb, nb, uvb) = vb;
            let pos = pa + Vec3.scale(pb - pa, t);
            let nor = Vec3.normalize(na + Vec3.scale(nb - na, t));
            let uv = Vec2.scale(uva, 1.0 - t) + Vec2.scale(uvb, t);
            (pos, nor, uv)
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
                let i1 = intersect((p0, n0, tx0), (p1, n1, tx1), d0, d1);
                let i2 = intersect((p0, n0, tx0), (p2, n2, tx2), d0, d2);
                Vector#{((p0, n0, tx0), i1, i2)}
            } else if (d1 > 0.0) {
                let i0 = intersect((p1, n1, tx1), (p0, n0, tx0), d1, d0);
                let i2 = intersect((p1, n1, tx1), (p2, n2, tx2), d1, d2);
                Vector#{((p1, n1, tx1), i2, i0)}
            } else {
                let i0 = intersect((p2, n2, tx2), (p0, n0, tx0), d2, d0);
                let i1 = intersect((p2, n2, tx2), (p1, n1, tx1), d2, d1);
                Vector#{((p2, n2, tx2), i0, i1)}
            }
        } else {
            if (d0 <= 0.0) {
                let i1 = intersect((p1, n1, tx1), (p0, n0, tx0), d1, d0);
                let i2 = intersect((p2, n2, tx2), (p0, n0, tx0), d2, d0);
                Vector#{((p1, n1, tx1), (p2, n2, tx2), i2), ((p1, n1, tx1), i2, i1)}
            } else if (d1 <= 0.0) {
                let i0 = intersect((p0, n0, tx0), (p1, n1, tx1), d0, d1);
                let i2 = intersect((p2, n2, tx2), (p1, n1, tx1), d2, d1);
                Vector#{((p2, n2, tx2), (p0, n0, tx0), i0), ((p2, n2, tx2), i0, i2)}
            } else {
                let i0 = intersect((p0, n0, tx0), (p2, n2, tx2), d0, d2);
                let i1 = intersect((p1, n1, tx1), (p2, n2, tx2), d1, d2);
                Vector#{((p0, n0, tx0), (p1, n1, tx1), i1), ((p0, n0, tx0), i1, i0)}
            }
        }
    }

    def signedDistance(plane: (Vec3, Float64), v: Vec3): Float64 = {
        let (Vec3(nx, ny, nz), offset) = plane;
        let Vec3(vx, vy, vz) = v;
        vx*nx + vy*ny + vz*nz + offset
    }

}
