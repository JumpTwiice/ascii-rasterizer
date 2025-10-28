struct Instance[r] {
    model: Model,
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
        scale = s#scale, 
        rotation = r#rotation, 
        position = t#position 
    } 

    pub def render(canvas: Canvas[r0], camera: Camera[r1], i: Instance[r2]): Unit \ r0 + r1 + r2 = match i->model {
        case Model.Model(_, vertices, triangles) => 
            let Vec3(s1, s2, s3) = i->scale;
            let Vec3(r1, r2, r3) = i->rotation;
            let Vec3(t1, t2, t3) = i->position; 
            let model = Mat4x4.translation(t1, t2, t3) * Mat4x4.rotationZ(r3) * Mat4x4.rotationY(r2) * Mat4x4.rotationX(r1) * Mat4x4.scale(s1, s2, s3);

            let view = Camera.viewMatrix(camera);

            let projection = view * model;

            let projected = Vector.map(Vec4.fromVec3 >> Mat4x4.mulVec4(projection), vertices);
            foreach ((p1, p2, p3, color) <- triangles) {
                let v0 = Vector.get(p1, projected);
                let v1 = Vector.get(p2, projected);
                let v2 = Vector.get(p3, projected);
                let Vec4(_, _, z0, _) = v0;
                let Vec4(_, _, z1, _) = v1;
                let Vec4(_, _, z2, _) = v2;
                let (x0, y0) = projectVertex(v0) |> toPixelCoord;
                let (x1, y1) = projectVertex(v1) |> toPixelCoord;
                let (x2, y2) = projectVertex(v2) |> toPixelCoord;
                Canvas.drawShadedTriangle(x0, y0, x1, y1, x2, y2, 1.0, 1.0, 1.0, z0, z1, z2, color, canvas) 
            }
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

    def projectVertex(v: Vec4): (Float64, Float64) = {
        let (cw, ch) = Options.dimensions();
        let (vw, vh) = Options.viewportDimensions();
        let f = Options.focalLength();
        let Vec3(x, y, z) = Mat3x4.mulVec4(viewportToCanvasMatrix(cw, ch, vw, vh, f), v);
        (x / z + Int32.toFloat64(cw / 2), y / z + Int32.toFloat64(ch / 2))
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

    def toPixelCoord(p: (Float64, Float64)): (Int32, Int32) = {
        let (x, y) = p;
        match Float64.tryToInt32(x) {
            case Some(x0) => match Float64.tryToInt32(y) {
                case Some(y0) => (x0, y0)
                case None => bug!("Could not fit y-coordinate into Int32")
            }
            case None => bug!("Could not fit x-coordinate into Int32")
        }
    }


}