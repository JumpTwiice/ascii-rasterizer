type alias Vertex2D = (Float64, Float64)
type alias Vertex3D = (Float64, Float64, Float64)

mod Vertex3D {

    pub def scale(s: Float64, v: Vertex3D): Vertex3D = 
        let (x, y, z) = v;
        (s*x, s*y, s*z)

    pub def rotate(r: Float64, axis: Axis, v: Vertex3D): Vertex3D = 
        let (x0, y0, z0) = v;
        match axis {
            case Axis.X => 
                let y = y0 * Math.cos(r) - z0 * Math.sin(r);
                let z = z0 * Math.cos(r) + y0 * Math.sin(r);
                (x0, y, z)
            case Axis.Y => 
                let x = x0 * Math.cos(r) - z0 * Math.sin(r);
                let z = z0 * Math.cos(r) + x0 * Math.sin(r);
                (x, y0, z)
            case Axis.Z => 
                let x = x0 * Math.cos(r) - y0 * Math.sin(r);
                let y = y0 * Math.cos(r) + x0 * Math.sin(r);
                (x, y, z0)
        }
        

    pub def translate(dv: Vertex3D, v: Vertex3D): Vertex3D =
        let (x0, y0, z0) = v;
        let (dx, dy, dz) = dv;
        (x0 + dx, y0 + dy, z0 + dz)

    pub def projectTo2D(v: Vertex3D): Vertex2D = {
        let (x, y, z) = v;
        let (w, h) = Options.dimensions();
        let f = Options.focalLength();
        let px = x * f / z + Int32.toFloat64(w / 2);
        let py = y * f / z + Int32.toFloat64(h / 2);
        (px, py)
    }

    pub def applyTransform(t: Transform[r], v: Vertex3D): Vertex3D \ r = 
        let scaled = scale(Transform.getScale(t), v);
        let (rx, ry, rz) = Transform.getRotation(t);
        let rotatedX = rotate(rx, Axis.X, scaled);
        let rotatedY = rotate(ry, Axis.Y, rotatedX);
        let rotatedZ = rotate(rz, Axis.Z, rotatedY);
        translate(Transform.getTranslation(t),rotatedZ)

}

mod Vertex2D {

    pub def toPixelCoord(p: Vertex2D): (Int32, Int32) = 
        let (x, y) = p;
        match Float64.tryToInt32(x) {
            case Some(x0) => match Float64.tryToInt32(y) {
                case Some(y0) => (x0, y0)
                case None => bug!("Could not fit y-coordinate into Int32")
            }
            case None => bug!("Could not fit x-coordinate into Int32")
        }
        
}
