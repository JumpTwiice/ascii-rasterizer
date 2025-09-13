struct Transform[r] {
    mut scale: Float64,
    mut rotation: Vertex3D,
    mut translation: Vertex3D
}

mod Transform {

    pub def origin(rc: Region[r]): Transform[r] \ r = 
        new Transform @ rc { scale = 1.0, rotation = (0.0, 0.0, 0.0), translation = (0.0, 0.0, 0.0)} 

    pub def transform(
        rc: Region[r], 
        scale: { scale = Float64 }, 
        rotation: { rotation = Vertex3D }, 
        translation: { translation = Vertex3D }
    ): Transform[r] \ r =
        new Transform @ rc { 
            scale = scale#scale, 
            rotation = rotation#rotation, 
            translation = translation#translation
        } 

    pub def scale(s: Float64, t: Transform[r]): Unit \ r = setScale(t->scale + s, t)

    pub def rotate(r: Float64, axis: Axis, t: Transform[r]): Unit \ r =
        let (x0, y0, z0) = t->rotation;
        match axis {
            case Axis.X => setRotation((x0 + r, y0, z0), t)
            case Axis.Y => setRotation((x0, y0 + r, z0), t)
            case Axis.Z => setRotation((x0, y0, z0 + r), t)
        }

    pub def translate(v: Vertex3D, t: Transform[r]): Unit \ r = 
        let (x0, y0, z0) = t->translation;
        let (x1, y1, z1) = v;
        t->translation = (x0 + x1, y0 + y1, z0 + z1)

    pub def setScale(s: Float64, t: Transform[r]): Unit \ r = t->scale = s

    pub def getScale(t: Transform[r]): Float64 \ r = t->scale

    pub def setRotation(r: Vertex3D, t: Transform[r]): Unit \ r = t->rotation = r

    pub def getRotation(t: Transform[r]): Vertex3D \ r = t->rotation

    pub def setTranslation(v: Vertex3D, t: Transform[r]): Unit \ r = t->translation = v

    pub def getTranslation(t: Transform[r]): Vertex3D \ r = t->translation

}