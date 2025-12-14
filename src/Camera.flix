struct Camera[r: Region] {
    mut position: Math.Vec3,
    mut rotation: Math.Vec3,
    mut fovY: Float64,
    mut aspect: Float64,
    mut zNear: Float64
}

mod Camera {
    use Math.{Mat4x4, Vec3}
    use Math.Vec3.Vec3

    pub def create(
        rc: Region[r], 
        p: { position = Vec3 }, 
        r: { rotation = Vec3 },
        fovY: { fovY = Float64 },
        aspect: { aspect = Float64 },
        zNear: { zNear = Float64 }
    ): Camera[r] \ r = new Camera @ rc {
        position = p#position,
        rotation = r#rotation,
        fovY = fovY#fovY,
        aspect = aspect#aspect,
        zNear = zNear#zNear
    }

    pub def frustumPlanes(c: Camera[r]): List[(Vec3, Float64)] \ r = {
        let thetaY = c->fovY / 2.0;
        let thetaX = Math.atan(c->aspect * Math.tan(thetaY));

        let near   = (Vec3(0.0, 0.0, 1.0), -c->zNear);
        let left   = (Vec3.normalize(Vec3( 1.0, 0.0, Math.tan(thetaX))), 0.0);
        let right  = (Vec3.normalize(Vec3(-1.0, 0.0, Math.tan(thetaX))), 0.0);
        let top    = (Vec3.normalize(Vec3(0.0, -1.0, Math.tan(thetaY))), 0.0);
        let bottom = (Vec3.normalize(Vec3(0.0,  1.0, Math.tan(thetaY))), 0.0);

        List#{near, left, right, top, bottom}
    }

    pub def translate(translation: Vec3, c: Camera[r]): Unit \ r = 
        c->position = c->position + translation

    pub def rotateX(angle: Float64, c: Camera[r]): Unit \ r = 
        c->rotation = c->rotation + Vec3.Vec3(angle, 0.0, 0.0)

    pub def rotateY(angle: Float64, c: Camera[r]): Unit \ r = 
        c->rotation = c->rotation + Vec3.Vec3(0.0, angle, 0.0)
    
    pub def rotateZ(angle: Float64, c: Camera[r]): Unit \ r = 
        c->rotation = c->rotation + Vec3.Vec3(0.0, 0.0, angle)

    pub def fovY(c: Camera[r]): Float64 \ r = c->fovY

    pub def viewMatrix(c: Camera[r]): Mat4x4 \ r =
        let Vec3.Vec3(rx, ry, rz) = c->rotation;
        let Vec3.Vec3(px, py, pz) = c->position;

        let rotation = Mat4x4.rotationZ(rz) * Mat4x4.rotationY(ry) * Mat4x4.rotationX(rx);
        
        let translation = Mat4x4.translation(-px, -py, -pz);
        rotation * translation

}
