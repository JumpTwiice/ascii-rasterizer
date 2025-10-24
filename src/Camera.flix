struct Camera[r: Region] {
    mut position: Math.Vec3,
    mut rotation: Math.Vec3
}

mod Camera {
    use Math.{Mat4x4, Vec3}

    pub def create(
        rc: Region[r], 
        p: { position = Vec3}, 
        r: { rotation = Vec3}
    ): Camera[r] \ r = new Camera @ rc {
        position = p#position,
        rotation = r#rotation
    }

    pub def translate(translation: Vec3, c: Camera[r]): Unit \ r = 
        c->position = c->position + translation

    pub def rotateX(angle: Float64, c: Camera[r]): Unit \ r = 
        c->rotation = c->rotation + Vec3.Vec3(angle, 0.0, 0.0)

    pub def rotateY(angle: Float64, c: Camera[r]): Unit \ r = 
        c->rotation = c->rotation + Vec3.Vec3(0.0, angle, 0.0)
    
    pub def rotateZ(angle: Float64, c: Camera[r]): Unit \ r = 
        c->rotation = c->rotation + Vec3.Vec3(0.0, 0.0, angle)

    pub def viewMatrix(c: Camera[r]): Mat4x4 \ r =
        let Vec3.Vec3(rx, ry, rz) = c->rotation;
        let Vec3.Vec3(px, py, pz) = c->position;

        let rotation = Mat4x4.rotationZ(rz) * Mat4x4.rotationY(ry) * Mat4x4.rotationX(rx);
        
        let translation = Mat4x4.translation(-px, -py, -pz);
        rotation * translation

}
