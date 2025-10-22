struct Camera[r: Region] {
    position: Math.Vec3,
    rotation: Math.Vec3
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

    pub def viewMatrix(c: Camera[r]): Mat4x4 =
        let Vec3.Vec3(rx, ry, rz) = c->rotation;
        let Vec3.Vec3(px, py, pz) = c->position;

        let rotation = Mat4x4.rotationZ(rz) * Mat4x4.rotationY(ry) * Mat4x4.rotationX(rx);
        
        let translation = Mat4x4.translation(-px, -py, -pz);
        rotation * translation

}
