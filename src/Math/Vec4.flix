mod Math { 

    pub enum Vec4 with Eq {
        case Vec4(Float64, Float64, Float64, Float64)
    }

    instance ToString[Vec4] {
        pub def toString(v: Vec4): String = 
            let Vec4.Vec4(x, y, z, w) = v;
            "(${x}, ${y}, ${z}, ${w})"
    }

    mod Vec4 {
        use Math.Vec3
        use Math.Vec3.Vec3

        pub def fromVec3(v: Vec3): Vec4 =
            let Vec3(x, y, z) = v;
            Vec4(x, y, z, 1.0)

        pub def toVec3(v: Vec4): Vec3 =
            let Vec4(x, y, z, w) = v;
            if (w == 0.0) Vec3(x, y, z) else Vec3(x / w, y / w, z / w)
            
    }

}