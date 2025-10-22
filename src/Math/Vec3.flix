mod Math {

    pub enum Vec3 with Eq {
        case Vec3(Float64, Float64, Float64)
    }

    instance Add[Vec3] {
        pub def add(x: Vec3, y: Vec3): Vec3 = Vec3.add(x, y)
    }
    
    instance Sub[Vec3] {
        pub def sub(x: Vec3, y: Vec3): Vec3 = Vec3.sub(x, y)
    }
    
    instance Mul[Vec3] {
        pub def mul(x: Vec3, y: Vec3): Vec3 = Vec3.cross(x, y)
    }

    instance Neg[Vec3] {
        pub def neg(x: Vec3): Vec3 = Vec3.neg(x)
    }

    instance ToString[Vec3] {
        pub def toString(v: Vec3): String = 
            let Vec3.Vec3(x, y, z) = v;
            "(${x}, ${y}, ${z})"
    }

    mod Vec3 {

        pub def fromXYZ(x: Float64, y: Float64, z: Float64): Vec3 = Vec3(x, y, z)

        pub def fromTuple(t: (Float64, Float64, Float64)): Vec3 = 
            let (x, y, z) = t;
            Vec3(x, y, z)

        pub def add(u: Vec3, v: Vec3): Vec3 =
            let Vec3(x1, y1, z1) = u;
            let Vec3(x2, y2, z2) = v;
            Vec3(x1 + x2, y1 + y2, z1 + z2)

        pub def sub(u: Vec3, v: Vec3): Vec3 =
            let Vec3(x1, y1, z1) = u;
            let Vec3(x2, y2, z2) = v;
            Vec3(x1 - x2, y1 - y2, z1 - z2)

        pub def scale(u: Vec3, s: Float64): Vec3 =
            let Vec3(x, y, z) = u;
            Vec3(x * s, y * s, z * s)

        pub def cross(u: Vec3, v: Vec3): Vec3 =
            let Vec3(x1, y1, z1) = u;
            let Vec3(x2, y2, z2) = v;
            Vec3(
                y1 * z2 - z1 * y2,
                z1 * x2 - x1 * z2,
                x1 * y2 - y1 * x2
            )

        pub def dot(u: Vec3, v: Vec3): Float64 =
            let Vec3(x1, y1, z1) = u;
            let Vec3(x2, y2, z2) = v;
            x1 * x2 + y1 * y2 + z1 * z2

        pub def neg(v: Vec3): Vec3 =
            let Vec3(x, y, z) = v;
            Vec3(-x, -y, -z)

        pub def length(v: Vec3): Float64 =
            Math.sqrt(dot(v, v))

        pub def normalize(v: Vec3): Vec3 =
            let len = length(v);
            if (len == 0.0) v else scale(v, 1.0 / len)

        pub def zero(): Vec3 = Vec3(0.0, 0.0, 0.0)
    }

}