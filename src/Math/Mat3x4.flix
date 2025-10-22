mod Math {

    pub enum Mat3x4 {
        case Mat3x4(Vector[Float64])
    }

    instance ToString[Mat3x4] {
        pub def toString(x: Mat3x4): String = {
            let Mat3x4.Mat3x4(m) = x;
            let row1 = "[ " + Vector.slice(start = 0, end = 4, m) |> Vector.join("  ") + " ]\n";
            let row2 = "[ " + Vector.slice(start = 4, end = 8, m) |> Vector.join("  ") + " ]\n";
            let row3 = "[ " + Vector.slice(start = 8, end = 12, m) |> Vector.join("  ") + " ]";
            row1 + row2 + row3
        }
    }

    mod Mat3x4 {
        use Math.Vec4
        use Math.Vec3
        use Math.Vec4.Vec4
        use Math.Vec3.Vec3

        pub def mulVec4(mat: Mat3x4, v: Vec4): Vec3 = {
            let Mat3x4(m) = mat;
            let Vec4(x, y, z, w) = v;
            let x0 = Vector.get(0, m) * x + Vector.get(1, m) * y + Vector.get(2, m) * z + Vector.get(3, m) * w;
            let y0 = Vector.get(4, m) * x + Vector.get(5, m) * y + Vector.get(6, m) * z + Vector.get(7, m) * w;
            let z0 = Vector.get(8, m) * x + Vector.get(9, m) * y + Vector.get(10, m) * z + Vector.get(11, m) * w;
            Vec3(x0, y0, z0)
        }

    }

}
