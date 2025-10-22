mod Math {

    pub enum Mat4x4 {
        case Mat4x4(Vector[Float64])
    }

    instance Mul[Mat4x4] {
        pub def mul(x: Mat4x4, y: Mat4x4): Mat4x4 = Mat4x4.mulMat4x4(x, y)
    }
    

    instance ToString[Mat4x4] {
        pub def toString(x: Mat4x4): String = {
            let Mat4x4.Mat4x4(m) = x;
            let row1 = "[ " + Vector.slice(start = 0, end = 4, m) |> Vector.join("  ") + " ]\n";
            let row2 = "[ " + Vector.slice(start = 4, end = 8, m) |> Vector.join("  ") + " ]\n";
            let row3 = "[ " + Vector.slice(start = 8, end = 12, m) |> Vector.join("  ") + " ]\n";
            let row4 = "[ " + Vector.slice(start = 12, end = 16, m) |> Vector.join("  ") + " ]";
            row1 + row2 + row3 + row4
        }
    }

    mod Mat4x4 {
        use Math.Vec3
        use Math.Vec4
        use Math.Vec4.Vec4

        pub def identity(): Mat4x4 = 
            Mat4x4(Vector#{
                1.0, 0.0, 0.0, 0.0,
                0.0, 1.0, 0.0, 0.0,
                0.0, 0.0, 1.0, 0.0,
                0.0, 0.0, 0.0, 1.0
            })

        pub def translation(tx: Float64, ty: Float64, tz: Float64): Mat4x4 = 
            Mat4x4(Vector#{
                1.0, 0.0, 0.0,  tx,
                0.0, 1.0, 0.0,  ty,
                0.0, 0.0, 1.0,  tz,
                0.0, 0.0, 0.0, 1.0
            })

        pub def scale(sx: Float64, sy: Float64, sz: Float64): Mat4x4 =
            Mat4x4(Vector#{
                 sx, 0.0, 0.0, 0.0,
                0.0,  sy, 0.0, 0.0,
                0.0, 0.0,  sz, 0.0,
                0.0, 0.0, 0.0, 1.0
            })

        pub def rotationX(angle: Float64): Mat4x4 = {
            let c = Math.cos(angle);
            let s = Math.sin(angle);
            Mat4x4(Vector#{
                1.0, 0.0, 0.0, 0.0,
                0.0,   c,  -s, 0.0,
                0.0,   s,   c, 0.0,
                0.0, 0.0, 0.0, 1.0
            })
        }

        pub def rotationY(angle: Float64): Mat4x4 = {
            let c = Math.cos(angle);
            let s = Math.sin(angle);
            Mat4x4(Vector#{
                  c, 0.0,   s, 0.0,
                0.0, 1.0, 0.0, 0.0,
                 -s, 0.0,   c, 0.0,
                0.0, 0.0, 0.0, 1.0
            })
        }

        pub def rotationZ(angle: Float64): Mat4x4 = {
            let c = Math.cos(angle);
            let s = Math.sin(angle);
            Mat4x4(Vector#{
                  c,  -s, 0.0, 0.0,
                  s,   c, 0.0, 0.0,
                0.0, 0.0, 1.0, 0.0,
                0.0, 0.0, 0.0, 1.0
            })
        }

        pub def mulVec4(mat: Mat4x4, v: Vec4): Vec4 = {
            let Mat4x4(m) = mat;
            let Vec4(x, y, z, w) = v;
            let x0 = Vector.get(0, m) * x + Vector.get(1, m) * y + Vector.get(2, m) * z + Vector.get(3, m) * w;
            let y0 = Vector.get(4, m) * x + Vector.get(5, m) * y + Vector.get(6, m) * z + Vector.get(7, m) * w;
            let z0 = Vector.get(8, m) * x + Vector.get(9, m) * y + Vector.get(10, m) * z + Vector.get(11, m) * w;
            let w0 = Vector.get(12, m) * x + Vector.get(13, m) * y + Vector.get(14, m) * z + Vector.get(15, m) * w;
            Vec4(x0, y0, z0, w0)
        }

        pub def mulVec3(mat: Mat4x4, v: Vec3): Vec3 =
            Vec4.toVec3(mulVec4(mat, Vec4.fromVec3(v)))

        pub def mulMat4x4(a: Mat4x4, b: Mat4x4): Mat4x4 = {
            let Mat4x4(ma) = a;
            let Mat4x4(mb) = b;
            let mc = Vector.init(i ->
                let row = i / 4;
                let col = Int32.modulo(i, 4);
                Vector.get(row * 4 + 0, ma) * Vector.get(0 * 4 + col, mb) +
                Vector.get(row * 4 + 1, ma) * Vector.get(1 * 4 + col, mb) +
                Vector.get(row * 4 + 2, ma) * Vector.get(2 * 4 + col, mb) +
                Vector.get(row * 4 + 3, ma) * Vector.get(3 * 4 + col, mb)
            , 16);
            Mat4x4(mc)
        }
    }

}
