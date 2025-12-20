type alias Color = (Int32, Int32, Int32)

enum Model {
    case Model(
        String, // name
        Vector[Math.Vec3], // vertices
        Vector[( // triangles
            Int32, Int32, Int32, // vertex indices
            Math.Vec3, Math.Vec3, Math.Vec3, // vertex normals
            Color // color
        )], 
        Float64
    )
}

mod Model {
    use Math.Vec3
    use Math.Vec3.Vec3

    pub def boundingSphere(model: Model): (Vec3, Float64) = match model {
        case Model(_, verts, _, _) => {
            let len = Int32.toFloat64(Vector.length(verts));
            let center = Vector.fold(verts) |> Vec3.divide(len);
            let radius = verts 
                |> Vector.map(v -> Vec3.length(v - center)) 
                |> Vector.maximum
                |> Option.getWithDefault(0.0);
            (center, radius)
        }
    }

    pub def cubeWithColor(front: Color, back: Color, top: Color, bot: Color, left: Color, right: Color): Model = 
        let vertices = Vector#{
            (1.0, 1.0, 1.0), (-1.0, 1.0, 1.0), (-1.0, -1.0, 1.0), (1.0, -1.0, 1.0), 
            (1.0, 1.0, -1.0), (-1.0, 1.0, -1.0), (-1.0, -1.0, -1.0), (1.0, -1.0, -1.0)
        };
        let triangles = Vector#{
            // Front face (z = +1)
            (0, 2, 1, Vec3.forward(), Vec3.forward(), Vec3.forward(), front),
            (0, 3, 2, Vec3.forward(), Vec3.forward(), Vec3.forward(), front),
            // Back face (z = -1)
            (4, 6, 7, Vec3.back(), Vec3.back(), Vec3.back(), back),
            (4, 5, 6, Vec3.back(), Vec3.back(), Vec3.back(), back),
            // Top face (y = +1)
            (4, 1, 5, Vec3.up(), Vec3.up(), Vec3.up(), top),
            (4, 0, 1, Vec3.up(), Vec3.up(), Vec3.up(), top),
            // Bottom face (y = -1)
            (3, 6, 2, Vec3.down(), Vec3.down(), Vec3.down(), bot),
            (3, 7, 6, Vec3.down(), Vec3.down(), Vec3.down(), bot),
            // Left face (x = -1)
            (1, 6, 5, Vec3.left(), Vec3.left(), Vec3.left(), left),
            (1, 2, 6, Vec3.left(), Vec3.left(), Vec3.left(), left),
            // Right face (x = +1)
            (4, 3, 0, Vec3.right(), Vec3.right(), Vec3.right(), right),
            (4, 7, 3, Vec3.right(), Vec3.right(), Vec3.right(), right)
        };
        Model.Model("Cube", Vector.map(Math.Vec3.fromTuple, vertices), triangles, 20.0)

    pub def sphere(): Model = {
        let color = (220, 220, 220);
        let radius = 1.0;
        let stacks = 16;
        let slices = 32;
        let step = slices + 1;

        let verts = Vector.init(idx -> {
            let stack = idx / step;
            let slice = Int32.modulo(idx, step);
            let phi = Math.pi() * Int32.toFloat64(stack) / Int32.toFloat64(stacks);
            let theta = 2.0 * Math.pi() * Int32.toFloat64(slice) / Int32.toFloat64(slices);
            let x = radius * Math.sin(phi) * Math.cos(theta);
            let y = radius * Math.cos(phi);
            let z = radius * Math.sin(phi) * Math.sin(theta);
            Vec3(x, y, z)
        }, (stacks + 1) * step);

        let triangles = region rc {
            let tris = MutList.empty(rc);
            foreach (i <- Range.Range(0, stacks)) {
                foreach (j <- Range.Range(0, slices)) {
                    let p0 = i * step + j;
                    let p1 = (i + 1) * step + j;
                    let p2 = p0 + 1;
                    let p3 = p1 + 1;
                    let n0 = Vec3.normalize(Vector.get(p0, verts));
                    let n1 = Vec3.normalize(Vector.get(p1, verts));
                    let n2 = Vec3.normalize(Vector.get(p2, verts));
                    let n3 = Vec3.normalize(Vector.get(p3, verts));
                    MutList.push((p0, p1, p2, n0, n1, n2, color), tris);
                    MutList.push((p2, p1, p3, n2, n1, n3, color), tris)
                }
            };
            MutList.toVector(tris)
        };

        Model.Model("Sphere", verts, triangles, 20.0)
    }

}
