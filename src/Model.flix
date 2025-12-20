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

}
