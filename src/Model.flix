type alias Color = (Int32, Int32, Int32)
type alias ColoredTriangle = (Int32, Int32, Int32, Color)

enum Model {
    case Model(String, Vector[Math.Vec3], Vector[(ColoredTriangle)])
}

mod Model {
    use Math.Vec3
    use Math.Vec3.Vec3

    pub def boundingSphere(model: Model): (Vec3, Float64) = match model {
        case Model(_, verts, _) => {
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
            (0, 2, 1, front), (0, 3, 2, front), // Front face (z = +1)
            (4, 6, 7, back), (4, 5, 6, back), // Back face (z = -1)
            (4, 1, 5, top), (4, 0, 1, top), // Top face (y = +1)
            (3, 6, 2, bot), (3, 7, 6, bot), // Bottom face (y = -1)
            (1, 6, 5, left), (1, 2, 6, left), // Left face (x = -1)
            (4, 3, 0, right), (4, 7, 3, right)  // Right face (x = +1)
        };
        Model.Model("Cube", Vector.map(Math.Vec3.fromTuple, vertices), triangles)

    pub def pyramid(): Model =
        let color = (255, 255, 255);
        let vertices = Vector#{
            (1.0, 0.0, 1.0), (1.0, 0.0, -1.0), (-1.0, 0.0, 1.0), (-1.0, 0.0, -1.0), (0.0, 1.0, 0.0)   
        };
        let triangles = Vector#{
            (1, 2, 3, color), (0, 1, 3, color), 
            (0, 2, 4, color), (0, 1, 4, color), 
            (2, 3, 4, color), (1, 3, 4, color), 
            (0, 4, 1, color)
        };
        Model.Model("Pyramid", Vector.map(Math.Vec3.fromTuple, vertices), triangles)

    pub def tetrahedron(color: Color): Model =
        let vertices = Vector#{
            (1.0, 1.0, 1.0), (-1.0, -1.0, 1.0), (-1.0, 1.0, -1.0), (1.0, -1.0, -1.0)
        };
        let triangles = Vector#{
            (0, 1, 2, color),
            (0, 3, 1, color),
            (0, 2, 3, color),
            (1, 3, 2, color)
        };
        Model.Model("Tetrahedron", Vector.map(Math.Vec3.fromTuple, vertices), triangles)

    pub def octahedron(top: Color, bottom: Color, equator: Color): Model =
        let vertices = Vector#{
            (0.0, 1.0, 0.0), // top
            (0.0, -1.0, 0.0), // bottom
            (1.0, 0.0, 0.0), (-1.0, 0.0, 0.0), (0.0, 0.0, 1.0), (0.0, 0.0, -1.0) // equator ring
        };
        let triangles = Vector#{
            (0, 2, 4, equator), (0, 4, 3, top), (0, 3, 5, equator), (0, 5, 2, top),
            (1, 4, 2, equator), (1, 3, 4, bottom), (1, 5, 3, equator), (1, 2, 5, bottom)
        };
        Model.Model("Octahedron", Vector.map(Math.Vec3.fromTuple, vertices), triangles)
}
