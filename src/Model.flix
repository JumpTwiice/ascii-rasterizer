type alias Triangle = (Int32, Int32, Int32)

enum Model {
    case Model(String, Vector[Math.Vec3], Vector[(Triangle)])
}

mod Model {
    pub def cube(): Model = 
        let vertices = Vector#{
            (1.0, 1.0, 1.0), (-1.0, 1.0, 1.0), (-1.0, -1.0, 1.0), (1.0, -1.0, 1.0), 
            (1.0, 1.0, -1.0), (-1.0, 1.0, -1.0), (-1.0, -1.0, -1.0), (1.0, -1.0, -1.0)
        };
        let triangles = Vector#{
            (0, 2, 1), (0, 3, 2), // Front face (z = +1)
            (4, 6, 7), (4, 5, 6), // Back face (z = -1)
            (4, 1, 5), (4, 0, 1), // Top face (y = +1)
            (3, 6, 2), (3, 7, 6), // Bottom face (y = -1)
            (1, 6, 5), (1, 2, 6), // Left face (x = -1)
            (4, 3, 0), (4, 7, 3)  // Right face (x = +1)
        };
        Model.Model("Cube", Vector.map(Math.Vec3.fromTuple, vertices), triangles)

    pub def pyramid(): Model=
        let vertices = Vector#{
            (1.0, 0.0, 1.0), (1.0, 0.0, -1.0), (-1.0, 0.0, 1.0), (-1.0, 0.0, -1.0), (0.0, 1.0, 0.0)   
        };
        let triangles = Vector#{
            (1, 2, 3), (0, 1, 3), (0, 2, 4), (0, 1, 4), (2, 3, 4), (1, 3, 4), (0, 4, 1)
        };
        Model.Model("Pyramid", Vector.map(Math.Vec3.fromTuple, vertices), triangles)
}
