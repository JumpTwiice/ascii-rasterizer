type alias Triangle = (Int32, Int32, Int32)

enum Model {
    case Model(String, Vector[Vertex3D], Vector[(Triangle)])
}

mod Model {
    pub def cube(): Model = 
        let vertices = Vector#{
            (1.0, 1.0, 1.0), (-1.0, 1.0, 1.0), (-1.0, -1.0, 1.0), (1.0, -1.0, 1.0), 
            (1.0, 1.0, -1.0), (-1.0, 1.0, -1.0), (-1.0, -1.0, -1.0), (1.0, -1.0, -1.0)
        };
        let triangles = Vector#{
            (0, 1, 2), (0, 2, 3), // Front face (z = +1)
            (4, 7, 6), (4, 6, 5), // Back face (z = -1)
            (4, 5, 1), (4, 1, 0), // Top face (y = +1)
            (3, 2, 6), (3, 6, 7), // Bottom face (y = -1)
            (1, 5, 6), (1, 6, 2), // Left face (x = -1)
            (4, 0, 3), (4, 3, 7) // Right face (x = +1)
        };
        Model.Model("Cube", vertices, triangles)
}
