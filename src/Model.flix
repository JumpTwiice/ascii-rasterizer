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
            (0, 1, 2), (0, 2, 3), (4, 0, 3), (4, 3, 7), (5, 4, 7), (5, 7, 6), 
            (1, 5, 6), (1, 6, 2), (4, 5, 1), (4, 1, 0), (2, 6, 7), (2, 7, 3)
        };
        Model.Model("Cube", vertices, triangles)
}
