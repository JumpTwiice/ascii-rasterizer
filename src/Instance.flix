struct Instance[r] {
    model: Model,
    mut transform: Transform[r]
}

mod Instance {

    pub def fromModelAndTransform(rc: Region[r], model: Model, transform: Transform[r]): Instance[r] \ r = 
        new Instance @ rc { model = model, transform = transform } 

    pub def render(c: Canvas[r0], i: Instance[r1]): Unit \ r0 + r1 = match i->model {
        case Model.Model(_, vertices, triangles) => 
            let projected = Vector.map(Vertex3D.applyTransform(i->transform) >> Vertex3D.projectTo2D, vertices);
            foreach ((p1, p2, p3) <- triangles) {
                let (x0, y0) = Vector.get(p1, projected) |> Vertex2D.toPixelCoord;
                let (x1, y1) = Vector.get(p2, projected) |> Vertex2D.toPixelCoord;
                let (x2, y2) = Vector.get(p3, projected) |> Vertex2D.toPixelCoord;
                Canvas.drawTriangle(x0, y0, x1, y1, x2, y2, (0, 0, 100), c) 
            }
    }

    pub def transform(i: Instance[r]): Transform[r] \ r = i->transform

}