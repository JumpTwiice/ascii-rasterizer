def main(): Unit \ IO = region rc {
    let (w, h) = Options.dimensions();
    let canvas = Canvas.empty(rc, w, h);
    let cubeModel = Model.cube();
    let cubeTransform = Transform.transform(rc, scale=2.0, rotation=(0.0, 0.0, 0.0), translation=(0.0, 0.0, 12.0));
    let cubeInstance = Instance.fromModelAndTransform(rc, cubeModel, cubeTransform);

    def loop(): Unit = {
        Instance.transform(cubeInstance) 
            !> Transform.rotate(0.05, Axis.X)
            !> Transform.rotate(0.05, Axis.Y)
            !> Transform.rotate(0.05, Axis.Z);
        Instance.render(canvas, cubeInstance);
        Canvas.shrink(rc, canvas) |> Canvas.toString |> println;
        Canvas.flush(canvas);
        Thread.sleep(Time.Duration.fromMilliSeconds(50));
        loop()
    };
    loop()
}