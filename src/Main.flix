def main(): Unit \ IO = region rc {
    run {
        let (w, h) = Options.dimensions();
        let canvas = Canvas.empty(rc, w, h);
        let cubeModel = Model.cube();
        let cubeTransform = Transform.transform(rc, scale=1.0, rotation=(0.0, 0.0, 0.0), translation=(0.0, 0.0, 10.0));
        let cubeInstance = Instance.fromModelAndTransform(rc, cubeModel, cubeTransform);

        def loop(): Unit = {
            cubeTransform
                !> Transform.rotate(0.05, Axis.X)
                !> Transform.rotate(0.05, Axis.Y)
                !> Transform.rotate(0.05, Axis.Z);
            Instance.render(canvas, cubeInstance);
            Canvas.display(canvas);
            Canvas.flush(canvas);
            Thread.sleep(Time.Duration.fromMilliSeconds(50));
            loop()
        };
        loop()
    } with Console.runWithIO
}

def main3(): Unit \ IO = region rc {
    run {
        let (w, h) = Options.dimensions();
        let canvas = Canvas.empty(rc, w, h);
        let cubeModel = Model.cube();
        let cubeTransform = Transform.transform(rc, scale=1.5, rotation=(45.0, 45.0, 45.0), translation=(0.0, 0.0, 10.0));
        let cubeInstance = Instance.fromModelAndTransform(rc, cubeModel, cubeTransform);
        Instance.render(canvas, cubeInstance);
        Canvas.display(canvas)

    } with Console.runWithIO
}