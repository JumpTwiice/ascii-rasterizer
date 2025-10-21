import java.awt.MouseInfo
import java.awt.Robot;
import java.lang.System;

def main(): Unit \ IO = region rc {
    run {
        let (w, h) = Options.dimensions();
        let canvas = Canvas.empty(rc, w, h);
        let cubeModel = Model.cube();
        let cubeTransform = Transform.transform(rc, scale=1.5, rotation=(0.0, 0.0, 0.0), translation=(0.0, 0.0, 10.0));
        let cubeInstance = Instance.fromModelAndTransform(rc, cubeModel, cubeTransform);

        def loop(t0: Int64): Unit = {
            let t1 = Clock.now();
            if (t1 - t0 >= 100i64) {
                cubeTransform
                    !> Transform.rotate(0.05, Axis.X)
                    !> Transform.rotate(0.05, Axis.Y)
                    !> Transform.rotate(0.05, Axis.Z);
                Instance.render(canvas, cubeInstance);
                Canvas.display(canvas);
                Canvas.flush(canvas);
                loop(t1)
            } else loop(t0)
        };
        loop(0i64)
    } with Console.runWithIO
    with Clock.runWithIO
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

def main4(): Unit \ IO = region rc {
    run {
        let (w, h) = Options.dimensions();
        let canvas = Canvas.empty(rc, w, h);
        Canvas.drawFilledTriangle(0, 0, 128, 0, 64, 64, (0, 0, 100), canvas);
        Canvas.display(canvas)
    } with Console.runWithIO
}

def main5(): Unit \ IO = {
    def loop() = {
        let pointer = MouseInfo.getPointerInfo();
        let pos = pointer.getLocation();
        println(pos.x);
        loop()
    };
    loop()
}

def main22(): Unit \ IO = region rc {
    run {
        let (w, h) = Options.dimensions();
        let canvas = Canvas.empty(rc, w, h);
        let cubeModel = Model.cube();
        let cubeTransform = Transform.transform(rc, scale=1.5, rotation=(0.0, 0.0, 0.0), translation=(0.0, 0.0, 10.0));
        let cubeInstance = Instance.fromModelAndTransform(rc, cubeModel, cubeTransform);

        let dt = 50i64;

        def loop(oldPointer, t0) = {
            let t1 = Clock.now();
            if (t1 - t0 >= dt) {
                let pointer = MouseInfo.getPointerInfo();
                let pos = pointer.getLocation();
                let oldPos = oldPointer.getLocation();
                let dx = Int32.toFloat64(oldPos.x - pos.x) / 100.0;
                let dy = Int32.toFloat64(oldPos.y - pos.y) / 100.0;
                cubeTransform
                    !> Transform.rotate(dy, Axis.X)
                    !> Transform.rotate(dx, Axis.Y);
                Instance.render(canvas, cubeInstance);
                Canvas.display(canvas);
                Canvas.flush(canvas);
                loop(pointer, t1)
            } else loop(oldPointer, t0)
        };
        loop(MouseInfo.getPointerInfo(), 0i64)
    } with Console.runWithIO
    with Clock.runWithIO
}

def main222(): Unit \ IO =
    let x = RichString.bgBlack("▄") |> RichString.toAnsiString;
    println(x)