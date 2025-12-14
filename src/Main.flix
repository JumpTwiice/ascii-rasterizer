import java.awt.MouseInfo
import java.awt.Robot;
import java.lang.System;
use Math.Mat4x4
use Math.Vec3.Vec3
use Math.Vec3

def defaultCamera(rc: Region[r], position: Vec3, rotation: Vec3): Camera[r] \ r = {
    let (vw, vh) = Options.viewportDimensions();
    let aspect = Int32.toFloat64(vw) / Int32.toFloat64(vh);
    let fovY = Math.pi() / 2.0;
    let zNear = Options.zNear();
    Camera.create(
        rc,
        position = position,
        rotation = rotation,
        fovY = fovY,
        aspect = aspect,
        zNear = zNear
    )
}

def rotatingCube(): Unit \ IO = region rc {
    run {
        let (w, h) = Options.dimensions();
        let canvas = Canvas.empty(rc, w, h);
        let cubeModel = Model.cubeWithColor(
            (255, 0, 0),
            (0, 255, 0),
            (0, 0, 255),
            (255, 0, 255),
            (0, 255, 255),
            (255, 255, 0)
        );
        let cubeInstance = Instance.fromModel(
            rc, cubeModel, 
            position = Vec3(0.0, 0.0, 2.5),
            rotation = Vec3(0.0, 0.0, 0.0),
            scale = Vec3(1.0, 1.0, 1.0)
        );
        let camera = defaultCamera(rc, Vec3(0.0, 0.0, 0.0), Vec3(0.0, 0.0, 0.0));

        def loop(t0: Int64): Unit = {
            let t1 = Clock.now();
            if (t1 - t0 >= 200i64) {
                Instance.rotateX(0.1, cubeInstance);
                Instance.rotateY(0.1, cubeInstance);
                Instance.rotateZ(0.1, cubeInstance);
                Instance.render(canvas, camera, cubeInstance);
                Canvas.display(canvas);
                Canvas.flush(canvas);
                loop(t1)
            } else loop(t0)
        };
        loop(0i64)
    } with Console.runWithIO
    with Clock.runWithIO
}

def cubeStatic(): Unit \ IO = region rc {
    run {
        let (w, h) = Options.dimensions();
        let canvas = Canvas.empty(rc, w, h);
        let cubeModel = Model.cubeWithColor(
            (255, 0, 0),
            (0, 255, 0),
            (0, 0, 255),
            (255, 0, 255),
            (0, 255, 255),
            (255, 255, 0)
        );
        let cubeInstance = Instance.fromModel(
            rc, cubeModel, 
            position = Vec3(0.0, 0.0, 2.5),
            rotation = Vec3(0.0, -0.7, 0.0),
            scale = Vec3(1.0, 1.0, 1.0)
        );
        let camera = defaultCamera(rc, Vec3(0.0, 0.0, 0.0), Vec3(0.0, 0.0, 0.0));
        Instance.render(canvas, camera, cubeInstance);
        Canvas.display(canvas)

    } with Console.runWithIO
}

def pyramid(): Unit \ IO = region rc {
    run {
        let (w, h) = Options.dimensions();
        let canvas = Canvas.empty(rc, w, h);
        let cubeModel = Model.pyramid();
        let cubeInstance = Instance.fromModel(
            rc, cubeModel, 
            position = Vec3(0.0, 1.0, 6.0),
            rotation = Vec3(Math.pi(), Math.pi() / 2.0, 0.0),
            scale = Vec3(1.0, 2.2, 1.0)
        );
        let camera = defaultCamera(rc, Vec3(0.0, 0.0, 0.0), Vec3(0.0, 0.0, 0.0));
        Instance.render(canvas, camera, cubeInstance);
        Canvas.display(canvas)

    } with Console.runWithIO
}

def triangle(): Unit \ IO = region rc {
    run {
        let (w, h) = Options.dimensions();
        let canvas = Canvas.empty(rc, w, h);
        let (x0, y0) = (w-1, 0);
        let (x1, y1) = (0, 0);
        let (x2, y2) = (w/2-1, h-1);
        
        Canvas.drawShadedTriangle(x0, y0, x1, y1, x2, y2, 0.5, 0.05, 0.2, 0.0, 0.0, 0.0, (0, 255, 255), canvas);
        Canvas.display(canvas)
    } with Console.runWithIO
}

def line(): Unit \ IO = region rc{
    run {
        let (w, h) = Options.dimensions();
        let canvas = Canvas.empty(rc, w, h);
        let (x1, y1) = (w - 1, h - 1);
        Canvas.drawLine(0, 0, x1, y1, (255, 255, 255), canvas);
        Canvas.display(canvas)
    } with Console.runWithIO
}

def mousePos(): Unit \ IO = {
    def loop() = {
        let pointer = MouseInfo.getPointerInfo();
        let pos = pointer.getLocation();
        println(pos.x);
        loop()
    };
    loop()
}

def main2(): Unit \ IO = region rc {
    run {
        let (w, h) = Options.dimensions();
        let canvas = Canvas.empty(rc, w, h);
        let cubeModel = Model.cubeWithColor(
            (255, 0, 0),
            (0, 255, 0),
            (0, 0, 255),
            (255, 0, 255),
            (0, 255, 255),
            (255, 255, 0)
        );
        let cube1 = Instance.fromModel(
            rc, cubeModel, 
            position = Vec3(0.0, 0.0, 0.0),
            rotation = Vec3(3.0, 2.0, 1.0),
            scale = Vec3(1.0, 1.0, 1.0)
        );
        let cube2 = Instance.fromModel(
            rc, cubeModel, 
            position = Vec3(6.0, 0.0, 0.0),
            rotation = Vec3(1.0, 2.0, 3.0),
            scale = Vec3(1.0, 1.0, 1.0)
        );
        let instances = cube1 :: cube2 :: Nil;
        let camera = defaultCamera(rc, Vec3(0.0, 0.0, 0.0), Vec3(0.0, 0.0, 0.0));

        let dt = 50i64;

        def loop(oldPointer, t0) = {
            let t1 = Clock.now();
            if (t1 - t0 >= dt) {
                let pointer = MouseInfo.getPointerInfo();
                let pos = pointer.getLocation();
                let oldPos = oldPointer.getLocation();
                let dx = Int32.toFloat64(oldPos.x - pos.x) / 500.0;
                let dy = Int32.toFloat64(oldPos.y - pos.y) / 500.0;
                if (dx != 0.0 or dy != 0.0) {
                    Camera.rotateY(dx, camera);
                    Camera.rotateX(-dy, camera);
                    renderInstances(canvas, camera, instances);
                    Canvas.display(canvas);
                    Canvas.flush(canvas)
                } else ();
                loop(pointer, t1)
            } else loop(oldPointer, t0)
        };
        loop(MouseInfo.getPointerInfo(), 0i64)
    } with Console.runWithIO
    with Clock.runWithIO
}

def renderInstances(canvas: Canvas[r0], camera: Camera[r1], instances: List[Instance[r2]]): Unit \ r0 + r1 + r2 = {
    foreach (i <- instances)
        Instance.render(canvas, camera, i)
}

def main(): Unit \ IO = region rc {
    run {
        let (w, h) = Options.dimensions();
        let canvas = Canvas.empty(rc, w, h);

        let tetraModel = Model.tetrahedron((255, 210, 120));
        let octModel = Model.octahedron((255, 180, 80), (80, 180, 255), (200, 200, 200));

        let tetra = Instance.fromModel(
            rc, tetraModel,
            position = Vec3(-1.8, 0.0, 8.0),
            rotation = Vec3(0.3, 0.5, -0.2),
            scale = Vec3(1.2, 1.2, 1.2)
        );
        let oct = Instance.fromModel(
            rc, octModel,
            position = Vec3(1.8, 0.0, 7.5),
            rotation = Vec3(0.0, 0.0, 0.0),
            scale = Vec3(1.1, 1.1, 1.1)
        );

        let instances = tetra :: oct :: Nil;

        let camera = defaultCamera(rc, Vec3(0.0, 0.0, 0.0), Vec3(0.0, 0.0, 0.0));

        let dt = 70i64;

        def loop(t0: Int64): Unit = {
            let t1 = Clock.now();
            if (t1 - t0 >= dt) {
                Instance.rotateX(-0.05, tetra);
                Instance.rotateY(0.07, tetra);
                Instance.rotateZ(0.06, tetra);

                Instance.rotateY(-0.06, oct);
                Instance.rotateZ(0.08, oct);

                renderInstances(canvas, camera, instances);
                Canvas.display(canvas);
                Canvas.flush(canvas);
                loop(t1)
            } else loop(t0)
        };

        loop(0i64)
    } with Console.runWithIO
    with Clock.runWithIO
}
