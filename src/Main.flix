import java.awt.MouseInfo
import java.awt.Robot;
import java.lang.System;
use Math.Mat4x4
use Math.Vec3.Vec3
use Math.Vec3

def showTextureImage(): Unit \ IO = region rc {
    let result = run {
        let texture = ImageLoader.readTextureFromFile("crate-texture-raw");
        let (tw, th) = Texture.dimensions(texture);
        let (maxW, maxH) = Options.dimensions();
        let scaleW = Int32.toFloat64(maxW) / Int32.toFloat64(tw);
        let scaleH = Int32.toFloat64(maxH) / Int32.toFloat64(th);
        let scale = Float64.min(1.0, Float64.min(scaleW, scaleH));

        let clampW = Float64.clampToInt32(min = 1, max = maxW, nanValue = 1);
        let clampH = Float64.clampToInt32(min = 2, max = maxH, nanValue = 2);

        let w = clampW(Float64.floor(Int32.toFloat64(tw) * scale));
        let h0 = clampH(Float64.floor(Int32.toFloat64(th) * scale));

        let h = if (h0 `Int32.modulo` 2 == 0) h0 else Int32.max(2, h0 - 1);

        let canvas = Canvas.empty(rc, w, h);
        Canvas.fillWithTexture(canvas, texture);
        Canvas.display(canvas)
    } with Console.runWithIO
    with FileRead.runWithIO;
    match result {
        case Ok(_) => ()
        case Err(_) => ()
    }
}

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

def main(): Unit \ IO = region rc {
    let res = run {
        let (w, h) = Options.dimensions();
        let canvas = Canvas.empty(rc, w, h);
        let cubeModel = Model.texturedCube(ImageLoader.readTextureFromFile("crate-texture-raw"));
        let cubeInstance = Instance.fromModel(
            rc, cubeModel, 
            position = Vec3(0.0, 0.0, 2.5),
            rotation = Vec3(0.0, 0.0, 0.0),
            scale = Vec3(1.0, 1.0, 1.0)
        );
        let camera = defaultCamera(rc, Vec3(0.0, 0.0, 0.0), Vec3(0.0, 0.0, 0.0));
        let secondsToRun = 10i64;
        let t = Clock.now();
        let dt = 100i64;
        def loop(t0: Int64): Unit = {
            let t1 = Clock.now();
            if (t1 - t >= secondsToRun * 1000i64) ()
            else if (t1 - t0 >= dt) {
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
    with FileRead.runWithIO;
    match res {
        case Ok(_) => ()
        case Err(_) => ()
    }
}

def movingCam(): Unit \ IO = region rc {
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
        let lightIndicators = lightIndicatorInstances(rc);
        let camera = defaultCamera(rc, Vec3(-3.0, 0.0, 0.0), Vec3(0.0, -0.7, 0.0));
        let secondsToRun = 5i64;
        let t = Clock.now();
        let dt = 100i64;
        def loop(t0: Int64): Unit = {
            let t1 = Clock.now();
            if (t1 - t >= secondsToRun * 1000i64) ()
            else if (t1 - t0 >= dt) {
                Camera.translate(Vec3(0.1, 0.0, 0.0), camera);
                Camera.rotateY(0.02, camera);
                Instance.render(canvas, camera, cubeInstance);
                renderInstances(canvas, camera, lightIndicators);
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
        let cubeModel = Model.texturedCube(ImageLoader.loadCheckerboardTexture(64, 64, 8));
        let cubeInstance = Instance.fromModel(
            rc, cubeModel, 
            position = Vec3(0.0, 0.0, 2.5),
            rotation = Vec3(0.0, 0.0, 0.0),
            scale = Vec3(1.0, 1.0, 1.0)
        );
        let lightIndicators = lightIndicatorInstances(rc);
        let camera = defaultCamera(rc, Vec3(0.0, 0.0, 0.0), Vec3(0.0, 0.0, 0.0));
        Instance.render(canvas, camera, cubeInstance);
        renderInstances(canvas, camera, lightIndicators);
        Canvas.display(canvas)

    } with Console.runWithIO
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
        let lightIndicators = lightIndicatorInstances(rc);
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
                    renderInstances(canvas, camera, lightIndicators);
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

def lightIndicatorInstances(rc: Region[r]): List[Instance[r]] \ r = {
    let lightModel = Model.sphere();
    def loop(ls, acc): List[Instance[r]] \ r = match ls {
        case Nil => acc
        case Light.Point(_, pos) :: rest => 
            let inst = Instance.fromModel(
                rc, lightModel,
                position = pos,
                rotation = Vec3.zero(),
                scale = Vec3(0.1, 0.1, 0.1)
            );
            loop(rest, inst :: acc)
        case _ :: rest => loop(rest, acc)
    };
    loop(Options.lights(), Nil)
}
