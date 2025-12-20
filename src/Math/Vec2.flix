mod Math {
    
    pub enum Vec2 {
        case Vec2(Float64, Float64)
    }


    instance Sub[Vec2] {
        pub def sub(x: Vec2, y: Vec2): Vec2 = Vec2.subtract(x, y)
    }

    instance Add[Vec2] {
        pub def add(x: Vec2, y: Vec2): Vec2 = Vec2.add(x, y)
    }

    mod Vec2 {
        pub def subtract(v1: Vec2, v2: Vec2): Vec2 = match (v1, v2) {
            case (Vec2(x1, y1), Vec2(x2, y2)) => Vec2(x1 - x2, y1 - y2)
        }

        pub def dot(v1: Vec2, v2: Vec2): Float64 = match (v1, v2) {
            case (Vec2(x1, y1), Vec2(x2, y2)) => x1 * x2 + y1 * y2
        }

        pub def length(v: Vec2): Float64 = match v {
            case Vec2(x, y) => Math.sqrt(x * x + y * y)
        }

        pub def normalize(v: Vec2): Vec2 = match v {
            case Vec2(x, y) => {
                let len = length(v);
                if (len == 0.0) Vec2(0.0, 0.0) else Vec2(x / len, y / len)
            }
        }

        pub def scale(v: Vec2, s: Float64): Vec2 = match v {
            case Vec2(x, y) => Vec2(x * s, y * s)
        }

        pub def add(v1: Vec2, v2: Vec2): Vec2 = match (v1, v2) {
            case (Vec2(x1, y1), Vec2(x2, y2)) => Vec2(x1 + x2, y1 + y2)
        }

        pub def toTuple(v: Vec2): (Float64, Float64) = match v {
            case Vec2(x, y) => (x, y)
        }

        pub def fromTuple(t: (Float64, Float64)): Vec2 = {
            let (x, y) = t;
            Vec2(x, y)
        }

        pub def zero(): Vec2 = Vec2(0.0, 0.0)

    }
}