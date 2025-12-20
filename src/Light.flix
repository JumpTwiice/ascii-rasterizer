enum Light {
    case Ambient(Float64) // Intensity
    case Point(Float64, Math.Vec3) // Intensity, position
    case Directional(Float64, Math.Vec3) // Intensity, direction
}

mod Light {
    use Math.Vec3
    use Math.Vec3.Vec3
    use Math.Vec4
    use Math.Mat4x4

    pub def lightingForPoint(p: Vec3, normal: Vec3, camDir: Vec3, specular: Float64, lights: List[Light]): Float64 = {
        let n = Vec3.normalize(normal);
        let v = Vec3.normalize(camDir);

        (0.0, lights) ||> Foldable.foldLeft(acc -> light -> match light {
            case Ambient(intensity) => acc + intensity
            case Point(intensity, pos) => {
                let l = Vec3.normalize(pos - p);
                acc + diffuseSpec(intensity, n, l, v, specular)
            }
            case Directional(intensity, dir) => {
                let l = Vec3.normalize(dir);
                acc + diffuseSpec(intensity, n, l, v, specular)
            }
        })
    }

    def diffuseSpec(intensity: Float64, n: Vec3, l: Vec3, v: Vec3, specular: Float64): Float64 = {
        let ndotl = Vec3.dot(n, l);
        let diffuse = if (ndotl > 0.0) intensity * ndotl else 0.0;

        let spec =
            if (not Float64.isNan(specular) and specular > 0.0 and ndotl > 0.0) {
                let r = Vec3.normalize(Vec3.scale(n, 2.0 * ndotl) - l);
                let rdotv = Vec3.dot(r, v);
                if (rdotv > 0.0) intensity * Float64.pow(base = rdotv, specular) else 0.0
            } else 0.0;

        diffuse + spec
    }

    pub def toCameraSpace(view: Mat4x4, light: Light): Light = match light {
        case Ambient(i) => Ambient(i)
        case Point(i, posWorld) => {
            let posCam = Mat4x4.mulVec3(view, posWorld);
            Point(i, posCam)
        }
        case Directional(i, Vec3(x, y, z)) => {
            let dirCam = (Mat4x4.mulVec4(view, Vec4.Vec4(x, y, z, 0.0)) |> Vec4.toVec3);
            Directional(i, dirCam)
        }
    }

}