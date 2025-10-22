mod Math {
    import java.lang.Math

    pub def sin(x: Float64): Float64 = unchecked_cast(Math.sin(x) as _ \ {})

    pub def cos(x: Float64): Float64 = unchecked_cast(Math.cos(x) as _ \ {})

    pub def tan(x: Float64): Float64 = unchecked_cast(Math.tan(x) as _ \ {})

    pub def sqrt(x: Float64): Float64 = unchecked_cast(Math.sqrt(x) as _ \ {})
    pub def pi(): Float64 = 3.14159265

}