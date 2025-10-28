mod Util {
    
    pub def unwrap(x: Option[a]): a = match x {
        case Some(v) => v
        case None => unreachable!()
    }

}