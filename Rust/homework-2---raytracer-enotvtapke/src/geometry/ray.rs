use crate::geometry::vec3::Vec3;

#[derive(Copy, Clone)]
pub struct Ray {
    pub orig: Vec3,
    pub dir: Vec3,
}

impl Ray {
    pub fn new(orig: Vec3, dir: Vec3) -> Self {
        Ray {
            orig,
            dir: dir / dir.norm(),
        }
    }
}
