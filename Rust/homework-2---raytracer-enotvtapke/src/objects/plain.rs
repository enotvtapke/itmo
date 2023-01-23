use crate::{Hit, Hittable, Inexact, Material, Ray, Vec3};

pub struct Plain {
    pub point: Vec3,
    pub side1: Vec3,
    pub side2: Vec3,
    pub normal: Vec3,
    pub material: Material,
}

impl Plain {
    pub fn new(point: Vec3, side1: Vec3, side2: Vec3, material: Material) -> Self {
        Plain {
            point,
            side1,
            side2,
            normal: side1.vector_product(side2).normalize(),
            material,
        }
    }
}

impl Hittable for Plain {
    fn hit(&self, ray: Ray) -> Option<Hit> {
        let cos = self.normal * ray.dir;
        if cos.abs().is_zero() {
            return None;
        }
        let d = -1. * self.normal * self.point;
        let t = -(self.normal * ray.orig + d) / (self.normal * ray.dir);
        if t < 0. {
            return None;
        }
        let hit_point = ray.orig + t * ray.dir;
        let projection1 = (hit_point - self.point) * self.side1;
        let projection2 = (hit_point - self.point) * self.side2;

        if projection1 > 0.
            && projection1 < self.side1 * self.side1
            && projection2 > 0.
            && projection2 < self.side2 * self.side2
        {
            Some(Hit::from_ray(
                ray.orig + (t - 1e-5) * ray.dir,
                self.normal,
                ray.dir,
                t,
                &self.material,
            ))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::Face;

    use super::*;

    fn assert_vec_eq(expected: Vec3, actual: Vec3) {
        if !(expected - actual).norm().is_zero() {
            panic!("Expected: {:?}\nFound: {:?}\n", expected, actual)
        }
    }

    #[test]
    fn new() {
        let plain = Plain::new(
            Vec3::new(0., 0., 0.),
            Vec3::new(2., 0., 0.),
            Vec3::new(0., 2., 0.),
            Material::default(),
        );
        assert_eq!(plain.normal, Vec3::new(0., 0., 1.))
    }

    #[test]
    fn hit() {
        let plain = Plain::new(
            Vec3::new(-1., -1., -2.),
            Vec3::new(2., 0., 0.),
            Vec3::new(0., 2., 0.),
            Material::default(),
        );
        let ray = Ray::new(Vec3::new(0., 0., 0.), Vec3::new(0., 0., -1.));
        let hit = plain.hit(ray).unwrap();
        assert_vec_eq(Vec3::new(0., 0., -2.), hit.point);
        assert_vec_eq(Vec3::new(0., 0., 1.), hit.normal);
        assert_eq!(Face::Front, hit.face);
    }

    #[test]
    fn not_hit() {
        let plain = Plain::new(
            Vec3::new(-1., -1., -2.),
            Vec3::new(2., 0., 0.),
            Vec3::new(0., 2., 0.),
            Material::default(),
        );
        let ray = Ray::new(Vec3::new(1.001, 1.001, 0.), Vec3::new(0., 0., -1.));
        assert!(plain.hit(ray).is_none())
    }

    #[test]
    fn hit_45_degrees() {
        let plain = Plain::new(
            Vec3::new(-1., -1., -2.),
            Vec3::new(2., 0., 0.),
            Vec3::new(0., 2., 0.),
            Material::default(),
        );
        let ray = Ray::new(Vec3::new(2., 0., 0.), Vec3::new(-1., 0., -1.));
        let hit = plain.hit(ray).unwrap();
        assert_vec_eq(Vec3::new(0., 0., -2.), hit.point);
        assert_vec_eq(Vec3::new(0., 0., 1.), hit.normal);
        assert_eq!(Face::Front, hit.face);
    }
}
