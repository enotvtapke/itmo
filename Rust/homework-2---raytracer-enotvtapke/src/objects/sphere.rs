use crate::objects::Hit;
use crate::objects::Hittable;
use crate::objects::Material;
use crate::Ray;
use crate::Vec3;

pub struct Sphere {
    pub center: Vec3,
    pub radius: f64,
    pub material: Material,
}

impl Default for Sphere {
    fn default() -> Self {
        Sphere::new(Vec3::new(0., 0., 0.), 1., Default::default())
    }
}

impl Sphere {
    pub fn new(center: Vec3, radius: f64, material: Material) -> Self {
        Sphere {
            center,
            radius,
            material,
        }
    }
}

impl Hittable for Sphere {
    fn hit(&self, ray: Ray) -> Option<Hit> {
        let a = ray.dir * ray.dir;
        let l = ray.orig - self.center;
        let b = 2. * ray.dir * l;
        let c = l * l - self.radius * self.radius;

        let discriminant = b * b - 4. * a * c;

        let t: f64;
        if c.abs() < 1e-10 {
            // Ray origin approximately on sphere surface
            return None;
        } else if c < 0. {
            // Ray origin inside of the sphere
            t = (-b + discriminant.sqrt()) / (2. * a);
        } else {
            t = (-b - discriminant.sqrt()) / (2. * a);
        }

        if discriminant < 0. || t < 0. {
            return None;
        }

        let point = ray.orig + ray.dir * t;
        Some(Hit::from_ray(
            point,
            point - self.center,
            ray.dir,
            t,
            &self.material,
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::{Face, Inexact};

    use super::*;

    fn assert_vec_eq(expected: Vec3, actual: Vec3) {
        if !(expected - actual).norm().is_zero() {
            panic!("Expected: {:?}\nFound: {:?}\n", expected, actual)
        }
    }

    #[test]
    fn should_intersect_when_ray_origin_inside_of_sphere() {
        let sphere: Sphere = Sphere::default();
        let ray = Ray::new(Vec3::new(0.0, 0.0, 0.0), Vec3::new(1.0, 1.0, 1.0));

        let expected = Vec3::new(1. / 3_f64.sqrt(), 1. / 3_f64.sqrt(), 1. / 3_f64.sqrt());
        let hit = sphere.hit(ray).unwrap();
        assert_vec_eq(expected, hit.point);
        assert_vec_eq(-1. * expected, hit.normal);
        assert_eq!(Face::Back, hit.face);
    }

    #[test]
    fn should_intersect_when_ray_origin_outside_of_sphere() {
        let sphere: Sphere = Default::default();
        let ray = Ray::new(Vec3::new(3.0, 3.0, 3.0), Vec3::new(-1.0, -1.0, -1.0));

        let expected = Vec3::new(1. / 3_f64.sqrt(), 1. / 3_f64.sqrt(), 1. / 3_f64.sqrt());
        assert_vec_eq(expected, sphere.hit(ray).unwrap().point);
    }

    #[test]
    fn should_not_intersect_when_ray_origin_on_sphere() {
        let sphere: Sphere = Default::default();
        let ray = Ray::new(Vec3::new(1.0, 0.0, 0.0), Vec3::new(1.0, 1.0, 1.0));

        assert!(sphere.hit(ray).is_none());
    }

    #[test]
    fn should_intersect_when_ray_touches_sphere() {
        let sphere: Sphere = Default::default();
        let ray = Ray::new(Vec3::new(-1.0, 0.0, -1.0), Vec3::new(0.0, 0.0, 1.0));

        let expected = Vec3::new(-1.0, 0.0, 0.0);
        assert_vec_eq(expected, sphere.hit(ray).unwrap().point);
    }

    #[test]
    fn should_not_intersect() {
        let sphere: Sphere = Default::default();
        let ray = Ray::new(Vec3::new(-1.0, 0.0, -1.0), Vec3::new(-0.001, 0.0, 1.0));
        assert!(sphere.hit(ray).is_none());
    }

    #[test]
    fn should_not_intersect_when_sphere_behind_ray() {
        let sphere: Sphere = Default::default();
        let ray = Ray::new(Vec3::new(0.0, 0.0, 2.0), Vec3::new(0.0, 0.0, 1.0));
        assert!(sphere.hit(ray).is_none());
    }

    #[test]
    fn should_intersect_when_sphere_not_default() {
        let sphere = Sphere {
            center: Vec3::new(3., 3., 3.),
            radius: 2.,
            material: Default::default(),
        };
        let ray = Ray::new(Vec3::new(0., 0., 0.), Vec3::new(1., 1., 2.));

        let expected = Vec3::new(
            2. - 1. / 6_f64.sqrt(),
            2. - 1. / 6_f64.sqrt(),
            2. * (2. - 1. / 6_f64.sqrt()),
        );
        assert_vec_eq(expected, sphere.hit(ray).unwrap().point);
    }
}
