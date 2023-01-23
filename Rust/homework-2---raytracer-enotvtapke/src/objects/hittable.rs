use crate::{Material, Ray, Vec3};

#[derive(PartialEq, Eq, Debug)]
pub enum Face {
    Front,
    Back,
}

pub struct Hit<'a> {
    pub point: Vec3,
    pub distance: f64,
    pub normal: Vec3,
    pub face: Face,
    pub material: &'a Material,
}

impl<'a> Hit<'a> {
    pub fn new(
        point: Vec3,
        normal: Vec3,
        face: Face,
        distance: f64,
        material: &'a Material,
    ) -> Self {
        Hit {
            point,
            normal: normal.normalize(),
            face,
            distance,
            material,
        }
    }

    pub fn from_ray(
        hit_point: Vec3,
        front_normal: Vec3,
        initial_ray_dir: Vec3,
        distance: f64,
        material: &'a Material,
    ) -> Self {
        if initial_ray_dir * front_normal < 0. {
            Hit::new(hit_point, front_normal, Face::Front, distance, material)
        } else {
            Hit::new(
                hit_point,
                -1. * front_normal,
                Face::Back,
                distance,
                material,
            )
        }
    }
}

pub trait Hittable {
    fn hit(&self, ray: Ray) -> Option<Hit>;
}

pub struct HittableSet {
    internal: Vec<Box<dyn Hittable>>,
}

impl HittableSet {
    pub fn new(hittable_list: Vec<Box<dyn Hittable>>) -> Self {
        HittableSet {
            internal: hittable_list,
        }
    }

    pub fn hit(&self, ray: Ray) -> Option<Hit> {
        let mut closest_hit: Option<Hit> = None;
        let mut closest_distance = f64::MAX;
        for hittable in &self.internal {
            if let Some(hit) = hittable.hit(ray) {
                if hit.distance < closest_distance {
                    closest_distance = hit.distance;
                    closest_hit = Some(hit);
                }
            }
        }
        closest_hit
    }
}
