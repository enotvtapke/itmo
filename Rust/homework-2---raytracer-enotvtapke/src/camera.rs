use crate::{Ray, Vec3};

pub struct Camera {
    pub resolution: (u32, u32),
    pub fov: f64,
    pub focal_length: f64,
    pub origin: Vec3,

    lower_left_corner: Vec3,
    horizontal: Vec3,
    vertical: Vec3,
}

impl Camera {
    pub fn new(resolution: (u32, u32), fov: f64, focal_length: f64, origin: Vec3) -> Self {
        let width = resolution.0;
        let height = resolution.1;
        let aspect_ratio = width as f64 / height as f64;

        let viewport_width = 2. * (fov / 360. * std::f64::consts::PI).tan() * focal_length;
        let viewport_height = viewport_width / aspect_ratio;

        let horizontal = Vec3::new(viewport_width, 0., 0.);
        let vertical = Vec3::new(0., viewport_height, 0.);
        let lower_left_corner =
            origin - horizontal / 2. - vertical / 2. - Vec3::new(0., 0., focal_length);
        Camera {
            resolution,
            fov,
            focal_length,
            origin,
            lower_left_corner,
            horizontal,
            vertical,
        }
    }

    pub fn get_ray(&self, x: u32, y: u32) -> Ray {
        let u = x as f64 / (self.resolution.0 - 1) as f64;
        let v = y as f64 / (self.resolution.1 - 1) as f64;
        Ray::new(
            self.origin,
            self.lower_left_corner + u * self.horizontal + v * self.vertical - self.origin,
        )
    }
}
