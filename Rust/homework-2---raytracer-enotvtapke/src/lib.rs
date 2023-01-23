pub use camera::*;
pub use geometry::*;

mod camera;
mod geometry;
mod objects;

fn reflect(i: Vec3, n: Vec3) -> Vec3 {
    i - n * 2. * (i * n)
}

fn refract(i: Vec3, n: Vec3, eta_t: f64, eta_i: f64) -> Vec3 {
    let cosi = -((-1_f64).max(1_f64.min(i * n)));
    if cosi < 0. {
        return refract(i, -1. * n, eta_i, eta_t);
    };
    let eta = eta_i / eta_t;
    let k = 1. - eta * eta * (1. - cosi * cosi);
    if k < 0. {
        Vec3::new(1., 1., 0.)
    } else {
        i * eta + n * (eta * cosi - k.sqrt())
    }
}

pub fn cast_ray(ray: Ray, objects: &HittableSet, lights: &[Light], depth: u8) -> Color {
    if depth == 0 {
        return Color::new(0.2, 0.7, 0.8);
    }

    let hit = objects.hit(ray);
    if hit.is_none() {
        return Color::new(0.2, 0.7, 0.8);
    }
    let hit = hit.unwrap();

    let (mut diffuse_light_intensity, mut specular_light_intensity) = (0., 0.);

    let reflect_dir = reflect(ray.dir, hit.normal).normalize();
    let reflect_color = cast_ray(Ray::new(hit.point, reflect_dir), objects, lights, depth - 1);

    let refract_dir = refract(ray.dir, hit.normal, hit.material.refractive_index, 1.).normalize();
    let refract_color = cast_ray(Ray::new(hit.point, refract_dir), objects, lights, depth - 1);

    for light in lights {
        let light_dir = (light.position - hit.point).normalize();

        let light_distance = (light.position - hit.point).norm();
        if let Some(shadow_hit) = objects.hit(Ray::new(hit.point, light_dir)) {
            if light_distance > (shadow_hit.point - hit.point).norm() {
                continue;
            }
        }

        diffuse_light_intensity += 0_f64.max(light_dir * hit.normal);

        specular_light_intensity += 0_f64
            .max(-1. * reflect(-1. * light_dir, hit.normal) * ray.dir)
            .powf(hit.material.specular_exponent)
    }
    hit.material.diffuse_color * diffuse_light_intensity * hit.material.albedo.0
        + Vec3::unit() * specular_light_intensity * hit.material.albedo.1
        + reflect_color * hit.material.albedo.2
        + refract_color * hit.material.albedo.3
}
