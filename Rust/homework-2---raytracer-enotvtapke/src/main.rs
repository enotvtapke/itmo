use raytracer::*;

fn main() {
    let width = 1024;
    let height = 768;
    let fov: f64 = 60.0;
    let focal_length = 1_f64;
    let origin = Vec3::new(0., 0., 0.);
    let resolution = (width, height);

    let camera = Camera::new(resolution, fov, focal_length, origin);

    let mut imgbuf = image::ImageBuffer::new(width, height);

    let objects = HittableSet::new(vec![
        Box::new(Sphere::new(Vec3::new(-3., 0., -16.), 2.0, IVORY.clone())),
        Box::new(Sphere::new(Vec3::new(-1., -1.5, -12.), 2.0, GLASS.clone())),
        Box::new(Sphere::new(
            Vec3::new(1.5, -0.5, -18.),
            3.0,
            RED_RUBBER.clone(),
        )),
        Box::new(Sphere::new(Vec3::new(7., 5., -18.), 4.0, MIRROR.clone())),
        Box::new(Plain::new(
            Vec3::new(-10., -4., -10.),
            Vec3::new(20., 0., 0.),
            Vec3::new(0., 0., -20.),
            RED_RUBBER.clone(),
        )),
    ]);

    let lights = vec![
        Light::new(Vec3::new(-20., 20., 20.), 1.),
        Light::new(Vec3::new(30., 50., -25.), 1.),
        Light::new(Vec3::new(30., 20., 30.), 1.),
    ];

    // From top left to bottom right
    for (x, y, pixel) in imgbuf.enumerate_pixels_mut() {
        let ray = camera.get_ray(x, height - 1 - y);
        let color = cast_ray(ray, &objects, &lights, 5);
        *pixel = image::Rgb(color.as_rgb());
    }

    imgbuf.save("result.bmp").unwrap();
}
