use crate::Vec3;

pub type Color = Vec3;

impl Color {
    pub fn as_rgb(&self) -> [u8; 3] {
        [
            (256. * self.x) as u8,
            (256. * self.y) as u8,
            (256. * self.z) as u8,
        ]
    }
}

#[derive(Clone, Debug)]
pub struct Material {
    pub refractive_index: f64,
    pub albedo: (f64, f64, f64, f64),
    pub diffuse_color: Color,
    pub specular_exponent: f64,
}

impl Default for Material {
    fn default() -> Self {
        Material {
            refractive_index: 0.,
            albedo: (2., 0., 0., 0.),
            diffuse_color: Color::zero(),
            specular_exponent: 0.,
        }
    }
}

pub static IVORY: Material = Material {
    refractive_index: 1.0,
    albedo: (0.9, 0.5, 0.1, 0.0),
    diffuse_color: Color {
        x: 0.4,
        y: 0.4,
        z: 0.3,
    },
    specular_exponent: 50.,
};

pub static GLASS: Material = Material {
    refractive_index: 1.5,
    albedo: (0.0, 0.9, 0.1, 0.8),
    diffuse_color: Color {
        x: 0.6,
        y: 0.7,
        z: 0.8,
    },
    specular_exponent: 125.,
};

pub static RED_RUBBER: Material = Material {
    refractive_index: 1.0,
    albedo: (1.4, 0.3, 0.0, 0.0),
    diffuse_color: Color::new(0.3, 0.1, 0.1),
    specular_exponent: 10.,
};

pub static MIRROR: Material = Material {
    refractive_index: 1.0,
    albedo: (0.0, 16.0, 0.8, 0.0),
    diffuse_color: Color::new(1.0, 1.0, 1.0),
    specular_exponent: 1425.,
};
