use std::ops::{Add, Div, Mul, Sub};

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Vec3 {
    pub x: f64,
    pub y: f64,
    pub z: f64,
}

impl Vec3 {
    pub const fn new(x: f64, y: f64, z: f64) -> Self {
        Vec3 { x, y, z }
    }

    pub fn norm(self) -> f64 {
        (self * self).sqrt()
    }

    pub fn normalize(&self) -> Self {
        *self / self.norm()
    }

    pub fn zero() -> Self {
        Vec3::new(0., 0., 0.)
    }

    pub fn unit() -> Self {
        Vec3::new(1., 1., 1.)
    }

    pub fn vector_product(self, other: Self) -> Self {
        let i = self.y * other.z - self.z * other.y;
        let j = -(self.x * other.z - self.z * other.x);
        let k = self.x * other.y - self.y * other.x;
        Vec3::new(i, j, k)
    }
}

pub trait Inexact {
    const EPS: Self;

    fn equal(self, other: Self) -> bool;
    fn is_zero(&self) -> bool;
}

impl Inexact for f64 {
    const EPS: f64 = 1e-4;

    fn equal(self, other: Self) -> bool {
        (self - other).abs() < Inexact::EPS
    }

    fn is_zero(self: &f64) -> bool {
        self.equal(0.)
    }
}

impl Mul for Vec3 {
    type Output = f64;

    fn mul(self, other: Self) -> f64 {
        self.x * other.x + self.y * other.y + self.z * other.z
    }
}

impl Mul<f64> for Vec3 {
    type Output = Self;

    fn mul(self, scalar: f64) -> Self {
        Self {
            x: self.x * scalar,
            y: self.y * scalar,
            z: self.z * scalar,
        }
    }
}

impl Mul<Vec3> for f64 {
    type Output = Vec3;

    fn mul(self, vec: Vec3) -> Vec3 {
        Vec3 {
            x: vec.x * self,
            y: vec.y * self,
            z: vec.z * self,
        }
    }
}

impl Add for Vec3 {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
}

impl Sub for Vec3 {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        self + (-1.0) * other
    }
}

impl Div<f64> for Vec3 {
    type Output = Self;

    fn div(self, scalar: f64) -> Self {
        Self {
            x: self.x / scalar,
            y: self.y / scalar,
            z: self.z / scalar,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new() {
        let vec = Vec3::new(-1.523, 0.0, 7.122);
        assert_eq!(vec.x, -1.523);
        assert_eq!(vec.y, 0.0);
        assert_eq!(vec.z, 7.122);
    }

    #[test]
    fn add() {
        let vec1 = Vec3::new(1.0, 5.0, 7.0);
        let vec2 = Vec3::new(-5.0, 10.0, 0.0);
        assert_eq!(Vec3::new(-4.0, 15.0, 7.0), vec1 + vec2);
    }

    #[test]
    fn sub() {
        let vec1 = Vec3::new(1.0, 5.0, 7.0);
        let vec2 = Vec3::new(-5.0, 10.0, 0.0);
        assert_eq!(Vec3::new(6.0, -5.0, 7.0), vec1 - vec2);
    }

    #[test]
    fn mul_on_scalar_right() {
        let vec = Vec3::new(1.0, -5.0, 0.0);
        assert_eq!(Vec3::new(3.0, -15.0, 0.0), vec * 3.0);
    }

    #[test]
    fn mul_on_scalar_left() {
        let vec = Vec3::new(1.0, -5.0, 0.0);
        assert_eq!(Vec3::new(3.0, -15.0, 0.0), 3.0 * vec);
    }

    #[test]
    fn scalar_mul() {
        let (x1, y1, z1) = (1.534_f64, 100.42_f64, -0.555_f64);
        let (x2, y2, z2) = (0.00_f64, -52.1234_f64, 0.01_f64);
        let vec1 = Vec3::new(x1, y1, z1);
        let vec2 = Vec3::new(x2, y2, z2);
        assert_eq!(x1 * x2 + y1 * y2 + z1 * z2, vec1 * vec2);
    }

    #[test]
    fn div_on_scalar() {
        let (x1, y1, z1) = (1.534_f64, 100.42_f64, -0.555_f64);
        let vec = Vec3::new(x1, y1, z1);
        assert_eq!(Vec3::new(x1 / 3.0, y1 / 3.0, z1 / 3.0), vec / 3.0);
    }

    #[test]
    fn norm() {
        let (x1, y1, z1) = (1.534_f64, 100.42_f64, -0.555_f64);
        let vec = Vec3::new(x1, y1, z1);
        assert_eq!((x1 * x1 + y1 * y1 + z1 * z1).sqrt(), vec.norm());
    }

    #[test]
    fn normalize() {
        let (x1, y1, z1) = (1.534_f64, 100.42_f64, -0.555_f64);
        let mut vec = Vec3::new(x1, y1, z1);
        vec = vec.normalize();
        assert!(vec.norm().equal(1.));
    }

    #[test]
    fn scalar_product() {
        println!(
            "{:?}",
            Vec3::new(20., 0., 0.).vector_product(Vec3::new(0., 0., -20.))
        );
    }
}
