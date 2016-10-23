#ifndef VSECTION_MATH_H
#define VSECTION_MATH_H  1

#include "vsection.h"   // choose vsectionf256.h
#include "vsectionmath_common.h"  

static inline fvec_section fastlog2(const fvec_section x) {
    fvec_section y = simd_reinterpret_f(x);
    union { uint32_t i; float f; } c = {(uint32_t)0x007FFFFF};
    union { uint32_t i; float f; } c2 = {(uint32_t)0x3f000000};
    fvec_section mxf = simd_or(simd_and(x, c.f), c2.f);
    y = simd_mul(y, 1.1920928955078125e-7f);
    //x = simd_nmul_add(r, ln2f_hi, x);  // x -= r * ln2f_hi;
    fvec_section tmp = simd_nmul_add(1.498030302f, mxf, simd_sub(y, 124.22551499f));
    return simd_sub(tmp, simd_div(1.72587999f, simd_add(0.3520887068f, mxf)));
}

static inline fvec_section fastlog(const fvec_section x) {
    return simd_mul(0.69314718f, fastlog2(x));
}

static inline fvec_section fasterlog(const fvec_section x) {
    fvec_section y = simd_reinterpret_f(x);
    return simd_mul_add(y, 8.2629582881927490e-8f, -87.989971088f);
}

static inline fvec_section fasterlog2(const fvec_section x) {
    fvec_section y = simd_reinterpret_f(x);
    return simd_mul_add(y, 1.1920928955078125e-7f, -126.94269504f);
}

static inline fvec_section polynomial_5(const fvec_section x, float c0, float c1, float c2, float c3, float c4, float c5) {
    // calculates polynomial c5*x^5 + c4*x^4 + c3*x^3 + c2*x^2 + c1*x + c0
    // VTYPE may be a vector type, CTYPE is a scalar type
    __typeof(x) x2 = simd_mul(x, x);
    __typeof(x) x4 = simd_mul(x2, x2);
    //return (c2+c3*x)*x2 + ((c4+c5*x)*x4 + (c0+c1*x));
    return simd_mul_add(simd_mul_add(c3, x, c2), x2, simd_mul_add(simd_mul_add(c5, x, c4), x4, simd_mul_add(c1, x, c0)));
}

// This function calculates pow(2,n) where n must be an integer. Does not check for overflow or underflow
static inline fvec_section vm_pow2n(const fvec_section n) {
    const float pow2_23 =  8388608.0;            // 2^23
    const float bias = 127.0;                    // bias in exponent
    fvec_section a = simd_add(n, bias + pow2_23);  // put n + bias in least significant bits
    return simd_shift_left(a, 23);      // a[:] << 23
}

static inline fvec_section check_overflow(fvec_section initial_x, fvec_section z) {
    // check for overflow: inrange  = abs(initial_x) < max_x;
    const float max_x = 87.3f;
    fvec_section inrange;  // BTYPE
    inrange = simd_lessthan(simd_abs(initial_x), max_x);
    // check for INF and NAN
    // inrange &= is_finite(initial_x);
    inrange = simd_and(inrange, simd_is_finite(initial_x)); 

    if (simd_horizontal_and(inrange)) {
        // fast normal path
        return z;
    }
    else {
        // overflow, underflow simd_and NAN
        fvec_section sbit = simd_sign_bit(initial_x);
        fvec_section inf_v = simd_inf_vector();
        
        fvec_section r = simd_select(sbit, 0.f, inf_v); // value in case of +/- overflow or INF
        z = simd_select(inrange, z, r);    // +/- underflow
        z = simd_select(simd_is_nan(initial_x), initial_x, z); // NAN goes through
        return z;
    }
}

static inline fvec_section exp_f(const fvec_section initial_x) {
    // Taylor coefficients
    const float P0expf   =  1.f/2.f;
    const float P1expf   =  1.f/6.f;
    const float P2expf   =  1.f/24.f;
    const float P3expf   =  1.f/120.f; 
    const float P4expf   =  1.f/720.f; 
    const float P5expf   =  1.f/5040.f; 

    fvec_section x, r, x2, z, n2; // VTYPE

    const float ln2f_hi  =  0.693359375f;
    const float ln2f_lo  = -2.12194440e-4f;
    
    x = initial_x;
    r = simd_round(simd_mul(initial_x, (float)VM_LOG2E));
    x = simd_nmul_add(r, ln2f_hi, x);  // x -= r * ln2f_hi;
    x = simd_nmul_add(r, ln2f_lo, x);  // x -= r * ln2f_lo;

    x2 = simd_mul(x, x);
    z = polynomial_5(x,P0expf,P1expf,P2expf,P3expf,P4expf,P5expf);    
    z = simd_mul_add(z, x2, x);                       // z *= x2;  z += x;

    // multiply by power of 2 
    n2 = vm_pow2n(r);

    //z = (z + 1.0f) * n2;
    z = simd_mul(simd_add(z, 1.0f), n2);

    return check_overflow(initial_x, z);
}

static inline fvec_section vm_fastpow2(const fvec_section p) {
    // (p < 0) ? 1.0f : 0.0f
    fvec_section inrange = simd_lessthan(p, 0.f);
    const fvec_section offset = simd_select(inrange, 1.f, 0.f);    // +/- underflow

    // (p < -126) ? -126.0f : p
    inrange = simd_lessthan(p, -126.f);
    const fvec_section clipp = simd_select(inrange, -126.f, p);

    const fvec_section z = simd_add(simd_sub(clipp, simd_trunc_fraction(clipp)), offset);

    const int32_t one_shiftlef_23 = 1 << 23;
    fvec_section tmp = simd_mul( simd_init_by(one_shiftlef_23), 
                           simd_sub(simd_add(simd_add(clipp, 121.2740575f),
                                             simd_div(27.7280233f, simd_sub(4.84252568f, z))),
                                    simd_mul(1.49012907f, z)));
    return simd_reinterpret_trunced_f(tmp);
}

static inline fvec_section fastexp_f(const fvec_section p) {
    fvec_section z = vm_fastpow2(simd_mul(p, 1.442695040f));
    return check_overflow(p, z);
}

static inline fvec_section fastsigmoid(const fvec_section v) {
    //return simd_div(1.f,simd_add(1.f, fastexp_f(simd_unary_minus(v))));
    return simd_approx_recipr(simd_add(1.f, fastexp_f(simd_unary_minus(v))));
}

static inline fvec_section fastsigmoid_prime(const fvec_section v) {
    fvec_section sigmoid = fastsigmoid(v);
    return simd_mul(sigmoid, simd_sub(1.f, sigmoid));
}

#endif // VSECTION_MATH_H
