/****************************  vsectionf256.h   *******************************
* Author:        w
* Date created:  2016-10-10
* Last modified: 2016-10-12
* Version:       0.02
* Project:       vector struct
* Description:
* Header file defining 256-bit floating point vector classes as interface
* to intrinsic functions in x86 microprocessors with AVX instruction set.
*
* Instructions:
* Use Gnu, Intel or Microsoft C++ compiler. Compile for the desired 
* instruction set, which must be at least AVX.
*
* The following float-vector section are defined here:
* fvec_section: a data section in a float-vection
*
* prefix vec8f_ means private function
*        vec8f     Vector of 8 single precision floating point numbers
* prefix simd_ means public function
*
* Each vector object is represented internally in the CPU as a 256-bit register.
* This header file defines operators and functions for these vectors.
*
* For example:
* fvec_section a(1.f, 2.f, 3.f, 4.f), b(5.f, 6.f, 7.f, 8.f), c;
* c = simd_add(a, b);     // now c contains (6.f, 8.f, 10.f, 12.f)
*
*
* (c) Copyright 2016 - 2016 GNU General Public License http://www.gnu.org/licenses
*****************************************************************************/

// check combination of header files
#if defined (VSECTIONF256_H)
#if    VSECTIONF256_H != 2
#error Two different versions of vsectionf256.h included
#endif
#else
#define VSECTIONF256_H  2

#include "instrset.h"  // Select supported instruction set

#if INSTRSET < 7   // AVX required
#error Please compile for the AVX instruction set or higher
#endif

typedef __m256 fvec_section;

#define simd_load(ptr) _mm256_load_ps(ptr)
#define simd_store(ptr, v) _mm256_store_ps(ptr, v)

#define simd_mul_add(a, b, c)  vec8f_mul_add(to_vec8f(a), to_vec8f(b), to_vec8f(c))
#define simd_nmul_add(a, b, c) vec8f_nmul_add(to_vec8f(a), to_vec8f(b), to_vec8f(c))

#define simd_mul(a, b) _mm256_mul_ps(to_vec8f(a), to_vec8f(b))
#define simd_div(a, b) _mm256_div_ps(to_vec8f(a), to_vec8f(b))
#define simd_add(a, b) _mm256_add_ps(to_vec8f(a), to_vec8f(b))
#define simd_sub(a, b) _mm256_sub_ps(to_vec8f(a), to_vec8f(b))
#define simd_round(a)  _mm256_round_ps(to_vec8f(a), 0+8)
#define simd_shift_left(a, n) _Generic((a), __m256:vec8f_shift_left, __m256i: vec8i_shift_left)(a, n)
// trunc fraction: 3.14f -> 3.00f
#define simd_trunc_fraction(a) ({ __m256i w = _mm256_cvttps_epi32(a); _mm256_cvtepi32_ps(w); })
// : 3.14f -> 3 -> 4.2038E-45
#define simd_reinterpret_trunced_f(a) ({__m256i vi = _mm256_cvttps_epi32(a); _mm256_castsi256_ps(vi); })
// reinterpret: 3.14 -> 107852331 -> 107852331.0f
#define simd_reinterpret_f(f) ({ __m256i i = _mm256_castps_si256(f); _mm256_cvtepi32_ps(i); })
// reinterpret: 3.14 -> 107852331
#define simd_init_by(x) _mm256_set1_ps(((float)(x)))
#define simd_unary_minus(a) _mm256_xor_ps(to_vec8f(a), constant8f((int)0x80000000,(int)0x80000000,(int)0x80000000,(int)0x80000000,(int)0x80000000,(int)0x80000000,(int)0x80000000,(int)0x80000000))
#define simd_approx_recipr(a) _mm256_rcp_ps(to_vec8f(a))


// is a < b
#define simd_lessthan(a, b) _mm256_cmp_ps(to_vec8f(a), to_vec8f(b), 1)
#define simd_and(a, b) _mm256_and_ps(to_vec8f(a), to_vec8f(b)) 
#define simd_or(a, b) _mm256_or_ps(to_vec8f(a), to_vec8f(b)) 

// Select between two operands. Corresponds to this pseudocode:
// for (int i = 0; i < 8; i++) result[i] = s[i] ? a[i] : b[i];
// Each byte in s must be either 0 (false) or 0xFFFFFFFF (true). No other values are allowed.
#define simd_select(s, a, b) _mm256_blendv_ps(to_vec8f(b), to_vec8f(a), to_vec8f(s))

#define simd_horizontal_and(a) (_mm256_testc_ps((a),constant8f(-1,-1,-1,-1,-1,-1,-1,-1)) != 0)
#define simd_horizontal_add(a) ({ \
            __m256 t1 = _mm256_hadd_ps(a,a);            \
            __m256 t2 = _mm256_hadd_ps(t1,t1);          \
            __m128 t3 = _mm256_extractf128_ps(t2,1);    \
            __m128 t4 = _mm_add_ss(_mm256_castps256_ps128(t2),t3);  \
            _mm_cvtss_f32(t4);                                      \
        })


static inline __m256i identity_256i(__m256i m) { return m; }
static inline __m128i identity_128i(__m128i m) { return m; }
static inline __m256  identity_256f(__m256  m) { return m; }
static inline __m128  identity_128f(__m128  m) { return m; }

#define to_vec8f(x) _Generic((x), __m256: identity_256f, float: _mm256_set1_ps)(x)
#define to_vec8i(x) _Generic((x), __m256i: identity_256i, int32_t: _mm256_set1_epi32)(x)

#define reinterpret_i(vm) _Generic((vm), __m256i: identity_256i, __m256: _mm256_castps_si256, __m128i:identity_128i, __m128:_mm_castps_si128)(vm)
#define reinterpret_f(vm) _Generic((vm), __m256i: _mm256_castsi256_ps, __m256: identity_256f, __m128i:_mm_castsi128_ps, __m128:identity_128f)(vm)

/*****************************************************************************
*
*          select functions
*
*****************************************************************************/
// Select between two __m256 sources, element by element. Used in various functions 
// and operators. Corresponds to this pseudocode:
// for (int i = 0; i < 8; i++) result[i] = s[i] ? a[i] : b[i];
// Each element in s must be either 0 (false) or 0xFFFFFFFF (true).
static inline __m256 vec8f_selectf (const __m256 s, const __m256 a, const __m256 b) {
    return _mm256_blendv_ps (b, a, s);
}

// Same, with two __m256d sources.
// and operators. Corresponds to this pseudocode:
// for (int i = 0; i < 4; i++) result[i] = s[i] ? a[i] : b[i];
// Each element in s must be either 0 (false) or 0xFFFFFFFFFFFFFFFF (true). No other 
// values are allowed.
static inline __m256d vec8f_selectd (const __m256d s, const __m256d a, const __m256d b) {
    return _mm256_blendv_pd (b, a, s);
}

/*****************************************************************************
*
*         Join two 128-bit 4-float vectors
*
*****************************************************************************/
#define set_m128r(lo,hi) _mm256_insertf128_ps(_mm256_castps128_ps256(lo),(hi),1)
    // _mm256_set_m128(hi,lo); // not defined in all versions of immintrin.h


#define constant8f(i0,i1,i2,i3,i4,i5,i6,i7) _mm256_castsi256_ps(_mm256_setr_epi32(i0,i1,i2,i3,i4,i5,i6,i7))

static inline fvec_section simd_inf_vector() {
    return constant8f(0x7F800000,0x7F800000,0x7F800000,0x7F800000,0x7F800000,0x7F800000,0x7F800000,0x7F800000);
}

static inline __m128 sign_v4f_bit(__m128 a) {
    __m128i t1 = reinterpret_i(a);
    __m128i t2 = _mm_sra_epi32(t1,_mm_cvtsi32_si128(31));
    return reinterpret_f(t2);
}

static inline fvec_section simd_sign_bit(const fvec_section a) {
#if defined __AVX2__ // 256 bit integer vectors are available, AVX2
    __m256i t1 = reinterpret_i(a);    // reinterpret as 32-bit integer
    __m256i t2 = _mm256_sra_epi32(t1, _mm_cvtsi32_si128(31)); // extend sign bit: t1 >> 31
    return reinterpret_f(t2);       // reinterpret as 32-bit Boolean
#elif defined __AVX__
    __m128 low = _mm256_castps256_ps128(a);
    __m128 high = _mm256_extractf128_ps(a, 1);
    return set_m128r(low, high);
#endif
}

static inline __m128 is_v4f_finite(__m128 a) {
    __m128i t1 = reinterpret_i(a); // reinterpret as 32-bit integer
    __m128i t2 = _mm_sll_epi32(t1,_mm_cvtsi32_si128(1)); // t1 << 1 : shift left
    __m128i exponentmask = _mm_set1_epi32(0xFF000000);
    __m128i maskedt2 = _mm_and_si128(t2, exponentmask);  // t2 & 0xFF000000
    __m128i isequal = _mm_cmpeq_epi32(maskedt2, exponentmask);  // (t2 & 0xFF000000) == 0xFF000000
    return reinterpret_f(_mm_xor_si128(isequal, _mm_set1_epi32(-1)));
}

// Function is_finite: gives true for elements that are normal, denormal or zero, 
// false for INF and NAN
// (the underscore in the name avoids a conflict with a macro in Intel's mathimf.h)
static inline fvec_section simd_is_finite(const fvec_section a) {
#if defined __AVX2__
    __m256i t1 = reinterpret_i(a);            // reinterpret as 32-bit integer
    __m256i t2 = _mm256_sll_epi32(t1, _mm_cvtsi32_si128(1)); // shift out sign bit: t1 << 1
    
    // exponent field is not all 1s
    //Vec8ib t3 = Vec8i(t2 & 0xFF000000) != 0xFF000000;
    __m256i t3 = _mm256_set1_epi32(0xFF000000);
    __m256i maskedt2 = _mm256_and_si256(t2, t3);
    // ~(maskedt2 == 0xFF000000)
    __m256i isequal = _mm256_cmpeq_epi32(maskedt2, t3);
    __m256i t = _mm256_xor_si256(isequal, _mm256_set1_epi32(-1)); // ~ : bitwise not
    return reinterpret_f(t);
#elif defined __AVX__
    __m128 low = _mm256_castps256_ps128(a);
    __m128 high = _mm256_extractf128_ps(a, 1);
    __m128 finitelow = is_v4f_finite(low);
    __m128 finitehigh = is_v4f_finite(high);
    return set_m128r(finitelow, finitehigh);
#endif
}

static inline __m128 is_v4f_nan(__m128 a) {
    __m128i t1 = reinterpret_i(a); // reinterpret as 32-bit integer
    __m128i t2 = _mm_sll_epi32(t1,_mm_cvtsi32_si128(1)); // t1 << 1 : shift left
    __m128i exponentmask = _mm_set1_epi32(0xFF000000);  // t3
    __m128i maskedt2 = _mm_and_si128(t2, exponentmask); // t2 & t3: t2 & 0xFF000000
    __m128i t5 = _mm_andnot_si128(exponentmask, t2);    // fraction
    __m128i isequal = _mm_cmpeq_epi32(maskedt2, exponentmask);  // (t2 & 0xFF000000) == 0xFF000000
    __m128i t5_equal_zero = _mm_cmpeq_epi32(t5, _mm_set1_epi32(0));
    __m128i t5_notequal_zero = _mm_xor_si128(t5_equal_zero, _mm_set1_epi32(-1));
    return reinterpret_f(_mm_and_si128(isequal, t5_notequal_zero));
}

static inline fvec_section simd_is_nan(const fvec_section a) {
#if defined __AVX2__        // 256 bit integer vectors are available, AVX2
    __m256i t1 = reinterpret_i(a); // reinterpret as 32-bit integer
    __m256i t2 = _mm256_sll_epi32(t1, _mm_cvtsi32_si128(1)); // shift out sign bit: t1 << 1

    // exponent mask: 0xFF000000;
    __m256i t3 = _mm256_set1_epi32(0xFF000000);
    __m256i t4 = _mm256_and_si256(t2, t3); // exponent
    __m256i t5 = _mm256_andnot_si256(t3,t2);// fraction
    //return Vec8ib(t4 == t3 && t5 != 0);// exponent = all 1s and fraction != 0
    __m256i is_t4_equal_t3 = _mm256_cmpeq_epi32(t4, t3);  // t4 == t3
    __m256i is_equal_zero = _mm256_cmpeq_epi32(t5, _mm256_set1_epi32(0)); // t5 == 0;
    __m256i isnot_equal_zero = _mm256_xor_si256(is_equal_zero, _mm256_set1_epi32(-1)); // t5 != 0
    __m256i res = _mm256_and_si256(is_t4_equal_t3, isnot_equal_zero);
    return reinterpret_f(res);
#elif defined __AVX__
    //return Vec8fb(is_nan(a.get_low()), is_nan(a.get_high()));
    __m128 low = _mm256_castps256_ps128(a);
    __m128 high = _mm256_extractf128_ps(a, 1);
    __m128 nanlow = is_v4f_nan(low);
    __m128 nanhigh = is_v4f_nan(high);
    return set_m128r(nanlow, nanhigh);
#endif
}

static inline __m256i vec8i_shift_left(const __m256i ia, int32_t n) {
#if defined __AVX2__
    __m256i ret = _mm256_sll_epi32(ia, _mm_cvtsi32_si128(n));
#elif defined (__AVX__)
    __m128i low = _mm256_castsi256_si128(ia);
    __m128i high = _mm256_extractf128_si256(ia, 1);
    __m128i sllow= _mm_sll_epi32(low,_mm_cvtsi32_si128(n));
    __m128i slhigh= _mm_sll_epi32(high,_mm_cvtsi32_si128(n));
    __m256i ret = _mm256_insertf128_si256(_mm256_castsi128_si256(sllow),(slhigh),1);
#endif
    return ret;
}
// shift left n places: a << n
static inline __m256 vec8f_shift_left(const __m256 a, int32_t n) {
    __m256i ia = reinterpret_i(a);
    return reinterpret_f(vec8i_shift_left(ia, n));
}

// function abs: absolute value
// Removes sign bit, even for -0.0f, -INF and -NAN
static inline fvec_section simd_abs(const fvec_section a) {
    __m256 mask = constant8f(0x7FFFFFFF,0x7FFFFFFF,0x7FFFFFFF,0x7FFFFFFF,0x7FFFFFFF,0x7FFFFFFF,0x7FFFFFFF,0x7FFFFFFF);
    return _mm256_and_ps(a,mask);
}

// return a * b + c;
static inline __m256 vec8f_mul_add(const __m256 a, const __m256 b, const __m256 c) {
#ifdef __FMA__
    return _mm256_fmadd_ps(a, b, c);
#elif defined (__FMA4__)
    return _mm256_macc_ps(a, b, c);
#else
    return _mm256_add_ps(_mm256_mul_ps(a, b), c);
#endif
}

// Multiply and inverse subtract: c - a * b
static inline __m256 vec8f_nmul_add(const __m256 a, const __m256 b, const __m256 c) {
#ifdef __FMA__
    return _mm256_fnmadd_ps(a, b, c);
#elif defined (__FMA4__)
    return _mm256_nmacc_ps(a, b, c);
#else
    return _mm256_sub_ps(c, _mm256_mul_ps(a, b));
#endif
}


#define vec8f_transpose8x8(row0, row1, row2, row3, row4, row5, row6, row7) do{\
    __m256 __t0, __t1, __t2, __t3, __t4, __t5, __t6, __t7;          \
    __m256 __tt0, __tt1, __tt2, __tt3, __tt4, __tt5, __tt6, __tt7;  \
    __t0 = _mm256_unpacklo_ps(row0, row1);                          \
    __t1 = _mm256_unpackhi_ps(row0, row1);                          \
    __t2 = _mm256_unpacklo_ps(row2, row3);                          \
    __t3 = _mm256_unpackhi_ps(row2, row3);                          \
    __t4 = _mm256_unpacklo_ps(row4, row5);                          \
    __t5 = _mm256_unpackhi_ps(row4, row5);                          \
    __t6 = _mm256_unpacklo_ps(row6, row7);                          \
    __t7 = _mm256_unpackhi_ps(row6, row7);                          \
    __tt0 = _mm256_shuffle_ps(__t0,__t2,_MM_SHUFFLE(1,0,1,0));      \
    __tt1 = _mm256_shuffle_ps(__t0,__t2,_MM_SHUFFLE(3,2,3,2));      \
    __tt2 = _mm256_shuffle_ps(__t1,__t3,_MM_SHUFFLE(1,0,1,0));      \
    __tt3 = _mm256_shuffle_ps(__t1,__t3,_MM_SHUFFLE(3,2,3,2));      \
    __tt4 = _mm256_shuffle_ps(__t4,__t6,_MM_SHUFFLE(1,0,1,0));      \
    __tt5 = _mm256_shuffle_ps(__t4,__t6,_MM_SHUFFLE(3,2,3,2));      \
    __tt6 = _mm256_shuffle_ps(__t5,__t7,_MM_SHUFFLE(1,0,1,0));      \
    __tt7 = _mm256_shuffle_ps(__t5,__t7,_MM_SHUFFLE(3,2,3,2));      \
    row0 = _mm256_permute2f128_ps(__tt0, __tt4, 0x20);              \
    row1 = _mm256_permute2f128_ps(__tt1, __tt5, 0x20);              \
    row2 = _mm256_permute2f128_ps(__tt2, __tt6, 0x20);              \
    row3 = _mm256_permute2f128_ps(__tt3, __tt7, 0x20);              \
    row4 = _mm256_permute2f128_ps(__tt0, __tt4, 0x31);              \
    row5 = _mm256_permute2f128_ps(__tt1, __tt5, 0x31);              \
    row6 = _mm256_permute2f128_ps(__tt2, __tt6, 0x31);              \
    row7 = _mm256_permute2f128_ps(__tt3, __tt7, 0x31);              \
} while(0)

#endif // VSECTIONF256_H
