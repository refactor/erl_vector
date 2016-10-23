#include "vsection_math.h"
#include "vector.h"

const size_t SECTION_LEN = (sizeof(fvec_section)/sizeof(float));

float
vec_dotprod(const float* v1, const float* v2, const size_t dim) {
    const size_t section_num = (int)ceil((double)dim / SECTION_LEN);
    fvec_section fvsec1, fvsec2;
    fvec_section sum = simd_init_by(0.f);
    for (size_t i = 0; i < section_num; ++i) {
        fvsec1 = simd_load(v1 + i * SECTION_LEN);
        fvsec2 = simd_load(v2 + i * SECTION_LEN);
        sum = simd_mul_add(fvsec1, fvsec2, sum);
    }
    return simd_horizontal_add(sum);
}

void
vec_log(float* y, const float* x, const size_t dim) {
    const size_t section_num = (int)ceil((double)dim / SECTION_LEN);
    for (size_t i = 0; i < section_num; ++i) {
        fvec_section fvec = simd_load(x + i * SECTION_LEN);
        fvec_section tmp = fastlog(fvec);
        simd_store(y + i * SECTION_LEN, tmp);
    }
}

void
vec_sigmoid(float* a, const float* z, const size_t dim) {
    const size_t section_num = (int)ceil((double)dim / SECTION_LEN);
    for (size_t i = 0; i < section_num; ++i) {
        fvec_section fvec = simd_load(z + i * SECTION_LEN);
        fvec_section tmp = fastsigmoid(fvec);
        simd_store(a + i * SECTION_LEN, tmp);
    }
}

void
vec_sigmoid_prime(float* a, const float* z, const size_t dim) {
    const size_t section_num = (int)ceil((double)dim / SECTION_LEN);
    for (size_t i = 0; i < section_num; ++i) {
        fvec_section fvec = simd_load(z + i * SECTION_LEN);
        fvec_section tmp = fastsigmoid_prime(fvec);
        simd_store(a + i * SECTION_LEN, tmp);
    }
}

float*
vec_add(float* dst_v, const float* src_v1, const float* src_v2, const size_t dim) {
    const size_t section_num = (int)ceil((double)dim / SECTION_LEN);
    fvec_section fvsec1, fvsec2;
    for (size_t i = 0; i < section_num; ++i) {
        fvsec1 = simd_load(src_v1 + i * SECTION_LEN);
        fvsec2 = simd_load(src_v2 + i * SECTION_LEN);
        fvec_section tmp = simd_add(fvsec1, fvsec2);
        simd_store(dst_v + i * SECTION_LEN, tmp);
    }
    return dst_v;
}

float*
vec_sub(float* dst_v, const float* src_v1, const float* src_v2, const size_t dim) {
    const size_t section_num = (int)ceil((double)dim / SECTION_LEN);
    fvec_section fvsec1, fvsec2;
    for (size_t i = 0; i < section_num; ++i) {
        fvsec1 = simd_load(src_v1 + i * SECTION_LEN);
        fvsec2 = simd_load(src_v2 + i * SECTION_LEN);
        fvec_section tmp = simd_sub(fvsec1, fvsec2);
        simd_store(dst_v + i * SECTION_LEN, tmp);
    }
    return dst_v;
}

float*
vec_schurprod(float* dst_v, const float* src_v1, const float* src_v2, const size_t dim) {
    const size_t section_num = (int)ceil((double)dim / SECTION_LEN);
    fvec_section fvsec1, fvsec2;
    for (size_t i = 0; i < section_num; ++i) {
        fvsec1 = simd_load(src_v1 + i * SECTION_LEN);
        fvsec2 = simd_load(src_v2 + i * SECTION_LEN);
        fvec_section tmp = simd_mul(fvsec1, fvsec2);
        simd_store(dst_v + i * SECTION_LEN, tmp);
    }
    return dst_v;
}

// dst_vector = src_vector * scale
float*
vec_scale_mul(float* dst_v, const float* src_v, const size_t dim, const float scale) {
    const size_t section_num = (int)ceil((double)dim / SECTION_LEN);
    const fvec_section vscale = simd_init_by(scale);
    for (size_t i = 0; i < section_num; ++i) {
        fvec_section fvsec = simd_load(src_v + i * SECTION_LEN);
        fvec_section tmp = simd_mul(fvsec, vscale);
        simd_store(dst_v + i * SECTION_LEN, tmp);
    }
    return dst_v;
}

// v1 += v2 * scale
void
vec_add_scalev(float* v1, const float* v2, const size_t dim, const float scale) {
    const size_t section_num = (int)ceil((double)dim / SECTION_LEN);
    const fvec_section vscale = simd_init_by(scale);
    fvec_section fvsec1, fvsec2;
    for (size_t i = 0; i < section_num; ++i) {
        fvsec1 = simd_load(v1 + i * SECTION_LEN);
        fvsec2 = simd_load(v2 + i * SECTION_LEN);
        fvec_section tmp = simd_mul_add(vscale, fvsec2, fvsec1);
        simd_store(v1 + i * SECTION_LEN, tmp);
    }
}

void
vec_transpose(float* dst_mtx[], float* src_mtx[], const int src_row, const int src_col) {
    const int blockrow_num = (int)ceil((double)src_row / SECTION_LEN);
    const int blockrow_rem = SECTION_LEN - (blockrow_num * SECTION_LEN - src_row);
    const int blockcol_num = (int)ceil((double)src_col / SECTION_LEN);
    const int blockcol_rem = SECTION_LEN - (blockcol_num * SECTION_LEN - src_col);

    for (int row = 0; row < blockrow_num; ++row) {
        __m256 m[8] = {simd_init_by(0.f), simd_init_by(0.f), simd_init_by(0.f), simd_init_by(0.f),
                       simd_init_by(0.f), simd_init_by(0.f), simd_init_by(0.f), simd_init_by(0.f) };
        const int row_lmt = (row == (blockrow_num-1) ? blockrow_rem : SECTION_LEN);
        for (int col = 0; col < blockcol_num; ++ col) {
            const int col_lmt = (col == (blockcol_num-1) ? blockcol_rem : SECTION_LEN);
            for (int i = 0; i < row_lmt; ++i) {
                m[i] = simd_load(&src_mtx[row * SECTION_LEN + i][col * SECTION_LEN]);
            }

            vec8f_transpose8x8(m[0], m[1], m[2], m[3], m[4], m[5], m[6], m[7]);

            for (int i = 0; i < col_lmt; ++i) {
                simd_store(&dst_mtx[col * SECTION_LEN + i][row * SECTION_LEN], m[i]);
            }
        }
    }
}
