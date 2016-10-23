extern const size_t SECTION_LEN;

void vec_log(float* y, const float* x, const size_t dim);
extern void vec_sigmoid(float* a, const float* z, const size_t dim);
extern void vec_sigmoid_prime(float* a, const float* z, const size_t dim);

extern float vec_dotprod(const float* v1, const float* v2, const size_t dim);
extern float* vec_schurprod(float* dst_v, const float* src_v1, const float* src_v2, const size_t dim);
extern float* vec_add(float* dst_v, const float* src_v1, const float* src_v2, const size_t dim);
extern float* vec_sub(float* dst_v, const float* src_v1, const float* src_v2, const size_t dim);
extern float* vec_scale_mul(float* dst_v, const float* src_v, const size_t dim, const float scale);

extern void vec_add_scalev(float* v1, const float* v2, const size_t dim, const float scale);
extern void vec_transpose(float* dst_mtx[], float* src_mtx[], const int src_row, const int src_col);
