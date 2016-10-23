#include "erl_nif.h"
#include <stdio.h>

#include <stdlib.h>
#include <math.h>
#include <string.h>

#include <limits.h>

#include <stdint.h>
#include "vector.h"

#define UNUSED __attribute__((unused))

#define ONES {1.0f,1.0f,1.0f,1.0f,1.0f,1.0f,1.0f,1.0f}
#define S_VEC(S) {S, S, S, S, S, S, S, S}

#define EXPAF (8388608/0.6931471806f)

#if defined(__clang__)
#if __has_extension(attribute_ext_vector_type)
typedef float v8sf __attribute__((ext_vector_type(8),aligned(32)));
#endif
#else
/* GCC */
typedef float v8sf __attribute__((vector_size(8*sizeof(float)),aligned(32)));
#endif

#define V8SF_SZ (sizeof(v8sf))
typedef struct vector_res {
    size_t dim;
    size_t sz;
    float* components;
} vector_res;

void* aligned_malloc(size_t size)
{
    // a number of requirements should be met
//    assert(alignment > 0);
//    assert((alignment & (alignment - 1)) == 0); // test for power of 2

    const size_t alignment = 32;
    // assert(aligment >= sizeof(void*)

//    assert(size >= sizeof(void*));
//    assert(size/sizeof(void*)*sizeof(void*) == size);

    // allocate extra memory and convert to size_t to perform calculations
    char* orig = enif_alloc(size + alignment + sizeof(void*));
    // calculate an aligned position in the allocated region
    // assumption: (size_t)orig does not lose lower bits
    char* aligned =
        orig + (
        (((size_t)orig + alignment + sizeof(void*)) & ~(alignment - 1)) -
        (size_t)orig
        );
    // save the original pointer to use it in aligned_free
    *((char**)aligned - 1) = orig;
    return aligned;
}

void del_vector_res(ErlNifEnv* env, void* obj) {
    vector_res* res = (vector_res*)obj;
    if (res != NULL && res->components != NULL) {
        enif_free( *((char**)res->components - 1) );
        //free(res->components);
        res->components = NULL;
        // free(res); // should be freed by enif_release_resource
    }
}

static ErlNifResourceType* VECTOR_RES_TYPE;

static int
open_resource(ErlNifEnv* env)
{
    const char* mod = "vector";
    const char* name = "vector_object";
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;

    VECTOR_RES_TYPE = enif_open_resource_type(env, mod, name, del_vector_res, flags, NULL);
    if(VECTOR_RES_TYPE == NULL) return -1;
    return 0;
}

static int loads = 0;

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    if(open_resource(env) == -1) return -1;

	/* Initialize private data. */
	*priv_data = NULL;

	loads++;

	return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
	/* Convert the private data to the new version. */
	*priv_data = *old_priv_data;

	loads++;

	return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
	if (loads == 1) {
		/* Destroy the private data. */
	}

	loads--;
}

static vector_res* clone_vector_res(const vector_res* ov) {
    void* ptr = NULL;
    if ( (ptr = aligned_malloc(ov->sz * V8SF_SZ)) == NULL) {
        return NULL;
    }
    memcpy(ptr, ov->components, ov->sz * V8SF_SZ);

    vector_res* res = enif_alloc_resource(VECTOR_RES_TYPE, sizeof(*res));
    if(res == NULL) {
        free(ptr);
        return NULL;
    }
    
    res->components = ptr;
    res->sz = ov->sz;
    res->dim = ov->dim;
    return res;
}
static vector_res* make_vector_res(size_t dim) {
    size_t sz = (int)ceil((double)dim / SECTION_LEN);
    void* ptr = NULL;
//    int ret = 0; 
//    if ((ret = posix_memalign(&ptr, 32, sz * V8SF_SZ)) != 0) {
//        printf("eRR: %d\r\n", ret);
    if ( (ptr = aligned_malloc(sz * V8SF_SZ)) == NULL) {
        return NULL;
    }
    //vector_res* res = (vector_res*)malloc(sizeof(*res));
    vector_res* res = enif_alloc_resource(VECTOR_RES_TYPE, sizeof(*res));
    if(res == NULL) {
        free(ptr);
        return NULL;
    }
    memset(ptr, 0, sz * V8SF_SZ);
    res->components = ptr;
    res->sz = sz;
    res->dim = dim;
    return res;
}

static ERL_NIF_TERM stub_dot_prod(ErlNifEnv* env, int UNUSED argc, const ERL_NIF_TERM argv[])
{
    vector_res *vec_res1, *vec_res2;
    if (!enif_get_resource(env, argv[0], VECTOR_RES_TYPE, (void**)&vec_res1) ||
        !enif_get_resource(env, argv[1], VECTOR_RES_TYPE, (void**)&vec_res2)) {
        return enif_make_badarg(env);
    }
    
    if (vec_res1->dim != vec_res2->dim) {
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "unmatched_dimension"));
    }

    float res = vec_dotprod(vec_res1->components, vec_res2->components, vec_res1->dim);
    return enif_make_double(env, res);
}

static ERL_NIF_TERM sumlist(ErlNifEnv* env, int UNUSED argc, const ERL_NIF_TERM argv[])
{
    unsigned len = 0;
    if (!enif_get_list_length(env, argv[0], &len))
        return enif_make_badarg(env);

    if (len == 0)
        return argv[0];

    vector_res *vec_res = NULL;
    ERL_NIF_TERM head, tail;
    if (!enif_get_list_cell(env, argv[0], &head, &tail) || 
        !enif_get_resource(env, head, VECTOR_RES_TYPE, (void**)&vec_res)) {
            return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "badarg"));
    }

    if (len == 1)
        return head;

    vector_res *vec_sum = clone_vector_res(vec_res);
    
    while (enif_get_list_cell(env, tail, &head, &tail) && 
           enif_get_resource(env, head, VECTOR_RES_TYPE, (void**)&vec_res)) {
        vec_add(vec_sum->components, vec_sum->components, vec_res->components, vec_res->dim);
    }

    ERL_NIF_TERM ret = enif_make_resource(env, vec_sum);
    enif_release_resource(vec_sum);
    return ret;
}

static ERL_NIF_TERM stub_add_scalev(ErlNifEnv* env, int UNUSED argc, const ERL_NIF_TERM argv[])
{
    vector_res *vec_res1, *vec_res2;
    double scale = 1.0;
    if (!enif_get_resource(env, argv[0], VECTOR_RES_TYPE, (void**)&vec_res1) ||
        !enif_get_resource(env, argv[1], VECTOR_RES_TYPE, (void**)&vec_res2) ||
        !enif_get_double(env, argv[2], &scale))
        return enif_make_badarg(env);

    if (vec_res1->dim != vec_res2->dim)
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"), enif_make_atom(env, "unmatched_dimension"));

    vector_res* res = clone_vector_res(vec_res1);
    if (res == NULL) 
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "OOM"));

    const float fscale = scale;
    vec_add_scalev(res->components, vec_res2->components, res->dim, fscale);

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

static ERL_NIF_TERM stub_add(ErlNifEnv* env, int UNUSED argc, const ERL_NIF_TERM argv[])
{
    vector_res *vec_res1, *vec_res2;
    if (!enif_get_resource(env, argv[0], VECTOR_RES_TYPE, (void**)&vec_res1) ||
        !enif_get_resource(env, argv[1], VECTOR_RES_TYPE, (void**)&vec_res2))
        return enif_make_badarg(env);
    
    if (vec_res1->dim != vec_res2->dim)
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"), enif_make_atom(env, "unmatched_dimension"));

    vector_res* res = make_vector_res(vec_res1->dim);
    if (res == NULL) 
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "OOM"));

    vec_add(res->components, vec_res1->components, vec_res2->components, vec_res1->dim);

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

static ERL_NIF_TERM cost_derivative(ErlNifEnv* env, int UNUSED argc, const ERL_NIF_TERM argv[])
{
    vector_res *vec_res1, *vec_res2;
    if (!enif_get_resource(env, argv[0], VECTOR_RES_TYPE, (void**)&vec_res1) ||
        !enif_get_resource(env, argv[1], VECTOR_RES_TYPE, (void**)&vec_res2))
        return enif_make_badarg(env);
    
    if (vec_res1->dim != vec_res2->dim)
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"), enif_make_atom(env, "unmatched_dimension"));

    vector_res* res = make_vector_res(vec_res1->dim);
    if (res == NULL)
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "OOM"));

    vec_sub(res->components, vec_res1->components, vec_res2->components, vec_res1->dim);

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

static ERL_NIF_TERM stub_schur_prod(ErlNifEnv* env, int UNUSED argc, const ERL_NIF_TERM argv[])
{
    vector_res *vec_res1, *vec_res2;
    if (!enif_get_resource(env, argv[0], VECTOR_RES_TYPE, (void**)&vec_res1) ||
        !enif_get_resource(env, argv[1], VECTOR_RES_TYPE, (void**)&vec_res2))
        return enif_make_badarg(env);
    
    if (vec_res1->dim != vec_res2->dim)
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"), enif_make_atom(env, "unmatched_dimension"));

    vector_res *res = make_vector_res(vec_res1->dim);
    if (res == NULL)
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "OOM"));
    
    vec_schurprod(res->components, vec_res1->components, vec_res2->components, vec_res1->dim);

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

static ERL_NIF_TERM v_multiply_v(ErlNifEnv* env, int UNUSED argc, const ERL_NIF_TERM argv[])
{
    vector_res *vec_res1, *vec_res2;
    if (!enif_get_resource(env, argv[0], VECTOR_RES_TYPE, (void**)&vec_res1) ||
        !enif_get_resource(env, argv[1], VECTOR_RES_TYPE, (void**)&vec_res2))
        return enif_make_badarg(env);
    
    ERL_NIF_TERM arr[vec_res1->dim];
    for (size_t i = 0; i < vec_res1->dim; ++i) {
        vector_res *res = make_vector_res(vec_res2->dim);
        if (res == NULL) {
            return enif_make_tuple2(env,
                    enif_make_atom(env, "error"), enif_make_atom(env, "OOM"));
        }

        const float f = vec_res1->components[i];
        vec_scale_mul(res->components, vec_res2->components, vec_res2->dim, f);

        ERL_NIF_TERM rv = enif_make_resource(env, res);
        enif_release_resource(res);
        arr[i] = rv;
    }

    return enif_make_list_from_array(env, arr, sizeof(arr)/sizeof(arr[0]));
}

static ERL_NIF_TERM stub_transpose(ErlNifEnv* env,int UNUSED argc, const ERL_NIF_TERM argv[] ) {
    unsigned len = 0;
    if (!enif_get_list_length(env, argv[0], &len))
        return enif_make_badarg(env);

    ERL_NIF_TERM head, tail;
    vector_res *vec_res = NULL;
    if (!enif_get_list_cell(env, argv[0], &head, &tail) || 
        !enif_get_resource(env, head, VECTOR_RES_TYPE, (void**)&vec_res)) {
            return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "badarg"));
    }
    
    const int veclen = vec_res->dim;
    vector_res* vectors[veclen];
    for (int i = 0; i < veclen; ++i) {
        vector_res *res = make_vector_res(len);
        if (res == NULL) {
            return enif_make_tuple2(env,
                    enif_make_atom(env, "error"), enif_make_atom(env, "OOM"));
        }
        vectors[i] = res;
    }
        
    int j = 0;
    do {
        for (size_t i = 0; i < vec_res->dim; ++i) {
            vectors[i]->components[j] = vec_res->components[i];
        }
        ++j;
    } while (enif_get_list_cell(env, tail, &head, &tail) && 
             enif_get_resource(env, head, VECTOR_RES_TYPE, (void**)&vec_res));
    
    ERL_NIF_TERM arr[veclen];
    for (int i = 0; i < veclen; ++i) {
        arr[i] = enif_make_resource(env, vectors[i]);
        enif_release_resource(vectors[i]);
    }
    return enif_make_list_from_array(env, arr, sizeof(arr)/sizeof(arr[0]));
}

static ERL_NIF_TERM build_vector(ErlNifEnv* env,int UNUSED argc, const ERL_NIF_TERM argv[] ) {
    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[0], &bin)) {
        return enif_make_badarg(env);
    }

    vector_res* res = make_vector_res(bin.size/sizeof(float));
    if (res == NULL) {
        return enif_make_tuple2(env,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "OOM"));
    }

    memcpy(res->components, bin.data, bin.size);

//    ERL_NIF_TERM ret = enif_make_resource_binary(env, res, ptr, sz * V8SF_SZ);
    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

static ERL_NIF_TERM get_vectorbin(ErlNifEnv* env,int UNUSED argc, const ERL_NIF_TERM argv[] ) {
    vector_res* vec_res = NULL;
    if (!enif_get_resource(env, argv[0], VECTOR_RES_TYPE, (void**)&vec_res)) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM binTerm;
    unsigned char* data = enif_make_new_binary(env, vec_res->dim * sizeof(float), &binTerm);
    memcpy(data, vec_res->components, vec_res->dim * sizeof(float));

    return binTerm;
}

static v8sf exp_v(v8sf z) {
    v8sf res;
    res[0] = expf(z[0]);
    res[1] = expf(z[1]);
    res[2] = expf(z[2]);
    res[3] = expf(z[3]);
    res[4] = expf(z[4]);
    res[5] = expf(z[5]);
    res[6] = expf(z[6]);
    res[7] = expf(z[7]);
    return res; 
}

static ERL_NIF_TERM mexp_vector(ErlNifEnv* env,int UNUSED argc, const ERL_NIF_TERM argv[] ) {
    vector_res* vec_res;
    if (!enif_get_resource(env, argv[0], VECTOR_RES_TYPE, (void**)&vec_res))
        return enif_make_badarg(env);

    vector_res* res = make_vector_res(vec_res->dim);
    if (res == NULL) 
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "OOM"));

    for (size_t i = 0; i < vec_res->dim; ++i) {
        res->components[i] = expf(vec_res->components[i]);
    }

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

static ERL_NIF_TERM log_vector(ErlNifEnv* env,int UNUSED argc, const ERL_NIF_TERM argv[] ) {
    vector_res* vec_res;
    if (!enif_get_resource(env, argv[0], VECTOR_RES_TYPE, (void**)&vec_res))
        return enif_make_badarg(env);

    vector_res* res = make_vector_res(vec_res->dim);
    if (res == NULL) 
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "OOM"));

    vec_log(res->components, vec_res->components, vec_res->dim);

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

static ERL_NIF_TERM stub_sigmoid(ErlNifEnv* env,int UNUSED argc, const ERL_NIF_TERM argv[] ) {
    vector_res* vec_res;
    if (!enif_get_resource(env, argv[0], VECTOR_RES_TYPE, (void**)&vec_res))
        return enif_make_badarg(env);

    vector_res* res = make_vector_res(vec_res->dim);
    if (res == NULL) 
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "OOM"));

    vec_sigmoid(res->components, vec_res->components, vec_res->dim);

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

static ERL_NIF_TERM stub_sigmoid_prime(ErlNifEnv* env,int UNUSED argc, const ERL_NIF_TERM argv[] ) {
    vector_res* vec_res;
    if (!enif_get_resource(env, argv[0], VECTOR_RES_TYPE, (void**)&vec_res))
        return enif_make_badarg(env);

    vector_res* res = make_vector_res(vec_res->dim);
    if (res == NULL) 
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "OOM"));

    vec_sigmoid_prime(res->components, vec_res->components, vec_res->dim);

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

static ERL_NIF_TERM argmax(ErlNifEnv* env,int UNUSED argc, const ERL_NIF_TERM argv[] ) {
    vector_res* vec;
    if (!enif_get_resource(env, argv[0], VECTOR_RES_TYPE, (void**)&vec)) {
        return enif_make_badarg(env);
    }

    float max = vec->components[0];
    size_t idx = 0;
    for (size_t i = 1; i < vec->dim; ++i) {
        if (max < vec->components[i] ) {
            max = vec->components[i];
            idx = i;
        }
    }
    return enif_make_uint(env, idx);
}

static ErlNifFunc nif_funcs[] = {
    {"build_vector",    1, build_vector},
    {"transpose",       1, stub_transpose},
    {"dot_prod",        2, stub_dot_prod},
	{"add",             2, stub_add},
	{"add_scalev",      3, stub_add_scalev},
    {"schur_prod",      2, stub_schur_prod},
    {"sigmoid",         1, stub_sigmoid},
    {"sigmoid_prime",   1, stub_sigmoid_prime},
    {"cost_derivative", 2, cost_derivative},
    {"v_multiply_v",    2, v_multiply_v},
    {"argmax",          1, argmax},
    {"sumlist",         1, sumlist},
    {"get_vectorbin",   1, get_vectorbin}
    ,{"log_vector",    1, log_vector}
    ,{"mexp_vector",   1, mexp_vector}
};

ERL_NIF_INIT(vector, nif_funcs, load, NULL, upgrade, unload)
