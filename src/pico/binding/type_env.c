#include "data/meta/assoc_header.h"
#include "data/meta/assoc_impl.h"

#include "pico/binding/type_env.h"

typedef enum {
    LQVar,
    LVar,
} LocalSort;

typedef struct {
    LocalSort sort;
    PiType* type;
} Local;

ASSOC_HEADER(Symbol, Local, sym_local, SymLocal)
ASSOC_IMPL(Symbol, Local, sym_local, SymLocal)

struct TypeEnv {
    Environment* env;
    SymLocalAssoc locals; // TODO: reset locals on new proc!
    SymbolArray labels;
};

TypeEnv* mk_type_env(Environment* env, Allocator* a) {
    TypeEnv* t_env = (TypeEnv*)mem_alloc(sizeof(TypeEnv), a);
    t_env->locals = mk_sym_local_assoc(32, a);
    t_env->labels= mk_u64_array(32, a);
    t_env->env = env;
    return t_env; 
}

TypeEntry type_env_lookup(Symbol s, TypeEnv* env) {
    // Search locally
    TypeEntry out;
    Local* lresult = sym_local_alookup(s, env->locals);
    if (lresult != NULL) {
        out.type = TELocal;
        if (lresult->sort == LQVar) {
            out.ptype = NULL;
            out.value = lresult->type;
            return out;
        } else if (lresult -> sort == LVar) {
            out.ptype = lresult->type;
            out.value = NULL;
            return out;
        }
    }

    // Now search globally
    EnvEntry e = env_lookup(s, env->env);
    if (e.success == Err) {
        out.type = TENotFound;
    } else {
        out.type = TEGlobal;
        out.ptype = e.type;
        out.value = e.type->sort == TKind ? e.value : NULL;
    }

    return out;
}

void type_var (Symbol var, PiType* type, TypeEnv* env) {
    sym_local_bind(var, (Local){.sort = LVar, .type = type}, &env->locals);
}

void type_qvar (Symbol var, PiType* type, TypeEnv* env) {
    sym_local_bind(var, (Local){.sort = LQVar, .type = type}, &env->locals);
}

void pop_type(TypeEnv* env) {
    sym_local_unbind(&env->locals);
}

void pop_types(TypeEnv* env, size_t n) {
    sym_local_unbindn(n, &env->locals);
}

bool label_present(Symbol s, TypeEnv* env) {
    return (find_u64(s, env->labels) != env->labels.len);
}

void add_labels (SymbolArray labels, TypeEnv* env) {
    for (size_t i = 0; i < labels.len; i++) {
        push_u64(labels.data[i], &env->labels);
    }
}

void pop_labels(TypeEnv* env, size_t n) {
    sym_local_unbindn(n, &env->locals);
}
