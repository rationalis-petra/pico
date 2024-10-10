#include "data/meta/assoc_header.h"
#include "data/meta/assoc_impl.h"

#include "pico/binding/type_env.h"

typedef struct {
    bool is_qvar;
    PiType* type;
} Local;

ASSOC_HEADER(Symbol, Local, sym_local, SymLocal)
ASSOC_IMPL(Symbol, Local, sym_local, SymLocal)

struct TypeEnv {
    Environment* env;
    SymLocalAssoc locals; // TODO: reset locals
};

TypeEnv* mk_type_env(Environment* env, Allocator* a) {
    TypeEnv* t_env = (TypeEnv*)mem_alloc(sizeof(TypeEnv), a);
    t_env->locals = mk_sym_local_assoc(32, a);
    t_env->env = env;
    return t_env; 
}

TypeEntry type_env_lookup(Symbol s, TypeEnv* env) {
    // Search locally
    TypeEntry out;
    Local* lresult = sym_local_alookup(s, env->locals);
    if (lresult != NULL) {
        out.type = TELocal;
        if (lresult->is_qvar) {
            out.ptype = NULL;
            out.value = lresult->type;
        } else {
            out.ptype = lresult->type;
            out.value = NULL;
        }
        return out;
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
    sym_local_bind(var, (Local){.is_qvar = false, .type = type}, &env->locals);
}

void type_qvar (Symbol var, PiType* type, TypeEnv* env) {
    sym_local_bind(var, (Local){.is_qvar = true, .type = type}, &env->locals);
}

void pop_type(TypeEnv* env) {
    sym_local_unbind(&env->locals);
}

void pop_types(TypeEnv* env, size_t n) {
    sym_local_unbindn(n, &env->locals);
}
