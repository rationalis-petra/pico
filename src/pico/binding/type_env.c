#include "pico/binding/type_env.h"
#include "pico/data/sym_ptr_assoc.h"

struct TypeEnv {
    Environment* env;
    SymPtrAssoc locals; // TODO: reset locals
};

TypeEnv* mk_type_env(Environment* env, Allocator* a) {
    TypeEnv* t_env = (TypeEnv*)mem_alloc(sizeof(TypeEnv), a);
    t_env->locals = mk_sym_ptr_assoc(32, a);
    t_env->env = env;
    return t_env; 
}

TypeEntry type_env_lookup(Symbol s, TypeEnv* env) {
    // Search locally
    TypeEntry out;
    PiType** lresult = (PiType**) sym_ptr_alookup(s, env->locals);
    if (lresult != NULL) {
        out.type = TELocal;
        out.ptype = *lresult;
        return out;
    }

    // Now search globally
    EnvEntry e = env_lookup(s, env->env);
    if (e.success == Err) {
        out.type = TENotFound;
    } else {
        out.type = TEGlobal;
        out.ptype = e.type;
    }

    return out;
}

void type_var (Symbol var, PiType* type, TypeEnv* env) {
    sym_ptr_bind(var, type, &env->locals);
}

void pop_type(TypeEnv* env) {
    sym_ptr_unbind(&env->locals);
}

void pop_types(TypeEnv* env, size_t n) {
    sym_ptr_unbindn(n, &env->locals);
}
