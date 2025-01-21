#include "platform/signals.h"
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
            out.is_module = false;
            out.ptype = NULL;
            out.value = lresult->type;
            return out;
        } else if (lresult -> sort == LVar) {
            out.is_module = false;
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
        out.is_module = e.is_module;
        out.ptype = e.type;
        if (!e.is_module) {
            out.value = (e.type->sort == TKind || e.type->sort == TConstraint) ? e.value : NULL;
        } else {
            out.module = e.value;
        }
    }

    return out;
}

InstanceEntry type_instance_lookup(uint64_t id, PtrArray args, TypeEnv* env) {
    PtrArray* instances = env_implicit_lookup(id, env->env);
    if (!instances) {
        return (InstanceEntry) {.type = IENotFound,};
    }

    size_t eql_count = 0;
    InstanceEntry out = {.type = IENotFound,};
    for (size_t i = 0; i < instances->len; i++) {
        InstanceSrc* src = instances->data[i];
        bool eql = true;
        for (size_t i = 0 ; i < args.len; i++) {
            eql &=  pi_type_eql(src->args.data[i], args.data[i]);
        }
        if (eql) {
            if (eql_count > 0) {
                return (InstanceEntry) {.type = IEAmbiguous,};
            }
            eql_count++;

            ModuleEntry* m_entry = get_def(src->src_sym, src->src);
            if (!m_entry) panic(mv_string("Module entry is null!"));

            if (m_entry->is_module) {
                panic(mv_string("env_lookup cannot yet handle modules"));
            }

            out = (InstanceEntry) {
                .type = IEAbsSymbol,
                .abvar.index = 0,
                .abvar.value = m_entry->value,
            };
        }
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
