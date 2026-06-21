#include "platform/signals.h"
#include "platform/memory/arena.h"
#include "data/meta/assoc_impl.h"

#include "pico/binding/type_env.h"
#include "pico/typecheck/unify.h"

ASSOC_CMP_IMPL(Symbol, Local, symbol_cmp, sym_local, SymLocal)

struct TypeEnv {
    Environment* env;
    SymLocalAssoc locals; // TODO: reset locals on new proc!
    SymPtrAssoc labels;
    Allocator* gpa;
};

TypeEnv* mk_type_env(Environment* env, Allocator* a) {
    TypeEnv* t_env = (TypeEnv*)mem_alloc(sizeof(TypeEnv), a);
    *t_env = (TypeEnv){
        .locals = mk_sym_local_assoc(32, a),
        .labels = mk_sym_ptr_assoc(32, a),
        .env = env,
        .gpa = a,
    };
    return t_env; 
}

Module* type_env_module(TypeEnv* env) {
    return env_module(env->env);
}

TypeEntry type_env_lookup(Symbol s, TypeEnv* env) {
    static PiType kind = {.sort = TKind, .kind.nargs = 0};
    // Search locally
    TypeEntry out;
    Local* lresult = sym_local_alookup(s, env->locals);
    if (lresult != NULL) {
        out.type = TELocal;
        if (lresult->sort == LQVar) {
            out.is_module = false;
            out.ptype = &kind;
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

/**
 * Given a parametric instance, e.g.
 * instance [A] {(show-elt (Show A))} (Show (List A)) [...], 
 * and a set of instantiation arguments (e.g. (List I64)), determine if there is
 * an instantiation of the parametric lookut that for the argument types.
 */
InstanceEntry parametric_instance_lookup(InstanceSrc* src, AddrPiList args, TypeEnv* env) {
    ArenaAllocator* arena = make_arena_allocator(1024, env->gpa);
    Allocator aa = aa_to_gpa(arena);
    PiAllocator pia = convert_to_pallocator(&aa);
    /**
     * Steps: 
     * 1. Replace over parameters with unification variables, substitute
     *    into args, then unify.
     * 2. If unification fails, this is a non-match, continue, otherwise
     *    search for dependent implicits with newly unified types. 
     * 3. If the search fails, this is a non-match. continue, otherwise
     *    create (or get) a specific instance with these arguments (use
     *    the modlue interface to do this).
     */

    SymPtrAssoc type_binds = mk_sym_ptr_assoc(src->over.len, &aa);
    for (size_t i = 0; i < src->over.len; i++) {
        sym_ptr_bind(src->over.data[i], mk_uvar(&pia), &type_binds);
    }

    for (size_t i = 0 ; i < args.len; i++) {
        PiType* comp_to = pi_type_subst(src->args.data[i], type_binds, NULL, &pia, &aa);
        UnifyContext ctx = {
            .a = &aa,
            .pia = &pia, 
            .current_module = env_module(env->env),
            .logger = NULL,
        };
        UnifyResult res = unify(comp_to, args.data[i], ctx);
        if (res.type != UOk) {
            goto early_exit;
        }
    }
    for (size_t i = 0 ; i < type_binds.len; i++) {
        UnifyContext ctx = {
            .a = &aa,
            .pia = &pia, 
            .current_module = env_module(env->env),
            .logger = NULL,
        };
        PiType* type = type_binds.data[i].val;
        squash_type(type, ctx);
        if (type->sort == TUVar) {
            goto early_exit;
        }
    };

    // Unify successful. Search for implicits
    PtrArray implicits = mk_ptr_array(src->dependencies.len, &aa);
    for (size_t i = 0; i < src->dependencies.len; i++) {
        PiType* instance = src->dependencies.data[i];
        if (instance->sort != TTraitInstance) {
            panic(mv_string("Instance dependencies not of type trait instance."));
        }

        // Need to substitute type variable for the value it takes.
        AddrPiList dependency_args = mk_addr_list(instance->instance.args.len, &pia);
        for (size_t i = 0; i < instance->instance.args.len; i++) {
            PiType* old_type = instance->instance.args.data[i];
            PiType* new_type = pi_type_subst(old_type, type_binds, NULL, &pia, &aa);
            push_addr(new_type, &dependency_args);
        }
        InstanceEntry entry = type_instance_lookup(instance->instance.instance_of, dependency_args, env);
        if (entry.type != IEAbsSymbol) {
            delete_arena_allocator(arena);
            return entry;
        }
        void** instance_ptr = entry.abvar.value;
        push_ptr(*instance_ptr, &implicits);
    }
    U64Array type_data = mk_u64_array(type_binds.len, &aa);
    for (size_t i = 0; i < type_binds.len; i++) {
        PiType* type = type_binds.data[i].val;
        
        size_t align = pi_align_of(*type);
        size_t size = pi_size_of(*type);
        size_t stack_sz = pi_stack_align(size);

        // TODO BUG LOGIC Check that stack_sz < max_uint_28
        uint64_t result = (align << 56) | (size << 28) | stack_sz;
        push_u64(result, &type_data);
    }

    // Implicit and unify search successful. Instantiate 
    void* instantiation = get_instantiation(src->src, src->src_sym, type_binds, type_data, implicits);

    if (!instantiation) {
        panic(mv_string("get_instantiation failed! This indicates that there is a bug in the instance lookup."));
    }
    
    // Return new entry/success
    delete_arena_allocator(arena);
    return (InstanceEntry) {
        .type = IEAbsSymbol,
        .abvar.index = 0,
        .abvar.value = instantiation,

    };

 early_exit:
    delete_arena_allocator(arena);
    return (InstanceEntry) {
        .type = IENotFound
    };
}

InstanceEntry type_instance_lookup(uint64_t id, AddrPiList args, TypeEnv* env) {
    PtrArray* instances = env_implicit_lookup(id, env->env);
    if (!instances) {
        return (InstanceEntry) {.type = IENotFound,};
    }

    InstanceEntry out = {.type = IENotFound,};
    InstSrcArray sources = mk_inst_src_array(1, env->gpa);
    for (size_t i = 0; i < instances->len; i++) {
        InstanceSrc* src = instances->data[i];
        bool eql = true;

        if (src->over.len > 0) {
            InstanceEntry entry = parametric_instance_lookup(src, args, env);
            if (entry.type == IEAbsSymbol) {
                push_inst_src(*src, &sources);
                out = entry;
            }
        } else {
            for (size_t i = 0 ; i < args.len; i++) {
                eql &= pi_type_eql(src->args.data[i], args.data[i], env->gpa);
            }
            if (eql) {
                push_inst_src(*src, &sources);

                ModuleEntry* m_entry = get_def(src->src_sym, src->src);
                if (!m_entry) panic(mv_string("Module entry is null!"));

                if (m_entry->is_module) {
                    panic(mv_string("type_instance_lookup cannot yet handle modules"));
                }

                out = (InstanceEntry) {
                    .type = IEAbsSymbol,
                    .abvar.index = 0,
                    .abvar.value = m_entry->value,
                };
            }
        }
    }
    switch (sources.len) { 
    case 0:
        return (InstanceEntry) {.type = IENotFound,};
    case 1:
        mem_free(sources.data, &sources.gpa);
        return out;
    default:
        return (InstanceEntry) {
            .type = IEAmbiguous,
            .ambiguous_sources = sources,
        }; 
    }
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

PtrArray* lookup_label(Symbol s, TypeEnv* env) {
    PtrArray** arr = (PtrArray**)sym_ptr_alookup(s, env->labels);
    if (arr) return *arr; else return NULL;
}

void add_labels (SymPtrAssoc labels, TypeEnv* env) {
    for (size_t i = 0; i < labels.len; i++) {
        sym_ptr_bind(labels.data[i].key, labels.data[i].val, &env->labels);
    }
}

void pop_labels(TypeEnv* env, size_t n) {
    sym_ptr_unbindn(n, &env->labels);
}

SymLocalAssoc get_local_vars(TypeEnv* env) {
    // TODO (BUG LOGIC): how to handle labels?
    return env->locals;
}

Environment* get_base(TypeEnv* env) {
    return env->env;
}
