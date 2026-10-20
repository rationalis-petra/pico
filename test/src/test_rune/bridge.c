#include <string.h>
#include "platform/memory/region.h"
#include "test/test_log.h"

#include "test_rune/helper.h"
#include "test_rune/rune.h"
#include "rune/eval/eval.h"

typedef struct {
    void* data;
    size_t len;
} List;

typedef struct {
    uint64_t is_some;
    int64_t value;
} MaybeInt;

typedef struct {
    int64_t fst;
    int64_t snd;
} PairIntInt;

typedef struct ListCell {
    uint64_t is_cons;
    int64_t val;
    struct ListCell* next;
} ListCell;

void run_rune_bridge_tests(TestLog* log, RuneEnv* env, RegionAllocator* region) {
    TestContext context = {
        .log = log,
        .env = NULL,
        .region = region,
    };

    if (test_start(log, mv_string("int-bridge"))) {
        int64_t expected = 9871;
        Bridge bridge = {.type = BInt};
        TEST_BRIDGE("9871");
    }

    if (test_start(log, mv_string("string-value"))) {
        String expected = mv_string("test");
        Bridge bridge = {.type = BString};
        TEST_BRIDGE("\"test\"");
    }

    if (test_start(log, mv_string("list-value"))) {
        int64_t ints[] = {1, 2, 3};
        List expected = {ints, 3};


        Bridge elt = {.type = BInt};
        Bridge bridge = {.type = BList, .element = &elt}; 
        TEST_BRIDGE("(list 1 2 3)");
    }

    if (test_start(log, mv_string("data-none"))) {
        MaybeInt expected = {.is_some = 0, .value = 0};
        Bridge int_bridge = {.type = BInt};

        BridgeVariant variants[] = {
            {.name = string_to_name(mv_string("none")),
             .len = 0,},
            {.name = string_to_name(mv_string("some")),
             .len = 1,
             .types = &int_bridge,},
        };
        Bridge bridge = {
            .type = BData,
            .data = {
                .len = sizeof(variants) / sizeof(BridgeVariant),
                .variants = variants,
            },
        };
        TEST_BRIDGE(":none");
    }

    if (test_start(log, mv_string("data-some"))) {
        MaybeInt expected = {.is_some = 1, .value = 8391287};
        Bridge int_bridge = {.type = BInt};

        BridgeVariant variants[] = {
            {.name = string_to_name(mv_string("none")),
             .len = 0,},
            {.name = string_to_name(mv_string("some")),
             .len = 1,
             .types = &int_bridge,},
        };
        Bridge bridge = {
            .type = BData,
            .data = {
                .len = sizeof(variants) / sizeof(BridgeVariant),
                .variants = variants,
            },
        };
        TEST_BRIDGE("(:some 8391287)");
    }

    if (test_start(log, mv_string("record-pair"))) {
        PairIntInt expected = {.fst = -12987, .snd = 98213};
        Bridge bint = {.type = BInt};
        BRecCell cells[] = {
            {.name = string_to_name(mv_string("fst")),
             .val = bint,},
            {.name = string_to_name(mv_string("snd")),
             .val = bint,},
        };
        Bridge bridge = {
            .type = BRecord,
            .record = {
                .len = sizeof(cells) / sizeof(BRecCell),
                .types = cells,
            }
        };
        TEST_BRIDGE("(record [.x -12987] [.y 98213])");
    }

    if (test_start(log, mv_string("ptr-wrap-list"))) {
        ListCell nil = {};
        ListCell v3 = {.is_cons = 1, .val = 3, .next = &nil};
        ListCell v2 = {.is_cons = 1, .val = 2, .next = &v3};
        ListCell expected = {.is_cons = 1, .val = 1, .next = &v2};

        Bridge bridge;
        Bridge cons_types[] = {{.type = BInt}, {.type = BPtrWrap, .wrapped = &bridge}};
        BridgeVariant variants[] = {
            {.name = string_to_name(mv_string("nil")),
             .len = 0,},
            {.name = string_to_name(mv_string("cons")),
             .len = 2,
             .types = cons_types,},
        };
        bridge = (Bridge) {
            .type = BData,
            .data = {
                .len = sizeof(variants) / sizeof(BridgeVariant),
                .variants = variants,
            }
        };
        TEST_BRIDGE("(:cons 1 (:cons 2 (:cons 3 :nil)))");
    }
}
