#include "platform/environment.h"
#include <stdlib.h>

StringOption get_env_var(String arg) {
    char* var = getenv((char*)arg.bytes);
    if (var) {
      return (StringOption) {
          .type = Some,
          .val = mv_string(var),
      };
    } else {
      return (StringOption) {.type = None,};
    }
}

