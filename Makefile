# Based on the makefile by Job Vranish (https://spin.atomicobject.com/2016/08/26/makefile-c-projects/)

TARGET_EXEC := pico
TARGET_SRC := ./src/main.c

BUILD_DIR := ./build
SRC_DIRS := ./src

TARGET_TEST := pico_test
TEST_BUILD_DIR := ./build/test
TEST_SRC_DIRS := ./test/src

C_VERSION := c11
CC := gcc

## Platform specifics and configuration
##-------------------------------------
BUILD_TYPE := Debug

ifeq ($(BUILD_TYPE), Release)
	OPT_FLAGS := -Ofast
	SANITIZE_FLAGS := 
else
	OPT_FLAGS := -Og

# Sanitisers currently aren't supported by gcc on windows
	ifeq ($(OS), Windows_NT)
		SANITIZE_FLAGS :=
	else
		SANITIZE_FLAGS := -fsanitize=address,leak
	endif
endif

# Find all the C files we want to compile
SRCS := $(shell find $(SRC_DIRS) -name '*.c' | grep -v $(TARGET_SRC))

# Prepends BUILD_DIR and appends .o to every src file
# As an example, ./your_dir/hello.c turns into ./build/./your_dir/hello.c.o
OBJS := $(SRCS:%=$(BUILD_DIR)/%.o)
TARGET_OBJ := $(TARGET_SRC:%=$(BUILD_DIR)/%.o)

# String substitution (suffix version without %).
# As an example, ./build/hello.c.o turns into ./build/hello.c.d
MAKE_DEPS := $(OBJS:.o=.d) $(TARGET_OBJ:.o=.d)

# Every folder in ./src will need to be passed to GCC so that it can find header files
#INC_DIRS := $(shell find $(SRC_DIRS) -type d)
INC_DIRS := include
# Add a prefix to INC_DIRS. So moduleA would become -ImoduleA. GCC understands this -I flag
INC_FLAGS := $(addprefix -I ,$(INC_DIRS))

# The -MMD and -MP flags together generate Makefiles for us!
# These files will have .d instead of .o as the output.
CFLAGS := $(CFLAGS) $(INC_FLAGS) -MMD -MP -Wall -Wextra -g -std=$(C_VERSION) -D_GNU_SOURCE $(SANITIZE_FLAGS) $(OPT_FLAGS)
LDFLAGS := $(SANITIZE_FLAGS) $(OPT_FLAGS)

# The final build step.
$(BUILD_DIR)/$(TARGET_EXEC): $(OBJS) $(TARGET_OBJ)
	$(CC) $(OBJS) $(TARGET_OBJ) -o $@ $(LDFLAGS) $(OPT_FLAGS)

# Build step for C source
$(BUILD_DIR)/%.c.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@


# Test stuff
# ---------------------------------------------
## Same process as above but for tests

TEST_SRCS := $(shell find $(TEST_SRC_DIRS) -name '*.c')
TEST_OBJS := $(TEST_SRCS:%=$(BUILD_DIR)/%.o)

# Final build step for tests 
$(BUILD_DIR)/$(TARGET_TEST): $(TEST_OBJS)
	$(CC) $(TEST_OBJS) -o $@ $(LDFLAGS)

# Build step for C tests
# $(BUILD_DIR)/test/%.c.o: %.c
# 	mkdir -p $(dir $@)
# 	$(CC) $(CFLAGS) -c $< -o $@

.PHONY: clean
clean:
	rm -r $(BUILD_DIR)

.PHONY: run
run: $(BUILD_DIR)/$(TARGET_EXEC)
	$(BUILD_DIR)/$(TARGET_EXEC) -d

.PHONY: test
test: $(BUILD_DIR)/$(TARGET_TEST)
	$(BUILD_DIR)/$(TARGET_TEST)

.PHONY: install
install: $(BUILD_DIR)/$(TARGET_EXEC)
	cp $(BUILD_DIR)/$(TARGET_EXEC) ~/.local/bin

# Include the .d makefiles. The - at the front suppresses the errors of missing
# Makefiles. Initially, all the .d files will be missing, and we don't want those
# errors to show up.
-include $(MAKE_DEPS)
