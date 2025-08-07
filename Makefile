# Based on the makefile by Job Vranish (https://spin.atomicobject.com/2016/08/26/makefile-c-projects/)

TARGET_EXEC := pico
MAIN_SRC := ./src/main.c

BUILD_DIR := ./build
RELEASE_DIR := $(BUILD_DIR)/release
DEBUG_DIR := $(BUILD_DIR)/debug
SRC_DIRS := ./src

C_VERSION := c99
CC := gcc

CONFIG = default.config
include ${CONFIG}

ifeq ($(OS), Windows_NT)
VULKAN_LINK=$(VULKAN_DIR)\Lib\vulkan-1.lib
VULKAN_INCLUDE=-I$(VULKAN_DIR)\Include 
else
VULKAN_LINK=-lvulkan
VULKAN_INCLUDE=
endif


## Platform specifics and configuration
##-------------------------------------
RELEASE_FLAGS := -Ofast -Werror
DEBUG_FLAGS := -O0 -DDEBUG -DDEBUG_ASSERT

# Sanitisers currently aren't supported by gcc on windows
ifneq ($(OS), Windows_NT)
	DEBUG_FLAGS := $(DEBUG_FLAGS) -lwayland-client $(SANITIZERS)
	LINK_FLAGS := -ldl -lm
    RELEASE_FLAGS := $(RELEASE_FLAGS) -lwayland-client
else
	LINK_FLAGS := 
endif

ifdef PROFILE
	DEBUG_FLAGS := $(DEBUG_FLAGS) -pg
    RELEASE_FLAGS := $(DEBUG_FLAGS) -pg
endif

ifdef HEDRON
	DEBUG_FLAGS := $(DEBUG_FLAGS) -DUSE_VULKAN $(VULKAN_INCLUDE)
    RELEASE_FLAGS := $(RELEASE_FLAGS) -DUSE_VULKAN $(VULKAN_INCLUDE)
	LINK_FLAGS := $(LINK_FLAGS) $(VULKAN_LINK)
endif

# Find all the C files we want to compile
SRCS := $(shell find $(SRC_DIRS) -name '*.c' | grep -v $(MAIN_SRC))

# Prepends BUILD_DIR and appends .o to every src file
# As an example, ./your_dir/hello.c turns into ./build/./your_dir/hello.c.o
RELEASE_OBJS := $(SRCS:%=$(RELEASE_DIR)/%.o)
DEBUG_OBJS := $(SRCS:%=$(DEBUG_DIR)/%.o)
MAIN_RELEASE_OBJ := $(MAIN_SRC:%=$(RELEASE_DIR)/%.o)
MAIN_DEBUG_OBJ := $(MAIN_SRC:%=$(DEBUG_DIR)/%.o)

# String substitution (suffix version without %).
# As an example, ./build/hello.c turns into ./build/hello.c.d
MAKE_DEPS := $(SRCS:%=$(BUILD_DIR)/%.d) $(MAIN_SRC:%=$(BUILD_DIR)/%.d)

# Every folder in ./src will need to be passed to GCC so that it can find header files
#INC_DIRS := $(shell find $(SRC_DIRS) -type d)
INC_DIRS := include
# Add a prefix to INC_DIRS. So moduleA would become -ImoduleA. GCC understands this -I flag
INC_FLAGS := $(addprefix -I ,$(INC_DIRS))

# The -MMD and -MP flags together generate Makefiles for us!
# These files will have .d instead of .o as the output.
CFLAGS := $(CFLAGS) $(INC_FLAGS) -MMD -MP -g -std=$(C_VERSION) -D_GNU_SOURCE

# The warnings we want 
CFLAGS := $(CFLAGS) -Wall -Wextra -Wundef -Wno-unused-parameter -Wnull-dereference -Wcast-align 
# these warnings are a bit pedantic, so are turned off for now
CFLAGS := $(CFLAGS) # -Wconversion -Wsign-conversion

# The final build step.
$(RELEASE_DIR)/$(TARGET_EXEC): $(RELEASE_OBJS) $(MAIN_RELEASE_OBJ)
	$(CC) $(RELEASE_OBJS) $(MAIN_RELEASE_OBJ) -o $@ $(CFLAGS) $(RELEASE_FLAGS) $(LINK_FLAGS)

$(DEBUG_DIR)/$(TARGET_EXEC): $(DEBUG_OBJS) $(MAIN_DEBUG_OBJ)
	$(CC) $(DEBUG_OBJS) $(MAIN_DEBUG_OBJ) -o $@ $(CFLAGS) $(DEBUG_FLAGS) $(LINK_FLAGS)

# Build step for C source (release)
$(RELEASE_DIR)/%.c.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@ $(RELEASE_FLAGS)

# Build step for C source (release)
$(DEBUG_DIR)/%.c.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@ $(DEBUG_FLAGS)

# Test stuff
# ---------------------------------------------
## Same process as above but for tests

TEST_DIR := $(BUILD_DIR)/test
TEST_INC_DIR := ./test/include
TEST_SRC_DIRS := ./test/src
TARGET_TEST := pico_test
TEST_FLAGS := $(DEBUG_FLAGS)

TEST_SRCS := $(shell find $(TEST_SRC_DIRS) -name '*.c')
TEST_OBJS := $(TEST_SRCS:%=$(TEST_DIR)/%.o)

# Final build step for tests 
$(TEST_DIR)/$(TARGET_TEST): $(TEST_OBJS) $(DEBUG_OBJS)
	$(CC) $(TEST_OBJS) $(DEBUG_OBJS) -I $(TEST_INC_DIR) -o $@ $(LINK_FLAGS) $(TEST_FLAGS) 

# Build step for C tests
$(TEST_DIR)/%.c.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -I $(TEST_INC_DIR) -c $< -o $@ $(TEST_FLAGS) 

.PHONY: clean
clean:
	rm -r $(BUILD_DIR)

.PHONY: release
release: $(RELEASE_DIR)/$(TARGET_EXEC)

.PHONY: debug
debug: $(DEBUG_DIR)/$(TARGET_EXEC)

.PHONY: test
test: $(TEST_DIR)/$(TARGET_TEST)

.PHONY: build_test
test: $(TEST_DIR)/$(TARGET_TEST)

.PHONY: install
install: $(RELEASE_DIR)/$(TARGET_EXEC)
	cp $(RELEASE_DIR)/$(TARGET_EXEC) ~/.local/bin

.PHONY: run
run: run-debug

.PHONY: run-release
run-release: $(RELEASE_DIR)/$(TARGET_EXEC)
	$(RELEASE_DIR)/$(TARGET_EXEC)

.PHONY: run-debug
run-debug: $(DEBUG_DIR)/$(TARGET_EXEC)
	$(DEBUG_DIR)/$(TARGET_EXEC)

.PHONY: run-test
run-test: $(TEST_DIR)/$(TARGET_TEST)
	$(TEST_DIR)/$(TARGET_TEST)

.PHONY: all
all: debug release test

# TODO: check shell; set appropriately
.PHONY: debug_mode
debug_mode:
	set -x LD_LIBRARY_PATH /usr/lib/debug

# use make <target> QUIET=1 to prevent make from printing! 
# can be used in scripts, e.g. git pre-commit hooks
ifdef QUIET
.SILENT:
endif

# Include the .d makefiles. The - at the front suppresses the errors of missing
# Makefiles. Initially, all the .d files will be missing, and we don't want those
# errors to show up.
-include $(MAKE_DEPS)
