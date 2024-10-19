# Based on the makefile by Job Vranish (https://spin.atomicobject.com/2016/08/26/makefile-c-projects/)

TARGET_EXEC := pico
MAIN_SRC := ./src/main.c

BUILD_DIR := ./build
RELEASE_DIR := $(BUILD_DIR)/release
DEBUG_DIR := $(BUILD_DIR)/debug
SRC_DIRS := ./src

C_VERSION := c11
CC := gcc

## Platform specifics and configuration
##-------------------------------------
BUILD_TYPE := Debug

RELEASE_FLAGS := -Ofast -flto=auto
DEBUG_FLAGS := -O0

# Sanitisers currently aren't supported by gcc on windows
ifneq ($(OS), Windows_NT)
	DEBUG_FLAGS := $(DEBUG_FLAGS) -fsanitize=address,leak
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
CFLAGS := $(CFLAGS) $(INC_FLAGS) -MMD -MP -Wall -Wextra -g -std=$(C_VERSION) -D_GNU_SOURCE

# The final build step.
$(RELEASE_DIR)/$(TARGET_EXEC): $(RELEASE_OBJS) $(MAIN_RELEASE_OBJ)
	$(CC) $(RELEASE_OBJS) $(MAIN_RELEASE_OBJ) -o $@ $(CFLAGS) $(RELEASE_FLAGS) 

$(DEBUG_DIR)/$(TARGET_EXEC): $(DEBUG_OBJS) $(MAIN_DEBUG_OBJ)
	$(CC) $(DEBUG_OBJS) $(MAIN_DEBUG_OBJ) -o $@ $(CFLAGS) $(DEBUG_FLAGS)

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
TEST_SRC_DIRS := ./test/src
TARGET_TEST := pico_test

TEST_SRCS := $(shell find $(TEST_SRC_DIRS) -name '*.c')
TEST_OBJS := $(TEST_SRCS:%=$(TEST_DIR)/%.o)

# Final build step for tests 
$(TEST_DIR)/$(TARGET_TEST): $(TEST_OBJS)
	$(CC) $(TEST_OBJS) -o $@ $(LDFLAGS)

# Build step for C tests
$(TEST_DIR)/%.c.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

.PHONY: clean
clean:
	rm -r $(BUILD_DIR)

.PHONY: release
release: $(RELEASE_DIR)/$(TARGET_EXEC)

.PHONY: debug
debug: $(DEBUG_DIR)/$(TARGET_EXEC)

.PHONY: test
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
test: $(TEST_DIR)/$(TARGET_TEST)
	$(TEST_DIR)/$(TARGET_TEST)

# Include the .d makefiles. The - at the front suppresses the errors of missing
# Makefiles. Initially, all the .d files will be missing, and we don't want those
# errors to show up.
-include $(MAKE_DEPS)
