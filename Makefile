## -----------------------------------------------------------------------------
##
##    Makefile for Pico
##
## -----------------------------------------------------------------------------

ifneq ($(OS), Windows_NT)
TARGET_EXEC := pico
else
TARGET_EXEC := pico.exe
endif

MAIN_SRC := ./src/main.c
BUILD_DIR := ./build
RELEASE_DIR := $(BUILD_DIR)/release
DEBUG_DIR := $(BUILD_DIR)/debug
SRC_DIRS := ./src
LINK_FLAGS :=
DEBUG_FLAGS :=
RELEASE_FLAGS :=

C_VERSION := c99
CC := gcc

# Process deafult.config
CONFIG = default.config
include ${CONFIG}

ifeq ($(WINDOW_SYSTEM), DEFAULT)

  # select default window system if linux
  ifneq ($(OS), Windows_NT)
    ifeq ($(XDG_SESSION_TYPE), x11)
      WINDOW_SYSTEM := X11
    else ifeq ($(XDG_SESSION_TYPE), wayland)
      WINDOW_SYSTEM := WAYLAND
    endif
  else ifeq ($(OS), Windows_NT)
    WINDOW_SYSTEM := WIN32
  endif
endif

## Emit info and warnings
##-------------------------------------
ifeq ($(HELP), YES)
	DUMMY := $(info To enable or disable certain features and packages, such as Windowing or Hedron, please see the file 'default.config')
	DUMMY := $(info To disable this message, set 'HELP=YES' to 'HELP=NO', or just delete the line entirely)
endif

RPAREN = )
MIN_GCC_VERSION := 13.0.0
CURRENT_GCC_VERSION := $(shell gcc --version | head -n 1 | sed 's/.*$(RPAREN) \([0-9.]*\).*/\1/')

ifneq ($(shell echo -e "$(CURRENT_GCC_VERSION)\n$(MIN_GCC_VERSION)" | sort -V | head -n 1), $(MIN_GCC_VERSION))
	DUMMY := $(warning GCC version is out of date - require at least $(MIN_GCC_VERSION) but have $(CURRENT_GCC_VERSION). It is very likely that the build will fail)
endif

ifeq ($(HEDRON), YES)
  ifeq ($(OS), Windows_NT)
    ifeq ($(wildcard $(VULKAN_DIR)), )
      DUMMY := $(warning Hedron is enabled and the Vulkan directory is set to $(VULKAN_DIR), which does not exist. Either disable Hedron or install the Vulkan SDK and point VULKAN_DIR at it)
    endif
  endif

else
  undefine HEDRON
endif

ifeq ($(OS), Windows_NT)
  VULKAN_LINK=$(VULKAN_DIR)\Lib\vulkan-1.lib
  VULKAN_INCLUDE=-I$(VULKAN_DIR)\Include 
else
  VULKAN_LINK=-lvulkan
  VULKAN_INCLUDE=
endif

ifdef HEDRON
    DEBUG_FLAGS := $(DEBUG_FLAGS) -DUSE_VULKAN $(VULKAN_INCLUDE)
    RELEASE_FLAGS := $(RELEASE_FLAGS) -DUSE_VULKAN $(VULKAN_INCLUDE)
    LINK_FLAGS := $(LINK_FLAGS) $(VULKAN_LINK)
endif

ifneq ($(DEBUG_ASSERT), YES)
  undefine DEBUG_ASSERT
endif

## Platform specifics and configuration
##-------------------------------------
RELEASE_FLAGS := $(RELEASE_FLAGS) -Ofast -Werror -g
DEBUG_FLAGS := $(DEBUG_FLAGS) -O0 -DDEBUG -DDEBUG_ASSERT -g3 -gdwarf-2 

ifeq ($(WINDOW_SYSTEM), X11)
  DEBUG_FLAGS := $(DEBUG_FLAGS) -DWINDOW_SYSTEM=1 -lX11 -lxkbcommon
  RELEASE_FLAGS := $(RELEASE_FLAGS) -DWINDOW_SYSTEM=1 -lX11 -lxkbcommon
else ifeq ($(WINDOW_SYSTEM), WAYLAND)
  DEBUG_FLAGS := $(DEBUG_FLAGS) -DWINDOW_SYSTEM=2 -lwayland-client -lxkbcommon
  RELEASE_FLAGS := $(RELEASE_FLAGS) -DWINDOW_SYSTEM=2 -lwayland-client -lxkbcommon
else ifeq ($(WINDOW_SYSTEM), WIN32)
  DEBUG_FLAGS := $(DEBUG_FLAGS) -DWINDOW_SYSTEM=3 
  RELEASE_FLAGS := $(RELEASE_FLAGS) -DWINDOW_SYSTEM=3 
endif

# Sanitisers currently aren't supported by gcc on windows
ifneq ($(OS), Windows_NT)
    DEBUG_FLAGS := $(DEBUG_FLAGS) $(SANITIZERS)
    LINK_FLAGS := $(LINK_FLAGS) -ldl -lm
    RELEASE_FLAGS := $(RELEASE_FLAGS) 
else
    LINK_FLAGS := $(LINK_FLAGS)
endif

ifeq ($(PROFILE), YES)
    DEBUG_FLAGS := $(DEBUG_FLAGS) -pg
    RELEASE_FLAGS := $(DEBUG_FLAGS) -pg
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
INC_DIRS := include
# Add a prefix to INC_DIRS. So moduleA would become -ImoduleA. GCC understands this -I flag
INC_FLAGS := $(addprefix -I ,$(INC_DIRS))

# The -MMD and -MP flags together generate Makefiles for us!
# These files will have .d instead of .o as the output.
CFLAGS := $(CFLAGS) $(INC_FLAGS) -MMD -MP -std=$(C_VERSION) -D_GNU_SOURCE

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

# Build step for C source (debug)
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
TEST_OBJS := $(TEST_SRCS:%=$(TEST_DIR)/%.o) $(DEBUG_OBJS)

# Final build step for tests 
$(TEST_DIR)/$(TARGET_TEST): $(TEST_OBJS)
	$(CC) $(TEST_OBJS) -I $(TEST_INC_DIR) -o $@ $(LINK_FLAGS) $(TEST_FLAGS) 

# Build step for C tests
$(TEST_DIR)/%.c.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -I $(TEST_INC_DIR) -c $< -o $@ $(TEST_FLAGS) 

# Installer
# ---------------------------------------------
# Objects from the 'platform' and 'components' diretories
# that may be useful regardless of application...
GENERIC_SRC_DIRS := ./src/data ./src/components ./src/platform/filesystem ./src/platform/memory ./src/platform/terminal
GENERIC_SRCS := $(shell find $(GENERIC_SRC_DIRS) -name '*.c' | grep -v $(MAIN_SRC)) 
GENERIC_SRCS := $(GENERIC_SRCS) ./src/platform/signals.c ./src/platform/thread.c ./src/platform/error.c ./src/platform/jump.c ./src/platform/environment.c
GENERIC_OBJS := $(GENERIC_SRCS:%=$(RELEASE_DIR)/%.o)


INSTALLER_DIR := $(BUILD_DIR)/installer
INSTALLER_INC_DIR := ./installer/include
INSTALLER_SRC_DIRS := ./installer/src
TARGET_INSTALLER := pico_installer

INSTALLER_FLAGS := $(RELEASE_FLAGS)

INSTALLER_SRCS := $(shell find $(INSTALLER_SRC_DIRS) -name '*.c')
INSTALLER_OBJS := $(INSTALLER_SRCS:%=$(INSTALLER_DIR)/%.o) $(GENERIC_OBJS)

# Final build step for installer 
$(INSTALLER_DIR)/$(TARGET_INSTALLER): $(INSTALLER_OBJS)
	$(CC) $(INSTALLER_OBJS) -I $(INSTALLER_INC_DIR) -o $@ $(LINK_FLAGS) $(INSTALLER_FLAGS) 

# Build step for C tests
$(INSTALLER_DIR)/%.c.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -I $(INSTALLER_INC_DIR) -c $< -o $@ $(INSTALLER_FLAGS) 

# Installer
# ---------------------------------------------
## Same process as above but for tests

KEEPER_DIR := $(BUILD_DIR)/keeper
KEEPER_INC_DIR := ./keeper/include
KEEPER_SRC_DIRS := ./keeper/src
ifneq ($(OS), Windows_NT)
TARGET_KEEPER := pico_keeper
else
TARGET_KEEPER := keeper.exe
endif

KEEPER_FLAGS := $(RELASE_FLAGS)

KEEPER_SRCS := $(shell find $(KEEPER_SRC_DIRS) -name '*.c')
KEEPER_OBJS := $(KEEPER_SRCS:%=$(KEEPER_DIR)/%.o) $(GENERIC_OBJS)

# Final build step for keeper 
$(KEEPER_DIR)/$(TARGET_KEEPER): $(KEEPER_OBJS)
	$(CC) $(KEEPER_OBJS) -I $(KEEPER_INC_DIR) -o $@ $(LINK_FLAGS) $(KEEPER_FLAGS) 

# Build step for C keeper objects/intermediates
$(KEEPER_DIR)/%.c.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -I $(KEEPER_INC_DIR) -c $< -o $@ $(KEEPER_FLAGS) 


#  Phony targets
# ---------------

.PHONY: clean
clean:
	rm -r $(BUILD_DIR)

.PHONY: release
release: $(RELEASE_DIR)/$(TARGET_EXEC)

.PHONY: debug
debug: $(DEBUG_DIR)/$(TARGET_EXEC)


# Run tests with make test
.PHONY: test
test: $(TEST_DIR)/$(TARGET_TEST)
	$(TEST_DIR)/$(TARGET_TEST)

.PHONY: installer
installer: $(INSTALLER_DIR)/$(TARGET_INSTALLER)

.PHONY: keeper
keeper: $(KEEPER_DIR)/$(TARGET_KEEPER)

.PHONY: run-keeper
run-keeper: $(KEEPER_DIR)/$(TARGET_KEEPER)
	$(KEEPER_DIR)/$(TARGET_KEEPER)

# Only build tests with make build-test
.PHONY: build-test
test: $(TEST_DIR)/$(TARGET_TEST)

.PHONY: run
run: run-debug

.PHONY: run-release
run-release: $(RELEASE_DIR)/$(TARGET_EXEC)
	$(RELEASE_DIR)/$(TARGET_EXEC)

.PHONY: run-debug
run-debug: $(DEBUG_DIR)/$(TARGET_EXEC)
	$(DEBUG_DIR)/$(TARGET_EXEC)

.PHONY: all
all: debug release test keeper installer

# TODO: (FEAT) check shell; set appropriately
.PHONY: debug_mode
debug_mode:
	set -x LD_LIBRARY_PATH /usr/lib/debug


ASSET_DIR := $(INSTALLER_DIR)/assets

# Installation
.PHONY: install
install: $(INSTALLER_DIR)/$(TARGET_INSTALLER) release keeper
	mkdir -p $(ASSET_DIR)
	cp $(RELEASE_DIR)/$(TARGET_EXEC) $(ASSET_DIR)
	cp $(KEEPER_DIR)/$(TARGET_KEEPER) $(ASSET_DIR)/$(TARGET_KEEPER)
	cp -r archive $(ASSET_DIR)
# TODO (BUG): remove this line and build more 'correct' updates into the Installer...
	rm -r ~/.local/share/pico
	cd $(INSTALLER_DIR); ./$(TARGET_INSTALLER)

# use make <target> QUIET=1 to prevent make from printing! 
# can be used in scripts, e.g. git pre-commit hooks
ifeq ($(QUIET), YES)
.SILENT:
endif

# Include the .d makefiles. The - at the front suppresses the errors of missing
# Makefiles. Initially, all the .d files will be missing, and we don't want those
# errors to show up.
-include $(MAKE_DEPS)
