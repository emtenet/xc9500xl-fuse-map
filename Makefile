PROJECT = xc9500xl
PROJECT_DESCRIPTION = AMD Xilinx XC9500XL CPLD Bitstream Documentation (Reverse Engineered)
PROJECT_VERSION = 1.0.0

ERLC_OPTS = -Werror \
 +warn_bif_clash \
 +warn_deprecated_function \
 +warn_export_all \
 +warn_export_vars \
 +warn_exported_vars \
 +warn_format \
 +warn_obsolete_guard \
 +warn_shadow_vars \
 +warn_unused_function \
 +warn_unused_record \
 +warn_unused_vars \
 +debug_info

DIALYZER_PLT_OPTS += --no_native

include erlang.mk
