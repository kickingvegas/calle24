## calle24

# Copyright 2025 Charles Y. Choi

# Configure EMACS_CONFIG_DIR to where you keep your Emacs configuration files.
EMACS_CONFIG_DIR = $(HOME)/.config/emacs
#EMACS_CONFIG_DIR = $(HOME)/.emacs.d

INSTALL_DIR=$(EMACS_CONFIG_DIR)/calle24

$(INSTALL_DIR):
	mkdir -p $(INSTALL_DIR)

.PHONY: install
install: $(INSTALL_DIR)/images

$(INSTALL_DIR)/images: $(INSTALL_DIR)
	rsync -avh images/ $@/

.PHONY: image-load-path
image-load-path:
	echo $(INSTALL_DIR)/images

.PHONY: uninstall
uninstall:
	rm -rf $(INSTALL_DIR)
