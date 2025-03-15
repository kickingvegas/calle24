## calle24

# Copyright 2025 Charles Y. Choi
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

## Requirements
# - Python 3.11+
# - GNU awk 5.3+
# - Python semver
# - Bash

# Configure EMACS_CONFIG_DIR to where you keep your Emacs configuration files.
EMACS_CONFIG_DIR = $(HOME)/.config/emacs
#EMACS_CONFIG_DIR = $(HOME)/.emacs.d

LISP_DIR=./lisp
MAIN_EL=$(realpath $(LISP_DIR)/calle24.el)

TIMESTAMP := $(shell /bin/date "+%Y%m%d_%H%M%S")
VERSION := $(shell ./scripts/read-version.sh $(MAIN_EL))
# BUMP_LEVEL: major|minor|patch|prerelease|build
BUMP_LEVEL=patch
VERSION_BUMP := $(shell python -m semver bump $(BUMP_LEVEL) $(VERSION))
VERSION_LAST_TAG := $(shell git tag --sort=-creatordate | head -n 1)

INSTALL_DIR=$(EMACS_CONFIG_DIR)/calle24
BIN_DIR=$(HOME)/bin
GETAPPEARANCE_EXEC=getappearance

$(INSTALL_DIR):
	mkdir -p $(INSTALL_DIR)

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

$(BIN_DIR)/$(GETAPPEARANCE_EXEC).swift: scripts/$(GETAPPEARANCE_EXEC).swift
	cp -f $< $(BIN_DIR)
	chmod uog+x $@

.PHONY: install-getappearance
install-getappearance: $(BIN_DIR)/$(GETAPPEARANCE_EXEC).swift

.PHONY: uninstall-getappearance
uninstall-getappearance: $(BIN_DIR)/$(GETAPPEARANCE_EXEC).swift
	rm -f $<

.PHONY: install
install: $(INSTALL_DIR)/images

$(INSTALL_DIR)/images: $(INSTALL_DIR)
	rsync -avh --size-only images/ "$@/"

.PHONY: image-load-path
image-load-path:
	echo $(INSTALL_DIR)/images

.PHONY: uninstall
uninstall:
	rm -rf $(INSTALL_DIR)


## Bump Patch Version
.PHONY: bump-calle24
bump-calle24:
	sed -i 's/;; Version: $(VERSION)/;; Version: $(VERSION_BUMP)/' $(MAIN_EL)

.PHONY: bump
bump: bump-calle24
	git commit -m 'Bump version to $(VERSION_BUMP)' $(MAIN_EL)
	git push

.PHONY: checkout-development
checkout-development:
	git checkout development
	git branch --set-upstream-to=origin/development development
	git fetch origin --prune
	git pull

.PHONY: checkout-main
checkout-main:
	git checkout main
	git branch --set-upstream-to=origin/main main
	git fetch origin --prune
	git pull

.PHONY: sync-development-with-main
sync-development-with-main: checkout-main checkout-development
	git merge main

.PHONY: new-sprint
new-sprint: VERSION_BUMP:=$(shell python -m semver nextver $(VERSION) prerelease)
new-sprint: sync-development-with-main bump

.PHONY: create-merge-development-branch
create-merge-development-branch: checkout-development
	git checkout -b merge-development-to-main-$(TIMESTAMP)
	git push --set-upstream origin merge-development-to-main-$(TIMESTAMP)

## Create GitHub pull request for development
.PHONY: create-pr
create-pr:
	gh pr create --base development --fill

.PHONY: create-patch-pr
create-patch-pr:
	gh pr create --base main --fill

## Create GitHub pull request for release
.PHONY: create-release-pr
create-release-pr: create-merge-development-branch
	gh pr create --base main \
--title "Merge development to main $(TIMESTAMP)" \
--fill-verbose

.PHONY: create-release-tag
create-release-tag: checkout-main bump
	git tag $(VERSION_BUMP)
	git push origin $(VERSION_BUMP)

.PHONY: create-gh-release
create-gh-release: VERSION_BUMP:=$(shell python -m semver nextver $(VERSION) $(BUMP_LEVEL))
create-gh-release: create-release-tag
	gh release create --draft --title v$(VERSION_BUMP) --generate-notes $(VERSION_BUMP)

.PHONY: status
status:
	git status
