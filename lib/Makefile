DRONES_DIR = $(shell git config "borg.drones-directory" || echo "$(HOME)/.config/doom/lib")
LOCAL_DIR = $(DRONES_DIR)/../.local

-include $(DRONES_DIR)/borg/borg.mk

bootstrap-borg:
	@git submodule--helper clone --name borg --path $(DRONES_DIR)/borg \
	--url git@github.com:emacscollective/borg.git
	@cd $(DRONES_DIR)/borg && git symbolic-ref HEAD refs/heads/main
	@cd $(DRONES_DIR)/borg && git reset --hard HEAD
	@cd $(DRONES_DIR)/.. && git submodule add --name borg git@github.com:emacscollective/borg.git lib/borg

bootstrap-epkg:
	@mkdir -p $(LOCAL_DIR)
	@git clone git@github.com:emacsmirror/epkgs.git $(LOCAL_DIR)/epkgs

update:
	@cd $(DRONES_DIR)/.. && sh ./etc/shell/update-submodule.sh

init:
	@git submodule update --init
	@git submodule foreach git reset --hard

sync:
	@git submodule sync
