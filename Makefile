MKDIR=mkdir -p
XARGS=xargs

OS=$(call lc,$(shell uname -s))

ROOT=$(PWD)


.PHONY: client clean contrib default deps fmt

default: client

clean:
	@git clean -dxf

# client
CLIENT_PATH=client
export GOPATH:=$(GOPATH):$(ROOT)/$(CLIENT_PATH)

CLIENT_SRC_PATH=$(CLIENT_PATH)/src/main
CLIENT_BIN=priv/client

client: fmt $(CLIENT_BIN)

$(CLIENT_BIN): $(wildcard $(CLIENT_SRC_PATH)/*.go)
	@echo $(wildcard $(CLIENT_SRC_PATH)/*.go)
	@cd $(CLIENT_PATH)/src  && go install -v main
	@$(MKDIR) priv && cp $(CLIENT_PATH)/bin/main $@

fmt:
	@cd $(CLIENT_PATH)/src && goimports -w *

deps:
	@go get -u -v golang.org/x/tools/cmd/goimports
	@cd src && go get -t -v *

test: client
	$(CLIENT_BIN) 127.0.0.1:5555

# Protocol Buffers Compiler
ifeq ($(OS), linux)
PROTOC_URL="https://github.com/google/protobuf/releases/download/v3.1.0/protoc-3.1.0-linux-x86_64.zip"
endif
ifeq ($(OS), darwin)
PROTOC_URL="https://github.com/google/protobuf/releases/download/v3.1.0/protoc-3.1.0-osx-x86_64.zip"
endif

contrib/bin/protoc:
	@cd contrib && wget -O /tmp/protoc.zip $(PROTOC_URL)
	@cd contrib && unzip /tmp/protoc.zip

# protocol
PB_FILES := $(wildcard protocol/*.proto)

PROTOC_GEN_GO=$(GO_BIN_PATH)/bin/protoc-gen-go
PB_GO_DIR=src/protocol
PB_GO_FILES := $(patsubst protocol/%.proto,$(PB_GO_DIR)/%.pb.go,$(PB_FILES))
PKG_PROTO=pkg/$(OS)_amd64/protocol.a

PROTOC_GEN_TS=$(GO_BIN_PATH)/bin/protoc-gen-ts
PB_TS_DIR=www/src/main/app/proto
PB_TS_FILES := $(patsubst protocol/%.proto,$(PB_TS_DIR)/%.ts,$(PB_FILES))

proto_dbg:
	@echo $(GO_BIN_PATH)
	@echo $(PB_GO_DIR)
	@echo $(PB_GO_FILES)
	@echo $(PROTOC_GEN_TS)
	@echo $(PB_TS_DIR)
	@echo $(PB_TS_FILES)

protocol: $(PROTOC_GEN_GO) $(PB_GO_DIR) $(PKG_PROTO) $(PB_TS_DIR) $(PROTOC_GEN_TS) $(PB_TS_FILES)

$(PROTOC_GEN_GO):
	@go get -u -v github.com/golang/protobuf/protoc-gen-go

$(PB_GO_DIR)/:
	@mkdir -p $@

$(PB_GO_DIR)/%.pb.go: protocol/%.proto
	@echo $^

$(PKG_PROTO): $(PB_FILES) $(PB_GO_FILES)
	protoc -I=protocol/ --go_out=$(PB_GO_DIR) protocol/*.proto
	cd src && go install protocol

$(PB_TS_DIR)/:
	@mkdir -p $@

$(PROTOC_GEN_TS):
	@go get -u -v github.com/enjoypi/protoc-gen-ts

$(PB_TS_DIR)/%.ts : protocol/%.proto
	protoc -I=protocol/ --ts_out=$(PB_TS_DIR) $^

# release
