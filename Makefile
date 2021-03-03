BIN_NAME = fermat

${BIN_NAME}-linux:
	cargo build --release --target=x86_64-unknown-linux-musl
	cp target/x86_64-unknown-linux-musl/release/${BIN_NAME} $@

${BIN_NAME}-windows.exe:
	cargo build --release --target=x86_64-pc-windows-msvc
	cp target/x86_64-pc-windows-msvc/release/${BIN_NAME}.exe $@

${BIN_NAME}-mac:
	cargo build --release --target=x86_64-apple-darwin
	cp target/x86_64-apple-darwin/release/${BIN_NAME} $@
