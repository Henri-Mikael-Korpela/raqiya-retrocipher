rm -rf ./pkg
cd ./frontend
wasm-pack build --target web --out-name frontend --out-dir ../pkg --no-pack --no-typescript