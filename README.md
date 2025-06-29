# rres-rs

A **WIP** port of [rres](https://github.com/raysan5/rres) (the Raylib resource file format and toolkit) in Rust!  
This project aims for compatibility with the original C rres, supporting resource packaging, reading, and unpacking in safe, idiomatic Rust.

---

## Features

- Safe parsing of `.rres` files
- Support for resource chunk reading
- CRC32, compression type, and encryption type management
- (Planned) Decompression (Deflate, LZ4, etc.) and decryption support
- (Planned) Central directory and resource multi-chunk

---

## Status

- [x] File and chunk header parsing
- [x] Resource data extraction
- [x] CRC32 validation
- [x] Type-safe enums for resource types, compression, encryption, etc.
- [x] Basic decompress/unpack (Deflate)
- [x] Basic decompress/unpack (LZ4)
- [ ] Complete property parsing for all types
- [ ] Full encryption/decryption support
- [ ] All resource types (Image, Wave, Vertex, etc.)

---

## Example

```rust
use rres_rs::CentralDir;
let cdir = CentralDir::load(path).unwrap();
```

---

## License

MIT, like the original rres.
