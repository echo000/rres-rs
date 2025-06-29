use flate2::read::ZlibDecoder;
use std::fs::File;
use std::io::{Error, ErrorKind, Read, Result, Seek, SeekFrom};
use std::path::{Path, PathBuf};

/// Reads a u16 in little endian.
fn read_u16_le<R: Read>(r: &mut R) -> std::io::Result<u16> {
    let mut buf = [0u8; 2];
    r.read_exact(&mut buf)?;
    Ok(u16::from_le_bytes(buf))
}

/// Reads a u32 in little endian.
fn read_u32_le<R: Read>(r: &mut R) -> std::io::Result<u32> {
    let mut buf = [0u8; 4];
    r.read_exact(&mut buf)?;
    Ok(u32::from_le_bytes(buf))
}

// Constants
pub const RRES_MAX_FILENAME_SIZE: usize = 1024;

/// File header (16 bytes)
#[derive(Debug, Clone)]
pub struct FileHeader {
    pub id: [u8; 4],      // File identifier: b"rres"
    pub version: u16,     // File version: 100 for version 1.0
    pub chunk_count: u16, // Number of resource chunks in the file (MAX: 65535)
    pub cd_offset: u32,   // Central Directory offset in file (0 if not available)
    pub reserved: u32,    // <reserved>
}

/// Resource chunk info header (32 bytes)
#[derive(Debug, Clone)]
pub struct ResourceChunkInfo {
    pub chunk_type: [u8; 4],         // Resource chunk type (FourCC)
    pub id: u32,                     // Resource chunk identifier (CRC32 filename hash or custom)
    pub comp_type: CompressionType,  // Data compression algorithm
    pub cipher_type: EncryptionType, // Data encryption algorithm
    pub flags: u16,                  // Data flags (if required)
    pub packed_size: u32, // Data chunk size (compressed/encrypted + custom data appended)
    pub base_size: u32,   // Data base size (uncompressed/unencrypted)
    pub next_offset: u32, // Next resource chunk global offset (if resource has multiple chunks)
    pub reserved: u32,    // <reserved>
    pub crc32: u32,       // Data chunk CRC32 (prop_count + props[] + data)
}

/// Resource chunk data
#[derive(Debug, Clone)]
pub struct ResourceChunkData {
    pub prop_count: u32,
    pub props: Vec<u32>,
    pub raw: Vec<u8>,
}

/// Resource chunk
#[derive(Debug, Clone)]
pub struct ResourceChunk {
    pub info: ResourceChunkInfo,
    pub data: ResourceChunkData,
}

/// Resource multi - supports multiple resource chunks
#[derive(Debug, Clone)]
pub struct ResourceMulti {
    pub chunks: Vec<ResourceChunk>,
}

/// CDIR: Central directory entry
#[derive(Debug, Clone)]
pub struct DirEntry {
    pub id: u32,          // Resource id
    pub offset: u32,      // Resource global offset in file
    pub reserved: u32,    // reserved
    pub filename: String, // Resource original fileName (NULL terminated and padded to 4-byte alignment)
}

/// CDIR: Central directory (conforms to ResourceChunkData)
#[derive(Debug, Clone)]
pub struct CentralDir {
    pub entries: Vec<DirEntry>,
    path: PathBuf,
}

/// FNTG: Font glyphs info (32 bytes)
#[derive(Debug, Clone)]
pub struct FontGlyphInfo {
    pub x: i32,
    pub y: i32,
    pub width: i32,
    pub height: i32,
    pub value: i32,
    pub offset_x: i32,
    pub offset_y: i32,
    pub advance_x: i32,
}

// ------------------- Enums --------------------------------

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
// Data type determines the properties and data for every chunk
pub enum ResourceDataType {
    Null = 0,
    Raw = 1,
    Text = 2,
    Image = 3,
    Wave = 4,
    Vertex = 5,
    FontGlyphs = 6,
    Link = 99,
    Directory = 100,
    // Future: PackFont = 110, PackMesh = 120, etc.
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Compression type for resource data
pub enum CompressionType {
    None = 0,
    Rle = 1,
    Deflate = 10,
    Lz4 = 20,
    Lzma2 = 30,
    Qoi = 40,
    // Future: add more as needed
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Encryption type for resource data
pub enum EncryptionType {
    None = 0,
    Xor = 1,
    Des = 10,
    Tdes = 11,
    Idea = 20,
    Aes = 30,
    AesGcm = 31,
    Xtea = 40,
    Blowfish = 50,
    Rsa = 60,
    Salsa20 = 70,
    Chacha20 = 71,
    Xchacha20 = 72,
    Xchacha20Poly1305 = 73,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Error types for resource loading
pub enum ErrorType {
    Success = 0,
    FileNotFound,
    FileFormat,
    MemoryAlloc,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Text encoding for text resources
pub enum TextEncoding {
    Undefined = 0,
    Utf8 = 1,
    Utf8Bom = 2,
    Utf16Le = 10,
    Utf16Be = 11,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Programming language for code resources
pub enum CodeLang {
    Undefined = 0,
    C,
    Cpp,
    Cs,
    Lua,
    Js,
    Python,
    Rust,
    Zig,
    Odin,
    Jai,
    Gdscript,
    Glsl,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Pixel format for image data
pub enum PixelFormat {
    Undefined = 0,
    UncompGrayscale = 1,
    UncompGrayAlpha,
    UncompR5G6B5,
    UncompR8G8B8,
    UncompR5G5B5A1,
    UncompR4G4B4A4,
    UncompR8G8B8A8,
    UncompR32,
    UncompR32G32B32,
    UncompR32G32B32A32,
    CompDxt1Rgb,
    CompDxt1Rgba,
    CompDxt3Rgba,
    CompDxt5Rgba,
    CompEtc1Rgb,
    CompEtc2Rgb,
    CompEtc2EacRgba,
    CompPvrtRgb,
    CompPvrtRgba,
    CompAstc4x4Rgba,
    CompAstc8x8Rgba,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Vertex attribute types for vertex data
pub enum VertexAttribute {
    Position = 0,
    TexCoord1 = 10,
    TexCoord2 = 11,
    TexCoord3 = 12,
    TexCoord4 = 13,
    Normal = 20,
    Tangent = 30,
    Color = 40,
    Index = 100,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Vertex format for vertex attributes
pub enum VertexFormat {
    UByte = 0,
    Byte,
    UShort,
    Short,
    UInt,
    Int,
    HFloat,
    Float,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Font style for font glyphs
pub enum FontStyle {
    Undefined = 0,
    Regular,
    Bold,
    Italic,
}

// ResourceDataType
impl TryFrom<u32> for ResourceDataType {
    type Error = Error;
    fn try_from(value: u32) -> Result<Self> {
        use ResourceDataType::*;
        match value {
            0 => Ok(Null),
            1 => Ok(Raw),
            2 => Ok(Text),
            3 => Ok(Image),
            4 => Ok(Wave),
            5 => Ok(Vertex),
            6 => Ok(FontGlyphs),
            99 => Ok(Link),
            100 => Ok(Directory),
            n => Err(Error::new(
                ErrorKind::InvalidData,
                format!("Unknown ResourceDataType: {n}"),
            )),
        }
    }
}

// CompressionType
impl TryFrom<u8> for CompressionType {
    type Error = Error;
    fn try_from(v: u8) -> Result<Self> {
        use CompressionType::*;
        match v {
            0 => Ok(None),
            1 => Ok(Rle),
            10 => Ok(Deflate),
            20 => Ok(Lz4),
            30 => Ok(Lzma2),
            40 => Ok(Qoi),
            n => Err(Error::new(
                ErrorKind::InvalidData,
                format!("Unknown CompressionType: {n}"),
            )),
        }
    }
}

impl TryFrom<u8> for EncryptionType {
    type Error = Error;

    fn try_from(value: u8) -> Result<Self> {
        match value {
            0 => Ok(EncryptionType::None),
            1 => Ok(EncryptionType::Xor),
            10 => Ok(EncryptionType::Des),
            11 => Ok(EncryptionType::Tdes),
            20 => Ok(EncryptionType::Idea),
            30 => Ok(EncryptionType::Aes),
            31 => Ok(EncryptionType::AesGcm),
            40 => Ok(EncryptionType::Xtea),
            50 => Ok(EncryptionType::Blowfish),
            60 => Ok(EncryptionType::Rsa),
            70 => Ok(EncryptionType::Salsa20),
            71 => Ok(EncryptionType::Chacha20),
            72 => Ok(EncryptionType::Xchacha20),
            73 => Ok(EncryptionType::Xchacha20Poly1305),
            n => Err(Error::new(
                ErrorKind::InvalidData,
                format!("Unknown EncryptionType value: {n}"),
            )),
        }
    }
}

// ErrorType
impl TryFrom<u8> for ErrorType {
    type Error = Error;
    fn try_from(v: u8) -> Result<Self> {
        use ErrorType::*;
        match v {
            0 => Ok(Success),
            1 => Ok(FileNotFound),
            2 => Ok(FileFormat),
            3 => Ok(MemoryAlloc),
            n => Err(Error::new(
                ErrorKind::InvalidData,
                format!("Unknown ErrorType: {n}"),
            )),
        }
    }
}

// TextEncoding
impl TryFrom<u8> for TextEncoding {
    type Error = Error;
    fn try_from(v: u8) -> Result<Self> {
        use TextEncoding::*;
        match v {
            0 => Ok(Undefined),
            1 => Ok(Utf8),
            2 => Ok(Utf8Bom),
            10 => Ok(Utf16Le),
            11 => Ok(Utf16Be),
            n => Err(Error::new(
                ErrorKind::InvalidData,
                format!("Unknown TextEncoding: {n}"),
            )),
        }
    }
}

// CodeLang
impl TryFrom<u8> for CodeLang {
    type Error = Error;
    fn try_from(v: u8) -> Result<Self> {
        use CodeLang::*;
        match v {
            0 => Ok(Undefined),
            1 => Ok(C),
            2 => Ok(Cpp),
            3 => Ok(Cs),
            4 => Ok(Lua),
            5 => Ok(Js),
            6 => Ok(Python),
            7 => Ok(Rust),
            8 => Ok(Zig),
            9 => Ok(Odin),
            10 => Ok(Jai),
            11 => Ok(Gdscript),
            12 => Ok(Glsl),
            n => Err(Error::new(
                ErrorKind::InvalidData,
                format!("Unknown CodeLang: {n}"),
            )),
        }
    }
}

// PixelFormat
impl TryFrom<u8> for PixelFormat {
    type Error = Error;
    fn try_from(v: u8) -> Result<Self> {
        use PixelFormat::*;
        match v {
            0 => Ok(Undefined),
            1 => Ok(UncompGrayscale),
            2 => Ok(UncompGrayAlpha),
            3 => Ok(UncompR5G6B5),
            4 => Ok(UncompR8G8B8),
            5 => Ok(UncompR5G5B5A1),
            6 => Ok(UncompR4G4B4A4),
            7 => Ok(UncompR8G8B8A8),
            8 => Ok(UncompR32),
            9 => Ok(UncompR32G32B32),
            10 => Ok(UncompR32G32B32A32),
            11 => Ok(CompDxt1Rgb),
            12 => Ok(CompDxt1Rgba),
            13 => Ok(CompDxt3Rgba),
            14 => Ok(CompDxt5Rgba),
            15 => Ok(CompEtc1Rgb),
            16 => Ok(CompEtc2Rgb),
            17 => Ok(CompEtc2EacRgba),
            18 => Ok(CompPvrtRgb),
            19 => Ok(CompPvrtRgba),
            20 => Ok(CompAstc4x4Rgba),
            21 => Ok(CompAstc8x8Rgba),
            n => Err(Error::new(
                ErrorKind::InvalidData,
                format!("Unknown PixelFormat: {n}"),
            )),
        }
    }
}

// VertexAttribute
impl TryFrom<u32> for VertexAttribute {
    type Error = Error;
    fn try_from(v: u32) -> Result<Self> {
        use VertexAttribute::*;
        match v {
            0 => Ok(Position),
            10 => Ok(TexCoord1),
            11 => Ok(TexCoord2),
            12 => Ok(TexCoord3),
            13 => Ok(TexCoord4),
            20 => Ok(Normal),
            30 => Ok(Tangent),
            40 => Ok(Color),
            100 => Ok(Index),
            n => Err(Error::new(
                ErrorKind::InvalidData,
                format!("Unknown VertexAttribute: {n}"),
            )),
        }
    }
}

// VertexFormat
impl TryFrom<u8> for VertexFormat {
    type Error = Error;
    fn try_from(v: u8) -> Result<Self> {
        use VertexFormat::*;
        match v {
            0 => Ok(UByte),
            1 => Ok(Byte),
            2 => Ok(UShort),
            3 => Ok(Short),
            4 => Ok(UInt),
            5 => Ok(Int),
            6 => Ok(HFloat),
            7 => Ok(Float),
            n => Err(Error::new(
                ErrorKind::InvalidData,
                format!("Unknown VertexFormat: {n}"),
            )),
        }
    }
}
// FontStyle
impl TryFrom<u8> for FontStyle {
    type Error = Error;
    fn try_from(v: u8) -> Result<Self> {
        use FontStyle::*;
        match v {
            0 => Ok(Undefined),
            1 => Ok(Regular),
            2 => Ok(Bold),
            3 => Ok(Italic),
            n => Err(Error::new(
                ErrorKind::InvalidData,
                format!("Unknown FontStyle: {n}"),
            )),
        }
    }
}

/// CRC32 Table (standard polynomial 0xEDB88320)
fn crc32_table() -> &'static [u32; 256] {
    use std::sync::OnceLock;
    static TABLE: OnceLock<[u32; 256]> = OnceLock::new();
    TABLE.get_or_init(|| {
        let mut table = [0u32; 256];
        for i in 0..256 {
            let mut crc = i as u32;
            for _ in 0..8 {
                crc = if crc & 1 != 0 {
                    0xEDB88320 ^ (crc >> 1)
                } else {
                    crc >> 1
                };
            }
            table[i as usize] = crc;
        }
        table
    })
}

/// Computes the CRC32 checksum for the given data.
pub fn compute_crc32(data: &[u8]) -> u32 {
    let mut crc = 0xFFFFFFFFu32;
    for &b in data {
        let idx = ((crc ^ (b as u32)) & 0xFF) as usize;
        crc = (crc >> 8) ^ crc32_table()[idx];
    }
    !crc
}

/// Gets the resource data type from a FourCC code.
pub fn get_data_type(fourcc: &[u8; 4]) -> Option<ResourceDataType> {
    match fourcc {
        b"NULL" => Some(ResourceDataType::Null),
        b"RAWD" => Some(ResourceDataType::Raw),
        b"TEXT" => Some(ResourceDataType::Text),
        b"IMGE" => Some(ResourceDataType::Image),
        b"WAVE" => Some(ResourceDataType::Wave),
        b"VRTX" => Some(ResourceDataType::Vertex),
        b"FNTG" => Some(ResourceDataType::FontGlyphs),
        b"LINK" => Some(ResourceDataType::Link),
        b"CDIR" => Some(ResourceDataType::Directory),
        _ => None,
    }
}

/// Password for encryption/decryption (could use RefCell/Arc<Mutex> as needed)
pub mod cipher_password {
    use std::sync::{OnceLock, RwLock};
    pub static PASSWORD: OnceLock<RwLock<String>> = OnceLock::new();

    /// Sets the password for encryption/decryption.
    pub fn set_password(pass: &str) {
        PASSWORD
            .get_or_init(|| RwLock::new("password12345".to_string()))
            .write()
            .unwrap()
            .replace_range(.., pass);
    }

    /// Gets the current password for encryption/decryption.
    pub fn get_password() -> String {
        PASSWORD
            .get_or_init(|| RwLock::new("password12345".to_string()))
            .read()
            .unwrap()
            .clone()
    }
}

impl FileHeader {
    /// Reads a file header from the provided reader.
    pub fn from_reader<R: Read>(r: &mut R) -> Result<Self> {
        let mut id = [0u8; 4];
        r.read_exact(&mut id)?;
        let version = read_u16_le(r)?;
        let chunk_count = read_u16_le(r)?;
        let cd_offset = read_u32_le(r)?;
        let reserved = read_u32_le(r)?;
        Ok(Self {
            id,
            version,
            chunk_count,
            cd_offset,
            reserved,
        })
    }
}

impl ResourceChunkInfo {
    /// Reads a resource chunk info header from the provided reader.
    pub fn from_reader<R: Read>(r: &mut R) -> Result<Self> {
        let mut chunk_type = [0u8; 4];
        r.read_exact(&mut chunk_type)?;
        let id = read_u32_le(r)?;
        let comp_type = {
            let mut b = [0];
            r.read_exact(&mut b)?;
            CompressionType::try_from(b[0])?
        };
        let cipher_type = {
            let mut b = [0];
            r.read_exact(&mut b)?;
            EncryptionType::try_from(b[0])?
        };
        let flags = read_u16_le(r)?;
        let packed_size = read_u32_le(r)?;
        let base_size = read_u32_le(r)?;
        // read 4-byte nextOffset, cast to u64
        let next_offset = read_u32_le(r)?;
        // read 4-byte reserved, cast to u64
        let reserved = read_u32_le(r)?;
        let crc32 = read_u32_le(r)?;
        Ok(Self {
            chunk_type,
            id,
            comp_type,
            cipher_type,
            flags,
            packed_size,
            base_size,
            next_offset,
            reserved,
            crc32,
        })
    }
}

impl ResourceChunk {
    /// Loads a single resource chunk for the provided id.
    pub fn load<P: AsRef<Path>>(path: P, rres_id: u32) -> Result<Self> {
        let mut file = File::open(path)?;
        let header = FileHeader::from_reader(&mut file)?;
        if &header.id != b"rres" || header.version != 100 {
            return Err(Error::new(
                ErrorKind::InvalidData,
                "Invalid rres file signature/version",
            ));
        }
        for _ in 0..header.chunk_count {
            let info = ResourceChunkInfo::from_reader(&mut file)?;
            if info.id == rres_id {
                let mut data = vec![0u8; info.packed_size as usize];
                file.read_exact(&mut data)?;
                let chunk_data = ResourceChunkData::from_packed(&info, &data)?;

                if compute_crc32(&data) != info.crc32 {
                    return Err(Error::new(ErrorKind::InvalidData, "CRC32 mismatch"));
                }
                return Ok(ResourceChunk {
                    info,
                    data: chunk_data,
                });
            } else {
                file.seek(SeekFrom::Current(info.packed_size as i64))?;
            }
        }
        Err(Error::new(ErrorKind::NotFound, "Resource ID not found"))
    }

    /// Unpacks the chunk data, decompressing and decrypting as needed.
    pub fn unpack(&mut self) -> Result<bool> {
        let mut changed = false;

        // Handle decryption first if needed
        if self.info.cipher_type != EncryptionType::None {
            self.data.raw = match self.info.cipher_type {
                EncryptionType::None => self.data.raw.clone(),
                cipher => self.decrypt(cipher)?,
            };
            self.info.cipher_type = EncryptionType::None;
            changed = true;
        }
        // Then handle decompression if needed
        if self.info.comp_type != CompressionType::None {
            self.data.raw = match self.info.comp_type {
                CompressionType::None => self.data.raw.clone(),
                comp => self.decompress(comp)?,
            };
            self.info.comp_type = CompressionType::None;
            changed = true;
        }

        if changed {
            self.info.packed_size = self.info.base_size;

            // --- Additional: update prop_count, props, and raw as in C ---
            let data = &self.data.raw;

            // Ensure there are at least 4 bytes for prop_count
            if data.len() < 4 {
                return Err(Error::new(
                    ErrorKind::UnexpectedEof,
                    "Chunk data too short for prop_count",
                ));
            }
            let prop_count = u32::from_le_bytes(data[0..4].try_into().unwrap()) as usize;
            self.data.prop_count = prop_count as u32;

            let props_bytes = 4 * prop_count;
            let min_len = 4usize
                .checked_add(props_bytes)
                .ok_or_else(|| Error::new(ErrorKind::InvalidData, "Overflow in props length"))?;

            if data.len() < min_len {
                return Err(Error::new(
                    ErrorKind::UnexpectedEof,
                    format!(
                        "Chunk data too short for props (have {}, need {})",
                        data.len(),
                        min_len
                    ),
                ));
            }

            // Read props if present
            self.data.props.clear();
            for i in 0..prop_count {
                let start = 4 + 4 * i;
                let end = start + 4;
                let prop = u32::from_le_bytes(data[start..end].try_into().unwrap());
                self.data.props.push(prop);
            }

            // Move raw pointer past prop_count and props
            let raw_start = min_len;
            if raw_start > data.len() {
                return Err(Error::new(
                    ErrorKind::UnexpectedEof,
                    format!(
                        "Chunk raw data out of range after props (raw_start {}, data.len {})",
                        raw_start,
                        data.len()
                    ),
                ));
            }
            self.data.raw = data[raw_start..].to_vec();
        }

        Ok(changed)
    }

    /// Decompresses the chunk data using the specified compression type.
    fn decompress(&self, comp: CompressionType) -> Result<Vec<u8>> {
        let data = self.data.raw.as_slice();
        match comp {
            CompressionType::None => Ok(data.to_vec()),
            CompressionType::Deflate => {
                let mut decoder = ZlibDecoder::new(data);
                let mut out = Vec::new();
                decoder.read_to_end(&mut out).map_err(|e| {
                    Error::new(
                        ErrorKind::InvalidData,
                        format!("Deflate decompression failed: {e}"),
                    )
                })?;
                Ok(out)
            }
            CompressionType::Rle => {
                // TODO: Implement RLE decompression
                Err(Error::new(
                    ErrorKind::InvalidData,
                    "RLE decompression not implemented",
                ))
            }
            CompressionType::Lz4 => {
                let packed_size = self.info.packed_size as usize;
                let base_size = self.info.base_size as i32;
                let chunk_type = &self.info.chunk_type;
                match lz4::block::decompress(&data[..packed_size], Some(base_size)) {
                    Ok(unpacked_data) => {
                        if unpacked_data.len() == base_size as usize {
                            println!(
                                "RRES: {}: Data decompressed successfully (LZ4)",
                                std::str::from_utf8(chunk_type).unwrap_or("????")
                            );
                        } else {
                            println!(
                                "RRES: WARNING: Decompressed data could be corrupted, unexpected size (expected {}, got {})",
                                base_size,
                                unpacked_data.len()
                            );
                        }
                        Ok(unpacked_data)
                    }
                    Err(e) => {
                        println!(
                            "RRES: WARNING: {}: Chunk data decompression failed: {}",
                            std::str::from_utf8(chunk_type).unwrap_or("????"),
                            e
                        );
                        Err(Error::new(
                            ErrorKind::InvalidData,
                            "LZ4 decompression failed",
                        ))
                    }
                }
            }
            CompressionType::Lzma2 => {
                // TODO: Implement LZMA2 decompression
                Err(Error::new(
                    ErrorKind::InvalidData,
                    "LZMA2 decompression not implemented",
                ))
            }
            CompressionType::Qoi => {
                // TODO: Implement QOI decompression
                Err(Error::new(
                    ErrorKind::InvalidData,
                    "QOI decompression not implemented",
                ))
            }
        }
    }
    /// Decrypts the chunk data using the specified cipher type.
    fn decrypt(&self, cipher: EncryptionType) -> Result<Vec<u8>> {
        let data = self.data.raw.as_slice();
        match cipher {
            EncryptionType::Xor => {
                // Example: XOR with a fixed key for demonstration
                let key = 0xAAu8;
                Ok(data.iter().map(|b| b ^ key).collect())
            }
            // Add more as needed
            _ => Ok(data.to_vec()),
        }
    }
}

impl ResourceChunkData {
    /// Parses chunk data from a packed slice, using the associated chunk info.
    pub fn from_packed(info: &ResourceChunkInfo, packed: &[u8]) -> Result<Self> {
        if info.comp_type == CompressionType::None && info.cipher_type == EncryptionType::None {
            if packed.len() < 4 {
                return Err(Error::new(ErrorKind::UnexpectedEof, "Chunk data too short"));
            }
            let prop_count = u32::from_le_bytes(packed[0..4].try_into().unwrap());
            let mut props = Vec::new();
            for i in 0..(prop_count as usize) {
                let start = 4 + i * 4;
                let end = start + 4;
                if end > packed.len() {
                    return Err(Error::new(
                        ErrorKind::UnexpectedEof,
                        "Chunk properties out of range",
                    ));
                }
                props.push(u32::from_le_bytes(packed[start..end].try_into().unwrap()));
            }
            let raw_start = 4 + (prop_count as usize) * 4;
            if raw_start > packed.len() {
                return Err(Error::new(
                    ErrorKind::UnexpectedEof,
                    "Chunk raw data out of range",
                ));
            }
            let raw = packed[raw_start..].to_vec();
            Ok(ResourceChunkData {
                prop_count,
                props,
                raw,
            })
        } else {
            // Compressed/encrypted: return all data as 'raw'
            Ok(ResourceChunkData {
                prop_count: 0,
                props: Vec::new(),
                raw: packed.to_vec(),
            })
        }
    }
}

impl ResourceMulti {
    /// Loads all resource chunks matching id.
    pub fn load<P: AsRef<Path>>(path: P, rres_id: u32) -> Result<Self> {
        let mut file = File::open(path)?;
        let header = FileHeader::from_reader(&mut file)?;
        if &header.id != b"rres" || header.version != 100 {
            return Err(Error::new(
                ErrorKind::InvalidData,
                "Invalid rres file signature/version",
            ));
        }
        let mut chunks = Vec::new();
        let mut found = false;

        for _ in 0..header.chunk_count {
            let info = ResourceChunkInfo::from_reader(&mut file)?;
            let chunk_pos = file.stream_position()?;
            if info.id == rres_id {
                found = true;
                let mut curr_pos = chunk_pos;
                // Loop over linked chunks
                loop {
                    file.seek(SeekFrom::Start(curr_pos))?;
                    let info = ResourceChunkInfo::from_reader(&mut file)?;
                    let mut data = vec![0u8; info.packed_size as usize];
                    file.read_exact(&mut data)?;
                    let chunk_data = ResourceChunkData::from_packed(&info, &data)?;
                    if compute_crc32(&data) != info.crc32 {
                        return Err(Error::new(ErrorKind::InvalidData, "CRC32 mismatch"));
                    }
                    chunks.push(ResourceChunk {
                        info: info.clone(),
                        data: chunk_data,
                    });
                    if info.next_offset == 0 {
                        break;
                    }
                    curr_pos = info.next_offset as u64;
                }
                break;
            } else {
                file.seek(SeekFrom::Current(info.packed_size as i64))?;
            }
        }
        if !found {
            return Err(Error::new(ErrorKind::NotFound, "Resource ID not found"));
        }
        Ok(ResourceMulti { chunks })
    }
}

impl CentralDir {
    /// Loads the central directory from file.
    pub fn load<P: AsRef<std::path::Path>>(path: P) -> Result<Self> {
        let mut file = File::open(&path)?;
        let header = FileHeader::from_reader(&mut file)?;
        if header.cd_offset == 0 {
            return Err(Error::new(
                ErrorKind::NotFound,
                "No central directory found",
            ));
        }
        file.seek(SeekFrom::Current(header.cd_offset as i64))?;
        let info = ResourceChunkInfo::from_reader(&mut file)?;
        let mut data = vec![0u8; info.packed_size as usize];
        file.read_exact(&mut data)?;
        let chunk_data = ResourceChunkData::from_packed(&info, &data)?;
        let count = chunk_data.props.first().copied().unwrap_or(0);
        let mut entries = Vec::new();
        let mut pos = 0;
        for _ in 0..count {
            let id = u32::from_le_bytes(chunk_data.raw[pos..pos + 4].try_into().unwrap());
            let offset = u32::from_le_bytes(chunk_data.raw[pos + 4..pos + 8].try_into().unwrap());
            let name_size =
                u32::from_le_bytes(chunk_data.raw[pos + 12..pos + 16].try_into().unwrap()) as usize;
            let filename = String::from_utf8_lossy(&chunk_data.raw[pos + 16..pos + 16 + name_size])
                .trim_end_matches(char::from(0))
                .to_string();
            entries.push(DirEntry {
                id,
                offset,
                reserved: 0, // reserved field is not used
                filename,
            });
            pos += 16 + name_size;
        }
        Ok(CentralDir {
            entries,
            path: path.as_ref().to_path_buf(),
        })
    }
    /// Gets resource id for a provided filename.
    pub fn get_resource_id(&self, filename: &str) -> Option<u32> {
        self.entries
            .iter()
            .find(|e| e.filename.to_lowercase() == filename.to_lowercase())
            .map(|e| e.id)
    }

    ///Returns the path to the central directory file.
    pub fn path(&self) -> &Path {
        &self.path
    }
}
