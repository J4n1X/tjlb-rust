use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::types::{BasicTypeEnum, IntType, FloatType};
use crate::lexer::{LangType, TypeBase};
use crate::codegen::CodegenError;

/// Convert a `LangType` to an LLVM type
/// # Errors
/// Returns `CodegenError::TypeError` if the type is invalid
pub fn lang_type_to_llvm<'ctx>(
    context: &'ctx Context,
    lang_type: &LangType,
) -> Result<BasicTypeEnum<'ctx>, CodegenError> {

    // If it's a pointer, then we just have to make that, LLVM does not differentiate (anymore)
    if lang_type.pointer_depth > 0 {
        return Ok(context.ptr_type(AddressSpace::default()).into());
    }

    // Get the base type
    Ok(match lang_type.base {
        TypeBase::SInt => match lang_type.size_bits {
            8 => context.i8_type().into(),
            16 => context.i16_type().into(),
            32 => context.i32_type().into(),
            64 => context.i64_type().into(),
            _ => {
                return Err(CodegenError::TypeError(
                    format!("Invalid signed integer size: {}", lang_type.size_bits),
                    crate::lexer::Position::new(0, 0),
                ))
            }
        },
        TypeBase::UInt => match lang_type.size_bits {
            8 => context.i8_type().into(),
            16 => context.i16_type().into(),
            32 => context.i32_type().into(),
            64 => context.i64_type().into(),
            _ => {
                return Err(CodegenError::TypeError(
                    format!("Invalid unsigned integer size: {}", lang_type.size_bits),
                    crate::lexer::Position::new(0, 0),
                ))
            }
        },
        TypeBase::SFloat => match lang_type.size_bits {
            32 => context.f32_type().into(),
            64 => context.f64_type().into(),
            _ => {
                return Err(CodegenError::TypeError(
                    format!("Invalid float size: {}", lang_type.size_bits),
                    crate::lexer::Position::new(0, 0),
                ))
            }
        },
        TypeBase::Void => {
            // Void can't be a basic type directly, but we handle it specially
            // For now, return i8 and the caller should check for void
            return Err(CodegenError::TypeError(
                "Void type cannot be used as a value type".to_string(),
                crate::lexer::Position::new(0, 0),
            ));
        }
    })
}

/// Check if a type is void
#[must_use]
pub fn is_void_type(lang_type: &LangType) -> bool {
    matches!(lang_type.base, TypeBase::Void) && lang_type.pointer_depth == 0
}

/// Get LLVM integer type for a given bit width
/// # Errors
/// Returns `CodegenError::TypeError` if the bit width is invalid
pub fn get_int_type(context: &'_ Context, bits: u32) -> Result<IntType<'_>, CodegenError> {
    match bits {
        8 => Ok(context.i8_type()),
        16 => Ok(context.i16_type()),
        32 => Ok(context.i32_type()),
        64 => Ok(context.i64_type()),
        _ => Err(CodegenError::TypeError(
            format!("Invalid integer size: {bits}"),
            crate::lexer::Position::new(0, 0),
        )),
    }
}

/// Get LLVM float type for a given bit width
/// # Errors
/// Returns `CodegenError::TypeError` if the bit width is invalid
pub fn get_float_type(context: &'_ Context, bits: u32) -> Result<FloatType<'_>, CodegenError> {
    match bits {
        32 => Ok(context.f32_type()),
        64 => Ok(context.f64_type()),
        _ => Err(CodegenError::TypeError(
            format!("Invalid float size: {bits}"),
            crate::lexer::Position::new(0, 0),
        )),
    }
}
