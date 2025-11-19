use inkwell::context::Context;
use inkwell::types::{BasicType, BasicTypeEnum, IntType, FloatType};
use crate::lexer::{LangType, TypeBase};
use crate::codegen::CodegenError;

/// Convert a LangType to an LLVM type
pub fn lang_type_to_llvm<'ctx>(
    context: &'ctx Context,
    lang_type: &LangType,
) -> Result<BasicTypeEnum<'ctx>, CodegenError> {
    // Get the base type
    let base_type: BasicTypeEnum<'ctx> = match lang_type.base {
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
    };

    // Apply pointer depth
    let mut result_type = base_type;
    for _ in 0..lang_type.pointer_depth {
        result_type = result_type.ptr_type(inkwell::AddressSpace::default()).into();
    }

    Ok(result_type)
}

/// Check if a type is void
pub fn is_void_type(lang_type: &LangType) -> bool {
    matches!(lang_type.base, TypeBase::Void) && lang_type.pointer_depth == 0
}

/// Get LLVM integer type for a given bit width
pub fn get_int_type<'ctx>(context: &'ctx Context, bits: u32) -> Result<IntType<'ctx>, CodegenError> {
    match bits {
        8 => Ok(context.i8_type()),
        16 => Ok(context.i16_type()),
        32 => Ok(context.i32_type()),
        64 => Ok(context.i64_type()),
        _ => Err(CodegenError::TypeError(
            format!("Invalid integer size: {}", bits),
            crate::lexer::Position::new(0, 0),
        )),
    }
}

/// Get LLVM float type for a given bit width
pub fn get_float_type<'ctx>(context: &'ctx Context, bits: u32) -> Result<FloatType<'ctx>, CodegenError> {
    match bits {
        32 => Ok(context.f32_type()),
        64 => Ok(context.f64_type()),
        _ => Err(CodegenError::TypeError(
            format!("Invalid float size: {}", bits),
            crate::lexer::Position::new(0, 0),
        )),
    }
}
