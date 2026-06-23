use thiserror::Error;

pub type Instructions = Vec<u8>;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Opcode {
    Constant,
    Null,
    True,
    False,

    Add,
    Sub,
    Mul,
    Div,
    Pop,
    Minus,
    Bang,

    Equal,
    NotEqual,
    GreaterThan,
    GreaterEqual,
    And,
    Or,
    BitAnd,
    BitOr,

    LessThan,
    LessEqual,
    JumpNotTruthy,
    Jump,

    Array,
    Dictionary,
    Index,
    Call,
    Closure,
    CurrentClosure,

    Return,
    ReturnValue,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    GetBuiltin,
    GetFree,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperandWidth {
    U8,
    U16,
}

impl OperandWidth {
    pub const fn size(self) -> usize {
        match self {
            Self::U8 => 1,
            Self::U16 => 2,
        }
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum CodeError {
    #[error("unknown opcode {0}")]
    UnknownOpcode(u8),
    #[error("instruction stream ended while reading operands for {0:?}")]
    MissingOperands(Opcode),
}

impl TryFrom<u8> for Opcode {
    type Error = CodeError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value <= Self::GetFree as u8 {
            // SAFETY: we already checked that the value is in the valid bound of OpCode
            Ok(unsafe { std::mem::transmute::<u8, Self>(value) })
        } else {
            Err(CodeError::UnknownOpcode(value))
        }
    }
}

impl Opcode {
    pub const fn operand_widths(self) -> &'static [OperandWidth] {
        match self {
            Self::Constant
            | Self::Array
            | Self::Dictionary
            | Self::JumpNotTruthy
            | Self::Jump
            | Self::SetGlobal
            | Self::GetGlobal => &[OperandWidth::U16],
            Self::Closure => &[OperandWidth::U16, OperandWidth::U8],
            Self::Call | Self::SetLocal | Self::GetLocal | Self::GetFree | Self::GetBuiltin => {
                &[OperandWidth::U8]
            }
            _ => &[],
        }
    }
}

impl std::fmt::Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

pub fn make(instructions: &mut Instructions, opcode: Opcode, operands: &[usize]) -> usize {
    let operand_widths = opcode.operand_widths();
    debug_assert_eq!(operands.len(), operand_widths.len());

    let instruction_size = 1 + operand_widths
        .iter()
        .map(|width| width.size())
        .sum::<usize>();
    instructions.reserve(instruction_size);
    instructions.push(opcode as u8);
    for (operand, width) in operands.iter().zip(operand_widths) {
        match width {
            OperandWidth::U8 => instructions.push(*operand as u8),
            OperandWidth::U16 => instructions.extend_from_slice(&(*operand as u16).to_be_bytes()),
        }
    }

    instruction_size
}

pub fn read_u16(instructions: &[u8]) -> u16 {
    u16::from_be_bytes([instructions[0], instructions[1]])
}

pub fn disassemble(instructions: &[u8]) -> Result<String, CodeError> {
    let mut output = String::new();
    let mut ip = 0;

    while ip < instructions.len() {
        let position = ip;
        let opcode = Opcode::try_from(instructions[ip])?;
        ip += 1;

        let mut operands = Vec::new();
        for width in opcode.operand_widths() {
            match width {
                OperandWidth::U8 => {
                    if ip >= instructions.len() {
                        return Err(CodeError::MissingOperands(opcode));
                    }
                    operands.push(usize::from(instructions[ip]));
                    ip += 1;
                }
                OperandWidth::U16 => {
                    if ip + 1 >= instructions.len() {
                        return Err(CodeError::MissingOperands(opcode));
                    }
                    operands.push(usize::from(read_u16(&instructions[ip..])));
                    ip += 2;
                }
            }
        }

        output.push_str(&format!("{position:04} {opcode}"));
        for operand in operands {
            output.push_str(&format!(" {operand}"));
        }
        output.push('\n');
    }

    Ok(output)
}

#[cfg(test)]
mod test {
    use crate::code::Instructions;

    use super::{Opcode, OperandWidth, disassemble, make, read_u16};

    #[test]
    fn encodes_and_decodes_u16_operands() {
        let mut instructions = Instructions::new();
        let instruction_size = make(&mut instructions, Opcode::Constant, &[513]);

        assert_eq!(instructions[0], Opcode::Constant as u8);
        assert_eq!(read_u16(&instructions[1..]), 513);
        assert_eq!(instruction_size, 3)
    }

    #[test]
    fn returns_operand_widths_and_encodes_u8_operands() {
        let mut instructions = Instructions::new();
        let instruction_size = make(&mut instructions, Opcode::SetLocal, &[255]);

        assert_eq!(Opcode::Constant.operand_widths(), &[OperandWidth::U16]);
        assert_eq!(Opcode::SetLocal.operand_widths(), &[OperandWidth::U8]);
        assert_eq!(Opcode::Add.operand_widths(), &[]);
        assert_eq!(instructions, [Opcode::SetLocal as u8, 255]);
        assert_eq!(instruction_size, 2)
    }

    #[test]
    fn disassembles_instructions_as_text() {
        let mut instructions = Instructions::new();
        make(&mut instructions, Opcode::Closure, &[2, 1]);
        make(&mut instructions, Opcode::Call, &[3]);
        make(&mut instructions, Opcode::GetFree, &[0]);
        make(&mut instructions, Opcode::GetBuiltin, &[2]);
        make(&mut instructions, Opcode::ReturnValue, &[]);

        assert_eq!(
            disassemble(&instructions).unwrap(),
            "0000 Closure 2 1\n0004 Call 3\n0006 GetFree 0\n0008 GetBuiltin 2\n0010 ReturnValue\n"
        );
    }
}
