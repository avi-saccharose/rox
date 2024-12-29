#![allow(dead_code)]
use std::collections::HashSet;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
enum OpCode {
    Constant(usize),
    Jump(usize),
    True,
    Pop,
    Push,
    False,
    Nil,
    Add,
    Sub,
    Mul,
    Print,
    Return,
    Div,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
enum Obj {
    Num(i64),
    Bool(bool),
    Nil,
    String(String),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
struct Chunk {
    code: Vec<OpCode>,
    constants: Vec<Obj>,
}

impl Chunk {
    fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Vm {
    ip: usize,
    code: Chunk,
    strings: HashSet<String>,
    stack: [Obj; 30],
    stack_ip: usize,
}

impl Default for Vm {
    fn default() -> Self {
        Self {
            ip: 0,
            code: Chunk::new(),
            strings: HashSet::new(),
            stack: [const { Obj::Nil }; 30],
            stack_ip: 0,
        }
    }
}

impl Vm {
    #[inline]
    fn push(&mut self, obj: Obj) -> Result<(), String> {
        self.stack[self.stack_ip] = obj;
        self.stack_ip += 1;
        Ok(())
    }

    fn peek(&self, idx: usize) -> &Obj {
        &self.stack[self.stack_ip]
    }

    #[inline]
    fn pop(&mut self) -> Result<Obj, String> {
        if self.stack_ip == 0 {
            dbg!("Stack is empty!!!");
            dbg!(self.stack_ip);
        } else {
            self.stack_ip -= 1;
        }
        Ok(self.stack[self.stack_ip].clone())
    }

    #[inline]
    fn read_constant(&mut self, idx: usize) -> Result<Obj, String> {
        if let Some(obj) = self.code.constants.get(idx) {
            return Ok(obj.clone());
        }
        Err("no".to_string())
    }

    #[inline]
    fn read_byte(&self) -> Option<&OpCode> {
        //self.ip += 1;
        self.code.code.get(self.ip - 1)
    }

    fn binary(&self, a: Obj, b: Obj, op: OpCode) -> Result<Obj, String> {
        match (a, b) {
            (Obj::Num(a), Obj::Num(b)) => match op {
                OpCode::Add => Ok(Obj::Num(a + b)),
                OpCode::Sub => Ok(Obj::Num(a - b)),
                OpCode::Mul => Ok(Obj::Num(a * b)),
                OpCode::Div => Ok(Obj::Num(a / b)),
                _ => unreachable!(),
            },
            (Obj::String(a), Obj::String(b)) => match op {
                OpCode::Add => Ok(Obj::String(format!("{a}{b}"))),
                _ => Err("no op on string".to_string()),
            },
            (_, _) => Err("Invalid operation".to_string()),
        }
    }

    fn run(&mut self, chunk: Chunk) -> Result<(), String> {
        self.code = chunk;
        self.ip = 0;
        loop {
            self.ip += 1;
            let op = match self.read_byte() {
                Some(op_code) => op_code,
                None => return Ok(()),
            };
            match op {
                OpCode::Pop => {
                    self.pop()?;
                }
                OpCode::Jump(loc) => {
                    self.ip = *loc;
                }
                OpCode::Add => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = self.binary(a, b, OpCode::Add)?;
                    self.push(result)?;
                }
                OpCode::Sub => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = self.binary(a, b, OpCode::Sub)?;
                    self.push(result)?;
                }
                OpCode::Div => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = self.binary(a, b, OpCode::Div)?;
                    self.push(result)?;
                }
                OpCode::Mul => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = self.binary(a, b, OpCode::Mul)?;
                    self.push(result)?;
                }
                OpCode::Constant(idx) => {
                    let value = self.read_constant(*idx)?;
                    self.push(value)?;
                }
                OpCode::Print => {
                    dbg!(&self);
                    let value = self.pop()?;
                    dbg!(value);
                }
                OpCode::Return => return Ok(()),
                _ => todo!(),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn vm_hello() {
        let chunk = Chunk {
            code: vec![OpCode::Constant(0), OpCode::Print, OpCode::Return],
            constants: vec![Obj::Num(12)],
        };
        let mut vm = Vm::default();
        vm.run(chunk);
        assert_eq!(Obj::Num(12), vm.pop().unwrap())
    }

    #[test]
    fn add_num() {
        let chunk = Chunk {
            code: vec![
                OpCode::Constant(0),
                OpCode::Constant(1),
                OpCode::Add,
                OpCode::Return,
            ],
            constants: vec![Obj::Num(4), Obj::Num(6)],
        };
        let mut vm = Vm::default();
        vm.run(chunk);
        assert_eq!(Obj::Num(10), vm.pop().unwrap())
    }
}
