pub enum Command {
    Push(i32),
    Pop,
    Add,
    Subtract,
}

pub struct VirtualMachine {
    stack: Vec<i32>,
}

impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        VirtualMachine { stack: Vec::new() }
    }
    pub fn execute(&mut self, commands: Vec<Command>) {
        for command in commands {
            match command {
                Command::Push(value) => self.stack.push(value),
                Command::Pop => {
                    self.stack.pop().expect("Stack underflow");
                }
                Command::Add => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(a + b);
                }
                Command::Subtract => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(a - b);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_program() {
        let program = vec![
            Command::Push(1),
            Command::Push(2),
            Command::Add,
            Command::Push(3),
            Command::Subtract,
        ];
        let mut vm = VirtualMachine::new();
        vm.execute(program);
        assert_eq!(vm.stack, vec![0]);
    }
}
