pub enum Command {
    Push(i32),
    Pop,
    Add,
    Subtract,
}

pub struct VirtualMachine {
    commands: Vec<Command>,
    stack: Vec<i32>,
}

impl VirtualMachine {
    pub fn new(program: Vec<Command>) -> VirtualMachine {
        VirtualMachine {
            commands: program,
            stack: Vec::new(),
        }
    }
    pub fn execute(&mut self) {
        for command in self.commands {
            match command {
                Command::Push(value) => self.stack.push(value),
                Command::Pop => self.stack.pop(),
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
