use std::{
    env,
    fs::File,
    io::{BufReader, BufWriter, Read, Write},
    vec,
};

macro_rules! exit_print {
    ($format:expr) => {
        println!("{}", $format);
        std::process::exit(1);
    };
}

#[derive(Debug)]
enum RuntimeError {
    PointerOutOfBounds(String),
    WrongInputFormat(String),
    LoopNeverOpened(String),
    LoopNeverClosed(String),
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
enum OpCode {
    IncrementPointer,
    DecrementPointer,
    IncrementMemoryAtPointer,
    DecrementMemoryAtPointer,
    Print,
    Input,
    LoopOpen,
    LoopClose,
    Loop(Vec<OpCode>),
    ResetMemory,  //Brainfuck+ operation | Will 0 out the memory.
    ResetPointer, //Brainfuck+ operation | Sets pointer to 0.
    Unknown,
}

struct Lexer {
    file_content: String,
}

struct LexerOutput {
    token_stream: Vec<OpCode>,
    memory_size: usize,
}

#[derive(Debug)]
struct OpCodeOpt {
    opcode: OpCode,
    count: usize,
}

impl Lexer {
    fn new(file_content: String) -> Self {
        Self { file_content }
    }

    fn command_line_env(&mut self) -> Result<(bool, bool, LexerOutput), RuntimeError> {
        let mut input = String::new();

        println!();
        print!(">>> | ");

        let _ = std::io::stdout().flush();

        std::io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        self.file_content = input.clone();

        Ok((
            input.contains(&"exit".to_owned()),
            input.contains(&"$".to_string()),
            self.lex(),
        ))
    }

    fn lex(&mut self) -> LexerOutput {
        let mut op_codes = vec![];

        let mut memory_size = 0;

        for chr in self.file_content.chars() {
            let op_code = match chr {
                '>' => {
                    memory_size += 1;
                    OpCode::IncrementPointer
                }
                '<' => OpCode::DecrementPointer,
                '+' => OpCode::IncrementMemoryAtPointer,
                '-' => OpCode::DecrementMemoryAtPointer,
                '.' => OpCode::Print,
                ',' => OpCode::Input,
                '[' => OpCode::LoopOpen,
                ']' => OpCode::LoopClose,
                ';' => OpCode::ResetMemory,
                '*' => OpCode::ResetPointer,
                _ => OpCode::Unknown,
            };

            match op_code {
                OpCode::Unknown => {}
                _ => op_codes.push(op_code),
            }
        }

        LexerOutput {
            token_stream: op_codes,
            memory_size,
        }
    }
}

struct Parser {
    token_stream: Vec<OpCode>,
    memory: Vec<u8>,
    pointer: usize,
    memory_capacity: usize,
}

impl Parser {
    fn new(token_stream: Vec<OpCode>, memory_capacity: usize) -> Self {
        Self {
            token_stream,
            memory: vec![0; memory_capacity],
            pointer: 0,
            memory_capacity,
        }
    }

    fn add_memory_capacity(&mut self, more_memory: usize) -> usize {
        self.memory_capacity += more_memory;

        //reallocate memory
        for _ in 0..more_memory {
            self.memory.push(0);
        }

        self.memory_capacity
    }

    fn get_memory_capacity(&self) -> usize {
        self.memory_capacity
    }

    fn get_memory(&self) -> Vec<u8> {
        self.memory.clone()
    }

    fn get_pointer(&self) -> usize {
        self.pointer
    }

    fn parse(&mut self) -> Result<Vec<OpCode>, RuntimeError> {
        self.parse_wrapper(self.token_stream.clone())
    }

    fn parse_wrapper(&self, token_stream: Vec<OpCode>) -> Result<Vec<OpCode>, RuntimeError> {
        //Loop recursion taken from:
        //https://github.com/Overv/bf/blob/master/src/main.rs
        let mut program: Vec<OpCode> = Vec::new();
        let mut loop_stack = 0;
        let mut loop_start = 0;

        for (i, op) in token_stream.iter().enumerate() {
            if loop_stack == 0 {
                let instr = match op {
                    OpCode::LoopOpen => {
                        loop_start = i;
                        loop_stack += 1;
                        None
                    }

                    OpCode::LoopClose => {
                        return Err(RuntimeError::LoopNeverOpened(
                            "Loop is never being opened".to_string(),
                        ))
                    }
                    default => Some(default),
                };

                if let Some(instr) = instr {
                    program.push(instr.clone())
                }
            } else {
                match op {
                    OpCode::LoopOpen => {
                        loop_stack += 1;
                    }
                    OpCode::LoopClose => {
                        loop_stack -= 1;

                        if loop_stack == 0 {
                            program.push(OpCode::Loop(
                                self.parse_wrapper(token_stream[loop_start + 1..i].to_vec())?,
                            ));
                        }
                    }
                    _ => (),
                }
            }
        }

        if loop_stack != 0 {
            return Err(RuntimeError::LoopNeverClosed(
                "Loop is never closed!".to_string(),
            ));
        }

        Ok(program)
    }

    fn c_compile_optimization(&self) -> Vec<OpCodeOpt> {
        let mut groups = vec![];
        let mut group = vec![];

        let mut current_token = &self.token_stream[0];
        let mut index = 0;

        loop {
            while index != self.token_stream.len() && current_token == &self.token_stream[index] {
                group.push(&self.token_stream[index]);
                index += 1;
            }

            groups.push(group.clone());
            group = vec![];

            if index == self.token_stream.len() {
                break;
            }
            current_token = &self.token_stream[index];
        }

        let mut opt_op_codes = vec![];

        for token_group in groups {
            opt_op_codes.push(OpCodeOpt {
                opcode: token_group[0].to_owned(),
                count: token_group.len(),
            });
        }
        opt_op_codes
    }

    fn compile_opt_c(&self, optimized_token_stream: Vec<OpCodeOpt>) -> std::io::Result<()> {
        //Will allways be input into bf.c / im too lazy to add a path flag
        let out_file = File::create("bf.c")?;

        let mut out_file = BufWriter::new(out_file);

        let mut compiled_program = String::new();

        compiled_program.push_str(
            &format!("#include <stdio.h>\r\n#include <string.h>\r\n\r\nint main() {{\n   char array[{mem_size}] = {{0}};\r\n   char *ptr = array;\r\n", mem_size = self.memory_capacity));

        let mut indent_level = 1;

        for opt_token in optimized_token_stream {
            let white_spaces = String::from(" ").repeat(indent_level * 3);
            match opt_token.opcode {
                OpCode::IncrementPointer => {
                    compiled_program.push_str(&format!("{white_spaces}ptr+={};\n", opt_token.count))
                }
                OpCode::DecrementPointer => {
                    compiled_program.push_str(&format!("{white_spaces}ptr-={};\n", opt_token.count))
                }
                OpCode::IncrementMemoryAtPointer => compiled_program
                    .push_str(&format!("{white_spaces}*ptr+={};\n", opt_token.count)),
                OpCode::DecrementMemoryAtPointer => compiled_program
                    .push_str(&format!("{white_spaces}*ptr-={};\n", opt_token.count)),
                OpCode::Print => {
                    compiled_program.push_str(&format!("{white_spaces}putchar(*ptr);\n"))
                }
                OpCode::Input => {
                    compiled_program.push_str(&format!("{white_spaces}*ptr = getchar();\n"))
                }
                OpCode::LoopOpen => {
                    indent_level += 1;
                    compiled_program.push_str(&format!("{white_spaces}while (*ptr) {{\n"))
                }
                OpCode::LoopClose => {
                    indent_level -= 1;
                    compiled_program.push_str(&format!("{white_spaces}}}\n"))
                }
                OpCode::ResetMemory => compiled_program.push_str(&format!(
                    "{white_spaces}memset(array, 0, {} * sizeof(char));\n",
                    self.memory_capacity
                )),
                OpCode::ResetPointer => {
                    compiled_program.push_str(&format!("{white_spaces}*ptr=0;\n"))
                }
                _ => {}
            }
        }

        compiled_program.push('}');

        out_file.write_all(compiled_program.as_bytes())?;

        Ok(())
    }

    fn compile_to_c(&self) -> std::io::Result<()> {
        let out_file = File::create("bf.c")?;

        let mut out_file = BufWriter::new(out_file);

        let mut compiled_program = String::new();

        compiled_program.push_str(
        &format!("#include <stdio.h>\r\nint main() {{\n   char array[{mem_size}] = {{0}};\r\n   char *ptr = array;\r\n", mem_size = self.memory_capacity));

        let mut indent_level = 1;

        for token in &self.token_stream {
            let white_spaces = String::from(" ").repeat(indent_level * 3);
            match token {
                OpCode::IncrementPointer => {
                    compiled_program.push_str(&format!("{white_spaces}++ptr;\n"))
                }
                OpCode::DecrementPointer => {
                    compiled_program.push_str(&format!("{white_spaces}--ptr;\n"))
                }
                OpCode::IncrementMemoryAtPointer => {
                    compiled_program.push_str(&format!("{white_spaces}++*ptr;\n"))
                }
                OpCode::DecrementMemoryAtPointer => {
                    compiled_program.push_str(&format!("{white_spaces}--*ptr;\n"))
                }
                OpCode::Print => {
                    compiled_program.push_str(&format!("{white_spaces}putchar(*ptr);\n"))
                }
                OpCode::Input => {
                    compiled_program.push_str(&format!("{white_spaces}*ptr = getchar();\n"))
                }
                OpCode::LoopOpen => {
                    indent_level += 1;
                    compiled_program.push_str(&format!("{white_spaces}while (*ptr) {{\n"))
                }
                OpCode::LoopClose => {
                    indent_level -= 1;
                    compiled_program.push_str(&format!("{white_spaces}}}\n"))
                }
                OpCode::ResetMemory => compiled_program.push_str(&format!(
                    "{white_spaces}memset(array, 0, {} * sizeof(char));\n",
                    self.memory_capacity
                )),
                OpCode::ResetPointer => {
                    compiled_program.push_str(&format!("{white_spaces}*ptr=0;\n"))
                }
                _ => {}
            };
        }

        compiled_program.push('}');

        out_file.write_all(compiled_program.as_bytes())?;

        Ok(())
    }

    fn simulate(&mut self, token_stream: Vec<OpCode>) -> Result<(), RuntimeError> {
        for token in token_stream {
            match token {
                OpCode::IncrementPointer => {
                    if self.pointer + 1 >= self.memory_capacity {
                        return Err(RuntimeError::PointerOutOfBounds(format!(
                            "Pointer of size `{}` out of bounds for memory of size `{}`",
                            self.pointer + 1,
                            self.memory_capacity
                        )));
                    }
                    self.pointer += 1;
                }
                OpCode::DecrementPointer => match self.pointer.checked_sub(1) {
                    Some(new_pointer) => self.pointer = new_pointer,
                    None => {
                        return Err(RuntimeError::PointerOutOfBounds(
                            "Pointer can not be negative".to_string(),
                        ))
                    }
                },
                OpCode::IncrementMemoryAtPointer => self.memory[self.pointer] += 1,
                OpCode::DecrementMemoryAtPointer => self.memory[self.pointer] -= 1,
                OpCode::Print => print!("{}", self.memory[self.pointer] as char),

                OpCode::Input => {
                    let mut input = String::new();

                    std::io::stdin()
                        .read_line(&mut input)
                        .expect("Failed to read line");

                    input = input.replace(['\r', '\n', ' '], "");

                    if input.len() != 1 || input.is_empty() {
                        return Err(RuntimeError::WrongInputFormat(format!(
                            "Input `{input}` can not be parsed, has to be exactly one char!"
                        )));
                    }

                    let chr = input.chars().next().unwrap();

                    self.memory[self.pointer] = chr as u8;
                }
                OpCode::Loop(nested) => {
                    while self.memory[self.pointer] != 0 {
                        self.simulate(nested.clone())?
                    }
                }
                OpCode::ResetMemory => self.memory = vec![0; self.memory_capacity],
                OpCode::ResetPointer => self.pointer = 0,
                _ => {}
            }
        }
        Ok(())
    }
}

fn read_file(path: &str) -> std::io::Result<String> {
    let f = File::open(path)?;
    let mut f = BufReader::new(f);
    let mut buffer = String::new();
    f.read_to_string(&mut buffer)?;

    Ok(buffer)
}

fn main() -> Result<(), RuntimeError> {
    let args: Vec<String> = env::args().collect();

    let file_option = args.get(1);

    if args.contains(&"-help".to_string()) {
        println!("[HELP]: \n   [FLAGS]:");
        println!("      Path has no flag and has to provided as the first argument | e.g brainfuck.exe program.bf | Required");
        println!(
            "     -[sim]ulate a given bf program | e.g: brainfuck.exe program.bf -sim | Optional, default"
        );
        println!(
            "     -[com]pile{{arg}} | Arguments: `c` | e.g: brainfuck.exe program.bf -comc | Optional"
        );
        println!("     -[o]ptimize | e.g: brainfuck.exe program.bf -comc -o | Optional");
        println!("     -[env]iorment | e.g brainfuck.exe -env, will open up a brainfuck CLI.\n\tWhile the CLI is running you can enter brainfuck code (or not) and it will work just like the python CLI.\n\tHint: enter $ to display memory, pointer and memory size | Optional");
        std::process::exit(0);
    }

    if args.contains(&"-env".to_string()) {
        println!("Running Brainfuck CLI!");
        let mut lexer = Lexer::new(String::new());

        let mut parser = Parser::new(vec![], 0);

        loop {
            let (exit, display_meta_data, current_input_stream) = lexer.command_line_env()?;

            let _ = parser.add_memory_capacity(current_input_stream.memory_size + 1);

            let program = parser.parse_wrapper(current_input_stream.token_stream)?;

            parser.simulate(program.clone())?;

            if display_meta_data {
                println!(
                    "Pointer: {}\nMemory: {:?}\nMemory capacity: {}",
                    parser.get_pointer(),
                    parser.get_memory(),
                    parser.get_memory_capacity()
                )
            }

            if exit {
                println!("Exiting Brainfuck CLI!");
                std::process::exit(0);
            }

            let _ = std::io::stdout().flush();
        }
    }

    let path = if let Some(path) = file_option {
        path
    } else {
        exit_print!(format!("[ERROR]: No file path provided!"));
    };

    let file_content = match read_file(path) {
        Ok(file_content) => file_content,
        Err(_) => {
            exit_print!(format!(
                "[ERROR]: Path `{}` to file can not be resolved!",
                path
            ));
        }
    };

    let mut lexer = Lexer::new(file_content);

    let lexer_output = lexer.lex();

    let mut parser = Parser::new(lexer_output.token_stream, lexer_output.memory_size + 1);
    let program = parser.parse()?;

    if args.contains(&"-sim".to_string()) {
        parser.simulate(program)?;
    }

    if args.contains(&"-comc".to_string()) {
        if args.contains(&"-o".to_string()) {
            parser
                .compile_opt_c(parser.c_compile_optimization())
                .expect("Failed to opt");
        } else {
            parser
                .compile_to_c()
                .expect("Failed compilation from brainfuck to c");
        }
    }

    Ok(())
}
