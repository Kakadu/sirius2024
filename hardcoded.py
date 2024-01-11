import json

# Создание JSON с кодом стековой машины
def import_(name):
    return { "kind": "import", "value": name }
def extern(name):
    return { "kind": "extern", "value": name }
def drop():
    return { "kind": "DROP" } 
def globalVar(name):
    return { "kind": "Global", "value": name }
def binop(name):
    return { "kind": "BINOP", "value": name }
def jmp(name):
    return { "kind": "JMP", "value": name }
def flabel(name):
    return { "kind": "FLABEL", "value": name }
def label(name):
    return { "kind": "LABEL", "value": name }
def st(arg):
    return { "kind": "ST", "value": arg  }
def ld(arg):
    return { "kind": "LD", "value": arg  }
def const(arg):
    return { "kind": "CONST", "value": arg  } 
def call(name, argc):
    return { "kind": "CALL", "fname": name, "argc": argc, "flg": False }
def cjmp(cond, dest):
    return { "kind": "CJMP", "name": cond, "value": dest }
def end():
    return { "kind": "END" }

print1 = [ 
    {"kind": "import", "value": "Std" },
    const(1), 
    st(globalVar("x")),
    drop(),
    ld(globalVar("x")),
    call("Lwrite", 1), 
    end()
]

fac = [ 
    import_("Std"),
    extern("Lwrite"),
    const(4), 
    st(globalVar("n")),
    drop(),
    const(1), 
    st(globalVar("acc")),
    drop(),
    jmp("L12"),

    flabel("L11"),
    ld(globalVar("acc")),
    ld(globalVar("n")),
    binop("*"),
    st(globalVar("acc")),
    drop(),
    ld(globalVar("n")),
    const(1),
    st(globalVar("n")),
    drop(), 
    const(1), 
    binop("-"),
    st(globalVar("n")),
    drop(),

    label("L12"),
    ld(globalVar("n")),
    const(1), 
    binop(">"),
    cjmp("nz", "L11"),

    ld(globalVar("acc")),
    #const(1), 
    call("Lwrite", 1), 
    end()
]

fib = [ 
    import_("Std"),
    extern("Lwrite"),
    extern("Lread"),
    
    const(5), 
    st(globalVar("n")),
    drop(),

    const(2), 
    st(globalVar("i")),
    drop(),
    
    const(1), 
    st(globalVar("fib1")),
    drop(),
    

    const(1), 
    st(globalVar("fib2")),
    drop(),

    const(1), 
    st(globalVar("fib")),
    drop(),

    jmp("L21"),

    flabel("L20"),


    ld(globalVar("fib1")),
    ld(globalVar("fib2")),
    binop("+"),
    st(globalVar("fib")),
    drop(),

    ld(globalVar("fib1")), 
    st(globalVar("fib2")),
    drop(),

    ld(globalVar("fib")), 
    st(globalVar("fib1")),
    drop(),
    
    ld(globalVar("i")),
    const(1),
    binop("+"),
    st(globalVar("i")),
    drop(),

    # slabel
    label("L21"),

    ld(globalVar("i")),
    ld(globalVar("n")),
    binop("<"),
    cjmp("nz", "L20"),

    ld(globalVar("fib")),
    call("Lwrite", 1), 

    end()
]
