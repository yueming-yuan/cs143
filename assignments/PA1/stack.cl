(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)


class Stack {
    
    isNil() : Bool { true };  
    isInt() : Bool { false };   

    gettop() : String {{abort();"0";}};
    getbase() : Stack {{abort();self;}};
    getnum() : Int {{abort();0;}};

    push(cmd : String, isn : Bool) : Stack {
        if isn then (new Cons_int).init(cmd, self)
        else (new Cons).init(cmd, self) fi
    };

};

class Cons inherits Stack {

    top : String;
    base : Stack;

    isNil() : Bool { false };

    pop() : String { top };
    
    gettop() : String { top };
    getbase() : Stack { base };

    init(t : String, rest : Stack) : Stack {
        {
            top <- t;
            base <- rest;
            self;
        }
    };
};

class Cons_int inherits Cons {
    
    num : Int;
    isInt() : Bool { true };
    getnum() : Int { num };
    
    init(t : String, rest : Stack) : Stack {
        {
            top <- t;
            base <- rest;
            let z : A2I <- new A2I in (num <- z.a2i(t));
            self;
        }
    };
};

    
class Main inherits IO {

    prompt() : String {
        {
            out_string(">");
            in_string();
        }
    };
    
    outstack(tstack : Stack): Object {
            if (not tstack.isNil()) then
            { 
                out_string(tstack.gettop());
                out_string("\n");
                outstack(tstack.getbase());
            }
        else 1 fi
    };    

    mystack : Stack <- new Stack;
    cont : Bool;

    main() : Object {
    {
        cont <- true;

        while cont loop     
            (let s : String <- prompt() in 
                if s = "x" then
                    cont <- false
                    else
                if s = "d" then 
                    (let tstack : Stack <- mystack in
                        outstack(tstack))
                    else
                if s = "e" then 
                    if mystack.gettop() = "+" then {
                        mystack <- mystack.getbase();
                        let i : Int <- mystack.getnum() in {
                            mystack <- mystack.getbase();     
                            let j : Int <- mystack.getnum() in {
                                mystack <- mystack.getbase();   
                                let z : A2I <- new A2I in (mystack <- mystack.push(z.i2a(i + j), true));
                            };
                        };
                    }
                        else
                    if mystack.gettop() = "s" then {
                        mystack <- mystack.getbase();
                        let s1 : String <- mystack.gettop() in 
                            (let f1 : Bool <- mystack.isInt() in {
                            mystack <- mystack.getbase();
                            let s2 : String <- mystack.gettop() in 
                                (let f2 : Bool <- mystack.isInt() in {
                                    mystack <- mystack.getbase();
                                    mystack <- mystack.push(s1, f1);
                                    mystack <- mystack.push(s2, f2);
                            });
                        });
                    }
                        else 1 fi fi
                    else
                if s = "s" then 
                    mystack <- mystack.push("s", false)
                    else
                if s = "+" then
                    mystack <- mystack.push("+", false)
                    else
                    mystack <- mystack.push(s, true)
                fi fi fi fi fi
            )
            pool;
        }
   };
};
