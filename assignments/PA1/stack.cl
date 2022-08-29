(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

class ListNode {
    str : String;

    next : ListNode;

    get_str() : String {
        str
    };

    set_str(new_str : String) : SELF_TYPE {
        {
            str <- new_str;
            self;
        }
    };

    get_next() : ListNode {
        next
    };

    set_next(new_next : ListNode) : SELF_TYPE {
        {
            next <- new_next;
            self;
        }
    };
};

class StackCommand inherits IO {
    execute(top : ListNode, current_input : String) : ListNode {
        (new ListNode).set_str(current_input).set_next(top)
    };
};

class IntStackCommand inherits StackCommand {
};

class PlusStackCommand inherits StackCommand {
};

class SwapStackCommand inherits StackCommand {
};

class EvaluateStackCommand inherits StackCommand {
    execute(top : ListNode, current_input : String) : ListNode {
        if (isvoid top) then top else
        if "+" = top.get_str() then
        {
            (
                let n1 : ListNode <- top.get_next(), n2 : ListNode <- n1.get_next(), a2i_obj : A2I <- (new A2I),
                    sum : Int <- a2i_obj.a2i(n1.get_str()) + a2i_obj.a2i(n2.get_str()) in
                {
                    n2.set_str(a2i_obj.i2a(sum));
                    n2;
                }
            );
        } else
        if "s" = top.get_str() then
        {
            (
                let n1 : ListNode <- top.get_next(), n2 : ListNode <- n1.get_next() in
                {
                    n1.set_next(n2.get_next());
                    n2.set_next(n1);
                    n2;
                }
            );
        } else top
        fi fi fi
    };
};

class DisplayStackCommand inherits StackCommand {
    execute(top : ListNode, current_input : String) : ListNode {
        {
            (
                let current_node : ListNode <- top in
                {
                    while (not (isvoid current_node)) loop
                    {
                        out_string("\n");
                        out_string(current_node.get_str());
                        current_node <- current_node.get_next();
                    } pool;
                    out_string("\n");
                }
            );
            top;
        }
    };
};

class StopStackCommand inherits StackCommand {
    execute(top : ListNode, current_input : String) : ListNode {
        top
    };
};

class Handler inherits IO {
    top : ListNode;

    sc : StackCommand;

    will_go_on : Bool <- true;

    update() : Object {
        let current_input : String <- prompt() in
        {
            if "+" = current_input then sc <- new PlusStackCommand else
            if "s" = current_input then sc <- new SwapStackCommand else
            if "e" = current_input then sc <- new EvaluateStackCommand else
            if "d" = current_input then sc <- new DisplayStackCommand else
            if "x" = current_input then { sc <- new StopStackCommand; will_go_on <- false; } else
                                        sc <- new IntStackCommand
            fi fi fi fi fi;
            top <- sc.execute(top, current_input);
        }
    };

    get_will_go_on() : Bool {
        will_go_on
    };

    prompt() : String {
        {
            out_string(">");
            in_string();
        }
    };
};

class Main inherits IO {
    h : Handler <- (new Handler);

    main() : Object {
        {
        
            while h.get_will_go_on() loop
                h.update()
            pool;
        }
    };
};
