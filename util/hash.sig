signature HASH =
sig

    type serialised
    type hash
    type 'a task

    val hash : serialised -> hash task
    
end


signature CRYPT =
sig
    type serialised
    type cypher
    type 'a task

    val encrypt : serialised * serialised-> cypher task
    val decrypt : cypher -> serialised task

end

signature ENCODING =
sig
    type serialised
    type encoded
    type 'a task
    
    val encode : serialised -> encoded task
    val decode : encoded -> serialised task
end
