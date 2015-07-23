signature CHUNK =
sig
    type 'a task

    type chunk
    type id

    val retrieve : id -> chunk option task

    val store : chunk * id -> bool task

end
