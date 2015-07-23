structure ATHENA_FILE =
sig
    type 'a task
    type 'a seq

    type chunk
    type id
    type file
    type filestream

    val read : file -> filestream task
    val chunkify : filestream -> (id * chunk) seq task

end
