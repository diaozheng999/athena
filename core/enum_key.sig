signature SEQ_KEY =
sig
  include ORD_KEY
  val succ : ord_key -> ord_key
end

signature ENUM_KEY =
sig
  include SEQ_KEY
  val enum : ord_key * ord_key -> ord_key list
end
