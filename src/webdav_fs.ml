module Fs = Mirage_fs_mem

let size fs name = Fs.size fs name
let read fs name offset length = Fs.read fs name offset length
let stat fs name = Fs.stat fs name
let listdir fs name = Fs.listdir fs name
let write fs name offset data = Fs.write fs name offset data
let destroy fs name = Fs.destroy fs name
let connect () = Fs.connect ""

let pp_error = Fs.pp_error
let pp_write_error = Fs.pp_write_error
