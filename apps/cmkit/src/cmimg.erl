-module(cmimg).
-export([convert/1]).

convert(#{ url := Url,
           dir := Dir,
           basename := Basename,
           sizes := Sizes }) ->
    
    U = cmkit:to_bin(Url),
    D = cmkit:to_bin(Dir),
    B = cmkit:to_bin(Basename),
    S = cmkit:bin_join(lists:map(fun cmkit:to_bin/1, Sizes), <<",">>),

    Cmd = <<"/Users/pedrogutierrez/.go/bin/goimg url ",
          U/binary,
          " ",
          D/binary,
          " ",
          B/binary,
          " ",
          "jpg",
          " ",
          S/binary
          >>,

    cmsh:sh(Cmd, []).

