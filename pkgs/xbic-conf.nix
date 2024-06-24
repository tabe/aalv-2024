{ pkgs }:
let
  inherit (pkgs) boost igraph;
in
pkgs.writeText "xbic-conf" ''
  IGRAPH_FLAGS = -I${igraph.dev}/include -L${igraph}/lib -ligraph
  BOOST_FLAGS = -I${boost.dev}/include
''
