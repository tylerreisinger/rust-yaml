pub mod lexer;

fn main()
{
    let mut lex = lexer::Lexer::new(
"%YAML 1.2  
---
 ? abcd\"ef: gh#
    three: four # hi
     five: sixsixsix
fifteen: !!int 'seve''n'
flow-style: one, two, three, '3'
some-stuff: :vecy

map: &id1
    - one: 1
    - two: 2
map2: *id1

flow-list: [a, b, c, d, e, 'f', 13, \"ghi\", j k,]
scalar-list: a, b, c
tag-1: !<yaml:abc> yeah
tag-2: !abc!def yeah2

lit-block: |-
  This is a literal
  block

  of text.
    I'm indented
      even more

       than before.


out: a

fold-block: >+
  This
  line
  should
   wrap

   but 
   not
   here

specific-indent: >2- #abc
    Indent
    2
    here
    a: b
 c: d

end: true
...
".to_string());

    loop
    {
        match lex.get_next_token() {
            Ok(tk) => println!("{}", tk),
            Err(err) => {
                println!("{}", err);
                break;
            }
        }
    }
}
