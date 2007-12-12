(* Html *)
(* Copyright (C)2000-2006 Berke Durak                               *)
(* Released under the GNU Lesser General Public License version 2.1 *)

open Entity;;

let sf = Printf.sprintf;;

type html_document =
  { head: html_head;
    body: html_element * html_properties }
and html_head =
  { title: string;
    author: string;
    charset: html_charset;
    style_sheet: string option }
and html_properties = (string * string) list
and html_charset = ASCII | ISO_8859_1 | UTF8
and html_method = GET | POST
and html_element =
| I_button of string * string (* name, value *)
| I_hidden of string * string
| I_text of string * string * int option * int option (* name, value, size, maxlength *)
| I_password of string * string * int option * int option (* name, value, size, maxlength *)
| I_text_area of string * int * int * string
| I_checkbox of string * string * bool
| I_radio of string * string * bool
| I_select of string * bool * int * (string * string * bool) list
| I_reset of string
| Img of string * int * int * string
| Form of html_method * string * html_element
| Anchor of url * html_element
| Seq of html_element list
| UL of html_element list
| P of html_element
| H of int * html_element
| T of string (* ISO-8859-1 text *)
| BT of string (* ISO-8859-1 text *)
| IT of string (* ISO-8859-1 text *)
| TT of string (* ISO-8859-1 text *)
| Pre of string (* pre-formatted text *)
| HR
| Table of html_table_row list
| Nop
| BR
| Div of string * html_element list
| Span of string * html_element
| Script of string * string
| With of html_properties * html_element
and html_table_row = html_table_cell list
and html_table_cell =
| C_contents of html_element
| C_halign of html_table_cell_halign * html_table_cell
| C_valign of html_table_cell_valign * html_table_cell
| C_rowspan of int * html_table_cell
| C_colspan of int * html_table_cell
| C_header of html_table_cell
| C_color of Rgb.t * html_table_cell
| C_width of int * html_table_cell
and html_table_cell_halign =
| Cha_left
| Cha_center
| Cha_right
| Cha_justify
| Cha_char of char
and html_table_cell_valign =
| Cva_top
| Cva_middle
| Cva_bottom
| Cva_baseline
and url = string
;;

let default_head =
  { title = "Untitled";
    author = "Ara HTTPD";
    charset = ISO_8859_1;
    style_sheet = None }
;;

let string_of_charset = function
  | ASCII -> "ascii"
  | ISO_8859_1 -> "iso-8859-1"
  | UTF8 -> "utf-8"
;;

let output (f : string -> unit) (fc : char -> unit) x =
  let indent = ref 0 in
  let put_indent () =
    for i = 1 to !indent do
      f "  "
    done
  in
  let text_avec_table t y =
    for i = 0 to String.length y - 1 do
      f t.(Char.code y.[i])
    done
  in
  let text = text_avec_table character_table in
  let gui = text in
  let ife f = function
    | Some x -> ignore (f x)
    | None -> () in
  let launch_tag x =
    put_indent ();
    f ("<"^x)
  and end_tag_linear x = f ("</"^x^">")
  in
  (*and flush_tag_without_nl () = f ">";
  and end_tag_without_nl x = f ("</"^x^">")
  and launch_linear_tag x = f ("<"^x)*)
  let spaces = ref true in
  let flush_tag () = if !spaces then begin f ">\n"; incr indent end else f ">"
  and flush_linear_tag () = if !spaces then f ">\n" else f ">"
  and end_tag x =
    if !spaces then
      begin
        decr indent;
        put_indent ();
        f ("</"^x^">\n")
      end
    else
      begin
        f ("</"^x^">")
      end
  in
  let flush_tag_without_nl () = f ">"
  and launch_linear_tag x = f ("<"^x)
  in
  let put_properties l =
    List.iter
      begin fun (k,v) ->
	f " ";
        f k;
        f "=\"";
        gui v;
        f "\""
      end
      l
  in
  let text_or_password ?(properties=[]) kind n v s m =
    launch_tag "INPUT";
    f " TYPE=";
    f kind;
    f " NAME=\"";
    gui n;
    f "\" VALUE=\"";
    gui v;
    f "\"";
    begin
      match s with
      | Some(s) -> f (" SIZE="^(string_of_int s))
      | None -> ();
    end;
    begin
      match m with
      | Some(m) -> f (" MAXLENGTH="^(string_of_int m))
      | None -> ();
    end;
    put_properties properties;
    flush_linear_tag ()
  in
  let start_tag ?(properties=[]) x =
    launch_tag x;
    put_properties properties;
    flush_tag ()
  (*and start_tag_lineaire x =
    launch_tag x;
    flush_linear_tag ()*)
  and start_tag_without_nl x =
    launch_tag x;
    flush_tag_without_nl ()
  in
  let linear_tag ?(properties=[]) x =
    launch_tag x;
    put_properties properties;
    flush_linear_tag ()
  in
  let rec cellule c i j =
    let rec loop he ha va rs cs rgb w c =
      match c with
      |	C_header (c)     -> loop true ha va rs cs rgb w c
      | C_halign (ha,c)  -> loop he (Some ha) va rs cs rgb w c
      | C_valign (va,c)  -> loop he ha (Some va) rs cs rgb w c
      | C_rowspan (rs,c) -> loop he ha va (Some rs) cs rgb w c
      | C_colspan (cs,c) -> loop he ha va rs (Some cs) rgb w c
      |	C_color (rgb,c)  -> loop he ha va rs cs (Some rgb) w c
      |	C_width (w,c)    -> loop he ha va rs cs rgb (Some w) c
      | C_contents e ->
	  launch_tag (if he then "TH" else "TD");
	  begin
	    let coefficient_parity_row = -2
	    and coefficient_parity_column = -1
	    and coefficient_head = -4
	    and shift = 11
	    and coefficient_total = 12
	    in
	    match rgb with
	      Some(rgb) ->
		f (Printf.sprintf " BGCOLOR=\"%s\""
		     (Rgb.to_string
			(let alpha =
			  (float_of_int
			     (((if he then 0 else coefficient_head)
				 + coefficient_parity_row * (i mod 2)
				 + coefficient_parity_column * (j mod 2)) + shift)) /.
			  (float_of_int coefficient_total)
			in
			Rgb.mix alpha Rgb.white rgb)));
	    | _ -> ()
	  end;
	  ife
	      (fun ha ->
		f " ALIGN=";
		match ha with
		| Cha_left -> f "LEFT"
		| Cha_center -> f "CENTER"
		| Cha_right -> f "RIGHT"
		| Cha_justify -> f "JUSTIFY"
		| Cha_char c -> f "\""; gui (String.make 1 c); f "\"")
	      ha;
	  ife
	      (fun va ->
		f " VALIGN=";
		match va with
		| Cva_top -> f "TOP"
		| Cva_middle -> f "MIDDLE"
		| Cva_bottom -> f "BOTTOM"
		| Cva_baseline -> f "BASELINE")
	      va;
	  ife (fun rs -> f (" ROWSPAN="^(string_of_int rs))) rs;
	  ife (fun cs -> f (" COLSPAN="^(string_of_int cs))) cs;
	  ife (fun w -> f (" WIDTH="^(string_of_int w))) w;
	  flush_tag ();
	  element e;
	  end_tag (if he then "TH" else "TD")
    in
    loop false None None None None None None c
  and element ?(properties=[]) y =
    match y with
    | With(p',y') -> element ~properties:(p' @ properties) y'
    | Script(ty,src) ->
	launch_tag "SCRIPT";
	f " TYPE=\"";
	gui ty;
	f "\" SRC=\"";
	gui src;
	f "\"";
        put_properties properties;
        flush_tag_without_nl ();
	end_tag "SCRIPT"
    | Anchor(u,e) ->
	launch_tag "A";
	f " HREF=\"";
	gui u;
	f "\"";
        put_properties properties;
	flush_tag_without_nl ();
        let spaces' = !spaces in
        spaces := false;
	element e;
        spaces := spaces';
	end_tag "A"
    | Img(path, width, height, alt) ->
	launch_linear_tag "IMG";
        f (sf " SRC=%S WIDTH=%d HEIGHT=%d ALT=\"" path width height);
        text_avec_table character_table alt;
        f "\"";
        put_properties properties;
        flush_tag_without_nl ()
    | Form(m,u,e) ->
	launch_tag "FORM";
	f (" METHOD="^(match m with POST -> "POST" | GET -> "GET")^" ACTION=\"");
	f u;
	f "\" ENCTYPE=\"application/x-www-form-urlencoded\"";
        put_properties properties;
	flush_tag ();
	element e;
	end_tag "FORM";
    | Div(c, z) ->
        launch_tag "DIV";
        f (sf " CLASS=%S" c);
        put_properties properties;
        flush_tag ();
        List.iter (fun t -> element t) z;
        end_tag "DIV"
    | Span(c, z) ->
        launch_linear_tag "SPAN";
        f (sf " CLASS=%S" c);
        put_properties properties;
        flush_tag_without_nl ();
        element z;
        end_tag_linear "SPAN"
    | Seq z -> List.iter (fun t -> element t) z
    | UL l ->
	start_tag ~properties "UL";
	List.iter (fun t ->
	  start_tag ~properties "LI";
	  element t;
	  end_tag "LI") l;
	end_tag "UL"
    | H(i, z) ->
	start_tag ~properties ("H"^(string_of_int i));
	element z;
	end_tag ("H"^(string_of_int i))
    | T z ->
        if !spaces then put_indent ();
	text_avec_table character_table_nl_to_br z;
        if !spaces then f "\n"
    | BT z -> start_tag ~properties "B"; text z; end_tag "B"
    | TT z -> start_tag ~properties "TT"; text z; end_tag "TT"
    | IT z -> start_tag ~properties "I"; text z; end_tag "I"
    | Pre z ->
	start_tag_without_nl "PRE"; text z;
	end_tag_linear "PRE"
    | HR -> linear_tag ~properties "HR"
    | BR -> linear_tag ~properties "BR"
    | P z -> linear_tag ~properties "P"; element z (* start_tag ~properties "P"; element z; end_tag "P" *)
    | Nop -> f " "
    | I_select (n,m,s,l) ->
	launch_tag "SELECT";
	f " SIZE="; f (string_of_int s);
	f " NAME=\""; gui n; f (if m then "\" MULTIPLE" else "\"");
        put_properties properties;
	flush_tag ();
	List.iter (fun (n,v,s) ->
	  launch_tag "OPTION";
	  f " VALUE=\""; gui n; f (if s then "\" SELECTED" else "\"");
	  flush_linear_tag ();
	  text v;
	  f " ") l;
	end_tag "SELECT"
    | I_reset (v) ->
	launch_tag "INPUT";
	f " TYPE=RESET VALUE=\""; gui v; f "\"";
        put_properties properties;
	flush_linear_tag ()
    | I_button (n,v) ->
	launch_tag "INPUT";
	f " TYPE=SUBMIT NAME=\""; gui n; f "\" VALUE=\""; gui v; f "\"";
        put_properties properties;
	flush_linear_tag ()
    | I_hidden (n,v) ->
	launch_tag "INPUT";
	f " TYPE=HIDDEN NAME=\""; gui n; f "\" VALUE=\""; gui v; f "\"";
        put_properties properties;
	flush_linear_tag ()
    | I_text (n,v,s,m) -> text_or_password ~properties "TEXT" n v s m
    | I_password (n,v,s,m) -> text_or_password ~properties "PASSWORD" n v s m
    | I_text_area (n,r,c,v) ->
	launch_tag "TEXTAREA";
	f (Printf.sprintf " ROWS=%d COLS=%d NAME=\"" r c); gui n; f "\"";
        put_properties properties;
	flush_linear_tag ();
	text v;
	end_tag_linear "TEXTAREA"
    | I_checkbox (n,v,c) ->
	launch_tag "INPUT";
	f " TYPE=CHECKBOX NAME=\""; gui n; f "\" VALUE=\""; gui v;
	f "\"";
	if c then f " CHECKED";
        put_properties properties;
	flush_linear_tag ();
    | I_radio (n,v,c) ->
	launch_tag "INPUT";
	f " TYPE=RADIO NAME=\""; gui n; f "\" VALUE=\""; gui v;
	f "\"";
	if c then f " CHECKED";
        put_properties properties;
	flush_linear_tag ();
    | Table l ->
	start_tag ~properties "TABLE";
	let (i,j) = (ref 0, ref 0) in
	List.iter
	  (fun r ->
	    start_tag ~properties "TR";
	    j := 0;
	    List.iter (fun r' -> cellule r' !i !j; incr j) r;
	    incr i;
	    end_tag "TR") l;
	end_tag "TABLE"
  in
  begin
    f "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n";
    start_tag "HTML";
    begin
      start_tag "HEAD";
      (* title *)
      begin
	start_tag "TITLE";
        put_indent (); text x.head.title;
        if !spaces then f "\n";
	end_tag "TITLE";
        (* css *)
        begin
          match x.head.style_sheet with
          | None -> ()
          | Some css ->
            launch_tag "LINK";
            f " REL=\"stylesheet\" TYPE=\"text/css\" HREF=\"";
            text css;
            f "\"";
            flush_linear_tag ()
        end;
        (* meta *)
	begin
	  launch_tag "META";
	  f (sf " HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=%s\""
                (string_of_charset x.head.charset));
	  flush_linear_tag ();

	  launch_tag "META";
	  f " NAME=\"Author\" CONTENT=\"";
	  text x.head.author;
	  f "\"";
	  flush_linear_tag ()

	end;
      end;
      end_tag "HEAD";
    end;
    (* body *)
    begin
      let (body, properties) = x.body in
      start_tag ~properties "BODY";
      element body;
      end_tag "BODY";
    end;
    end_tag "HTML";
  end
;;
    
let rec map_text f = function
| Form(a,b,e) -> Form(a,b,map_text f e)
| Seq l -> Seq(List.map (map_text f) l)
| UL l -> UL(List.map (map_text f) l)
| P e -> P(map_text f e)
| H(x,e) -> H(x,map_text f e)
| T t -> f (fun x -> T x) t
| BT t -> f (fun x -> BT x) t
| IT t -> f (fun x -> BT x) t
| TT t -> f (fun x -> BT x) t
| Div(c,l) -> Div(c,List.map (map_text f) l)
| Span(c,e) -> Span(c,map_text f e)
| Table(l) -> Table(List.map (List.map (map_cell f)) l)
| x -> x
and map_cell f = function
| C_contents(e) -> C_contents(map_text f e)
| C_halign(h,c) -> C_halign(h,map_cell f c)
| C_valign(v,c) -> C_valign(v,map_cell f c)
| C_rowspan(x,c) -> C_rowspan(x,map_cell f c)
| C_colspan(x,c) -> C_colspan(x,map_cell f c)
| C_header(c) -> C_header(map_cell f c)
| C_color(x,c) -> C_color(x,map_cell f c)
| C_width(x,c) -> C_width(x,map_cell f c)
;;

let output_to_channel oc = output (output_string oc) (output_char oc);;
let output_to_buffer b x = output (Buffer.add_string b) (Buffer.add_char b) x;;
