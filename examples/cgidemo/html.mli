(* Html *)
(* Copyright (C)2000-2006 Berke Durak                               *)
(* Released under the GNU Lesser General Public License version 2.1 *)

type html_document = { head : html_head; body : html_element * html_properties; }
and html_head = {
  title : string;
  author : string;
  charset : html_charset;
  style_sheet : string option
}
and html_properties = (string * string) list
and html_charset = ASCII | ISO_8859_1 | UTF8
and html_method = GET | POST
and html_element =
    I_button of string * string
  | I_hidden of string * string
  | I_text of string * string * int option * int option
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
  | T of string
  | BT of string
  | IT of string
  | TT of string
  | Pre of string
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
    C_contents of html_element
  | C_halign of html_table_cell_halign * html_table_cell
  | C_valign of html_table_cell_valign * html_table_cell
  | C_rowspan of int * html_table_cell
  | C_colspan of int * html_table_cell
  | C_header of html_table_cell
  | C_color of Rgb.t * html_table_cell
  | C_width of int * html_table_cell
and html_table_cell_halign =
    Cha_left
  | Cha_center
  | Cha_right
  | Cha_justify
  | Cha_char of char
and html_table_cell_valign = Cva_top | Cva_middle | Cva_bottom | Cva_baseline
and url = string
val string_of_charset : html_charset -> string
val output : (string -> unit) -> (char -> unit) -> html_document -> unit
val output_to_channel : out_channel -> html_document -> unit
val output_to_buffer : Buffer.t -> html_document -> unit
val default_head : html_head
val map_text : ((string -> html_element) -> string -> html_element) -> html_element -> html_element
