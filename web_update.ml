open Yojson

let header = "<!DOCTYPE html>
<!-- Website template by freewebsitetemplates.com -->
<html>
	<head>
		<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>
		<title>Updates - Elias' Spot</title>
		<link rel=\"stylesheet\" href=\"css/style.css\" type=\"text/css\" />
		<!--[if IE 7]>
			<link rel=\"stylesheet\" href=\"css/ie7.css\" type=\"text/css\" />
		<![endif]-->
	</head>
	<body>
		<div class=\"page\">
			<div class=\"header\">
				<a href=\"index.html\" id=\"logo\"><img src=\"images/elias_spot2.png\" alt=\"\"/></a>
				<ul>
					<li><a href=\"index.html\">Home</a></li>
					<li><a href=\"gallery.html\">Gallery</a></li>
					<li class=\"selected\"><a href=\"updates.html\">Updates</a></li>
				</ul>
			</div>
			<div class=\"body\">
				<ul>
						<div class=\"featured\">
						</div>";;

let footer = "				</ul>
				<ul class=\"paging\">
					<li><a href=\"#\"><<</a></li>
					<li><a href=\"#\">First</a></li>
					<li><a href=\"#\">1</a></li>
					<!--<li><a href=\"#\">2</a></li>
					<li><a href=\"#\">3</a></li>
					<li><a href=\"#\">21</a></li>-->
					<li><a href=\"#\">Last</a></li>
					<li><a href=\"#\">>></a></li>
				</ul>
			</div>
			<div class=\"footer\">
				<ul>
					<li><a href=\"index.html\">Home</a></li>
					<li><a href=\"about.html\">About</a></li>
					<li><a href=\"updates.html\">Gallery</a></li>
				</ul>
			</div>
		</div>
	</body>
</html>";;

let update_begin = "						<div class=\"featured\">" ^ "\n";;

let update_end = "\n" ^ "						</div>";;

(*Store update in json format*)
let make_update ~title ~description ~page ~update_no =
	let open Basic in
	let (update : json) = `Assoc [ ("Title", `String title); 
																 ("Description", `String description); 
																 ("Page", `Int page); 
																 ("Update Number", `Int update_no)] 
	in
	let update = pretty_to_string update in (*convert to string*)
	let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o666 "updates.json" in
	output_string oc update; (*open and store update in updates.json*)
	close_out oc;;

(*wrap in html tags*)
let to_html (title,desc,page,upd_n) =
	let title = "<h3>"^"Update "^(string_of_int upd_n)^": "^title^"</h3>" in
	let description = "<p>"^desc^"</p>" in
	update_begin ^ title ^ "\n" ^ description ^ update_end
	;;

(*add updates to the html page*)
let push_updates () =
	let json = Basic.stream_from_file "updates.json" in
	let add_to_page items page =
		let pg = ref page in
		try
		while true do
			items |> Stream.next
			|> fun json ->
			let title = json |> Basic.Util.member "Title" |> Basic.Util.to_string in
			let desc = json |> Basic.Util.member "Description" |> Basic.Util.to_string in
			let page = json |> Basic.Util.member "Page" |> Basic.Util.to_int in
			let upd_n = json |> Basic.Util.member "Update Number" |> Basic.Util.to_int in
			pg := !pg ^ (to_html (title,desc,page,upd_n));
		done;
		""; (*end try with a string statement instead of unit from while loop*)
		with e -> !pg
	in
	let page = add_to_page json header in
	let page = page ^ footer in
	let out = open_out "/var/www/html/updates.html" in
	output_string out page;
	close_out out;;

let () =
	let usage_msg = "usage: web_update --title --description --update_number\n" in
	let title = ref "" in
	let desc = ref "" in
	let update_number = ref 0 in
	let page_number = ref 0 in
	let force = ref false in
	let speclist = [
    "--title", Arg.Set_string title, "The title of the update";
    "--description", Arg.Set_string desc, "Flag to generate a password";
    "--update_number", Arg.Set_int update_number, "Update Number";
    "--page", Arg.Set_int page_number, "The Page where the update should go";
    "--update", Arg.Set force, "Update without making an entry" ]
  in
  Arg.parse speclist print_endline usage_msg;
  match !force with true -> push_updates (); exit 0; |false -> ();
  make_update ~title:!title ~description:!desc ~page:!page_number ~update_no:!update_number;
	push_updates ()
;;