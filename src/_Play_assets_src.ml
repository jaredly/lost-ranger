
type loc = {x: int; y: int; width: int; height: int}
let sprite loc sheet ~scale ~flip ~pos:(x,y) = Reprocessing.Draw.subImagef sheet
~texPos:(loc.x, loc.y)
~texWidth:loc.width
~texHeight:loc.height
~width:((if flip then -1. else 1.) *. scale *. float_of_int loc.width)
~height:(scale *. float_of_int loc.height)
~pos:((if flip then x +. float_of_int loc.width *. scale else x), y)
type sprite = pos:(float * float) -> width:float -> height:float -> unit
(*$
let raw = {|
	<SubTexture name="alien_arm.png" x="170" y="36" width="16" height="42"/>
	<SubTexture name="alien_body.png" x="80" y="228" width="44" height="47"/>
	<SubTexture name="alien_head.png" x="0" y="56" width="92" height="92"/>
	<SubTexture name="alien_leg.png" x="140" y="38" width="28" height="36"/>
	<SubTexture name="boar_body.png" x="0" y="150" width="86" height="76"/>
	<SubTexture name="boar_head.png" x="66" y="410" width="60" height="79"/>
	<SubTexture name="boar_leg.png" x="128" y="478" width="24" height="23"/>
	<SubTexture name="boar_tail.png" x="154" y="478" width="14" height="31"/>
	<SubTexture name="female_arm.png" x="132" y="333" width="28" height="66"/>
	<SubTexture name="female_body.png" x="126" y="211" width="44" height="59"/>
	<SubTexture name="female_head.png" x="0" y="228" width="78" height="72"/>
	<SubTexture name="female_leg.png" x="140" y="76" width="28" height="56"/>
	<SubTexture name="fox_body.png" x="0" y="0" width="116" height="54"/>
	<SubTexture name="fox_ear.png" x="170" y="469" width="12" height="20"/>
	<SubTexture name="fox_leg.png" x="42" y="476" width="16" height="23"/>
	<SubTexture name="fox_tail.png" x="0" y="476" width="40" height="20"/>
	<SubTexture name="gnome_arm.png" x="170" y="80" width="14" height="42"/>
	<SubTexture name="gnome_body.png" x="118" y="0" width="34" height="36"/>
	<SubTexture name="gnome_head.png" x="94" y="56" width="44" height="81"/>
	<SubTexture name="gnome_leg.png" x="154" y="0" width="24" height="34"/>
	<SubTexture name="hedgehog_body.png" x="0" y="302" width="72" height="40"/>
	<SubTexture name="male_arm.png" x="158" y="401" width="28" height="66"/>
	<SubTexture name="male_body.png" x="88" y="150" width="44" height="59"/>
	<SubTexture name="male_head.png" x="0" y="344" width="64" height="64"/>
	<SubTexture name="male_leg.png" x="134" y="139" width="28" height="56"/>
	<SubTexture name="skeleton_arm.png" x="164" y="134" width="20" height="66"/>
	<SubTexture name="skeleton_body.png" x="80" y="277" width="44" height="60"/>
	<SubTexture name="skeleton_head.png" x="0" y="410" width="64" height="64"/>
	<SubTexture name="skeleton_leg.png" x="162" y="333" width="24" height="56"/>
	<SubTexture name="zombie_arm.png" x="128" y="410" width="28" height="66"/>
	<SubTexture name="zombie_body.png" x="126" y="272" width="44" height="59"/>
	<SubTexture name="zombie_head.png" x="66" y="344" width="64" height="64"/>
	<SubTexture name="zombie_leg.png" x="134" y="139" width="28" height="56"/>
|}

let split sep text = Str.split(Str.regexp_string sep) text

let lines = split "\n" raw

let _ = print_newline()
let sprites = lines |> List.map (fun line -> begin
  let parts = split "\"" line in
  match parts with
  | [_; file; _; x; _; y; _; width; _; height; _] ->
    let [name; _] = split "." file in
    (name, x, y, width, height)
  | _ -> failwith("bad line")
end)

let _ = sprites |> List.iter (fun (name, x, y, width, height) ->
    Printf.printf {|let %s = sprite({x= %s; y= %s; width= %s; height= %s})
let %s_width = %s.
let %s_height = %s.
|}
    name x y width height name width name height
)

*)
let alien_arm = sprite({x= 170; y= 36; width= 16; height= 42})
let alien_arm_width = 16.
let alien_arm_height = 42.
let alien_body = sprite({x= 80; y= 228; width= 44; height= 47})
let alien_body_width = 44.
let alien_body_height = 47.
let alien_head = sprite({x= 0; y= 56; width= 92; height= 92})
let alien_head_width = 92.
let alien_head_height = 92.
let alien_leg = sprite({x= 140; y= 38; width= 28; height= 36})
let alien_leg_width = 28.
let alien_leg_height = 36.
let boar_body = sprite({x= 0; y= 150; width= 86; height= 76})
let boar_body_width = 86.
let boar_body_height = 76.
let boar_head = sprite({x= 66; y= 410; width= 60; height= 79})
let boar_head_width = 60.
let boar_head_height = 79.
let boar_leg = sprite({x= 128; y= 478; width= 24; height= 23})
let boar_leg_width = 24.
let boar_leg_height = 23.
let boar_tail = sprite({x= 154; y= 478; width= 14; height= 31})
let boar_tail_width = 14.
let boar_tail_height = 31.
let female_arm = sprite({x= 132; y= 333; width= 28; height= 66})
let female_arm_width = 28.
let female_arm_height = 66.
let female_body = sprite({x= 126; y= 211; width= 44; height= 59})
let female_body_width = 44.
let female_body_height = 59.
let female_head = sprite({x= 0; y= 228; width= 78; height= 72})
let female_head_width = 78.
let female_head_height = 72.
let female_leg = sprite({x= 140; y= 76; width= 28; height= 56})
let female_leg_width = 28.
let female_leg_height = 56.
let fox_body = sprite({x= 0; y= 0; width= 116; height= 54})
let fox_body_width = 116.
let fox_body_height = 54.
let fox_ear = sprite({x= 170; y= 469; width= 12; height= 20})
let fox_ear_width = 12.
let fox_ear_height = 20.
let fox_leg = sprite({x= 42; y= 476; width= 16; height= 23})
let fox_leg_width = 16.
let fox_leg_height = 23.
let fox_tail = sprite({x= 0; y= 476; width= 40; height= 20})
let fox_tail_width = 40.
let fox_tail_height = 20.
let gnome_arm = sprite({x= 170; y= 80; width= 14; height= 42})
let gnome_arm_width = 14.
let gnome_arm_height = 42.
let gnome_body = sprite({x= 118; y= 0; width= 34; height= 36})
let gnome_body_width = 34.
let gnome_body_height = 36.
let gnome_head = sprite({x= 94; y= 56; width= 44; height= 81})
let gnome_head_width = 44.
let gnome_head_height = 81.
let gnome_leg = sprite({x= 154; y= 0; width= 24; height= 34})
let gnome_leg_width = 24.
let gnome_leg_height = 34.
let hedgehog_body = sprite({x= 0; y= 302; width= 72; height= 40})
let hedgehog_body_width = 72.
let hedgehog_body_height = 40.
let male_arm = sprite({x= 158; y= 401; width= 28; height= 66})
let male_arm_width = 28.
let male_arm_height = 66.
let male_body = sprite({x= 88; y= 150; width= 44; height= 59})
let male_body_width = 44.
let male_body_height = 59.
let male_head = sprite({x= 0; y= 344; width= 64; height= 64})
let male_head_width = 64.
let male_head_height = 64.
let male_leg = sprite({x= 134; y= 139; width= 28; height= 56})
let male_leg_width = 28.
let male_leg_height = 56.
let skeleton_arm = sprite({x= 164; y= 134; width= 20; height= 66})
let skeleton_arm_width = 20.
let skeleton_arm_height = 66.
let skeleton_body = sprite({x= 80; y= 277; width= 44; height= 60})
let skeleton_body_width = 44.
let skeleton_body_height = 60.
let skeleton_head = sprite({x= 0; y= 410; width= 64; height= 64})
let skeleton_head_width = 64.
let skeleton_head_height = 64.
let skeleton_leg = sprite({x= 162; y= 333; width= 24; height= 56})
let skeleton_leg_width = 24.
let skeleton_leg_height = 56.
let zombie_arm = sprite({x= 128; y= 410; width= 28; height= 66})
let zombie_arm_width = 28.
let zombie_arm_height = 66.
let zombie_body = sprite({x= 126; y= 272; width= 44; height= 59})
let zombie_body_width = 44.
let zombie_body_height = 59.
let zombie_head = sprite({x= 66; y= 344; width= 64; height= 64})
let zombie_head_width = 64.
let zombie_head_height = 64.
let zombie_leg = sprite({x= 134; y= 139; width= 28; height= 56})
let zombie_leg_width = 28.
let zombie_leg_height = 56.
