
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
let process name raw source =
	let split sep text = Str.split(Str.regexp_string sep) text in

	let lines = split "\n" raw in

	let _ = print_endline("module " ^ name ^ " = struct\n  let source = \"" ^ source ^ "\"\n") in
	let sprites = lines |> List.map (fun line -> begin
		let parts = split "\"" line in
		match parts with
		| _::file:: _:: x:: _:: y:: _:: width:: _:: height:: _ ->
			let [name; _] = split "." file in
			Some(name, x, y, width, height)
		| _ -> None
	end) |> List.filter (fun x -> x <> None) |> List.map (fun (Some x) -> x) in

	let _ = sprites |> List.iter (fun (name, x, y, width, height) ->
			Printf.printf {|let %s = sprite({x= %s; y= %s; width= %s; height= %s})
	let %s_width = %s.
	let %s_height = %s.
	|}
			name x y width height name width name height
	) in
	print_endline("end\n")

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

let _ = process "Players" raw "spritesheet_characters.png"

let raw_items = {|
	<SubTexture name="apple.png" x="390" y="260" width="128" height="128"/>
	<SubTexture name="arrow.png" x="390" y="130" width="128" height="128"/>
	<SubTexture name="axe_bronze.png" x="390" y="0" width="128" height="128"/>
	<SubTexture name="axe_diamond.png" x="260" y="1820" width="128" height="128"/>
	<SubTexture name="axe_gold.png" x="260" y="1690" width="128" height="128"/>
	<SubTexture name="axe_iron.png" x="260" y="1560" width="128" height="128"/>
	<SubTexture name="axe_silver.png" x="260" y="1430" width="128" height="128"/>
	<SubTexture name="boat.png" x="260" y="1300" width="128" height="128"/>
	<SubTexture name="bow.png" x="260" y="1170" width="128" height="128"/>
	<SubTexture name="bowArrow.png" x="260" y="1040" width="128" height="128"/>
	<SubTexture name="bowl.png" x="260" y="910" width="128" height="128"/>
	<SubTexture name="fish.png" x="260" y="780" width="128" height="128"/>
	<SubTexture name="fish_cooked.png" x="260" y="650" width="128" height="128"/>
	<SubTexture name="fishingPole.png" x="260" y="520" width="128" height="128"/>
	<SubTexture name="flail_bronze.png" x="260" y="390" width="128" height="128"/>
	<SubTexture name="flail_diamond.png" x="260" y="260" width="128" height="128"/>
	<SubTexture name="flail_gold.png" x="260" y="130" width="128" height="128"/>
	<SubTexture name="flail_iron.png" x="260" y="0" width="128" height="128"/>
	<SubTexture name="flail_silver.png" x="130" y="1820" width="128" height="128"/>
	<SubTexture name="hammer_bronze.png" x="130" y="1690" width="128" height="128"/>
	<SubTexture name="hammer_diamond.png" x="390" y="1300" width="128" height="128"/>
	<SubTexture name="hammer_gold.png" x="130" y="1430" width="128" height="128"/>
	<SubTexture name="hammer_iron.png" x="130" y="1300" width="128" height="128"/>
	<SubTexture name="hammer_silver.png" x="130" y="1170" width="128" height="128"/>
	<SubTexture name="hoe_bronze.png" x="130" y="1040" width="128" height="128"/>
	<SubTexture name="hoe_diamond.png" x="130" y="910" width="128" height="128"/>
	<SubTexture name="hoe_gold.png" x="130" y="780" width="128" height="128"/>
	<SubTexture name="hoe_iron.png" x="130" y="650" width="128" height="128"/>
	<SubTexture name="hoe_silver.png" x="130" y="520" width="128" height="128"/>
	<SubTexture name="minecart.png" x="130" y="390" width="128" height="128"/>
	<SubTexture name="ore_coal.png" x="130" y="260" width="128" height="128"/>
	<SubTexture name="ore_diamond.png" x="130" y="130" width="128" height="128"/>
	<SubTexture name="ore_emerald.png" x="130" y="0" width="128" height="128"/>
	<SubTexture name="ore_gold.png" x="0" y="1820" width="128" height="128"/>
	<SubTexture name="ore_iron.png" x="0" y="1690" width="128" height="128"/>
	<SubTexture name="ore_ironAlt.png" x="0" y="1560" width="128" height="128"/>
	<SubTexture name="ore_ruby.png" x="0" y="1430" width="128" height="128"/>
	<SubTexture name="ore_silver.png" x="0" y="1300" width="128" height="128"/>
	<SubTexture name="pick_bronze.png" x="0" y="1170" width="128" height="128"/>
	<SubTexture name="pick_diamond.png" x="0" y="1040" width="128" height="128"/>
	<SubTexture name="pick_gold.png" x="0" y="910" width="128" height="128"/>
	<SubTexture name="pick_iron.png" x="0" y="780" width="128" height="128"/>
	<SubTexture name="pick_silver.png" x="0" y="650" width="128" height="128"/>
	<SubTexture name="seed.png" x="0" y="520" width="128" height="128"/>
	<SubTexture name="shovel_bronze.png" x="0" y="390" width="128" height="128"/>
	<SubTexture name="shovel_diamond.png" x="0" y="260" width="128" height="128"/>
	<SubTexture name="shovel_gold.png" x="0" y="130" width="128" height="128"/>
	<SubTexture name="shovel_iron.png" x="0" y="0" width="128" height="128"/>
	<SubTexture name="shovel_silver.png" x="130" y="1560" width="128" height="128"/>
	<SubTexture name="stew.png" x="390" y="1170" width="128" height="128"/>
	<SubTexture name="sword_bronze.png" x="390" y="1040" width="128" height="128"/>
	<SubTexture name="sword_diamond.png" x="390" y="910" width="128" height="128"/>
	<SubTexture name="sword_gold.png" x="390" y="780" width="128" height="128"/>
	<SubTexture name="sword_iron.png" x="390" y="650" width="128" height="128"/>
	<SubTexture name="sword_silver.png" x="390" y="520" width="128" height="128"/>
	<SubTexture name="wheat.png" x="390" y="390" width="128" height="128"/>
	|}

let _ = process "Items" raw_items "spritesheet_items.png"

let raw_tiles = {|
	<SubTexture name="brick_grey.png" x="0" y="0" width="128" height="128"/>
	<SubTexture name="brick_red.png" x="260" y="1560" width="128" height="128"/>
	<SubTexture name="cactus_inside.png" x="650" y="1040" width="128" height="128"/>
	<SubTexture name="cactus_side.png" x="650" y="910" width="128" height="128"/>
	<SubTexture name="cactus_top.png" x="650" y="780" width="128" height="128"/>
	<SubTexture name="cotton_blue.png" x="650" y="650" width="128" height="128"/>
	<SubTexture name="cotton_green.png" x="650" y="520" width="128" height="128"/>
	<SubTexture name="cotton_red.png" x="650" y="390" width="128" height="128"/>
	<SubTexture name="cotton_tan.png" x="650" y="260" width="128" height="128"/>
	<SubTexture name="dirt.png" x="650" y="130" width="128" height="128"/>
	<SubTexture name="dirt_grass.png" x="650" y="0" width="128" height="128"/>
	<SubTexture name="dirt_sand.png" x="520" y="1820" width="128" height="128"/>
	<SubTexture name="dirt_snow.png" x="520" y="1690" width="128" height="128"/>
	<SubTexture name="fence_stone.png" x="520" y="1560" width="128" height="128"/>
	<SubTexture name="fence_wood.png" x="520" y="1430" width="128" height="128"/>
	<SubTexture name="glass.png" x="520" y="1300" width="128" height="128"/>
	<SubTexture name="glass_frame.png" x="520" y="1170" width="128" height="128"/>
	<SubTexture name="grass1.png" x="520" y="650" width="128" height="128"/>
	<SubTexture name="grass2.png" x="520" y="520" width="128" height="128"/>
	<SubTexture name="grass3.png" x="520" y="390" width="128" height="128"/>
	<SubTexture name="grass4.png" x="520" y="260" width="128" height="128"/>
	<SubTexture name="grass_brown.png" x="520" y="1040" width="128" height="128"/>
	<SubTexture name="grass_tan.png" x="520" y="910" width="128" height="128"/>
	<SubTexture name="grass_top.png" x="520" y="780" width="128" height="128"/>
	<SubTexture name="gravel_dirt.png" x="520" y="130" width="128" height="128"/>
	<SubTexture name="gravel_stone.png" x="520" y="0" width="128" height="128"/>
	<SubTexture name="greysand.png" x="390" y="1820" width="128" height="128"/>
	<SubTexture name="greystone.png" x="390" y="1690" width="128" height="128"/>
	<SubTexture name="greystone_ruby.png" x="390" y="1560" width="128" height="128"/>
	<SubTexture name="greystone_ruby_alt.png" x="390" y="1430" width="128" height="128"/>
	<SubTexture name="greystone_sand.png" x="390" y="1300" width="128" height="128"/>
	<SubTexture name="ice.png" x="390" y="1170" width="128" height="128"/>
	<SubTexture name="lava.png" x="390" y="1040" width="128" height="128"/>
	<SubTexture name="leaves.png" x="390" y="910" width="128" height="128"/>
	<SubTexture name="leaves_orange.png" x="390" y="780" width="128" height="128"/>
	<SubTexture name="leaves_orange_transparent.png" x="390" y="650" width="128" height="128"/>
	<SubTexture name="leaves_transparent.png" x="390" y="520" width="128" height="128"/>
	<SubTexture name="mushroom_brown.png" x="390" y="390" width="128" height="128"/>
	<SubTexture name="mushroom_red.png" x="390" y="260" width="128" height="128"/>
	<SubTexture name="mushroom_tan.png" x="390" y="130" width="128" height="128"/>
	<SubTexture name="oven.png" x="390" y="0" width="128" height="128"/>
	<SubTexture name="redsand.png" x="260" y="1820" width="128" height="128"/>
	<SubTexture name="redstone.png" x="260" y="1690" width="128" height="128"/>
	<SubTexture name="redstone_emerald.png" x="650" y="1170" width="128" height="128"/>
	<SubTexture name="redstone_emerald_alt.png" x="260" y="1430" width="128" height="128"/>
	<SubTexture name="redstone_sand.png" x="260" y="1300" width="128" height="128"/>
	<SubTexture name="rock.png" x="260" y="1170" width="128" height="128"/>
	<SubTexture name="rock_moss.png" x="260" y="1040" width="128" height="128"/>
	<SubTexture name="sand.png" x="260" y="910" width="128" height="128"/>
	<SubTexture name="snow.png" x="260" y="780" width="128" height="128"/>
	<SubTexture name="stone.png" x="260" y="650" width="128" height="128"/>
	<SubTexture name="stone_browniron.png" x="260" y="520" width="128" height="128"/>
	<SubTexture name="stone_browniron_alt.png" x="260" y="390" width="128" height="128"/>
	<SubTexture name="stone_coal.png" x="260" y="260" width="128" height="128"/>
	<SubTexture name="stone_coal_alt.png" x="260" y="130" width="128" height="128"/>
	<SubTexture name="stone_diamond.png" x="260" y="0" width="128" height="128"/>
	<SubTexture name="stone_diamond_alt.png" x="130" y="1820" width="128" height="128"/>
	<SubTexture name="stone_dirt.png" x="130" y="1690" width="128" height="128"/>
	<SubTexture name="stone_gold.png" x="130" y="1560" width="128" height="128"/>
	<SubTexture name="stone_gold_alt.png" x="130" y="1430" width="128" height="128"/>
	<SubTexture name="stone_grass.png" x="130" y="1300" width="128" height="128"/>
	<SubTexture name="stone_iron.png" x="130" y="1170" width="128" height="128"/>
	<SubTexture name="stone_iron_alt.png" x="130" y="1040" width="128" height="128"/>
	<SubTexture name="stone_sand.png" x="130" y="910" width="128" height="128"/>
	<SubTexture name="stone_silver.png" x="130" y="780" width="128" height="128"/>
	<SubTexture name="stone_silver_alt.png" x="130" y="650" width="128" height="128"/>
	<SubTexture name="stone_snow.png" x="130" y="520" width="128" height="128"/>
	<SubTexture name="table.png" x="130" y="390" width="128" height="128"/>
	<SubTexture name="track_corner.png" x="130" y="260" width="128" height="128"/>
	<SubTexture name="track_corner_alt.png" x="130" y="130" width="128" height="128"/>
	<SubTexture name="track_straight.png" x="130" y="0" width="128" height="128"/>
	<SubTexture name="track_straight_alt.png" x="0" y="1820" width="128" height="128"/>
	<SubTexture name="trunk_bottom.png" x="0" y="1690" width="128" height="128"/>
	<SubTexture name="trunk_mid.png" x="0" y="1560" width="128" height="128"/>
	<SubTexture name="trunk_side.png" x="0" y="1430" width="128" height="128"/>
	<SubTexture name="trunk_top.png" x="0" y="1300" width="128" height="128"/>
	<SubTexture name="trunk_white_side.png" x="0" y="1170" width="128" height="128"/>
	<SubTexture name="trunk_white_top.png" x="0" y="1040" width="128" height="128"/>
	<SubTexture name="water.png" x="0" y="910" width="128" height="128"/>
	<SubTexture name="wheat_stage1.png" x="0" y="780" width="128" height="128"/>
	<SubTexture name="wheat_stage2.png" x="0" y="650" width="128" height="128"/>
	<SubTexture name="wheat_stage3.png" x="0" y="520" width="128" height="128"/>
	<SubTexture name="wheat_stage4.png" x="0" y="390" width="128" height="128"/>
	<SubTexture name="wood.png" x="0" y="260" width="128" height="128"/>
	<SubTexture name="wood_red.png" x="0" y="130" width="128" height="128"/>
|}

let _ = process "Tiles" raw_tiles "spritesheet_tiles.png"

let raw_extra = {|
    <sprite n="arrow_fletched.png" x="84" y="0" w="24" h="140" pX="0.5" pY="0.5"/>
    <sprite n="arrow_iron.png" x="0" y="0" w="20" h="140" pX="0.5" pY="0.5"/>
    <sprite n="arrow_stone.png" x="20" y="0" w="20" h="140" pX="0.5" pY="0.5"/>
    <sprite n="arrow_thinner.png" x="0" y="140" w="24" h="128" pX="0.5" pY="0.5"/>
    <sprite n="arrow_thinner2.png" x="60" y="0" w="24" h="130" pX="0.5" pY="0.5"/>
    <sprite n="arrow_wood.png" x="40" y="0" w="20" h="140" pX="0.5" pY="0.5"/>
    <sprite n="dirt_top.png" x="0" y="428" w="128" h="52" pX="0.5" pY="0.5"/>
    <sprite n="grass_top.png" x="0" y="480" w="128" h="52" pX="0.5" pY="0.5"/>
    <sprite n="grass_top_2.png" x="0" y="376" w="128" h="52" pX="0.5" pY="0.5"/>
    <sprite n="ice_top.png" x="0" y="268" w="128" h="54" pX="0.5" pY="0.5"/>
    <sprite n="sand_top.png" x="0" y="322" w="128" h="54" pX="0.5" pY="0.5"/>
|}

let _ = process "ExtraItems" raw_extra "extra_items.png"

*)module Players = struct
  let source = "spritesheet_characters.png"

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
	end

module Items = struct
  let source = "spritesheet_items.png"

let apple = sprite({x= 390; y= 260; width= 128; height= 128})
	let apple_width = 128.
	let apple_height = 128.
	let arrow = sprite({x= 390; y= 130; width= 128; height= 128})
	let arrow_width = 128.
	let arrow_height = 128.
	let axe_bronze = sprite({x= 390; y= 0; width= 128; height= 128})
	let axe_bronze_width = 128.
	let axe_bronze_height = 128.
	let axe_diamond = sprite({x= 260; y= 1820; width= 128; height= 128})
	let axe_diamond_width = 128.
	let axe_diamond_height = 128.
	let axe_gold = sprite({x= 260; y= 1690; width= 128; height= 128})
	let axe_gold_width = 128.
	let axe_gold_height = 128.
	let axe_iron = sprite({x= 260; y= 1560; width= 128; height= 128})
	let axe_iron_width = 128.
	let axe_iron_height = 128.
	let axe_silver = sprite({x= 260; y= 1430; width= 128; height= 128})
	let axe_silver_width = 128.
	let axe_silver_height = 128.
	let boat = sprite({x= 260; y= 1300; width= 128; height= 128})
	let boat_width = 128.
	let boat_height = 128.
	let bow = sprite({x= 260; y= 1170; width= 128; height= 128})
	let bow_width = 128.
	let bow_height = 128.
	let bowArrow = sprite({x= 260; y= 1040; width= 128; height= 128})
	let bowArrow_width = 128.
	let bowArrow_height = 128.
	let bowl = sprite({x= 260; y= 910; width= 128; height= 128})
	let bowl_width = 128.
	let bowl_height = 128.
	let fish = sprite({x= 260; y= 780; width= 128; height= 128})
	let fish_width = 128.
	let fish_height = 128.
	let fish_cooked = sprite({x= 260; y= 650; width= 128; height= 128})
	let fish_cooked_width = 128.
	let fish_cooked_height = 128.
	let fishingPole = sprite({x= 260; y= 520; width= 128; height= 128})
	let fishingPole_width = 128.
	let fishingPole_height = 128.
	let flail_bronze = sprite({x= 260; y= 390; width= 128; height= 128})
	let flail_bronze_width = 128.
	let flail_bronze_height = 128.
	let flail_diamond = sprite({x= 260; y= 260; width= 128; height= 128})
	let flail_diamond_width = 128.
	let flail_diamond_height = 128.
	let flail_gold = sprite({x= 260; y= 130; width= 128; height= 128})
	let flail_gold_width = 128.
	let flail_gold_height = 128.
	let flail_iron = sprite({x= 260; y= 0; width= 128; height= 128})
	let flail_iron_width = 128.
	let flail_iron_height = 128.
	let flail_silver = sprite({x= 130; y= 1820; width= 128; height= 128})
	let flail_silver_width = 128.
	let flail_silver_height = 128.
	let hammer_bronze = sprite({x= 130; y= 1690; width= 128; height= 128})
	let hammer_bronze_width = 128.
	let hammer_bronze_height = 128.
	let hammer_diamond = sprite({x= 390; y= 1300; width= 128; height= 128})
	let hammer_diamond_width = 128.
	let hammer_diamond_height = 128.
	let hammer_gold = sprite({x= 130; y= 1430; width= 128; height= 128})
	let hammer_gold_width = 128.
	let hammer_gold_height = 128.
	let hammer_iron = sprite({x= 130; y= 1300; width= 128; height= 128})
	let hammer_iron_width = 128.
	let hammer_iron_height = 128.
	let hammer_silver = sprite({x= 130; y= 1170; width= 128; height= 128})
	let hammer_silver_width = 128.
	let hammer_silver_height = 128.
	let hoe_bronze = sprite({x= 130; y= 1040; width= 128; height= 128})
	let hoe_bronze_width = 128.
	let hoe_bronze_height = 128.
	let hoe_diamond = sprite({x= 130; y= 910; width= 128; height= 128})
	let hoe_diamond_width = 128.
	let hoe_diamond_height = 128.
	let hoe_gold = sprite({x= 130; y= 780; width= 128; height= 128})
	let hoe_gold_width = 128.
	let hoe_gold_height = 128.
	let hoe_iron = sprite({x= 130; y= 650; width= 128; height= 128})
	let hoe_iron_width = 128.
	let hoe_iron_height = 128.
	let hoe_silver = sprite({x= 130; y= 520; width= 128; height= 128})
	let hoe_silver_width = 128.
	let hoe_silver_height = 128.
	let minecart = sprite({x= 130; y= 390; width= 128; height= 128})
	let minecart_width = 128.
	let minecart_height = 128.
	let ore_coal = sprite({x= 130; y= 260; width= 128; height= 128})
	let ore_coal_width = 128.
	let ore_coal_height = 128.
	let ore_diamond = sprite({x= 130; y= 130; width= 128; height= 128})
	let ore_diamond_width = 128.
	let ore_diamond_height = 128.
	let ore_emerald = sprite({x= 130; y= 0; width= 128; height= 128})
	let ore_emerald_width = 128.
	let ore_emerald_height = 128.
	let ore_gold = sprite({x= 0; y= 1820; width= 128; height= 128})
	let ore_gold_width = 128.
	let ore_gold_height = 128.
	let ore_iron = sprite({x= 0; y= 1690; width= 128; height= 128})
	let ore_iron_width = 128.
	let ore_iron_height = 128.
	let ore_ironAlt = sprite({x= 0; y= 1560; width= 128; height= 128})
	let ore_ironAlt_width = 128.
	let ore_ironAlt_height = 128.
	let ore_ruby = sprite({x= 0; y= 1430; width= 128; height= 128})
	let ore_ruby_width = 128.
	let ore_ruby_height = 128.
	let ore_silver = sprite({x= 0; y= 1300; width= 128; height= 128})
	let ore_silver_width = 128.
	let ore_silver_height = 128.
	let pick_bronze = sprite({x= 0; y= 1170; width= 128; height= 128})
	let pick_bronze_width = 128.
	let pick_bronze_height = 128.
	let pick_diamond = sprite({x= 0; y= 1040; width= 128; height= 128})
	let pick_diamond_width = 128.
	let pick_diamond_height = 128.
	let pick_gold = sprite({x= 0; y= 910; width= 128; height= 128})
	let pick_gold_width = 128.
	let pick_gold_height = 128.
	let pick_iron = sprite({x= 0; y= 780; width= 128; height= 128})
	let pick_iron_width = 128.
	let pick_iron_height = 128.
	let pick_silver = sprite({x= 0; y= 650; width= 128; height= 128})
	let pick_silver_width = 128.
	let pick_silver_height = 128.
	let seed = sprite({x= 0; y= 520; width= 128; height= 128})
	let seed_width = 128.
	let seed_height = 128.
	let shovel_bronze = sprite({x= 0; y= 390; width= 128; height= 128})
	let shovel_bronze_width = 128.
	let shovel_bronze_height = 128.
	let shovel_diamond = sprite({x= 0; y= 260; width= 128; height= 128})
	let shovel_diamond_width = 128.
	let shovel_diamond_height = 128.
	let shovel_gold = sprite({x= 0; y= 130; width= 128; height= 128})
	let shovel_gold_width = 128.
	let shovel_gold_height = 128.
	let shovel_iron = sprite({x= 0; y= 0; width= 128; height= 128})
	let shovel_iron_width = 128.
	let shovel_iron_height = 128.
	let shovel_silver = sprite({x= 130; y= 1560; width= 128; height= 128})
	let shovel_silver_width = 128.
	let shovel_silver_height = 128.
	let stew = sprite({x= 390; y= 1170; width= 128; height= 128})
	let stew_width = 128.
	let stew_height = 128.
	let sword_bronze = sprite({x= 390; y= 1040; width= 128; height= 128})
	let sword_bronze_width = 128.
	let sword_bronze_height = 128.
	let sword_diamond = sprite({x= 390; y= 910; width= 128; height= 128})
	let sword_diamond_width = 128.
	let sword_diamond_height = 128.
	let sword_gold = sprite({x= 390; y= 780; width= 128; height= 128})
	let sword_gold_width = 128.
	let sword_gold_height = 128.
	let sword_iron = sprite({x= 390; y= 650; width= 128; height= 128})
	let sword_iron_width = 128.
	let sword_iron_height = 128.
	let sword_silver = sprite({x= 390; y= 520; width= 128; height= 128})
	let sword_silver_width = 128.
	let sword_silver_height = 128.
	let wheat = sprite({x= 390; y= 390; width= 128; height= 128})
	let wheat_width = 128.
	let wheat_height = 128.
	end

module Tiles = struct
  let source = "spritesheet_tiles.png"

let brick_grey = sprite({x= 0; y= 0; width= 128; height= 128})
	let brick_grey_width = 128.
	let brick_grey_height = 128.
	let brick_red = sprite({x= 260; y= 1560; width= 128; height= 128})
	let brick_red_width = 128.
	let brick_red_height = 128.
	let cactus_inside = sprite({x= 650; y= 1040; width= 128; height= 128})
	let cactus_inside_width = 128.
	let cactus_inside_height = 128.
	let cactus_side = sprite({x= 650; y= 910; width= 128; height= 128})
	let cactus_side_width = 128.
	let cactus_side_height = 128.
	let cactus_top = sprite({x= 650; y= 780; width= 128; height= 128})
	let cactus_top_width = 128.
	let cactus_top_height = 128.
	let cotton_blue = sprite({x= 650; y= 650; width= 128; height= 128})
	let cotton_blue_width = 128.
	let cotton_blue_height = 128.
	let cotton_green = sprite({x= 650; y= 520; width= 128; height= 128})
	let cotton_green_width = 128.
	let cotton_green_height = 128.
	let cotton_red = sprite({x= 650; y= 390; width= 128; height= 128})
	let cotton_red_width = 128.
	let cotton_red_height = 128.
	let cotton_tan = sprite({x= 650; y= 260; width= 128; height= 128})
	let cotton_tan_width = 128.
	let cotton_tan_height = 128.
	let dirt = sprite({x= 650; y= 130; width= 128; height= 128})
	let dirt_width = 128.
	let dirt_height = 128.
	let dirt_grass = sprite({x= 650; y= 0; width= 128; height= 128})
	let dirt_grass_width = 128.
	let dirt_grass_height = 128.
	let dirt_sand = sprite({x= 520; y= 1820; width= 128; height= 128})
	let dirt_sand_width = 128.
	let dirt_sand_height = 128.
	let dirt_snow = sprite({x= 520; y= 1690; width= 128; height= 128})
	let dirt_snow_width = 128.
	let dirt_snow_height = 128.
	let fence_stone = sprite({x= 520; y= 1560; width= 128; height= 128})
	let fence_stone_width = 128.
	let fence_stone_height = 128.
	let fence_wood = sprite({x= 520; y= 1430; width= 128; height= 128})
	let fence_wood_width = 128.
	let fence_wood_height = 128.
	let glass = sprite({x= 520; y= 1300; width= 128; height= 128})
	let glass_width = 128.
	let glass_height = 128.
	let glass_frame = sprite({x= 520; y= 1170; width= 128; height= 128})
	let glass_frame_width = 128.
	let glass_frame_height = 128.
	let grass1 = sprite({x= 520; y= 650; width= 128; height= 128})
	let grass1_width = 128.
	let grass1_height = 128.
	let grass2 = sprite({x= 520; y= 520; width= 128; height= 128})
	let grass2_width = 128.
	let grass2_height = 128.
	let grass3 = sprite({x= 520; y= 390; width= 128; height= 128})
	let grass3_width = 128.
	let grass3_height = 128.
	let grass4 = sprite({x= 520; y= 260; width= 128; height= 128})
	let grass4_width = 128.
	let grass4_height = 128.
	let grass_brown = sprite({x= 520; y= 1040; width= 128; height= 128})
	let grass_brown_width = 128.
	let grass_brown_height = 128.
	let grass_tan = sprite({x= 520; y= 910; width= 128; height= 128})
	let grass_tan_width = 128.
	let grass_tan_height = 128.
	let grass_top = sprite({x= 520; y= 780; width= 128; height= 128})
	let grass_top_width = 128.
	let grass_top_height = 128.
	let gravel_dirt = sprite({x= 520; y= 130; width= 128; height= 128})
	let gravel_dirt_width = 128.
	let gravel_dirt_height = 128.
	let gravel_stone = sprite({x= 520; y= 0; width= 128; height= 128})
	let gravel_stone_width = 128.
	let gravel_stone_height = 128.
	let greysand = sprite({x= 390; y= 1820; width= 128; height= 128})
	let greysand_width = 128.
	let greysand_height = 128.
	let greystone = sprite({x= 390; y= 1690; width= 128; height= 128})
	let greystone_width = 128.
	let greystone_height = 128.
	let greystone_ruby = sprite({x= 390; y= 1560; width= 128; height= 128})
	let greystone_ruby_width = 128.
	let greystone_ruby_height = 128.
	let greystone_ruby_alt = sprite({x= 390; y= 1430; width= 128; height= 128})
	let greystone_ruby_alt_width = 128.
	let greystone_ruby_alt_height = 128.
	let greystone_sand = sprite({x= 390; y= 1300; width= 128; height= 128})
	let greystone_sand_width = 128.
	let greystone_sand_height = 128.
	let ice = sprite({x= 390; y= 1170; width= 128; height= 128})
	let ice_width = 128.
	let ice_height = 128.
	let lava = sprite({x= 390; y= 1040; width= 128; height= 128})
	let lava_width = 128.
	let lava_height = 128.
	let leaves = sprite({x= 390; y= 910; width= 128; height= 128})
	let leaves_width = 128.
	let leaves_height = 128.
	let leaves_orange = sprite({x= 390; y= 780; width= 128; height= 128})
	let leaves_orange_width = 128.
	let leaves_orange_height = 128.
	let leaves_orange_transparent = sprite({x= 390; y= 650; width= 128; height= 128})
	let leaves_orange_transparent_width = 128.
	let leaves_orange_transparent_height = 128.
	let leaves_transparent = sprite({x= 390; y= 520; width= 128; height= 128})
	let leaves_transparent_width = 128.
	let leaves_transparent_height = 128.
	let mushroom_brown = sprite({x= 390; y= 390; width= 128; height= 128})
	let mushroom_brown_width = 128.
	let mushroom_brown_height = 128.
	let mushroom_red = sprite({x= 390; y= 260; width= 128; height= 128})
	let mushroom_red_width = 128.
	let mushroom_red_height = 128.
	let mushroom_tan = sprite({x= 390; y= 130; width= 128; height= 128})
	let mushroom_tan_width = 128.
	let mushroom_tan_height = 128.
	let oven = sprite({x= 390; y= 0; width= 128; height= 128})
	let oven_width = 128.
	let oven_height = 128.
	let redsand = sprite({x= 260; y= 1820; width= 128; height= 128})
	let redsand_width = 128.
	let redsand_height = 128.
	let redstone = sprite({x= 260; y= 1690; width= 128; height= 128})
	let redstone_width = 128.
	let redstone_height = 128.
	let redstone_emerald = sprite({x= 650; y= 1170; width= 128; height= 128})
	let redstone_emerald_width = 128.
	let redstone_emerald_height = 128.
	let redstone_emerald_alt = sprite({x= 260; y= 1430; width= 128; height= 128})
	let redstone_emerald_alt_width = 128.
	let redstone_emerald_alt_height = 128.
	let redstone_sand = sprite({x= 260; y= 1300; width= 128; height= 128})
	let redstone_sand_width = 128.
	let redstone_sand_height = 128.
	let rock = sprite({x= 260; y= 1170; width= 128; height= 128})
	let rock_width = 128.
	let rock_height = 128.
	let rock_moss = sprite({x= 260; y= 1040; width= 128; height= 128})
	let rock_moss_width = 128.
	let rock_moss_height = 128.
	let sand = sprite({x= 260; y= 910; width= 128; height= 128})
	let sand_width = 128.
	let sand_height = 128.
	let snow = sprite({x= 260; y= 780; width= 128; height= 128})
	let snow_width = 128.
	let snow_height = 128.
	let stone = sprite({x= 260; y= 650; width= 128; height= 128})
	let stone_width = 128.
	let stone_height = 128.
	let stone_browniron = sprite({x= 260; y= 520; width= 128; height= 128})
	let stone_browniron_width = 128.
	let stone_browniron_height = 128.
	let stone_browniron_alt = sprite({x= 260; y= 390; width= 128; height= 128})
	let stone_browniron_alt_width = 128.
	let stone_browniron_alt_height = 128.
	let stone_coal = sprite({x= 260; y= 260; width= 128; height= 128})
	let stone_coal_width = 128.
	let stone_coal_height = 128.
	let stone_coal_alt = sprite({x= 260; y= 130; width= 128; height= 128})
	let stone_coal_alt_width = 128.
	let stone_coal_alt_height = 128.
	let stone_diamond = sprite({x= 260; y= 0; width= 128; height= 128})
	let stone_diamond_width = 128.
	let stone_diamond_height = 128.
	let stone_diamond_alt = sprite({x= 130; y= 1820; width= 128; height= 128})
	let stone_diamond_alt_width = 128.
	let stone_diamond_alt_height = 128.
	let stone_dirt = sprite({x= 130; y= 1690; width= 128; height= 128})
	let stone_dirt_width = 128.
	let stone_dirt_height = 128.
	let stone_gold = sprite({x= 130; y= 1560; width= 128; height= 128})
	let stone_gold_width = 128.
	let stone_gold_height = 128.
	let stone_gold_alt = sprite({x= 130; y= 1430; width= 128; height= 128})
	let stone_gold_alt_width = 128.
	let stone_gold_alt_height = 128.
	let stone_grass = sprite({x= 130; y= 1300; width= 128; height= 128})
	let stone_grass_width = 128.
	let stone_grass_height = 128.
	let stone_iron = sprite({x= 130; y= 1170; width= 128; height= 128})
	let stone_iron_width = 128.
	let stone_iron_height = 128.
	let stone_iron_alt = sprite({x= 130; y= 1040; width= 128; height= 128})
	let stone_iron_alt_width = 128.
	let stone_iron_alt_height = 128.
	let stone_sand = sprite({x= 130; y= 910; width= 128; height= 128})
	let stone_sand_width = 128.
	let stone_sand_height = 128.
	let stone_silver = sprite({x= 130; y= 780; width= 128; height= 128})
	let stone_silver_width = 128.
	let stone_silver_height = 128.
	let stone_silver_alt = sprite({x= 130; y= 650; width= 128; height= 128})
	let stone_silver_alt_width = 128.
	let stone_silver_alt_height = 128.
	let stone_snow = sprite({x= 130; y= 520; width= 128; height= 128})
	let stone_snow_width = 128.
	let stone_snow_height = 128.
	let table = sprite({x= 130; y= 390; width= 128; height= 128})
	let table_width = 128.
	let table_height = 128.
	let track_corner = sprite({x= 130; y= 260; width= 128; height= 128})
	let track_corner_width = 128.
	let track_corner_height = 128.
	let track_corner_alt = sprite({x= 130; y= 130; width= 128; height= 128})
	let track_corner_alt_width = 128.
	let track_corner_alt_height = 128.
	let track_straight = sprite({x= 130; y= 0; width= 128; height= 128})
	let track_straight_width = 128.
	let track_straight_height = 128.
	let track_straight_alt = sprite({x= 0; y= 1820; width= 128; height= 128})
	let track_straight_alt_width = 128.
	let track_straight_alt_height = 128.
	let trunk_bottom = sprite({x= 0; y= 1690; width= 128; height= 128})
	let trunk_bottom_width = 128.
	let trunk_bottom_height = 128.
	let trunk_mid = sprite({x= 0; y= 1560; width= 128; height= 128})
	let trunk_mid_width = 128.
	let trunk_mid_height = 128.
	let trunk_side = sprite({x= 0; y= 1430; width= 128; height= 128})
	let trunk_side_width = 128.
	let trunk_side_height = 128.
	let trunk_top = sprite({x= 0; y= 1300; width= 128; height= 128})
	let trunk_top_width = 128.
	let trunk_top_height = 128.
	let trunk_white_side = sprite({x= 0; y= 1170; width= 128; height= 128})
	let trunk_white_side_width = 128.
	let trunk_white_side_height = 128.
	let trunk_white_top = sprite({x= 0; y= 1040; width= 128; height= 128})
	let trunk_white_top_width = 128.
	let trunk_white_top_height = 128.
	let water = sprite({x= 0; y= 910; width= 128; height= 128})
	let water_width = 128.
	let water_height = 128.
	let wheat_stage1 = sprite({x= 0; y= 780; width= 128; height= 128})
	let wheat_stage1_width = 128.
	let wheat_stage1_height = 128.
	let wheat_stage2 = sprite({x= 0; y= 650; width= 128; height= 128})
	let wheat_stage2_width = 128.
	let wheat_stage2_height = 128.
	let wheat_stage3 = sprite({x= 0; y= 520; width= 128; height= 128})
	let wheat_stage3_width = 128.
	let wheat_stage3_height = 128.
	let wheat_stage4 = sprite({x= 0; y= 390; width= 128; height= 128})
	let wheat_stage4_width = 128.
	let wheat_stage4_height = 128.
	let wood = sprite({x= 0; y= 260; width= 128; height= 128})
	let wood_width = 128.
	let wood_height = 128.
	let wood_red = sprite({x= 0; y= 130; width= 128; height= 128})
	let wood_red_width = 128.
	let wood_red_height = 128.
	end

module ExtraItems = struct
  let source = "extra_items.png"

let arrow_fletched = sprite({x= 84; y= 0; width= 24; height= 140})
	let arrow_fletched_width = 24.
	let arrow_fletched_height = 140.
	let arrow_iron = sprite({x= 0; y= 0; width= 20; height= 140})
	let arrow_iron_width = 20.
	let arrow_iron_height = 140.
	let arrow_stone = sprite({x= 20; y= 0; width= 20; height= 140})
	let arrow_stone_width = 20.
	let arrow_stone_height = 140.
	let arrow_thinner = sprite({x= 0; y= 140; width= 24; height= 128})
	let arrow_thinner_width = 24.
	let arrow_thinner_height = 128.
	let arrow_thinner2 = sprite({x= 60; y= 0; width= 24; height= 130})
	let arrow_thinner2_width = 24.
	let arrow_thinner2_height = 130.
	let arrow_wood = sprite({x= 40; y= 0; width= 20; height= 140})
	let arrow_wood_width = 20.
	let arrow_wood_height = 140.
	let dirt_top = sprite({x= 0; y= 428; width= 128; height= 52})
	let dirt_top_width = 128.
	let dirt_top_height = 52.
	let grass_top = sprite({x= 0; y= 480; width= 128; height= 52})
	let grass_top_width = 128.
	let grass_top_height = 52.
	let grass_top_2 = sprite({x= 0; y= 376; width= 128; height= 52})
	let grass_top_2_width = 128.
	let grass_top_2_height = 52.
	let ice_top = sprite({x= 0; y= 268; width= 128; height= 54})
	let ice_top_width = 128.
	let ice_top_height = 54.
	let sand_top = sprite({x= 0; y= 322; width= 128; height= 54})
	let sand_top_width = 128.
	let sand_top_height = 54.
	end

(*$*)