-- concatenates a list of strings
let concatList = lam list.
    foldl concat [] list

-- returns a table data element with the given characteristics
let makeTDElem = lam color. lam elem_width. lam elem_height. lam quote.
    foldl concat [] ["<td ",
        "bgcolor=",quote,color,quote,
        " width=",quote,(int2string elem_width),quote,
        " height=",quote,(int2string elem_height),quote,
        "></td>\n"]

utest makeTDElem "green" 1 2 "\"" with "<td bgcolor=\"green\" width=\"1\" height=\"2\"></td>\n"

let circUnconnectedToDot = lam name. lam quote. lam settings. lam value_str.
    let figName = concat name "fig" in
    concatList [figName,"[id=",quote,figName,quote," ","label=",quote,quote,settings.0," xlabel=",quote,value_str,quote," ","];",
                name,"[id=",quote,name,quote," shape=point style=filled color=black height=0.05 width=0.05",
                "];",
                figName,"--",name,";"]

-- gets the resistor component in dot
let resistorToDot = lam quote. lam name. lam value. lam custom_settings. lam isConnected.
    let settings = match custom_settings with Some (setting,unit) then (setting,unit) else
                ("style=filled color=black fillcolor=none shape=rect height=0.1 width=0.3 "," &Omega;") in
    if not (isConnected) then circUnconnectedToDot name quote settings (concat value settings.1) else
    concatList [name,"[id=",quote,name,quote,
                " xlabel=",quote,value,settings.1,quote," ",
                settings.0,
                " label=",quote,quote,"];"]

-- gets the battery component in dot
let circBatteryToDot = lam quote. lam name. lam value. lam custom_settings. lam isConnected.
    let side_width = 1 in
    let center_width = 10 in
    let side_height = 5 in
    let center_height = 10 in
    
    let settings = match custom_settings with Some (setting,unit) then (setting,unit) else
        let setting = foldl concat [] ["shape=none, color=none height=0 width=0 margin=0 label=<
        <table BORDER=",quote,"0",quote," CELLBORDER=",quote,"0",quote," CELLSPACING=",quote,"0",quote," CELLPADDING=",quote,"0",quote,"> 
            <tr>",
                (foldl (lam str. lam x. concat str (makeTDElem x.0 x.1 x.2 quote))) "" 
                    [("black",side_width,side_height),("none",center_width,side_height),("none",side_width,side_height)],
            "</tr> 
            <tr>",
                (foldl (lam str. lam x. concat str (makeTDElem x.0 x.1 x.2 quote))) "" 
                    [("black",side_width,side_height),("none",center_width,center_height),("black",side_width,side_height)],
            "</tr>
            <tr>",
                (foldl (lam str. lam x. concat str (makeTDElem x.0 x.1 x.2 quote))) "" 
                    [("black",side_width,side_height), ("none",center_width,side_height),("none",side_width,side_height)],
            "</tr>   
        </table>>"
    ] in (setting,"V") in
    if not (isConnected) then circUnconnectedToDot name quote settings (concat value settings.1) else
    concatList [name,"[id=",quote,name,quote," ",
                        "xlabel=",quote,value,settings.1,quote," ",
                        settings.0,"];"]


-- gets the ground component in dot
let circGroundToDot = lam quote. lam name. lam custom_settings. lam isConnected.
    let width =5 in
    let height = 1 in
    let settings = match custom_settings with Some (setting,unit) then (setting,unit) else
        let w = foldl concat [] ["shape=none, color=none height=0 width=0 margin=0 label=<
    <table CELLBORDER=",quote,"0",quote," CELLSPACING=",quote,"0",quote," CELLPADDING=",quote,"0",quote," >\n<tr>",
            (foldl (lam str. lam x. concat str (makeTDElem x width height quote))) "" ["black","black","black","black","black"],
        " </tr>\n<tr>",
           makeTDElem "none" width (muli 2 height) quote,
       "</tr>\n<tr>",
            (foldl (lam str. lam x. concat str (makeTDElem x width height quote))) "" ["none","black","black","black","none"],
        "</tr>\n<tr>",
            makeTDElem "none" width (muli 2 height) quote,
        "</tr>\n<tr>",
            (foldl (lam str. lam x. concat str (makeTDElem x width height quote))) "" ["none","none","black","none","none"],
        "</tr>\n</table>> "] in
        (w,"") in
    if not (isConnected) then circUnconnectedToDot name quote settings ""
    else foldl concat [] [name,"[id=",quote,name,quote," ","label=",quote,quote,settings.0," ","];"]

let circOtherToDot = lam quote. lam name. lam value. lam _. lam custom_settings. lam isConnected.
    
    let settings = match custom_settings with Some (setting,unit) then (setting,unit) else
        (foldl concat [] ["style=filled fillcolor=white shape=circle label=",quote,quote, " xlabel=",quote,value,quote]," ") in
    let value_str = match value with "" then "" else (foldl concat [] [value," ",settings.1," "]) in
    if not (isConnected) then circUnconnectedToDot name quote settings value_str
    else concatList [name,"[id=",quote,name,quote," ",
                "xlabel=",quote,value_str,quote," ",
                settings.0,"];"]

-- returns a component in dot.
let componentToDot = lam comp. lam quote. lam fig_settings.
    match comp with Component (comp_type,name,maybe_value,isConnected) then
        let figure_setting = 
            let fig = find (lam x. setEqual eqchar x.0 comp_type) fig_settings in
            match fig with Some (_,setting,unit) then Some (setting,unit) else None() in
        -- round to integer
        let value_str = match maybe_value with None () then "" else int2string (roundfi maybe_value) in
        match comp_type with "resistor" then
            resistorToDot quote name value_str figure_setting isConnected
        else match comp_type with "battery" then
            circBatteryToDot quote name value_str figure_setting isConnected
        else match comp_type with "ground" then
            circGroundToDot quote name figure_setting isConnected
        else 
            circOtherToDot quote name value_str "unit" figure_setting isConnected
    else []
