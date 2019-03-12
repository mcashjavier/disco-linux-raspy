System`Private`NewContextPath[{"System`","ExternalService`"}];

(Unprotect[#];Clear[#];)&/@{
	System`AnatomyForm,
	System`AnatomyPlot3D,
	System`SkinStyle
};

Unprotect["System`AnatomyPlot3D`Private`*"]
ClearAll["System`AnatomyPlot3D`Private`*"]

Begin["AnatomyPlot3D`Private`"]

anatomyFormRules = {
   System`AnatomyForm["Natural"] :> {All -> Inherited},
   System`AnatomyForm["Highlighted"] :> {All -> Directive[Red]},
   System`AnatomyForm[] :> {All -> FaceForm[]},
   System`AnatomyForm[
     rules : {_Rule ..}] :> DeleteDuplicatesBy[
     MapAt[
     	If[Head[#] === Directive, #, Directive[#]] &, 
     	rules, 
     	{{All, 2}}
     ],
     First],
   System`AnatomyForm[assoc_Association] :> 
    With[{rules = Normal[assoc, Association]/.Rule[Verbatim[_],val_]:>Rule[All, val]},
     DeleteDuplicatesBy[
     MapAt[
     	If[Head[#] === Directive, #, Directive[#]] &, 
     	rules, 
     	{{All, 2}}
     ],
     First]],
   System`AnatomyForm[
     arg_List] :> (Reverse[
      DeleteDuplicates[
       Reverse[((If[MatchQ[#, _Rule], #, 
              All -> (# /. {"Natural" -> Inherited, 
                  "Highlighted" -> Directive[Red]})] & /@ 
            Flatten[arg]) /. 
          Rule[entity_, dir_] :> 
           Rule[entity, 
            If[Head[dir] === Directive, dir, Directive[dir]]])], #1[[
          1]] === #2[[1]] &]]),
   System`AnatomyForm[
     arg_] :> ({All -> (arg /. {"Natural" -> Inherited, 
         "Highlighted" -> Directive[Red]})})};

applyAnatomyForm[e_Entity, af_AnatomyForm] := 
 Module[{initModelandTissueTypes, initOpts, initGeometryAndDirectives, 
 	styleRules, subPartStructureTypeRules, 
 	initAnatomyFormRules, newSubpart},
 	initAnatomyFormRules = af /. anatomyFormRules;
  (* The following retrieves the 3D model and the tissue types of the 
  subarts in a single EntityValue call *)
  initModelandTissueTypes = 
   EntityValue[e, {"Graphics3D", "ImageAtomsToTissueType"}];
  
  (* If the 3D model is Missing, exit immediately, 
  there is nothing that can be done *)
  If[! MatchQ[initModelandTissueTypes[[1]], _Graphics3D], 
   Return[initModelandTissueTypes[[1]]]];
  
  initOpts = Options[initModelandTissueTypes[[1]]];
  initGeometryAndDirectives = initModelandTissueTypes[[1, 1]];
  
  (* Break down each entity into its atomic parts, assign the directives to those subparts, *)
  (* deleting Missing atomic subparts *)
  styleRules = 
   With[{(* remove Rule with LHS of All since we can't get ImageAtoms of those *)
   	prunedInitAnatomyFormRules = DeleteCases[initAnatomyFormRules, Rule[All,_]]},
   	With[{ia = EntityValue[prunedInitAnatomyFormRules[[All, 1]], "ImageAtoms"]},
   		If[MatchQ[Length[prunedInitAnatomyFormRules[[All, 1]]], Length[ia]],
   			With[{imageAtomRules = (Rule @@@ Transpose[{prunedInitAnatomyFormRules[[All, 1]], ia}]) /. 
   				Rule[ent_, _Missing] :> Rule[ent, ent]},
   				Flatten[(initAnatomyFormRules /. ent_Entity :> (ent /. imageAtomRules)) /. 
   					Rule[ents : {_Entity ..}, dir_] :> (Rule[#, dir] & /@ ents)]], 
   			{}]]];
  (* Cleanup the above to remove All->Inherited inside of Directive *)
  styleRules = styleRules/.Rule[a_,Directive[All->Inherited]]:>Rule[a, Directive[Inherited]];
  (* Cleanup the above some more to remove Rule[All, Directive[Inherited,All->Inherited]] *)
  styleRules = DeleteCases[styleRules,Rule[All, Directive[Inherited,All->Inherited]]];
       
  If[styleRules === Rule[All, Inherited], 
   Return[
    Graphics3D[DeleteCases[initGeometryAndDirectives, Rule[All, Inherited]], 
     Sequence @@ initOpts]]];
  subPartStructureTypeRules = 
   With[{tmp = initModelandTissueTypes[[2]]}, 
    If[MatchQ[
      tmp, _Missing], {e -> (EntityValue[e, 
          "StructureType"] /. {x_} :> x)}, tmp]];
  
  (* Reconstruct a new Graphics3D by walking through each subpart and seeing 
  if it needs to be modified *)
  Graphics3D[
   (* map over all subparts *)
   (Function[modelSubpart,
       With[{subpartEntity = modelSubpart[[-1, 2]]["Entity"], 
         subpartStructureType = 
          modelSubpart[[-1, 2]]["Entity"] /. 
           subPartStructureTypeRules},
        newSubpart = modelSubpart;
        (* for each subpart, 
        map over all the AnatomyForm rules to see if any need applied, 
        checking both the TissueType and the entity itself *)
        Function[anatomyFormRule,
          If[(
          	((subpartStructureType === anatomyFormRule[[1]]) || 
          	 (subpartEntity === anatomyFormRule[[1]]))&&
             (!MatchQ[anatomyFormRule[[2]], Directive[Inherited]])
             ) ||
            ((anatomyFormRule[[1]]===All)&&
             (!((subpartStructureType === anatomyFormRule[[1]]) || 
             (subpartEntity === anatomyFormRule[[1]])))&&
             (!MatchQ[anatomyFormRule[[2]], Directive[Inherited]]) &&
             (FreeQ[styleRules, subpartEntity])
             ),
           newSubpart = {newSubpart[[1]] /. 
              Directive[d___] :> 
               Directive[d, Sequence @@ anatomyFormRule[[2]]], 
             newSubpart[[2]]}]] /@ styleRules; 
        newSubpart]] /@ 
      initGeometryAndDirectives) /. (Rule[All, Inherited]) :> (Sequence @@ {}), 
   Sequence @@ initOpts]
  ]

evaluateAnatomyForm[anatomyInput_List] := Module[{styleF, result},
  styleF[0] = System`AnatomyForm["Natural"];
  result = recurseAF[anatomyInput, {styleF, 0}];
  Remove[styleF];
  result
  ]
Clear[recurseAF];
recurseAF[list_List, {styleF_, level_}] := Module[{},
   (* Start new list with the anatomy form of the previous level as default *)
   styleF[level + 1] = styleF[level];
   recurseAF[#, {styleF, level + 1}] & /@ list
   ];
recurseAF[GraphicsGroup[gg_, opts:OptionsPattern[]], {styleF_, level_}] := Module[{},
   (* Start new list with the anatomy form of the previous level as default *)
   styleF[level + 1] = styleF[level];
   GraphicsGroup[recurseAF[#, {styleF, level + 1}] & /@ gg, opts]
   ];
recurseAF[Tooltip[arg1_, e:Entity["AnatomicalStructure", __], opts:OptionsPattern[]], {styleF_, level_}] := Module[{},
   (* Start new list with the anatomy form of the previous level as default *)
   styleF[level + 1] = styleF[level];
   Tooltip[Sequence@@(recurseAF[#, {styleF, level + 1}] & /@ {arg1}), e["Name"], opts]
   ];
recurseAF[Tooltip[arg1_, opts:OptionsPattern[]], {styleF_, level_}] := Module[{},
   (* Start new list with the anatomy form of the previous level as default *)
   styleF[level + 1] = styleF[level];
   With[{seq=Sequence@@(recurseAF[#, {styleF, level + 1}] & /@ {arg1})},
   	Tooltip[seq, arg1, opts]]
   ];
recurseAF[Tooltip[arg1_, arg2_, opts:OptionsPattern[]], {styleF_, level_}] := Module[{},
   (* Start new list with the anatomy form of the previous level as default *)
   styleF[level + 1] = styleF[level];
   Tooltip[Sequence@@(recurseAF[#, {styleF, level + 1}] & /@ {arg1}), arg2, opts]
   ];
recurseAF[Rotate[arg1_, {u_?VectorQ,v_?VectorQ}], {styleF_, level_}] := Module[{},
   (* Start new list with the anatomy form of the previous level as default *)
   styleF[level + 1] = styleF[level];
   Rotate[Sequence@@(recurseAF[#, {styleF, level + 1}] & /@ {arg1}), {u,v}]
   ];
recurseAF[Rotate[arg1_, theta_, w_?VectorQ], {styleF_, level_}] := Module[{},
   (* Start new list with the anatomy form of the previous level as default *)
   styleF[level + 1] = styleF[level];
   Rotate[Sequence@@(recurseAF[#, {styleF, level + 1}] & /@ {arg1}), theta, w]
   ];
recurseAF[Rotate[arg1_, theta_, w_?VectorQ, p_?VectorQ], {styleF_, level_}] := Module[{},
   (* Start new list with the anatomy form of the previous level as default *)
   styleF[level + 1] = styleF[level];
   Rotate[Sequence@@(recurseAF[#, {styleF, level + 1}] & /@ {arg1}), theta, w, p]
   ];
recurseAF[Rotate[arg1_, theta_, w_?VectorQ, p:Entity["AnatomicalStructure", __]], {styleF_, level_}] := Module[{},
   (* Start new list with the anatomy form of the previous level as default *)
   styleF[level + 1] = styleF[level];
   Rotate[Sequence@@(recurseAF[#, {styleF, level + 1}] & /@ {arg1}), theta, w, p["RegionCentroid"]]
   ];
recurseAF[Rotate[arg1_, theta_, {u_?VectorQ, v_?VectorQ}], {styleF_, level_}] := Module[{},
   (* Start new list with the anatomy form of the previous level as default *)
   styleF[level + 1] = styleF[level];
   Rotate[Sequence@@(recurseAF[#, {styleF, level + 1}] & /@ {arg1}), theta, {u, v}]
   ];
recurseAF[Scale[arg1_, arg2__], {styleF_, level_}] := Module[{},
   (* Start new list with the anatomy form of the previous level as default *)
   styleF[level + 1] = styleF[level];
   Scale[Sequence@@(recurseAF[#, {styleF, level + 1}] & /@ {arg1}), arg2]
   ];
recurseAF[GeometricTransformation[arg1_, arg2__], {styleF_, level_}] := Module[{},
   (* Start new list with the anatomy form of the previous level as default *)
   styleF[level + 1] = styleF[level];
   GeometricTransformation[Sequence@@(recurseAF[#, {styleF, level + 1}] & /@ {arg1}), arg2]
   ];
recurseAF[Translate[arg1_, arg2_], {styleF_, level_}] := Module[{},
   (* Start new list with the anatomy form of the previous level as default *)
   styleF[level + 1] = styleF[level];
   Translate[Sequence@@(recurseAF[#, {styleF, level + 1}] & /@ {arg1}), arg2]
   ];
recurseAF[Inset[arg1_, opts:OptionsPattern[]], {styleF_, level_}] := Module[{},
   (* Start new list with the anatomy form of the previous level as default *)
   styleF[level + 1] = styleF[level];
   Inset[Sequence@@(recurseAF[#, {styleF, level + 1}] & /@ {arg1}), opts]
   ];
recurseAF[Inset[arg1_, arg2:Entity["AnatomicalStructure", __], arg3___], {styleF_, level_}] := Module[{},
   (* Start new list with the anatomy form of the previous level as default *)
   styleF[level + 1] = styleF[level];
   Inset[Sequence@@(recurseAF[#, {styleF, level + 1}] & /@ {arg1}), arg2["RegionCentroid"], arg3]
   ];
recurseAF[Annotation[arg1_, arg2__], {styleF_, level_}] := Module[{},
   (* Start new list with the anatomy form of the previous level as default *)
   styleF[level + 1] = styleF[level];
   Annotation[Sequence@@(recurseAF[#, {styleF, level + 1}] & /@ {arg1}), arg2]
   ];
recurseAF[Button[arg1_, arg2__], {styleF_, level_}] := Module[{},
   (* Start new list with the anatomy form of the previous level as default *)
   styleF[level + 1] = styleF[level];
   Button[Sequence@@(recurseAF[#, {styleF, level + 1}] & /@ {arg1}), arg2]
   ];
recurseAF[EventHandler[arg1_, arg2__], {styleF_, level_}] := Module[{},
   (* Start new list with the anatomy form of the previous level as default *)
   styleF[level + 1] = styleF[level];
   EventHandler[Sequence@@(recurseAF[#, {styleF, level + 1}] & /@ {arg1}), arg2]
   ];
recurseAF[Hyperlink[arg1_, arg2__], {styleF_, level_}] := Module[{},
   (* Start new list with the anatomy form of the previous level as default *)
   styleF[level + 1] = styleF[level];
   Hyperlink[Sequence@@(recurseAF[#, {styleF, level + 1}] & /@ {arg1}), arg2]
   ];
recurseAF[Mouseover[arg1_, arg2__], {styleF_, level_}] := Module[{},
   (* Start new list with the anatomy form of the previous level as default *)
   styleF[level + 1] = styleF[level];
   Mouseover[Sequence@@(recurseAF[#, {styleF, level + 1}] & /@ {arg1}), Sequence@@(recurseAF[#, {styleF, level + 1}] & /@ {arg2})]
   ];
recurseAF[PopupWindow[arg1_, arg2__], {styleF_, level_}] := Module[{},
   (* Start new list with the anatomy form of the previous level as default *)
   styleF[level + 1] = styleF[level];
   PopupWindow[Sequence@@(recurseAF[#, {styleF, level + 1}] & /@ {arg1}), arg2]
   ];
recurseAF[StatusArea[arg1_, arg2__], {styleF_, level_}] := Module[{},
   (* Start new list with the anatomy form of the previous level as default *)
   styleF[level + 1] = styleF[level];
   StatusArea[Sequence@@(recurseAF[#, {styleF, level + 1}] & /@ {arg1}), arg2]
   ];
recurseAF[af_AnatomyForm, {styleF_, level_}] := (
   styleF[level] = mergeStyles[styleF[level], af];
   Nothing
   );
recurseAF[dir_?directiveQ, {styleF_, level_}] := (
   styleF[level] = 
    mergeStyles[styleF[level], System`AnatomyForm[All -> dir]];
   dir
   );

recurseAF[prim_, {styleF_, level_}] := prim;

directiveQ[dir_] := System`Dump`ValidDirective @@ {dir}

recurseAF[
   entity : Entity["AnatomicalStructure", __], {styleF_, level_}] := 
  applyAnatomyForm[entity, styleF[level]];

mergeStyles[System`AnatomyForm[dir1___], 
  System`AnatomyForm[dir2___]] := Module[{expandDirectives, mergeResultsAndVerifyRules, cleanupInheriteds},
  expandDirectives = (If[MatchQ[#,_Rule], #, Rule[All, #]]&/@Flatten[{
  	If[dir1===Null, FaceForm[],{Normal[dir1]/.Rule[Verbatim[_],val_]:>Rule[All, val]}], 
  	If[dir2===Null, FaceForm[],{Normal[dir2]/.Rule[Verbatim[_],val_]:>Rule[All, val]}]} /. "Natural" -> (Rule[All, Inherited])])/. Rule[ec:EntityClass["AnatomicalStructure", _], style_]:> Sequence@@(Rule[#,style]&/@EntityList[ec]);
  mergeResultsAndVerifyRules = ((Rule[#[[1,1]], Directive@@(#[[All,2]]/.Directive[x__]:>Sequence[x])]&/@GatherBy[expandDirectives,#[[1]]&])/.Directive[x_]:>x);
  cleanupInheriteds = (mergeResultsAndVerifyRules /. Directive[x___, Inherited, y___] :> Directive[y] /; FreeQ[{y}, Inherited])/.Directive[]:>Inherited;
  System`AnatomyForm[cleanupInheriteds]]

Clear[System`AnatomyPlot3D]

anatomyOptionDefaults = {
	System`SkinStyle -> None
}

Options[System`AnatomyPlot3D] = SortBy[
		Join[
			Options[Graphics3D],
			anatomyOptionDefaults
		], ToString
	];

SetOptions[System`AnatomyPlot3D, {Boxed -> False, Lighting -> "Neutral", ViewPoint -> {0,-1.9,0}}];

duplicateFilter[opts___] :=
 DeleteCases[FilterRules[Flatten[{opts, FilterRules[Options[System`AnatomyPlot3D], Except[Flatten[{opts}]]]}],
  Options[Graphics3D]], Alternatives@@Options[Graphics3D]]
  
(*nickl note: statically assigning this rather than re-generating every time iAnatomyPlot3D is called*)
$grules = {
	Text[str_, rest__] :> 
     Text[str, Sequence@@({rest} /. {e : Entity["AnatomicalStructure", _] :> 
          e["RegionCentroid"]})],
    Polygon[p_, opts___] :> 
     Polygon[(p /. {e : Entity["AnatomicalStructure", _] :> 
          e["RegionCentroid"]}), opts],
    Line[l_, opts___] :> 
     Line[(l /. {e : Entity["AnatomicalStructure", _] :> 
          e["RegionCentroid"]}), opts],
    Arrow[l_,setback___] :> 
     Arrow[(l /. {e : Entity["AnatomicalStructure", _] :> 
          e["RegionCentroid"]}), Sequence@@({setback} /. {e : Entity["AnatomicalStructure", _] :> (e["EnclosingSphere"][[2]])})],
    Tube[p1_, 
      p2___, opts:OptionsPattern[]] :> (Tube[
      p1 /. {e : Entity["AnatomicalStructure", _] :> e["RegionCentroid"]}, 
      Sequence@@({p2} /. {e : Entity["AnatomicalStructure", _] :> (e["EnclosingSphere"][[2]])}), opts]),
    Arrow[Tube[p1_, 
      p2___, opts:OptionsPattern[]], p3___] :> Arrow[Tube[
      p1 /. {e : Entity["AnatomicalStructure", _] :> e["RegionCentroid"]}, 
      Sequence@@({p2} /. {e : Entity["AnatomicalStructure", _] :> (e["EnclosingSphere"][[2]])}), opts], 
      Sequence@@({p3} /. {e : Entity["AnatomicalStructure", _] :> (e["EnclosingSphere"][[2]])})],
    Arrow[(h:Line|BSplineCurve|BezierCurve)[p1_, opts:OptionsPattern[]], p2___] :> Arrow[h[
      p1 /. {e : Entity["AnatomicalStructure", _] :> e["RegionCentroid"]}, opts], 
      Sequence@@({p2} /. {e : Entity["AnatomicalStructure", _] :> (e["EnclosingSphere"][[2]])})],
    Point[p_, opts___] :> 
     Point[p /. {e : Entity["AnatomicalStructure", _] :> 
         e["RegionCentroid"]}, opts],
    Sphere[centers_, r___] :> 
     Sphere[(centers /. {e : Entity["AnatomicalStructure", _] :> 
          e["RegionCentroid"]}), Sequence@@({r} /. {e : Entity["AnatomicalStructure", _] :> (e["EnclosingSphere"][[2]])})],
    SphericalShell[centers_, r___] :> 
     SphericalShell[(centers /. {e : Entity["AnatomicalStructure", _] :> 
          e["RegionCentroid"]}), Sequence@@({r} /. {e : Entity["AnatomicalStructure", _] :> (e["EnclosingSphere"][[2]])})],
    Ball[centers_, r___] :> 
     Ball[(centers /. {e : Entity["AnatomicalStructure", _] :> 
          e["RegionCentroid"]}), Sequence@@({r} /. {e : Entity["AnatomicalStructure", _] :> (e["EnclosingSphere"][[2]])})],
    Cylinder[p1_, 
      p2___] :> (Cylinder[
      p1 /. {e : Entity["AnatomicalStructure", _] :> e["RegionCentroid"]}, 
      Sequence@@({p2} /. {e : Entity["AnatomicalStructure", _] :> (e["EnclosingSphere"][[2]])})]),
    Cuboid[p1_, 
      p2___] :> (Cuboid[p1, 
        p2] /. {e : Entity["AnatomicalStructure", _] :> 
         e["RegionCentroid"]}),
    Parallelepiped[p_, v_] :> 
     Parallelepiped[(p /. {e : Entity["AnatomicalStructure", _] :> 
          e["RegionCentroid"]}), v /. {e : Entity["AnatomicalStructure", _] :> 
          e["RegionCentroid"]}], 
    Hexahedron[p_, opts___] :> 
     Hexahedron[(p /. {e : Entity["AnatomicalStructure", _] :> 
          e["RegionCentroid"]}), opts],
    Tetrahedron[p_, opts___] :> 
     Tetrahedron[(p /. {e : Entity["AnatomicalStructure", _] :> 
          e["RegionCentroid"]}), opts],
    Triangle[g_, opts___] :> 
     Triangle[(g/.{e : Entity["AnatomicalStructure", _] :> e["RegionCentroid"]}), opts],
    Pyramid[p_, opts___] :> 
     Pyramid[(p /. {e : Entity["AnatomicalStructure", _] :> 
          e["RegionCentroid"]}), opts],
    Prism[p_, opts___] :> 
     Prism[(p /. {e : Entity["AnatomicalStructure", _] :> 
          e["RegionCentroid"]}), opts],
    Simplex[p_, opts___] :> 
     Simplex[(p /. {e : Entity["AnatomicalStructure", _] :> 
          e["RegionCentroid"]}), opts],
    Cone[p_, r___] :> 
     Cone[(p /. {e : Entity["AnatomicalStructure", _] :> 
          e["RegionCentroid"]}), Sequence@@({r} /. {e : Entity["AnatomicalStructure", _] :> (e["EnclosingSphere"][[2]])})],
    BezierCurve[p_, opts___] :> 
     BezierCurve[(p /. {e : Entity["AnatomicalStructure", _] :> 
          e["RegionCentroid"]}), opts],
    BSplineCurve[p_, opts___] :> 
     BSplineCurve[(p /. {e : Entity["AnatomicalStructure", _] :> 
          e["RegionCentroid"]}), opts],
    BSplineSurface[p_] :> 
     BSplineSurface[(p /. {e : Entity["AnatomicalStructure", _] :> 
          e["RegionCentroid"]})],
    Rule[Lighting, val_]:>Rule[Lighting, val/.(e : Entity["AnatomicalStructure", __]:>e["RegionCentroid"])],
    AffineHalfSpace[g__] :> 
     AffineHalfSpace[Sequence@@({g}/.{e : Entity["AnatomicalStructure", _] :> e["RegionCentroid"]})],
    AffineSpace[g__] :> 
     AffineSpace[Sequence@@({g}/.{e : Entity["AnatomicalStructure", _] :> e["RegionCentroid"]})],
    ConicHullRegion[g__] :> 
     ConicHullRegion[Sequence@@({g}/.{e : Entity["AnatomicalStructure", _] :> e["RegionCentroid"]})],
    CapsuleShape[g_, rest_] :> 
     CapsuleShape[(g/.{e : Entity["AnatomicalStructure", _] :> e["RegionCentroid"]}), rest],
    GraphicsComplex[g_, rest_, opts___] :> 
     GraphicsComplex[(g/.{e : Entity["AnatomicalStructure", _] :> e["RegionCentroid"]}), rest, opts],
    Ellipsoid[g_, rest_] :> 
     Ellipsoid[(g/.{e : Entity["AnatomicalStructure", _] :> e["RegionCentroid"]}), rest /.{e : Entity["AnatomicalStructure", _] :> (e["EnclosingSphere"][[2]])}],
    HalfLine[g__] :> 
     HalfLine[Sequence@@({g}/.{e : Entity["AnatomicalStructure", _] :> e["RegionCentroid"]})],
    HalfPlane[g__] :> 
     HalfPlane[Sequence@@({g}/.{e : Entity["AnatomicalStructure", _] :> e["RegionCentroid"]})],
    Hyperplane[g__] :> 
     Hyperplane[Sequence@@({g}/.{e : Entity["AnatomicalStructure", _] :> e["RegionCentroid"]})],
    HalfSpace[g__] :> 
     HalfSpace[Sequence@@({g}/.{e : Entity["AnatomicalStructure", _] :> e["RegionCentroid"]})],
    Text[Style[g:Entity["AnatomicalStructure", _], rest1__], rest2__] :> Text[Style[g, rest1], rest2],
    (* Handle Style for convenience at SW insistence, jfultz opposition, per mtrott plan to leave undocumented *) 
    Style[g:Entity["AnatomicalStructure", _], rest__] :> {If[Length[{rest}]>1, AnatomyForm[{rest}], AnatomyForm[rest]], g},
    Style[g:{Entity["AnatomicalStructure", _]..}, rest__] :> {If[Length[{rest}]>1, AnatomyForm[{rest}], AnatomyForm[rest]], g}
};

Clear[System`AnatomyPlot3D];
Clear[iAnatomyPlot3D];
iAnatomyPlot3D[anatomyInput_, opts : OptionsPattern[System`AnatomyPlot3D]] := 
 Module[{expandedFirstArgument, inputEntities, optEntities, fullInputEntities, finalOpts, skinStyleValue, iAnatomyInput, models, entityModelPairs, missingModels},
  iAnatomyInput = If[MatchQ[anatomyInput,_List], anatomyInput, {anatomyInput}];
  (* expand the 1st argument, including the AnatomyForm directives and handle any nested AnatomyForms *)
  expandedFirstArgument = evaluateAnatomyForm[{iAnatomyInput /. $grules}/.AnatomyForm[AnatomyForm[x_]]:>AnatomyForm[x]];
  inputEntities = Cases[iAnatomyInput, _Entity, Infinity];
  optEntities = Cases[{opts}, _Entity, Infinity];
  (* re-fetch models, should be cached, to see if any are missing *)
  fullInputEntities = DeleteDuplicates[Join[inputEntities, optEntities]];
  models = EntityValue[fullInputEntities, "Graphics3D"];
  entityModelPairs = Transpose[{fullInputEntities, models}];
  missingModels = Cases[entityModelPairs, {e_Entity, _Missing}:>e];
  Message[MessageName[AnatomyPlot3D, "missmod"], HoldForm[#]]&/@missingModels;
  (* the following strips out the explicit options of Graphics3D. No need to bake in the results *)
  (* unless the options are different than the default Graphics3D options. Also, allows the PlotRange to support *)
  (* an Entity spec and sorts the list of options *)
  finalOpts = 
   DeleteCases[duplicateFilter[opts]/.{
   	Rule[PlotRange, e : Entity["AnatomicalStructure", __]] :> Rule[PlotRange, e["RegionBounds"]/._Missing :> Automatic], 
   	Rule[Lighting, val_]:> With[{rc=val/.(e : Entity["AnatomicalStructure", __]:>e["RegionCentroid"])}, 
   		Rule[Lighting, If[FreeQ[rc, _Missing], rc, "Neutral"]]],
   	Rule[ViewVector, val_]:>With[{rc = val/.(e : Entity["AnatomicalStructure", __]:>e["RegionCentroid"])},
   		Rule[ViewVector, If[FreeQ[rc, _Missing], rc, Automatic]]]
   	},Alternatives@@Options[Graphics3D]];
  Block[{Inherited = (Sequence @@ {})},
  	(* First check if the SkinStyling option needs to be applied *)
  	skinStyleValue = System`SkinStyle/.{opts};
  	If[!MatchQ[skinStyleValue, None]&&!MatchQ[skinStyleValue, System`SkinStyle],
   With[{
   	g3D=Graphics3D[(expandedFirstArgument /. Graphics3D[x_, ___] :> x), finalOpts], 
   	asp = DeleteDuplicates[DeleteMissing[EntityValue[inputEntities, "AssociatedSkinPart"]]]},
   	Graphics3D[{g3D[[1]],
   		If[MatchQ[asp, {_Entity..}],
   		(#[[1]]&/@(EntityValue[asp, "Graphics3D"]))/.Directive[x__]:>Directive[x,
   		If[MatchQ[skinStyleValue, Automatic], 
   		Opacity[.3], 
   		If[directiveQ[skinStyleValue], skinStyleValue, Sequence@@{}]]],{}]}/._Missing->Sequence@@{}, finalOpts]],
   Graphics3D[(expandedFirstArgument /. Graphics3D[x_, ___] :> x)/._Missing->Sequence@@{}, finalOpts]]]
  ]

iAnatomyPlot3D[opts : OptionsPattern[System`AnatomyPlot3D]] := Graphics3D[{},
	duplicateFilter[opts]]

System`AnatomyPlot3D[args___, 
   opts : OptionsPattern[System`AnatomyPlot3D]] /; (ArgumentCountQ[System`AnatomyPlot3D, 
    Length[DeleteCases[{args}, _Rule, Infinity]], 0, 1]) := 
 Block[{res},
   res = iAnatomyPlot3D[args, Sequence@@Flatten[{opts}]];
   res /; ! MatchQ[res, _Missing | $Failed]] /; 
  FreeQ[{args}, _Rule, {1}]

End[];
  
SetAttributes[
	{AnatomyPlot3D, AnatomyForm, SkinStyle},
	{ReadProtected, Protected}
];

  
  
System`Private`RestoreContextPath[];