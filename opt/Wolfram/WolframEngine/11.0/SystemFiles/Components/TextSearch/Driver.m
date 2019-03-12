Package["TextSearch`"]

PackageScope["$IndexedFileCount"]

PackageScope["DCreateEmptyIndex"]
PackageScope["DAddToIndex"]

PackageScope["ColumnQuery"]
PackageScope["SymbolicQuery"]
PackageScope["LuceneQuery"]

PackageScope["DQuery"]
PackageScope["DQueryBulkReturn"]
PackageScope["DQueryCount"]
PackageScope["DQueryReturnObject"]
PackageScope["DGetAllDocuments"]
PackageScope["DGetColumn"]
PackageScope["DCreateSearchResultSnippet"]
PackageScope["DSupportsQ"]
PackageScope["DGetSearchResultObjectInternalProperty"]

PackageExport["SearchResultObject"]

(* the driver API is entirely stateless: nothing here should depend
on previous API calls. *)

Options[DAddToIndex] =
{
	Method -> "BM25",				(*< The ranking method to use. ex. "BM25", "TFIDF" *)
	ContentFieldOptions -> None,	(*< an Association of field options, such as <|Language -> ..., ...|> *)
	Language -> Automatic,			(*< The language of the content being indexed. When Automatic, we attempt to automatically detect the language. *)
	SearchSynonyms -> None			(*< Synonyms to be used when searching. *)
};

DQuery[___] := $NotImplemented;
DQueryBulkReturn[___] := $NotImplemented;
DQueryCount[___] := $NotImplemented;
DQueryReturnObject[___] := $Failed;
DGetAllDocuments[___] := $NotImplemented;
DGetColumn[___] := $NotImplemented;

(* Function to get ResultObject properties that are internal without exposing them to the user
 * so that some processing can be done in an driver-agnostic fashion.
 *)
DGetSearchResultObjectInternalProperty[args___] := $Failed

DCreateSearchResultSnippet[args___] := $Failed

DSupportsQ[_, _] := False
DSupportsQ[_, ColumnQuery] := True
DSupportsQ[IndexHandle[i_], what_] := DSupportsQ[i, what]

PackageScope["$NotImplemented"]

PackageScope["NIFallback"]
SetAttributes[NIFallback, HoldAll];
NIFallback[body_, then___] := Replace[body, $NotImplemented :> NIFallback[then]];
NIFallback[] := $Failed;


PackageScope["DRemoveStaleDocumentsFromIndex"]
PackageScope["DGetIndexedFiles"]
PackageScope["DGetDocumentCount"]

PackageScope["DCreateHandle"]
PackageScope["DDeleteHandle"]
DCreateHandle[_, _, _] := $Failed;

PackageScope["$ValidDrivers"]

$ValidDrivers = {"CLucene", "Lucene", "OldLucene", "WLNative"};

(* Generic implementation of a "search result" for drivers
 * that don't yet implement it themselves.
 *)

DQueryReturnObject[handle_, query_] /; Not@DSupportsQ[handle, SearchResultObject] := SearchResultObject[handle, query, DQueryBulkReturn[handle, query]]
SearchResultObject[handle_, query_, res_]["Count"] := If[res==={}, 0, Length[res[[1]]]]
DGetSearchResultObjectInternalProperty[SearchResultObject[handle_, query_, res_], All] := res
DGetSearchResultObjectInternalProperty[o:SearchResultObject[handle_, query_, res_], Span[i_, j_]] := If[j <= o["Count"], Take[res, All, {i, j}], $Failed]

(* compound SearchResultObject *)
SearchResultObject[l:{__SearchResultObject}]["Count"] := Total[#["Count"]& /@ l]
SearchResultObject[l:{__SearchResultObject}][All] := Union@@(#[All]& /@ l)
(o:SearchResultObject[l:{__SearchResultObject}])[Span[i_,j_]] := If[j <= o["Count"], o[All][[i;;j]], $Failed]