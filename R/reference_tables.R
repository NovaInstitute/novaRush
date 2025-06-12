# create reference tables and context
# Namespace context for RDF output

#' make_surveycto_centext
#' @description
#' This function creates a context for SurveyCTO RDF output. It defines the prefixes and namespaces used in the RDF triples.
#' @returns
#' @export
#'
#' @examples
make_surveycto_centext <- function(){
  tribble(
    ~prefix, ~namespace,
    "xsd", "http://www.w3.org/2001/XMLSchema#",
    "rdfs", "http://www.w3.org/2000/01/rdf-schema#",
    "owl", "http://www.w3.org/2002/07/owl#",
    "survey", "https://w3id.org/survey-ontology#",
    "dcat", "http://www.w3.org/ns/dcat#",
    "dcterms", "http://purl.org/dc/terms/",
    "prov", "http://www.w3.org/ns/prov#",
    "foaf", "http://xmlns.com/foaf/0.1/",
    "vcard", "http://www.w3.org/2006/vcard/ns#",
    "geo", "http://www.w3.org/2003/01/geo/wgs84_pos#",
    "gsp", "http://www.opengis.net/ont/geosparql#",
    "time", "http://www.w3.org/2006/time#",
    "ma", "http://www.w3.org/ns/ma-ont#",
    "schema", "http://schema.org/",
    "skos", "http://www.w3.org/2004/02/skos/core#",
    "sioc", "http://rdfs.org/sioc/ns#",
    "gs1", "http://gs1.org/voc/"
  )

}

#' make_cto_semantic_mapping
#' @description This function creates a mapping between SurveyCTO field types and their corresponding semantic labels.
#' @returns
#' @export
#'
#' @examples
make_cto_semantic_mapping <- function(){
  tribble(
    ~CTO_label, ~Semantic_label,
    "audio", "dcat:Distribution",
    "barcode", "xsd:string",
    "begin_group", "survey:QuestionGroup",
    "begin_repeat", "survey:RepeatGroup",
    "calculate", "survey:CalculatedVariable",
    "date", "xsd:date",
    "datetime", "xsd:dateTime",
    "decimal", "xsd:decimal",
    "device_id", "prov:Entity",
    "end_group", "survey:QuestionGroup",
    "end_repeat", "survey:RepeatGroup",
    "file", "dcat:Distribution",
    "geopoint", "geo:Point",
    "geoshape", "geo:Geometry",
    "geotrace", "geo:Geometry",
    "hidden", "survey:HiddenField",
    "image", "foaf:Image",
    "integer", "xsd:integer",
    "note", "rdfs:comment",
    "phonenumber", "vcard:hasPhone",
    "photo", "foaf:Image",
    "range", "survey:ScaleQuestion",
    "rank", "survey:RankingQuestion",
    "select_multiple", "survey:MultipleChoiceQuestion",
    "select_one", "survey:SingleChoiceQuestion",
    "select_one_from_file", "survey:SingleChoiceQuestion",
    "signature", "foaf:Image",
    "start", "prov:startedAtTime",
    "subscript", "survey:SubscriptVariable",
    "text", "xsd:string",
    "time", "xsd:time",
    "today", "xsd:date",
    "username", "foaf:accountName",
    "video", "dcat:Distribution"
  )
}


#' make_extended_cto_semantic_mapping
#' @description
#' The `make_extended_cto_semantic_mapping` function creates an extended mapping between SurveyCTO field types and their corresponding semantic labels.
#'
#' @returns
#' @export
#'
#' @examples
make_extended_cto_semantic_mapping <- function(){
  tribble(
  ~CTO_label, ~Semantic_label, ~Alternative_label, ~Notes,
  "audio", "dcat:Distribution", "ma:MediaResource", "Media Ontology for audio files",
  "barcode", "xsd:string", "gs1:GTIN", "GS1 for product barcodes",
  "begin_group", "survey:QuestionGroup", "owl:Thing", "Generic grouping concept",
  "calculate", "survey:CalculatedVariable", "math:Expression", "Mathematical expressions",
  "date", "xsd:date", "time:DateTimeDescription", "Time ontology alternative",
  "datetime", "xsd:dateTime", "time:Instant", "Use time:Instant for temporal points",
  "decimal", "xsd:decimal", "xsd:double", "Use xsd:double for floating point",
  "device_id", "prov:Entity", "dcterms:identifier", "Dublin Core identifier",
  "file", "dcat:Distribution", "foaf:Document", "Use foaf:Document for general files",
  "geopoint", "geo:Point", "gsp:Point", "GeoSPARQL alternative",
  "geoshape", "geo:Geometry", "gsp:Geometry", "GeoSPARQL for complex shapes",
  "geotrace", "geo:Geometry", "gsp:LineString", "GeoSPARQL for line traces",
  "hidden", "survey:HiddenField", "ui:HiddenInput", "UI ontology for hidden fields",
  "image", "foaf:Image", "schema:ImageObject", "Schema.org for images",
  "integer", "xsd:integer", "xsd:int", "Use xsd:int for 32-bit integers",
  "note", "rdfs:comment", "skos:note", "SKOS for documentation notes",
  "phonenumber", "vcard:hasPhone", "schema:telephone", "Schema.org telephone",
  "range", "survey:ScaleQuestion", "survey:LikertScale", "Specific scale type",
  "rank", "survey:RankingQuestion", "survey:OrderingQuestion", "Question ordering",
  "select_multiple", "survey:MultipleChoiceQuestion", "survey:CheckboxQuestion", "More specific survey type",
  "select_one", "survey:SingleChoiceQuestion", "survey:RadioButtonQuestion", "More specific survey type",
  "signature", "foaf:Image", "schema:DigitalDocument", "Digital signature document",
  "start", "prov:startedAtTime", "time:hasBeginning", "Time ontology for start times",
  "text", "xsd:string", "rdfs:Literal", "Use rdfs:Literal for generic text",
  "time", "xsd:time", "time:TimePosition", "Time ontology alternative",
  "username", "foaf:accountName", "sioc:name", "SIOC ontology for user names",
  "video", "dcat:Distribution", "ma:MediaResource", "Media Ontology for video files"
  )
  }
